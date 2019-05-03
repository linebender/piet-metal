//  Copyright 2019 The xi-editor authors.

#include <metal_stdlib>
using namespace metal;

#include "PietShaderTypes.h"

struct RenderData {
    float4 clipSpacePosition [[position]];
    float2 textureCoordinate;
};

vertex RenderData
vertexShader(uint vertexID [[ vertex_id ]],
             constant RenderVertex *vertexArray [[ buffer(RenderVertexInputIndexVertices) ]])
{
    RenderData out;
    float2 clipSpacePosition = vertexArray[vertexID].position;
    out.clipSpacePosition.xy = clipSpacePosition;
    out.clipSpacePosition.z = 0.0;
    out.clipSpacePosition.w = 1.0;
    out.textureCoordinate = vertexArray[vertexID].textureCoordinate;
    return out;
}

fragment float4 fragmentShader(RenderData in [[stage_in]],
                               texture2d<half> texture [[ texture(0) ]]) {
    const half4 sample = texture.read(uint2(in.textureCoordinate));
    return float4(sample);
}

// Distance field rendering of strokes

// Accumulate distance field.
void stroke(thread float &df, float2 pos, float2 start, float2 end) {
    float2 lineVec = end - start;
    float2 dPos = pos - start;
    float t = saturate(dot(lineVec, dPos) / dot(lineVec, lineVec));
    float field = length(lineVec * t - dPos);
    df = min(df, field);
}

// TODO: figure out precision so we can move more stuff to half
half renderDf(float df, float halfWidth) {
    return saturate(halfWidth + 0.5 - df);
}

// Data structures for representing per-tile command list.

// The commands are sequential in the tile, with an opcode as the first ushort.
// Then I use pointer casting, but perhaps a union would be more appropriate?

struct CmdEnd {
    ushort cmd;
};

// Render a circle. Used mainly for debugging (color is fixed and
// coordinates are limited to integers), but could be adapted to
// real use.
struct CmdCircle {
    ushort cmd;
    packed_ushort4 bbox;
};

// Render one line segment to the distance field buffer.
struct CmdLine {
    ushort cmd;
    packed_float2 start;
    packed_float2 end;
};

// Draw a stroke based on the distance field buffer.
struct CmdStroke {
    ushort cmd;
    half halfWidth;
    uint rgba;
};

// Accumulate one line segment to the signed area buffer.
struct CmdFill {
    ushort cmd;
    packed_float2 start;
    packed_float2 end;
};

// Accumulate line intersecting the left edge of a tile to the signed area buffer.
struct CmdFillEdge {
    ushort cmd;
    // This is only one bit, it's a bit wasteful
    float sign;
    // Y coordinate at point of intersection with left edge.
    float y;
};

// Draw a fill based on the signed area buffer.
struct CmdDrawFill {
    ushort cmd;
    short backdrop;
    uint rgba;
};

// Maybe these should be an enum.
#define CMD_END 0
#define CMD_CIRCLE 1
#define CMD_LINE 2
#define CMD_STROKE 3
#define CMD_FILL 4
#define CMD_FILL_EDGE 5
#define CMD_DRAW_FILL 6

struct TileEncoder {
public:
    TileEncoder(device char *dst) {
        this->dst = dst;
    }
    void encodeCircle(ushort4 bbox) {
        device CmdCircle *cmd = (device CmdCircle *)dst;
        cmd->cmd = CMD_CIRCLE;
        cmd->bbox = bbox;
        dst += sizeof(CmdCircle);
    }
    void encodeLine(const device PietStrokeLine &line) {
        device CmdLine *cmd = (device CmdLine *)dst;
        cmd->cmd = CMD_LINE;
        cmd->start = line.start;
        cmd->end = line.end;
        dst += sizeof(CmdLine);
    }
    void encodeStroke(const device PietStrokeLine &line) {
        device CmdStroke *cmd = (device CmdStroke *)dst;
        cmd->cmd = CMD_STROKE;
        cmd->rgba = line.rgbaColor;
        cmd->halfWidth = 0.5 * line.width;
        dst += sizeof(CmdStroke);
    }
    void encodeFill(float2 start, float2 end) {
        device CmdFill *cmd = (device CmdFill *)dst;
        cmd->cmd = CMD_FILL;
        cmd->start = start;
        cmd->end = end;
        dst += sizeof(CmdFill);
    }
    void encodeFillEdge(float sign, float y) {
        device CmdFillEdge *cmd = (device CmdFillEdge *)dst;
        cmd->cmd = CMD_FILL_EDGE;
        cmd->sign = sign;
        cmd->y = y;
        dst += sizeof(CmdFillEdge);
    }
    void encodeDrawFill(const device PietFill &fill, int backdrop) {
        device CmdDrawFill *cmd = (device CmdDrawFill *)dst;
        cmd->cmd = CMD_DRAW_FILL;
        cmd->backdrop = backdrop;
        cmd->rgba = fill.rgbaColor;
        dst += sizeof(CmdDrawFill);
    }
    void end() {
        device CmdEnd *cmd = (device CmdEnd *)dst;
        cmd->cmd = CMD_END;
    }
private:
    // Pointer to command buffer for tile.
    device char *dst;
};

// Traverse the scene graph and produce a command list for a tile.
kernel void
tileKernel(device const char *scene [[buffer(0)]],
           device char *tiles [[buffer(1)]],
           uint2 gid [[thread_position_in_grid]],
           uint tix [[thread_index_in_simdgroup]])
{
    uint tileIx = gid.y * maxTilesWidth + gid.x;
    ushort x0 = gid.x * tileWidth;
    ushort y0 = gid.y * tileHeight;
    device char *dst = tiles + tileIx * tileBufSize;
    TileEncoder encoder(dst);
    
    // Size of the region covered by one SIMD group. TODO, don't hardcode.
    const ushort stw = 16 * 16;
    const ushort sth = 2 * 16;
    ushort sx0 = x0 & ~(stw - 1);
    ushort sy0 = y0 & ~(sth - 1);
    
    device const SimpleGroup *group = (device const SimpleGroup *)scene;
    device const ushort4 *bboxes = (device const ushort4 *)&group->bbox[0];
    uint n = group->nItems;
    device const PietItem *items = (device const PietItem *)(scene + group->itemsIx);
    for (uint i = 0; i < n; i += 32) {
        bool hit = false;
        if (i + tix < n) {
            ushort4 bbox = bboxes[i + tix];
            hit = bbox.z >= sx0 && bbox.x < sx0 + stw && bbox.w >= sy0 && bbox.y < sy0 + sth;
        }
        simd_vote vote = simd_ballot(hit);
        uint v = simd_vote::vote_t(vote);
        while (v) {
            uint k = ctz(v);
            // To explore: use simd_broadcast rather then second global memory read.
            uint ix = i + k;
            ushort4 bbox = bboxes[ix];
            if (bbox.z >= x0 && bbox.x < x0 + tileWidth && bbox.w >= y0 && bbox.y < y0 + tileHeight) {
                ushort itemType = items[ix].itemType;
                switch (itemType) {
                    case PIET_ITEM_CIRCLE:
                        encoder.encodeCircle(bbox);
                        break;
                    case PIET_ITEM_LINE: {
                        // set up line equation, ax + by + c = 0
                        device const PietStrokeLine &line = items[ix].line;
                        float a = line.end.y - line.start.y;
                        float b = line.start.x - line.end.x;
                        float c = -(a * line.start.x + b * line.start.y);
                        // TODO: is this bound as tight as it can be?
                        float hw = 0.5 * line.width + 0.5;
                        float left = a * (x0 - hw);
                        float right = a * (x0 + tileWidth + hw);
                        float top = b * (y0 - hw);
                        float bot = b * (y0 + tileHeight + hw);
                        // If all four corners are on same side of line, cull
                        float s00 = sign(top + left + c);
                        float s01 = sign(top + right + c);
                        float s10 = sign(bot + left + c);
                        float s11 = sign(bot + right + c);
                        if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0) {
                            encoder.encodeLine(line);
                            encoder.encodeStroke(line);
                        }
                        break;
                    }
                    case PIET_ITEM_FILL: {
                        device const PietFill &fill = items[ix].fill;
                        device const float2 *pts = (device const float2 *)(scene + fill.pointsIx);
                        uint nPoints = fill.nPoints;
                        float backdrop = 0;
                        bool anyFill = false;
                        // TODO: use simd ballot to quick-reject segments with no contribution
                        for (uint j = 0; j < nPoints; j++) {
                            float2 start = pts[j];
                            float2 end = pts[j + 1 == nPoints ? 0 : j + 1];
                            float ymin = min(start.y, end.y);
                            float ymax = max(start.y, end.y);
                            if (ymax >= y0 && ymin < y0 + tileHeight) {
                                // set up line equation, ax + by + c = 0
                                float a = end.y - start.y;
                                float b = start.x - end.x;
                                float c = -(a * start.x + b * start.y);
                                float left = a * x0;
                                float right = a * (x0 + tileWidth);
                                float ytop = max(float(y0), ymin);
                                float ybot = min(float(y0 + tileHeight), ymax);
                                float top = b * ytop;
                                float bot = b * ybot;
                                // top left of tile
                                float sTopLeft = sign(left + float(y0) * b + c);
                                float s00 = sign(top + left + c);
                                float s01 = sign(top + right + c);
                                float s10 = sign(bot + left + c);
                                float s11 = sign(bot + right + c);
                                if (sTopLeft == sign(a) && ymin <= y0) {
                                    backdrop -= s00;
                                }
                                if (min(start.x, end.x) < x0 && max(start.x, end.x) > x0) {
                                    float yEdge = mix(start.y, end.y, (start.x - x0) / b);
                                    if (yEdge >= y0 && yEdge < y0 + tileHeight) {
                                        // line intersects left edge of this tile
                                        encoder.encodeFillEdge(s00, yEdge);
                                        if (b > 0.0) {
                                            encoder.encodeFill(start, float2(x0, yEdge));
                                        } else {
                                            encoder.encodeFill(float2(x0, yEdge), end);
                                        }
                                        anyFill = true;
                                    } else if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0) {
                                        encoder.encodeFill(start, end);
                                        anyFill = true;
                                    }
                                } else if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0) {
                                    encoder.encodeFill(start, end);
                                    anyFill = true;
                                }
                            }
                        }
                        if (anyFill || backdrop != 0.0) {
                            encoder.encodeDrawFill(fill, backdrop);
                        }
                        break;
                    }
                }
            }
            v &= ~(1 << k);  // aka v &= (v - 1)
        }
    }
    encoder.end();
}

// Interpret the commands in the command list to produce a pixel.
kernel void
renderKernel(texture2d<half, access::write> outTexture [[texture(0)]],
             const device char *tiles [[buffer(0)]],
             uint2 gid [[thread_position_in_grid]],
             uint2 tgid [[threadgroup_position_in_grid]])
{
    uint tileIx = tgid.y * maxTilesWidth + tgid.x;
    const device char *src = tiles + tileIx * tileBufSize;
    uint x = gid.x;
    uint y = gid.y;
    float2 xy = float2(x, y);

    // Render state (maybe factor out?)
    half3 rgb = half3(1.0);
    float df = 1e9;
    half signedArea = 0.0;

    ushort cmd;
    while ((cmd = *(const device ushort *)src) != CMD_END) {
        switch (cmd) {
            case CMD_CIRCLE: {
                const device CmdCircle *circle = (const device CmdCircle *)src;
                src += sizeof(CmdCircle);
                ushort4 bbox = circle->bbox;
                float2 xy0 = float2(bbox.x, bbox.y);
                float2 xy1 = float2(bbox.z, bbox.w);
                float2 center = mix(xy0, xy1, 0.5);
                float r = length(xy - center);
                // I should make this shade an ellipse properly but am too lazy.
                // But see WebRender ellipse.glsl (linked in notes)
                float circleR = min(center.x - xy0.x, center.y - xy0.y);
                float alpha = saturate(circleR - r);
                rgb = mix(rgb, half3(0.0), alpha);
                break;
            }
            case CMD_LINE: {
                const device CmdLine *line = (const device CmdLine *)src;
                src += sizeof(CmdLine);
                stroke(df, xy, line->start, line->end);
                break;
            }
            case CMD_STROKE: {
                const device CmdStroke *stroke = (const device CmdStroke *)src;
                src += sizeof(CmdStroke);
                half alpha = renderDf(df, stroke->halfWidth);
                half4 fg = unpack_unorm4x8_srgb_to_half(stroke->rgba);
                rgb = mix(rgb, fg.rgb, fg.a * alpha);
                df = 1e9;
                break;
            }
            case CMD_FILL: {
                const device CmdFill *fill = (const device CmdFill *)src;
                src += sizeof(CmdFill);
                float2 start = fill->start - xy;
                float2 end = fill->end - xy;
                float2 window = saturate(float2(start.y, end.y));
                // maybe should be an epsilon test for better numerical stability
                if (window.x != window.y) {
                    float2 t = (window - start.y) / (end.y - start.y);
                    float2 xs = mix(float2(start.x), float2(end.x), t);
                    // This fudge factor might be inadequate when xmax is large, could
                    // happen with small slopes.
                    float xmin = min(min(xs.x, xs.y), 1.0) - 1e-6;
                    float xmax = max(xs.x, xs.y);
                    float b = min(xmax, 1.0);
                    float c = max(b, 0.0);
                    float d = max(xmin, 0.0);
                    float area = (b + 0.5 * (d * d - c * c) - xmin) / (xmax - xmin);
                    // TODO: evaluate accuracy loss from more use of half
                    signedArea += half(area * (window.x - window.y));
                }
                break;
            }
            case CMD_FILL_EDGE: {
                const device CmdFillEdge *fill = (const device CmdFillEdge *)src;
                src += sizeof(CmdFillEdge);
                signedArea += fill->sign * saturate(y - fill->y + 1);
                break;
            }
            case CMD_DRAW_FILL: {
                const device CmdDrawFill *draw = (const device CmdDrawFill *)src;
                src += sizeof(CmdDrawFill);
                half alpha = signedArea + half(draw->backdrop);
                alpha = min(abs(alpha), 1.0h); // nonzero winding rule
                // even-odd is: alpha = abs(alpha - 2.0 * round(0.5 * alpha))
                // also: abs(2 * fract(0.5 * (x - 1.0)) - 1.0)
                half4 fg = unpack_unorm4x8_srgb_to_half(draw->rgba);
                rgb = mix(rgb, fg.rgb, fg.a * alpha);
                signedArea = 0.0;
                break;
            }
        }
    }
    // Linear to sRGB conversion. Note that if we had writable sRGB textures
    // we could let this be done in the write call.
    rgb = select(1.055 * pow(rgb, 1/2.4) - 0.055, 12.92 * rgb, rgb < 0.0031308);
    half4 rgba = half4(rgb, 1.0);
    outTexture.write(rgba, gid);
}
