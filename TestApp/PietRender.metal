//  Copyright 2019 The xi-editor authors.

#include <metal_stdlib>
using namespace metal;

#include "PietShaderTypes.h"

struct RenderData {
    float4 clipSpacePosition [[position]];
    float2 textureCoordinate;
    float pointSize [[point_size]];
    half4 solidColor;
};

vertex RenderData
vertexShader(uint vertexID [[ vertex_id ]],
             constant RenderVertex *vertexArray [[ buffer(RenderVertexInputIndexVertices) ]],
             texture2d<half> loTexture [[texture(0)]])
{
    RenderData out;
    float2 clipSpacePosition = vertexArray[vertexID].position;
    out.clipSpacePosition.xy = clipSpacePosition;
    out.clipSpacePosition.z = 0.0;
    out.clipSpacePosition.w = 1.0;
    float2 xy = vertexArray[vertexID].textureCoordinate;
    out.textureCoordinate = xy;
    // Note: this assumes tileWidth == tileHeight
    // If this is not true, a quad made of 2 tris is a better choice.
    out.pointSize = tileWidth;
    uint2 tileXY = uint2(xy.x / tileWidth, xy.y / tileHeight);
    out.solidColor = loTexture.read(tileXY);
    return out;
}

fragment half4 fragmentShader(RenderData in [[stage_in]],
                               texture2d<half> texture [[texture(0)]]) {
    const half4 loSample = in.solidColor;
    if (loSample.a == 0.0) {
        uint2 coords = uint2(in.clipSpacePosition.xy);
        const half4 sample = texture.read(coords);
        return sample;
    } else {
        return loSample;
    }
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
    ushort _padding;
    packed_ushort4 bbox;
};

// Render one line segment to the distance field buffer.
struct CmdLine {
    ushort cmd;
    ushort _padding;
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
    ushort _padding;
    packed_float2 start;
    packed_float2 end;
};

// Accumulate line intersecting the left edge of a tile to the signed area buffer.
struct CmdFillEdge {
    ushort cmd;
    // This is only one bit, it's a bit wasteful
    short sign;
    // Y coordinate at point of intersection with left edge.
    float y;
};

// Draw a fill based on the signed area buffer.
struct CmdDrawFill {
    ushort cmd;
    short backdrop;
    uint rgba;
};

struct CmdSolid {
    ushort cmd;
    ushort _padding;
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
#define CMD_SOLID 7
#define CMD_BAIL 86

struct TileEncoder {
public:
    TileEncoder(device char *dst) {
        this->dst = dst;
        this->tileBegin = dst;
        this->solidColor = 0xffffffff;
    }
    void encodeCircle(ushort4 bbox) {
        device CmdCircle *cmd = (device CmdCircle *)dst;
        cmd->cmd = CMD_CIRCLE;
        cmd->bbox = bbox;
        solidColor = 0;
        dst += sizeof(CmdCircle);
    }
    void encodeLine(float2 start, float2 end) {
        device CmdLine *cmd = (device CmdLine *)dst;
        cmd->cmd = CMD_LINE;
        cmd->start = start;
        cmd->end = end;
        dst += sizeof(CmdLine);
    }
    void encodeStroke(uint rgbaColor, float width) {
        device CmdStroke *cmd = (device CmdStroke *)dst;
        cmd->cmd = CMD_STROKE;
        cmd->rgba = rgbaColor;
        cmd->halfWidth = 0.5 * width;
        solidColor = 0;
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
        solidColor = 0;
        dst += sizeof(CmdDrawFill);
    }
    void encodeSolid(uint rgba) {
        // A fun optimization would be to alpha-composite semi-opaque
        // solid blocks.
        
        // Another optimization is to skip encoding the default bg color.
        if ((rgba & 0xff000000) == 0xff000000) {
            solidColor = rgba;
            dst = tileBegin;
        }
        device CmdSolid *cmd = (device CmdSolid *)dst;
        // Note: could defer writing, not sure how much of a win that is
        cmd->cmd = CMD_SOLID;
        cmd->rgba = rgba;
        dst += sizeof(CmdSolid);
    }
    // return solid color
    uint end() {
        if (solidColor) {
            *(device ushort *)tileBegin = CMD_BAIL;
        } else {
            device CmdEnd *cmd = (device CmdEnd *)dst;
            cmd->cmd = CMD_END;
        }
        return solidColor;
    }
private:
    // Pointer to command buffer for tile.
    device char *dst;
    device char *tileBegin;
    uint solidColor;
};

// Traverse the scene graph and produce a command list for a tile.
kernel void
tileKernel(device const char *scene [[buffer(0)]],
           device char *tiles [[buffer(1)]],
           texture2d<half, access::write> outTexture [[texture(0)]],
           uint2 gid [[thread_position_in_grid]],
           uint tix [[thread_index_in_simdgroup]],
           uint sgSize [[threads_per_simdgroup]])
{
    uint tileIx = gid.y * maxTilesWidth + gid.x;
    ushort x0 = gid.x * tileWidth;
    ushort y0 = gid.y * tileHeight;
    device char *dst = tiles + tileIx * tileBufSize;
    TileEncoder encoder(dst);
    
    // I think on some hardware the SIMD group size is 64. Right now we
    // just limit it to 32 because I can't figure out easily how to do the
    // ctz on 64 bit values. Might be worth coming back to this, always
    // possible to represent v as two uints.
    const uint groupSize = sgSize < 32 ? sgSize : 32;
    // Careful to avoid undefined behavior; there might be a cleaner way to write this.
    const uint groupMask = groupSize == 32 ? ~0u : (1 << groupSize) - 1;
    const uint groupWidth = tilerGroupWidth < sgSize ? tilerGroupWidth : sgSize;

    // Size of the region covered by one group
    const ushort stw = groupWidth * tileWidth;
    const ushort sth = (groupSize / groupWidth) * tileHeight;
    ushort sx0 = x0 & ~(stw - 1);
    ushort sy0 = y0 & ~(sth - 1);
    
    device const SimpleGroup *group = (device const SimpleGroup *)scene;
    device const ushort4 *bboxes = (device const ushort4 *)&group->bbox[0];
    uint n = group->nItems;
    device const PietItem *items = (device const PietItem *)(scene + group->itemsIx);
    for (uint i = 0; i < n; i += groupSize) {
        bool hitCoarse = false;
        uint groupTix = tix & (groupSize - 1);
        if (i + groupTix < n) {
            ushort4 bbox = bboxes[i + groupTix];
            hitCoarse = bbox.z >= sx0 && bbox.x < sx0 + stw && bbox.w >= sy0 && bbox.y < sy0 + sth;
        }
        simd_vote vote = simd_ballot(hitCoarse);
        uint v = (simd_vote::vote_t(vote) >> (tix & ~(groupSize - 1))) & groupMask;
        while (v) {
            uint k = ctz(v);
            // To explore: use simd_broadcast rather then second global memory read.
            uint ix = i + k;
            ushort4 bbox = bboxes[ix];
            bool hit = bbox.z >= x0 && bbox.x < x0 + tileWidth && bbox.w >= y0 && bbox.y < y0 + tileHeight;
            ushort itemType = items[ix].itemType;
            switch (itemType) {
                case PIET_ITEM_CIRCLE:
                    if (hit) {
                        encoder.encodeCircle(bbox);
                    }
                    break;
                case PIET_ITEM_LINE: {
                    // set up line equation, ax + by + c = 0
                    if (hit) {
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
                            encoder.encodeLine(line.start, line.end);
                            encoder.encodeStroke(line.rgbaColor, line.width);
                        }
                    }
                    break;
                }
                case PIET_ITEM_FILL: {
                    device const PietFill &fill = items[ix].fill;
                    device const float2 *pts = (device const float2 *)(scene + fill.pointsIx);
                    uint nPoints = fill.nPoints;
                    float backdrop = 0;
                    bool anyFill = false;
                    // use simd ballot to quick-reject segments with no contribution
                    // Note: we only do a strip 1 tile tall for now. We could do a taller strip
                    // but that would require more complexity in the left-ray test.
                    for (uint j = 0; j < nPoints; j += groupWidth) {
                        bool fillHit = false;
                        uint gTix = tix & (groupSize - 1);
                        uint fillIx = j + gTix;
                        if (fillIx < nPoints && gTix < groupWidth) {
                            float2 start = pts[fillIx];
                            float2 end = pts[fillIx + 1 == nPoints ? 0 : fillIx + 1];
                            float2 xymin = min(start, end);
                            float2 xymax = max(start, end);
                            if (xymax.y >= y0 && xymin.y < y0 + tileHeight && xymin.x < sx0 + stw) {
                                // set up line equation, ax + by + c = 0
                                float a = end.y - start.y;
                                float b = start.x - end.x;
                                float c = -(a * start.x + b * start.y);
                                float left = a * sx0;
                                float right = a * (sx0 + stw);
                                float ytop = max(float(y0), xymin.y);
                                float ybot = min(float(y0 + tileHeight), xymax.y);
                                float top = b * ytop;
                                float bot = b * ybot;
                                // top left of rightmost tile in strip
                                float sTopLeft = sign(right - a * (tileWidth) + float(y0) * b + c);
                                float s00 = sign(top + left + c);
                                float s01 = sign(top + right + c);
                                float s10 = sign(bot + left + c);
                                float s11 = sign(bot + right + c);
                                if (sTopLeft == sign(a) && xymin.y <= y0) {
                                    // left ray intersects, need backdrop
                                    fillHit = true;
                                }
                                if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0 && xymax.x > sx0) {
                                    // intersects strip
                                    fillHit = true;
                                }
                            }
                        }
                        uint fillVote = (simd_vote::vote_t(simd_ballot(fillHit)) >> (tix & ~(groupSize - 1))) & groupMask;
                        while (fillVote) {
                            uint fillSubIx = ctz(fillVote);
                            fillIx = j + fillSubIx;

                            if (hit) {
                                float2 start = pts[fillIx];
                                float2 end = pts[fillIx + 1 == nPoints ? 0 : fillIx + 1];
                                float2 xymin = min(start, end);
                                float2 xymax = max(start, end);
                                // Note: no y-based cull here because it's been done in the earlier pass.
                                // If we change that to do a strip taller than 1 tile, re-introduce here.

                                // set up line equation, ax + by + c = 0
                                float a = end.y - start.y;
                                float b = start.x - end.x;
                                float c = -(a * start.x + b * start.y);
                                float left = a * x0;
                                float right = a * (x0 + tileWidth);
                                float ytop = max(float(y0), xymin.y);
                                float ybot = min(float(y0 + tileHeight), xymax.y);
                                float top = b * ytop;
                                float bot = b * ybot;
                                // top left of tile
                                float sTopLeft = sign(left + float(y0) * b + c);
                                float s00 = sign(top + left + c);
                                float s01 = sign(top + right + c);
                                float s10 = sign(bot + left + c);
                                float s11 = sign(bot + right + c);
                                if (sTopLeft == sign(a) && xymin.y <= y0) {
                                    backdrop -= s00;
                                }
                                if (xymin.x < x0 && xymax.x > x0) {
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
                                } else if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0
                                           && xymin.x < x0 + tileWidth && xymax.x > x0) {
                                    encoder.encodeFill(start, end);
                                    anyFill = true;
                                }
                            } // end if (hit)

                            fillVote &= ~(1 << fillSubIx);
                        }
                    }
                    if (anyFill) {
                        encoder.encodeDrawFill(fill, backdrop);
                    } else if (backdrop != 0.0) {
                        encoder.encodeSolid(fill.rgbaColor);
                    }
                    break;
                }
                case PIET_ITEM_STROKE_POLYLINE: {
                    device const PietStrokePolyLine &poly = items[ix].poly;
                    device const float2 *pts = (device const float2 *)(scene + poly.pointsIx);
                    uint nPoints = poly.nPoints - 1;
                    bool anyStroke = false;
                    float hw = 0.5 * poly.width + 0.5;
                    // use simd ballot to quick-reject segments with no contribution
                    for (uint j = 0; j < nPoints; j += groupWidth) {
                        bool polyHit = false;
                        uint gTix = tix & (groupSize - 1);
                        uint polyIx = j + gTix;
                        if (polyIx < nPoints && gTix < groupWidth) {
                            float2 start = pts[polyIx];
                            float2 end = pts[polyIx + 1];
                            float2 xymin = min(start, end);
                            float2 xymax = max(start, end);
                            if (xymax.y > sy0 - hw && xymin.y < sy0 + sth + hw &&
                                xymax.x > sx0 - hw && xymin.x < sx0 + stw + hw) {
                                // set up line equation, ax + by + c = 0
                                float a = end.y - start.y;
                                float b = start.x - end.x;
                                float c = -(a * start.x + b * start.y);
                                float left = a * (sx0 - hw);
                                float right = a * (sx0 + stw + hw);
                                float top = b * (y0 - hw);
                                float bot = b * (y0 + tileHeight + hw);
                                float s00 = sign(top + left + c);
                                float s01 = sign(top + right + c);
                                float s10 = sign(bot + left + c);
                                float s11 = sign(bot + right + c);
                                if (s00 * s01 + s00 * s10 + s00 * s11 < 3.0) {
                                    // intersects strip
                                    polyHit = true;
                                }
                            }
                        }
                        uint polyVote = (simd_vote::vote_t(simd_ballot(polyHit)) >> (tix & ~(groupSize - 1))) & groupMask;
                        while (polyVote) {
                            uint polySubIx = ctz(polyVote);
                            polyIx = j + polySubIx;
                            
                            if (hit) {
                                float2 start = pts[polyIx];
                                float2 end = pts[polyIx + 1];
                                float2 xymin = min(start, end);
                                float2 xymax = max(start, end);
                                if (xymax.y > y0 - hw && xymin.y < y0 + tileHeight + hw &&
                                    xymax.x > x0 - hw && xymin.x < x0 + tileWidth + hw) {
                                    float a = end.y - start.y;
                                    float b = start.x - end.x;
                                    float c = -(a * start.x + b * start.y);
                                    float hw = 0.5 * poly.width + 0.5;
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
                                        encoder.encodeLine(start, end);
                                        anyStroke = true;
                                    }
                                }
                            } // end if (hit)
                            
                            polyVote &= ~(1 << polySubIx);
                        }
                    }
                    if (anyStroke) {
                        encoder.encodeStroke(poly.rgbaColor, poly.width);
                    }
                    break;
                }
            }
            v &= ~(1 << k);  // aka v &= (v - 1)
        }
    }
    uint solidColor = encoder.end();
    outTexture.write(unpack_unorm4x8_to_half(solidColor), gid);
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
    while (1) {
        // Note: this has to be the biggest struct (could use a union for this)
        CmdFill cmdBuf = *(const device CmdFill *)src;
        cmd = cmdBuf.cmd;
        if (cmd == CMD_END) {
            break;
        }
        switch (cmd) {
            case CMD_CIRCLE: {
                const thread CmdCircle *circle = (const thread CmdCircle *)&cmdBuf;
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
                const thread CmdLine *line = (const thread CmdLine *)&cmdBuf;
                src += sizeof(CmdLine);
                stroke(df, xy, line->start, line->end);
                break;
            }
            case CMD_STROKE: {
                const thread CmdStroke *stroke = (const thread CmdStroke *)&cmdBuf;
                src += sizeof(CmdStroke);
                half alpha = renderDf(df, stroke->halfWidth);
                half4 fg = unpack_unorm4x8_srgb_to_half(stroke->rgba);
                rgb = mix(rgb, fg.rgb, fg.a * alpha);
                df = 1e9;
                break;
            }
            case CMD_FILL: {
                const thread CmdFill *fill = (const thread CmdFill *)&cmdBuf;
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
                const thread CmdFillEdge *fill = (const thread CmdFillEdge *)&cmdBuf;
                src += sizeof(CmdFillEdge);
                signedArea += fill->sign * saturate(y - fill->y + 1);
                break;
            }
            case CMD_DRAW_FILL: {
                const thread CmdDrawFill *draw = (const thread CmdDrawFill *)&cmdBuf;
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
            case CMD_SOLID: {
                const thread CmdSolid *solid = (const thread CmdSolid *)&cmdBuf;
                src += sizeof(CmdSolid);
                half4 fg = unpack_unorm4x8_srgb_to_half(solid->rgba);
                rgb = mix(rgb, fg.rgb, fg.a);
                break;
            }
            case CMD_BAIL:
                return;
        }
    }
    // Linear to sRGB conversion. Note that if we had writable sRGB textures
    // we could let this be done in the write call.
    rgb = select(1.055 * pow(rgb, 1/2.4) - 0.055, 12.92 * rgb, rgb < 0.0031308);
    half4 rgba = half4(rgb, 1.0);
    outTexture.write(rgba, gid);
}
