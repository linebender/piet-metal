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

struct CmdCircle {
    ushort cmd;
    packed_ushort4 bbox;
};

struct CmdLine {
    ushort cmd;
    packed_float2 start;
    packed_float2 end;
};

struct CmdStroke {
    ushort cmd;
    uint rgba;
    half halfWidth;
};

// Maybe these should be an enum.
#define CMD_END 0
#define CMD_CIRCLE 1
#define CMD_LINE 2
#define CMD_STROKE 3

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
                    case PIET_ITEM_LINE:
                        encoder.encodeLine(items[ix].line);
                        encoder.encodeStroke(items[ix].line);
                        break;
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
    half3 rgb = half3(1);
    float df = 1e9;

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
                float circleR = min(center.x - xy0.x, center.y - xy0.y);
                float alpha = saturate(circleR - r);
                rgb = mix(rgb, half3(0), alpha);
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
                half3 fg = unpack_unorm4x8_srgb_to_half(stroke->rgba).xyz;
                df = 1e9;
                rgb = mix(rgb, fg, alpha);
                break;
            }
        }
    }
    // Linear to sRGB conversion. Note that if we had writable sRGB textures
    // we could let this be done in the write call.
    rgb = select(pow(rgb, 1/2.4) - 0.055, 12.92 * rgb, rgb < 0.0031308);
    half4 rgba = half4(rgb, 1.0);
    outTexture.write(rgba, gid);
}
