//  Copyright 2019 The xi-editor authors.

#include <metal_stdlib>
using namespace metal;

#include "PietShaderTypes.h"

typedef struct {
    float4 clipSpacePosition [[position]];
    float2 textureCoordinate;
} RenderData;

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

// Maybe these should be an enum.
#define CMD_END 0
#define CMD_CIRCLE 1

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
    half3 rgb = half3((x & 255) / 255.0, (y & 255) / 255.0, 0);
    ushort cmd;
    while ((cmd = *(const device ushort *)src) != CMD_END) {
        switch (cmd) {
            case CMD_CIRCLE:
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
                rgb = mix(rgb, half3(1), alpha);
                break;
        }
    }
    // Linear to sRGB conversion. Note that if we had writable sRGB textures
    // we could let this be done in the write call.
    rgb = select(pow(rgb, 1/2.4) - 0.055, 12.92 * rgb, rgb < 0.0031308);
    half4 rgba = half4(rgb, 1.0);
    outTexture.write(rgba, gid);
}

kernel void
tileKernel(device const float *scene [[buffer(0)]],
           device char *tiles [[buffer(1)]],
           uint2 gid [[thread_position_in_grid]],
           uint tix [[thread_index_in_simdgroup]])
{
    uint tileIx = gid.y * maxTilesWidth + gid.x;
    ushort x0 = gid.x * tileWidth;
    ushort y0 = gid.y * tileHeight;
    device char *dst = tiles + tileIx * tileBufSize;
    
    // Size of the region covered by one SIMD group. TODO, don't hardcode.
    const ushort stw = 16 * 16;
    const ushort sth = 2 * 16;
    ushort sx0 = x0 & ~(stw - 1);
    ushort sy0 = y0 & ~(sth - 1);
    
    device const SimpleGroup *group = (device const SimpleGroup *)scene;
    device const ushort4 *bboxes = (device const ushort4 *)&group->bbox[0];
    uint n = group->nItems;
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
            ushort4 bbox = bboxes[i + k];
            if (bbox.z >= x0 && bbox.x < x0 + tileWidth && bbox.w >= y0 && bbox.y < y0 + tileHeight) {
                device CmdCircle *cmd = (device CmdCircle *)dst;
                cmd->cmd = CMD_CIRCLE;
                cmd->bbox = bbox;
                dst += sizeof(CmdCircle);
            }
            v &= ~(1 << k);  // aka v &= (v - 1)
        }
    }
    device CmdEnd *cmd = (device CmdEnd *)dst;
    cmd->cmd = CMD_END;
}
