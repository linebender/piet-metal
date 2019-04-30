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
    constexpr sampler textureSampler(filter::nearest);
    const half4 sample = texture.read(uint2(in.textureCoordinate));
    return float4(sample);
}

kernel void
renderKernel(texture2d<half, access::write> outTexture [[texture(0)]],
             uint2 gid [[thread_position_in_grid]])
{
    uchar x = gid.x & 0xff;
    uchar y = gid.y & 0xff;
    outTexture.write(half4(x / 255.0, y / 255.0, 0, 1), gid);
}
