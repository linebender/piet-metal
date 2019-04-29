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
    float2 clipSpacePosition = vertexArray[vertexID].position.xy;
    out.clipSpacePosition.xy = clipSpacePosition;
    out.clipSpacePosition.z = 0.0;
    out.clipSpacePosition.w = 1.0;
    out.textureCoordinate = vertexArray[vertexID].textureCoordinate;
    return out;
}

fragment float4 fragmentShader(RenderData in [[stage_in]]) {
    int i;
    // This is a delay loop to experiment with different amounts of computational load.
    float z = 1.0;
    for (i = 0; i < 1000; i++) {
        z = z * 0.999;
    }
    return float4(in.textureCoordinate.x, in.textureCoordinate.y, z, 1);
}
