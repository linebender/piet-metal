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

kernel void
renderKernel(texture2d<half, access::write> outTexture [[texture(0)]],
             device ushort4 *scene [[buffer(0)]],
             uint2 gid [[thread_position_in_grid]])
{
    uint x = gid.x;
    uint y = gid.y;
    half3 rgb = half3((x & 255) / 255.0, (y & 255) / 255.0, 0);
    for (int i = 0; i < 256; i++) {
        ushort4 bbox = scene[i];
        if (x >= bbox.x && x < bbox.z && y >= bbox.y && y < bbox.w) {
            float x0 = bbox.x;
            float y0 = bbox.y;
            float x1 = bbox.z;
            float y1 = bbox.w;
            float xc = 0.5 * (x0 + x1);
            float yc = 0.5 * (y0 + y1);
            float dx = x - xc;
            float dy = y - yc;
            float r = sqrt(dx * dx + dy * dy);
            // I should make this shade an ellipse properly but am too lazy.
            float circleR = min(xc - x0, yc - y0);
            float alpha = saturate(circleR - r);
            rgb = mix(rgb, half3(1), alpha);
        }
    }
    outTexture.write(half4(rgb, 1), gid);
}

kernel void
tileKernel(device float *scene [[buffer(0)]],
           device float *tiles [[buffer(1)]])
{
    
}
