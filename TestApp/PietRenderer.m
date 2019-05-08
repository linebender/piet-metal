//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietRenderer.h"
#import "PietShaderTypes.h"
#include "piet_metal.h"

@implementation PietRenderer {
    id<MTLDevice> _device;
    id<MTLComputePipelineState> _tilePipelineState;
    id<MTLComputePipelineState> _computePipelineState;
    id<MTLRenderPipelineState> _renderPipelineState;
    id<MTLCommandQueue> _commandQueue;
    id<MTLTexture> _texture;
    id<MTLTexture> _loTexture;
    id<MTLBuffer> _sceneBuf;
    id<MTLBuffer> _tileBuf;
    id<MTLBuffer> _vertexBuf;
    vector_uint2 _viewportSize;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView {
    self = [super init];
    if (self) {
        NSError *error = NULL;
        _device = mtkView.device;
        // Note: this is consciously not sRGB, we do the conversion before writing the texture.
        mtkView.colorPixelFormat = MTLPixelFormatBGRA8Unorm;
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
        MTLRenderPipelineDescriptor *pipelineDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
        id<MTLFunction> tileFunction = [defaultLibrary newFunctionWithName:@"tileKernel"];
        id<MTLFunction> kernelFunction = [defaultLibrary newFunctionWithName:@"renderKernel"];
        id<MTLFunction> vertexFunction = [defaultLibrary newFunctionWithName:@"vertexShader"];
        id<MTLFunction> fragmentFunction = [defaultLibrary newFunctionWithName:@"fragmentShader"];
        pipelineDescriptor.vertexFunction = vertexFunction;
        pipelineDescriptor.fragmentFunction = fragmentFunction;
        pipelineDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _tilePipelineState = [_device newComputePipelineStateWithFunction:tileFunction error:&error];
        _computePipelineState = [_device newComputePipelineStateWithFunction:kernelFunction error:&error];
        if (!_tilePipelineState || !_computePipelineState) {
            NSLog(@"Failed to create compute pipeline state, error %@", error);
            return nil;
        }
        _renderPipelineState = [_device newRenderPipelineStateWithDescriptor:pipelineDescriptor error: &error];

        _commandQueue = [_device newCommandQueue];
        
        NSUInteger tileBufSizeBytes = maxTilesWidth * maxTilesHeight * tileBufSize;
        // Note: consider using managed here, worth experimenting with.
        MTLResourceOptions sceneOptions = MTLResourceStorageModeShared | MTLResourceCPUCacheModeWriteCombined;
        _sceneBuf = [_device newBufferWithLength:16*1024*1024 options:sceneOptions];
        _tileBuf = [_device newBufferWithLength:tileBufSizeBytes options:MTLResourceStorageModePrivate];
    }
    return self;
}

- (void)drawInMTKView:(nonnull MTKView *)view {
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"RenderCommand";

    uint nTilesX = (_viewportSize.x + tileWidth - 1) / tileWidth;
    uint nTilesY = (_viewportSize.y + tileHeight - 1) / tileHeight;

    uint nTilerGroupsX = (nTilesX + tilerGroupWidth - 1) / tilerGroupWidth;
    uint nTilerGroupsY = (nTilesY + tilerGroupHeight - 1) / tilerGroupHeight;

    // Run tile compute shader.
    id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
    [computeEncoder setComputePipelineState:_tilePipelineState];
    [computeEncoder setTexture:_loTexture atIndex:0];
    [computeEncoder setBuffer:_sceneBuf offset:0 atIndex:0];
    [computeEncoder setBuffer:_tileBuf offset:0 atIndex:1];
    MTLSize tilegroupSize = MTLSizeMake(tilerGroupWidth, tilerGroupHeight, 1);
    MTLSize tilegroupCount = MTLSizeMake(nTilerGroupsX, nTilerGroupsY, 1);
    [computeEncoder dispatchThreadgroups:tilegroupCount threadsPerThreadgroup:tilegroupSize];
    [computeEncoder endEncoding];

    // Run compute shader for rendering.
    computeEncoder = [commandBuffer computeCommandEncoder];
    [computeEncoder setComputePipelineState:_computePipelineState];
    [computeEncoder setTexture:_texture atIndex:0];
    [computeEncoder setBuffer:_tileBuf offset:0 atIndex:0];
    MTLSize threadgroupSize = MTLSizeMake(tileWidth, tileHeight, 1);
    MTLSize threadgroupCount = MTLSizeMake(nTilesX, nTilesY, 1);
    [computeEncoder dispatchThreadgroups:threadgroupCount threadsPerThreadgroup:threadgroupSize];
    [computeEncoder endEncoding];
    
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if (renderPassDescriptor != nil) {
        id<MTLRenderCommandEncoder> renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        [renderEncoder setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, -1.0, 1.0}];
        [renderEncoder setRenderPipelineState:_renderPipelineState];
        [renderEncoder setVertexBuffer:_vertexBuf offset:0 atIndex:RenderVertexInputIndexVertices];
        [renderEncoder setVertexTexture:_loTexture atIndex:0];
        [renderEncoder setFragmentTexture:_texture atIndex:0];
        [renderEncoder drawPrimitives:MTLPrimitiveTypePoint vertexStart:0 vertexCount:nTilesX * nTilesY];
        [renderEncoder endEncoding];
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer commit];
}

- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size {
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
    // TODO: try not to allocate as wildly on smooth resize (maybe round up
    // the size).
    MTLTextureDescriptor *descriptor = [[MTLTextureDescriptor alloc] init];
    descriptor.textureType = MTLTextureType2D;
    descriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    descriptor.width = _viewportSize.x;
    descriptor.height = _viewportSize.y;
    descriptor.usage = MTLTextureUsageShaderWrite | MTLTextureUsageShaderRead;
    _texture = [_device newTextureWithDescriptor:descriptor];

    uint nTilesX = (_viewportSize.x + tileWidth - 1) / tileWidth;
    uint nTilesY = (_viewportSize.y + tileHeight - 1) / tileHeight;

    descriptor.width = nTilesX;
    descriptor.height = nTilesY;
    _loTexture = [_device newTextureWithDescriptor:descriptor];
    
    uint vertexLen = nTilesX * nTilesY * sizeof(RenderVertex);
    MTLResourceOptions vertexOptions = MTLResourceStorageModeShared | MTLResourceCPUCacheModeWriteCombined;
    _vertexBuf = [_device newBufferWithLength:vertexLen options:vertexOptions];
    RenderVertex *vertices = (RenderVertex *)_vertexBuf.contents;
    uint ix = 0;
    float scaleX = 2.0 / _viewportSize.x;
    float scaleY = 2.0 / _viewportSize.y;
    for (uint y = 0; y < nTilesY; y++) {
        for (uint x = 0; x < nTilesX; x++) {
            RenderVertex rv;
            uint x0 = x * tileWidth + (tileWidth / 2);
            uint y0 = y * tileHeight + (tileHeight / 2);
            rv.position.x = x0 * scaleX - 1.0;
            rv.position.y = y0 * -scaleY + 1.0;
            rv.textureCoordinate.x = x0;
            rv.textureCoordinate.y = y0;
            vertices[ix++] = rv;
        }
    }

    [self initScene];
}

/*
- (void)initCardioid {
    float cx = 1024;
    float cy = 768;
    float r = 750;
    int n = 97;
    SceneEncoder *encoder = [[SceneEncoder alloc] initWithBuffer:_sceneBuf];
    [encoder beginGroup: 2 * (n - 1)];
    for (int i = 1; i < n; i++) {
        float th0 = 2 * M_PI * i / n;
        float th1 = 2 * M_PI * ((i * 2) % n) / n;
        vector_float2 start = simd_make_float2(cx + r * cos(th0), cy - r * sin(th0));
        vector_float2 end = simd_make_float2(cx + r * cos(th1), cy - r * sin(th1));
        [encoder circle:start radius: 8];
        [encoder line:start to:end width:2 color:0xff800000];
    }
    [encoder endGroup];
}

- (void)fillTest {
    const int n = 256;
    SceneEncoder *encoder = [[SceneEncoder alloc] initWithBuffer:_sceneBuf];
    [encoder beginGroup: n];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < 3; j++) {
            uint32_t x = arc4random() % _viewportSize.x;
            uint32_t y = arc4random() % _viewportSize.y;
            [encoder addPt:simd_make_float2(x, y)];
        }
        [encoder fill:arc4random() | 0xff000000];
    }
    [encoder endGroup];
}

- (void)initCircles {
    const int radius = 8;
    const int n = 256;
    SceneEncoder *encoder = [[SceneEncoder alloc] initWithBuffer:_sceneBuf];
    [encoder beginGroup:n + 1];
    for (int i = 0; i < n; i++) {
        uint32_t x = arc4random() % _viewportSize.x;
        uint32_t y = arc4random() % _viewportSize.y;
        [encoder circle:simd_make_float2(x, y) radius:radius];
    }
    [encoder line:simd_make_float2(100, 500) to:simd_make_float2(700, 600) width:100 color:0xff800000];
    [encoder endGroup];
}

 - (void)initScene {
    //[self initCircles];
    //[self initCardioid];
    [self fillTest];
}
*/

- (void)initScene {
    init_test_scene(_sceneBuf.contents, _sceneBuf.allocatedSize);
}

@end
