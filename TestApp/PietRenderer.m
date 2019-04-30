//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietRenderer.h"
#import "PietShaderTypes.h"

@implementation PietRenderer {
    id<MTLDevice> _device;
    id<MTLComputePipelineState> _tilePipelineState;
    id<MTLComputePipelineState> _computePipelineState;
    id<MTLRenderPipelineState> _renderPipelineState;
    id<MTLCommandQueue> _commandQueue;
    id<MTLTexture> _texture;
    id<MTLBuffer> _sceneBuf;
    id<MTLBuffer> _tileBuf;
    vector_uint2 _viewportSize;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView {
    self = [super init];
    if (self) {
        NSError *error = NULL;
        _device = mtkView.device;
        // Note: consider sRGB here (though ultimately there are complex questions where in the
        // pipeline these conversions should be).
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
        
        NSUInteger tileBufSizeBytes = maxTilesWidth * maxTilesHeight * tileBufSize * 4;
        // Note: consider using managed here, worth experimenting with.
        MTLResourceOptions sceneOptions = MTLResourceStorageModeShared | MTLResourceCPUCacheModeWriteCombined;
        _sceneBuf = [_device newBufferWithLength:16*1024*1024 options:sceneOptions];
        _tileBuf = [_device newBufferWithLength:tileBufSizeBytes options:MTLResourceStorageModePrivate];
    }
    return self;
}

- (void)drawInMTKView:(nonnull MTKView *)view {
    RenderVertex quadVertices[] = {
        //Viewport Positions, Texture Coordinates
        { {  1,  -1 }, { 1.f, 1.f } },
        { { -1,  -1 }, { 0.f, 1.f } },
        { { -1,   1 }, { 0.f, 0.f } },
        
        { {  1,  -1 }, { 1.f, 1.f } },
        { { -1,   1 }, { 0.f, 0.f } },
        { {  1,   1 }, { 1.f, 0.f } },
    };
    quadVertices[0].textureCoordinate.x = _viewportSize.x;
    quadVertices[0].textureCoordinate.y = _viewportSize.y;
    quadVertices[1].textureCoordinate.y = _viewportSize.y;
    quadVertices[3].textureCoordinate.x = _viewportSize.x;
    quadVertices[3].textureCoordinate.y = _viewportSize.y;
    quadVertices[5].textureCoordinate.x = _viewportSize.x;
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"RenderCommand";

    uint nTilesX = (_viewportSize.x + tileWidth - 1) / tileWidth;
    uint nTilesY = (_viewportSize.y + tileHeight - 1) / tileHeight;

    uint nTilerGroupsX = (nTilesX + tilerGroupWidth - 1) / tilerGroupWidth;
    uint nTilerGroupsY = (nTilesY + tilerGroupHeight - 1) / tilerGroupHeight;

    // Run tile compute shader.
    id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
    [computeEncoder setComputePipelineState:_tilePipelineState];
    [computeEncoder setTexture:_texture atIndex:0];
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
        [renderEncoder setVertexBytes:quadVertices
                               length:sizeof(quadVertices)
                              atIndex:RenderVertexInputIndexVertices];
        [renderEncoder setFragmentTexture:_texture atIndex:0];
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle vertexStart:0 vertexCount:6];
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
    
    [self initScene];
}

- (void)initScene {
    const int radius = 100;
    uint16_t *bboxBuf = (uint16_t *)_sceneBuf.contents;
    for (int i = 0; i < nCircles; i++) {
        uint32_t x = arc4random() % _viewportSize.x;
        uint32_t y = arc4random() % _viewportSize.y;
        bboxBuf[i * 4 + 0] = x >= radius ? x - radius : 0;
        bboxBuf[i * 4 + 1] = y >= radius ? y - radius : 0;
        bboxBuf[i * 4 + 2] = x + radius < _viewportSize.x ? x + radius : _viewportSize.x;
        bboxBuf[i * 4 + 3] = y + radius < _viewportSize.y ? y + radius : _viewportSize.y;
    }
}

@end
