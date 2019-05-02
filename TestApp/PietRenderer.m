//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietRenderer.h"
#import "PietShaderTypes.h"
#import "SceneEncoder.h"

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
        [encoder line:start to:end width:2 color:0x800000];
    }
    [encoder endGroup];
}

- (void)initScene {
    /*
    const int radius = 8;
    const int n = 256;
    SceneEncoder *encoder = [[SceneEncoder alloc] initWithBuffer:_sceneBuf];
    [encoder beginGroup:n + 1];
    for (int i = 0; i < n; i++) {
        uint32_t x = arc4random() % _viewportSize.x;
        uint32_t y = arc4random() % _viewportSize.y;
        [encoder circle:simd_make_float2(x, y) radius:radius];
    }
    [encoder line:simd_make_float2(100, 500) to:simd_make_float2(700, 600) width:100 color:0x800000];
    [encoder endGroup];
    */
    [self initCardioid];
}

@end
