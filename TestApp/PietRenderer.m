//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietRenderer.h"
#import "PietShaderTypes.h"

@implementation PietRenderer {
    id<MTLDevice> _device;
    id<MTLRenderPipelineState> _renderPipelineState;
    id<MTLCommandQueue> _commandQueue;
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
        id<MTLFunction> vertexFunction = [defaultLibrary newFunctionWithName:@"vertexShader"];
        id<MTLFunction> fragmentFunction = [defaultLibrary newFunctionWithName:@"fragmentShader"];
        pipelineDescriptor.vertexFunction = vertexFunction;
        pipelineDescriptor.fragmentFunction = fragmentFunction;
        pipelineDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;
        
        _renderPipelineState = [_device newRenderPipelineStateWithDescriptor:pipelineDescriptor error: &error];

        _commandQueue = [_device newCommandQueue];
    }
    return self;
}

- (void)drawInMTKView:(nonnull MTKView *)view {
    static RenderVertex quadVertices[] = {
        //Viewport Positions, Texture Coordinates
        { {  1,  -1 }, { 1.f, 0.f } },
        { { -1,  -1 }, { 0.f, 0.f } },
        { { -1,   1 }, { 0.f, 1.f } },
        
        { {  1,  -1 }, { 1.f, 0.f } },
        { { -1,   1 }, { 0.f, 1.f } },
        { {  1,   1 }, { 1.f, 1.f } },
    };
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"RenderCommand";
    //id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
    
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if (renderPassDescriptor != nil) {
        id<MTLRenderCommandEncoder> renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        [renderEncoder setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, -1.0, 1.0}];
        [renderEncoder setRenderPipelineState:_renderPipelineState];
        [renderEncoder setVertexBytes:quadVertices
                               length:sizeof(quadVertices)
                              atIndex:RenderVertexInputIndexVertices];
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle vertexStart:0 vertexCount:6];
        [renderEncoder endEncoding];
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer commit];
}

- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size {
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
