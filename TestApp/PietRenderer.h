//  Copyright 2019 The xi-editor authors.

@import MetalKit;

@interface PietRenderer : NSObject<MTKViewDelegate>

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;

@end
