@interface SceneEncoder: NSObject

- (nonnull instancetype)initWithBuffer:(nonnull id<MTLBuffer>)buffer;

- (void)beginGroup:(uint)nItems;

- (void)endGroup;

- (void)circle:(vector_float2)center radius:(float)radius;

- (void)line:(vector_float2)start to:(vector_float2)end width:(float) width color:(uint) rgba;

@end
