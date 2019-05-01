//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietShaderTypes.h"
#import "SceneEncoder.h"

@implementation SceneEncoder {
    char *_buf;
    uint _bboxIx;
    uint _ix;
}

- (nonnull instancetype)initWithBuffer:(nonnull id<MTLBuffer>)buffer {
    _buf = buffer.contents;
    _ix = 0;
    return self;
}

- (void)beginGroup:(uint)nItems {
    SimpleGroup *group = (SimpleGroup *)(_buf + _ix);
    // Does zero-size array work in obj-C?
    _bboxIx = _ix + sizeof(SimpleGroup) - sizeof(vector_ushort4);
    _ix = _bboxIx + sizeof(vector_ushort4) * (nItems);
    group->nItems = nItems;
    group->itemsIx = _ix;
}

- (void)endGroup {
    // This will do more when we have nested groups.
}

- (void)circle:(vector_float2)center radius:(float)radius {
    vector_short4 bbox;
    bbox.x = MAX(floor(center.x - radius), 0.0);
    bbox.y = MAX(floor(center.y - radius), 0.0);
    bbox.z = ceil(center.x + radius);
    bbox.w = ceil(center.y + radius);
    vector_short4 *bboxPtr = (vector_short4 *)(_buf + _bboxIx);
    *bboxPtr = bbox;
    _bboxIx += sizeof(vector_short4);
    PietCircle *circle = &[self allocItem]->circle;
    circle->itemType = PIET_ITEM_CIRCLE;
}

- (PietItem *)allocItem {
    PietItem *item = (PietItem *)(_buf + _ix);
    _ix += sizeof(PietItem);
    return item;
}

@end

