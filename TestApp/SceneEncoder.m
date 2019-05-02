//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietShaderTypes.h"
#import "SceneEncoder.h"

@implementation SceneEncoder {
    char *_buf;
    uint _bboxIx;
    uint _ix;
    uint _count;
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
    _count = nItems;
}

- (void)endGroup {
    if (_count != 0) {
        NSLog(@"Not enough items encoded in group.");
    }
}

- (void)circle:(vector_float2)center radius:(float)radius {
    vector_float4 bbox = simd_make_float4(center.x - radius,
                                          center.y - radius,
                                          center.x + radius,
                                          center.y + radius);
    PietCircle *circle = &[self allocItem:bbox]->circle;
    circle->itemType = PIET_ITEM_CIRCLE;
}

// The color argument is actually ABGR, which is the native format.
// Maybe rename?
- (void)line:(vector_float2)start to:(vector_float2)end width:(float) width color:(uint) rgba {
    float half = 0.5 * width;
    vector_float4 bbox = simd_make_float4(MIN(start.x, end.x) - half,
                                          MIN(start.y, end.y) - half,
                                          MAX(start.x, end.x) + half,
                                          MAX(start.y, end.y) + half);
    PietStrokeLine *line = &[self allocItem:bbox]->line;
    line->itemType = PIET_ITEM_LINE;
    line->rgbaColor = rgba;
    line->width = width; // should this be half?
    line->start = start;
    line->end = end;
}

- (PietItem *)allocItem:(vector_float4)bbox {
    if (_count == 0) {
        NSLog(@"encoder group count overflow");
        return nil;
    }
    _count -= 1;
    vector_short4 *bboxPtr = (vector_short4 *)(_buf + _bboxIx);
    bboxPtr->x = MAX(bbox.x, 0.0);
    bboxPtr->y = MAX(bbox.y, 0.0);
    bboxPtr->z = bbox.z;
    bboxPtr->w = bbox.w;
    _bboxIx += sizeof(vector_short4);
    PietItem *item = (PietItem *)(_buf + _ix);
    _ix += sizeof(PietItem);
    return item;
}

@end

