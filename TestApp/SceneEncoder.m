//  Copyright 2019 The xi-editor authors.

@import MetalKit;

#import "PietShaderTypes.h"
#import "SceneEncoder.h"

@implementation SceneEncoder {
    char *_buf;
    uint _bboxIx;
    uint _ix;
    uint _count;
    // Index of beginning of free space (currently allocation is just a bump).
    uint _freeSpace;
    uint _pointCount;
    vector_float4 _bbox;
}

- (nonnull instancetype)initWithBuffer:(nonnull id<MTLBuffer>)buffer {
    _buf = buffer.contents;
    _ix = 0;
    _freeSpace = 0;
    _pointCount = 0;
    _bbox = simd_make_float4(0.0);
    return self;
}

- (uint)alloc:(uint)size {
    uint ix = _freeSpace;
    _freeSpace += size;
    return ix;
}

- (void)beginGroup:(uint)nItems {
    uint size = sizeof(SimpleGroup) - sizeof(vector_ushort4) + nItems * (sizeof(vector_ushort4) + sizeof(PietItem));
    uint ix = [self alloc:size];
    SimpleGroup *group = (SimpleGroup *)(_buf + ix);
    // Does zero-size array work in obj-C?
    _bboxIx = ix + sizeof(SimpleGroup) - sizeof(vector_ushort4);
    _ix = _bboxIx + sizeof(vector_ushort4) * nItems;
    group->nItems = nItems;
    group->itemsIx = _ix;
    _count = nItems;
}

- (void)endGroup {
    if (_count != 0) {
        NSLog(@"Not enough items encoded in group.");
    }
    /*
    for (int i = 0; i < _freeSpace / 4; i++) {
        NSLog(@"%04x: %08x", i * 4, ((uint *)_buf)[i]);
    }
     */
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

// This isn't dealing with subpaths and has a crude allocation strategy.
- (void)addPt:(vector_float2)xy {
    uint ix = [self alloc:sizeof(vector_float2)];
    vector_float2 *dst = (vector_float2 *)(_buf + ix);
    *dst = xy;
    if (_pointCount == 0) {
        _bbox = simd_make_float4(xy, xy);
    } else {
        _bbox = simd_make_float4(
                                 MIN(_bbox.x, xy.x),
                                 MIN(_bbox.y, xy.y),
                                 MAX(_bbox.z, xy.x),
                                 MAX(_bbox.w, xy.y)
        );
    }
    _pointCount++;
}

- (void)fill:(uint)rgba {
    PietFill *fill = &[self allocItem:_bbox]->fill;
    fill->itemType = PIET_ITEM_FILL;
    fill->rgbaColor = rgba;
    fill->nPoints = _pointCount;
    // This is a hack, needs to be fixed if we have real allocation.
    fill->pointsIx = _freeSpace - _pointCount * sizeof(vector_float2);
    _pointCount = 0;
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

