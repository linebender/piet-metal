@interface SceneEncoder: NSObject

- (nonnull instancetype)initWithBuffer:(nonnull id<MTLBuffer>)buffer;

- (void)beginGroup:(uint)nItems;

- (void)endGroup;

- (void)circle:(vector_float2)center radius:(float)radius;

- (void)line:(vector_float2)start to:(vector_float2)end width:(float) width color:(uint) rgba;

- (void)addPt:(vector_float2)xy;

- (void)fill:(uint)rgba;

@end

// The following are legacy definitions - encoding in ObjC will not be supported
// going forward.
typedef struct SimpleGroup {
    uint nItems;
    // Offset in bytes to items
    uint itemsIx;
    vector_ushort4 bbox[1];
} SimpleGroup;

typedef struct PietCircle {
    uint itemType;
} PietCircle;

// A single line to be stroked, with default parameters
typedef struct PietStrokeLine {
    uint itemType;
    uint flags; // reserved, partially for alignment
    uint rgbaColor;
    float width;
    vector_float2 start;
    vector_float2 end;
} PietStrokeLine;

typedef struct PietFill {
    uint itemType;
    uint flags; // will be used for winding number rule
    uint rgbaColor;
    uint nPoints;
    uint pointsIx;
} PietFill;

typedef struct PietStrokePolyLine {
    uint itemType;
    uint rgbaColor;
    float width;
    uint nPoints;
    uint pointsIx;
} PietStrokePolyLine;

typedef union PietItem {
    uint itemType;
    PietCircle circle;
    PietStrokeLine line;
    PietFill fill;
    PietStrokePolyLine poly;
} PietItem;

// This should be an enum but the storage needs to be of fixed size
#define PIET_ITEM_CIRCLE 1
#define PIET_ITEM_LINE 2
#define PIET_ITEM_FILL 3
#define PIET_ITEM_STROKE_POLYLINE 4
