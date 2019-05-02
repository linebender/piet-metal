//  Copyright 2019 The xi-editor authors.

typedef struct
{
    // This is a clip space coordinate (-1 to 1).
    vector_float2 position;
    // This is now an integer coordinate for reading the texture.
    vector_float2 textureCoordinate;
} RenderVertex;

typedef enum RenderVertexInputIndex
{
    RenderVertexInputIndexVertices = 0,
} RenderVertexInputIndex;

// Size in pixels of an individual tile
#define tileWidth 16
#define tileHeight 16

// Size (in tiles) of a threadgroup for tiling
#define tilerGroupWidth 16
#define tilerGroupHeight 16

// The number of bytes in a buffer for a single tile.
// For prototyping, this is a hard maximum, but for production we'd want
// a mechanism to overflow.
#define tileBufSize 4096

// For simplicity, we're going to hardcode these dimensions. For production,
// they need to be dynamic.
#define maxTilesWidth 256
#define maxTilesHeight 256

// Number of circles in the scene. Needs to be stored in the scene but
// is a define for expedience.
#define nCircles 256

typedef struct SimpleGroup {
    uint nItems;
    // Offset in bytes to items
    uint itemsIx;
    vector_ushort4 bbox[1];
} SimpleGroup;

// A single line to be stroked, with default parameters
typedef struct PietStrokeLine {
    uint itemType;
    uint flags; // reserved, partially for alignment
    uint rgbaColor;
    float width;
    vector_float2 start;
    vector_float2 end;
} PietStrokeLine;

typedef struct PietCircle {
    uint itemType;
} PietCircle;

typedef union PietItem {
    uint itemType;
    PietCircle circle;
    PietStrokeLine line;
} PietItem;

// This should be an enum but the storage needs to be of fixed size
#define PIET_ITEM_CIRCLE 1
#define PIET_ITEM_LINE 2
