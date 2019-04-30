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
