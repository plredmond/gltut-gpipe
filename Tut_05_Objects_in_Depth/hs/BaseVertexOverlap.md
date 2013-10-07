# Base Vertex With Overlap

Since we don't deal with VAOs in GPipe, I'm not sure this tutorial is useful.

In the previous C++ tutorial, `OverlapNoDepth.cpp`, Jason loads triangle data into one buffer and creates two VAOs which know how to access different parts of the buffer. In this C++ tutorial, `BaseVertexOverlap.cpp`, there is only one VAO which knows about the beginning of the same buffer, and the draw call for the second object is made using glDrawElementsBaseVertex.

