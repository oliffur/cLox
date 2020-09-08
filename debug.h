#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

// Prints out all instructions in `chunk` as a stack.
void disassembleChunk(Chunk* chunk, const char* name);

// OFFSET - LINE NO - OP_CODE - INDEX - VALUE
int disassembleInstruction(Chunk* chunk, int offset);

#endif