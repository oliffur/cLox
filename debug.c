#include <stdio.h>

#include "debug.h"
#include "object.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count;) {
    // return the offset of next instruction, since they are of different sizes
    offset = disassembleInstruction(chunk, offset);
  }
  printf("== end %s ==\n", name);
}

// Simply prints an op-code along with its offset
static int simp(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

// Prints a constant, along with its index in the constant table
static int cons(const char* name, Chunk* chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);  // name and index in constants table
  printValue(chunk->constants.values[constant]);  // actual constant itself
  printf("'\n");

  // One byte for the name, one byte for the constant
  return offset + 2;
}

static int byte(const char* name, Chunk* chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);
  return offset + 2; 
}

static int jump(const char* name, int sign, Chunk* chunk, int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
  return offset + 3;
}

static int invk(const char* name, Chunk* chunk, int offset) {  // invoke
  uint8_t constant = chunk->code[offset + 1];
  uint8_t argCount = chunk->code[offset + 2];
  printf("%-16s (%d args) %4d '", name, argCount, constant);
  printValue(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 3;
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("%04d ", offset);  // print byte offset
  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
    printf("   | ");
  } else {
    printf("%4d ", chunk->lines[offset]);
  }

  uint8_t instruction = chunk->code[offset];  // read opcode
  switch (instruction) {
    case OP_CONSTANT:       return cons("OP_CONSTANT", chunk, offset);
    case OP_NIL:            return simp("OP_NIL", offset);
    case OP_TRUE:           return simp("OP_TRUE", offset);
    case OP_FALSE:          return simp("OP_FALSE", offset);
    case OP_POP:            return simp("OP_POP", offset);
    case OP_GET_LOCAL:      return byte("OP_GET_LOCAL", chunk, offset);
    case OP_SET_LOCAL:      return byte("OP_SET_LOCAL", chunk, offset);
    case OP_GET_GLOBAL:     return cons("OP_GET_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL:  return cons("OP_DEFINE_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL:     return cons("OP_SET_GLOBAL", chunk, offset);
    case OP_GET_CAPVAR:     return byte("OP_GET_CAPVAR", chunk, offset);
    case OP_SET_CAPVAR:     return byte("OP_SET_CAPVAR", chunk, offset);
    case OP_GET_PROPERTY:   return cons("OP_GET_PROPERTY", chunk, offset);
    case OP_SET_PROPERTY:   return cons("OP_SET_PROPERTY", chunk, offset);
    case OP_GET_SUPER:      return cons("OP_GET_SUPER", chunk, offset);
    case OP_EQUAL:          return simp("OP_EQUAL", offset);
    case OP_GREATER:        return simp("OP_GREATER", offset);
    case OP_LESS:           return simp("OP_LESS", offset);
    case OP_ADD:            return simp("OP_ADD", offset);
    case OP_SUBTRACT:       return simp("OP_SUBTRACT", offset);
    case OP_MULTIPLY:       return simp("OP_MULTIPLY", offset);
    case OP_DIVIDE:         return simp("OP_DIVIDE", offset);
    case OP_NOT:            return simp("OP_NOT", offset);
    case OP_NEGATE:         return simp("OP_NEGATE", offset);
    case OP_PRINT:          return simp("OP_PRINT", offset);
    case OP_JUMP:           return jump("OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:  return jump("OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_LOOP:           return jump("OP_LOOP", -1, chunk, offset);
    case OP_CALL:           return byte("OP_CALL", chunk, offset);
    case OP_INVOKE:         return invk("OP_INVOKE", chunk, offset);
    case OP_SUPER_INVOKE:   return invk("OP_SUPER_INVOKE", chunk, offset);
    case OP_CLOSURE: {
      offset++;
      uint8_t constant = chunk->code[offset++];
      printf("%-16s %4d ", "OP_CLOSURE", constant);
      printValue(chunk->constants.values[constant]);
      printf("\n");

      ObjFunction* function = AS_FUNCTION(
          chunk->constants.values[constant]);
      for (int j = 0; j < function->capvarCount; j++) {
        int isLocal = chunk->code[offset++];
        int index = chunk->code[offset++];
        printf("%04d      |                     %s %d\n",
               offset - 2, isLocal ? "local" : "capvar", index);
      }

      return offset;
    }
    case OP_MOVE_CAPVAR:    return simp("OP_MOVE_CAPVAR", offset);
    case OP_RETURN:         return simp("OP_RETURN", offset);
    case OP_CLASS:          return cons("OP_CLASS", chunk, offset);
    case OP_INHERIT:        return simp("OP_INHERIT", offset);
    case OP_METHOD:         return cons("OP_METHOD", chunk, offset);
    default:
      printf("Unknown opcode %d\n", instruction);
      return offset + 1;
  }
}