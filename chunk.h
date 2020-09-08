#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/*
 * A chunk is a blob of bytecode that lox code gets compiled into.
 * 
 * Each chunk is an array of instructions stored in a contiguous array for
 * faster lookups and more saved space. Each instruction consists of an op-code
 * which defines it, together with an operand--binary data that follows.
 *
 * The instructions will be read as OP_CODE, DATA, OP_CODE, DATA, etc., and each
 * OP_CODE has a set size of DATA associated with it so the interpreter knows
 * how many bytes to read after each OP_CODE.
 */

/* * * * * * * * * * * * * * * *  Language Hint  * * * * * * * * * * * * * * * *
C makes extensive use of typedef to avoid verbosity when instancing later on.
So for example, an instance of a struct S declared without typedef would be
`struct S my_s;` and with typedef, simply `S my_s;`
*/

typedef enum {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_GET_GLOBAL,
  OP_DEFINE_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_CAPVAR,
  OP_SET_CAPVAR,
  OP_GET_SUPER,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LOOP,
  OP_CALL,
  OP_INVOKE,
  OP_SUPER_INVOKE,
  OP_CLOSURE,
  OP_MOVE_CAPVAR,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,
  OP_RETURN,
  OP_CLASS,
  OP_INHERIT,
  OP_METHOD,
} OpCode;



typedef struct {
  int count;
  int capacity;

  /* * * * * * * * * * * * * * *  Language Hint  * * * * * * * * * * * * * * * *
  In C pointers are frequently used to refer to arrays that are (1) allocated on
  the heap and (2) for which the size isn't known at compile-time.
  */

  uint8_t* code;  // array

  // separate array to store line information for errors
  int* lines;  // array

  // all literal constants are stored in a `constant pool`
  ValueArray constants;

} Chunk;

void initChunk(Chunk* chunk);

// When freeX(Y* y) is called in clox, all of y's member variables that are on
// the heap are freed, but not y itself.
void freeChunk(Chunk* chunk);

// Write byte, line to chunk.
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// Add a constant to the constant pool, and return the index.
int addConstant(Chunk* chunk, Value value);

#endif