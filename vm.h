#ifndef clox_vm_h
#define clox_vm_h

/* A virtual machine that executes bytecode 
 *
 * The input to a vm is the compiler output `chunk`. In interpret(), the first
 * thing the VM does is call compile to get a chunk wrapped in an ObjClosure
 * object. The output of the VM is running code (!) along with about
 * InterpretResult for error notification.
 *
 * The VM uses the following tools to aid with interpretation:
 *
 * - a stack of Value's, which is the main workhorse for calculations
 *
 * - a table of global variables; the VM needs to keep track of how global
 *   variables' values change over time and to be able to look them up by name
 *
 * - a table of strings, which keeps track of not only string values, but also
 *   string names such as identifier (class/variable) names
 *
 * - a stack of CallFrames; a call frame is created once a function is invoked,
 *   and contains a pointer to the function, a return address, an instruction
 *   pointer, and a pointer for where the frame starts to aid with resolving
 *   local variables.
 *   o the closures in the call frames contain a list of Value pointers that
 *     point to captured values
 *   o when the VM is initialized, remember that it is in a `pretend main()` 
 *     function, so a dummy call frame is initialized as well.
 *   
 * - the head of the heap linked list, for garbage collection
 *
 * - a linked list of captured values that are considered `open`; the benefit
 *   of the VM maintaining a global list of captured values is to promote
 *   sharing of closure variables, for example when two functions close over 
 *   the same variable. For faster lookup, this array is sorted by the index of 
 *   the value. These pointers are maintained as a linked list for faster 
 *   insertion in the middle.
 *
 * During execution, its toolkit looks something like this:
 *
 *      OP_SOME_BYTECODE       index1 'operand1'
 * ip-> OP_OTHER_BYTECODE      index2
 *      OP_POP
 *      OP_PUSH                index3 'operand2'
 *
 *      [ stack_val ][ <fn some_fn> ][ stack_val2 ][ stack_val3 ]
 *                  ^^                                          ^^
 *                 start                                     stacktop
 */

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjClosure* closure;
  // return address
  uint8_t* ip;
  // where on the stack this function's local variables begin
  // TODO better name: stack_fn_start?
  Value* start;  // pointer to the beginning of the call frame on vm stack
} CallFrame;

typedef struct {
  /*  TODO: move these comments above
  // Bytecode compiler output
  Chunk* chunk;
  
  // Instruction pointer; for even faster execution, store this as a local
  // function so that the C compiler would keep it in a register.
  //
  // use a direct pointer instead of an integer index since it’s faster to
  // dereference the pointer than calculate the offset from the index
  //
  // points to the instruction about to be executed
  uint8_t* ip;
  */

  CallFrame frames[FRAMES_MAX];
  int frameCount;

  // VM stack where values are pushed and popped as execution runs
  Value stack[STACK_MAX];
  Value* stackTop;  // points just past the last item

  Table globals;
  Table strings;

  ObjString* initString;

  ObjCapvar* stackCapvars;  // linked list

  // GC params
  size_t bytesAllocated;
  size_t nextGC;

  Obj* objects;  // Head of the heap linked list

  // Objects that the garbage collector is currently working through
  int inprogCount;
  int inprogCapacity;
  Obj** inprogStack;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);


/* * * * * * * * * * * * * * * *  Language  Hint  * * * * * * * * * * * * * * *
extern is used to declare a variable without defining it:

extern int a; a = 6;  // ERROR: a was not defined!
extern int a;  // OK. Declaration. No memory will be allocated.
int a;  // OK. Definition. Memory will be allocated, and a = 0 (most likely)
extern int a; int a = 6;  // OK! Declaration followed by definition.
*/
extern VM vm;

// Pushes and pops values from the value stack.
void push(Value value);
Value pop();

#endif