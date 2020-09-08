#ifndef clox_object_h
#define clox_object_h

/* Objects stored on the heap, including strings.
 *
 * When a string is allocated, it gets copied onto the C heap, and a pointer to 
 * the heap object gets stored in a Value, which is then added to the constants
 * table and assigned an index (the index goes into the chunk).
 */

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

// From "value.h":
// typedef struct sObj Obj;
// typedef struct sObjString ObjString;

#define OBJ_TYPE(value)         (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value)  isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)         isObjType(value, OBJ_CLASS)
#define IS_STRING(value)        isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value)      isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)      isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)        isObjType(value, OBJ_NATIVE)
#define IS_CLOSURE(value)       isObjType(value, OBJ_CLOSURE)

#define AS_BOUND_METHOD(value)  ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value)         ((ObjClass*)AS_OBJ(value))
#define AS_STRING(value)        ((ObjString*)AS_OBJ(value))
#define AS_FUNCTION(value)      ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value)      ((ObjInstance*)AS_OBJ(value))
#define AS_CSTRING(value)       (((ObjString*)AS_OBJ(value))->chars)
#define AS_NATIVE(value)        (((ObjNative*)AS_OBJ(value))->function)
#define AS_CLOSURE(value)       ((ObjClosure*)AS_OBJ(value))

typedef enum {
  OBJ_BOUND_METHOD,
  OBJ_CLASS,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_CAPVAR,
} ObjType;

struct sObj {
  ObjType type;

  // Garbage collection fields: mark, and a pointer to the next Obj, creating a
  // linked list of objects on the heap.
  bool isMarked;
  struct sObj* next;
};

/* * * * * * * * * * * * * * * *  Language Hint  * * * * * * * * * * * * * * * *
Why doesn't this work?

#define IS_OBJ_TYPE(value, type) IS_OBJ(value) && AS_OBJ(value)->type == type

If IS_OBJ_TYPE(getValue(), some_type), getValue() will be called twice! And why
is this function not with the other IS_X() macros? Because unlike macros, this
function definition requires full definition of sObj before it can be called.
*/
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}



/* * * * * * * * * * * * * * * *  Language Hint  * * * * * * * * * * * * * * * *
Many objects begin with an Obj. This is designed to enable a clever pattern: a
pointer to a struct can be safely converted to a pointer to its first field and
back due to the shared bytes. Thus, an ObjString* can safely be cast to Obj*. 
AKA, inheritance! :)
*/
struct sObjString {
  Obj obj;  // Inheritance!
  int length;
  char* chars;
  uint32_t hash;  // for hash table lookup
};

// Assumes it can't take ownership of `chars`; instead it conservatively creates
// a copy of the characters on the heap that the ObjString can own. Normally,
// `chars` is not on the heap (it is usually part of a lexeme).
ObjString* copyString(const char* chars, int length);

// This function claims ownership of the string you give it. Usually, `chars`
// is already on the heap. For an example, see concatenate().
ObjString* takeString(char* chars, int length);



typedef struct {
  Obj obj;
  int arity;  // number of parameters
  int capvarCount;  // number of captured variables
  Chunk chunk;
  ObjString* name;
} ObjFunction;
ObjFunction* newFunction();

// Runtime representation for captured variables. These are allocated on the C
// heap, and initially they point onto the captured variable on the stack. When
// the variable needs to be moved on the heap, this object actually houses the
// value in `heapobj`.
typedef struct ObjCapvar {
  Obj obj;
  // A pointer to where the captured variable lives. If it's on the vm stack,
  // this will point to that stack location. If it's moved to the heap, this ptr
  // will point to the heapobj Value below.
  Value* ptr;
  Value heapobj;
  struct ObjCapvar* next;  // linked list
} ObjCapvar;
ObjCapvar* newCapvar(Value* slot);

// ObjFunction's are constant after compile time, but closures can capture
// surrounding variables.
typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjCapvar** capvars;  // array of ObjCapvar pointers
  int capvarCount;
} ObjClosure;
ObjClosure* newClosure(ObjFunction* function);



typedef struct {
  Obj obj;
  ObjString* name;
  Table methods;
} ObjClass;
ObjClass* newClass(ObjString* name);

typedef struct {
  Obj obj;
  ObjClass* klass;
  // Instance variables; allows dynamic typing; users can add fields at runtime.
  Table fields; 
} ObjInstance;
ObjInstance* newInstance(ObjClass* klass);

// When the user executes a method access, the closure for that method is
// retrieved and wrapped in a "bound method".
typedef struct {
  Obj obj;
  Value receiver;
  ObjClosure* method;
} ObjBoundMethod;
ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);



typedef Value (*NativeFn)(int argCount, Value* args);
typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;
ObjNative* newNative(NativeFn function);



void printObject(Value value);

#endif