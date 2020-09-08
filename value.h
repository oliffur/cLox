#ifndef clox_value_h
#define clox_value_h

#include <string.h>

/* Values represent literal `things` such as numbers, booleans, and heap objects
 * that the virtual machine will use when interpreting. So, for example, when
 * `2+3` is interpreted, `2` and `3` will be Values that the VM will use.
 */

#include "common.h"

// Foward declared due to cyclic dependencies. See object.h. `struct` is needed
// here because the struct declarations in object.h are not typedef'ed
typedef struct sObj Obj;
typedef struct sObjString ObjString;

// NAN boxing; in IEEE there are 2^51 possible NANs, so instead of storing a
// Value in 16 bytes, it can be stored in 8 bytes by packing non-double values
// into NAN values.
#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
// Quiet NAN
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1 // 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;

#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))
#define AS_OBJ(v)       ((Obj*)(uintptr_t)((v) & ~(SIGN_BIT | QNAN)))
#define IS_OBJ(v)       (((v) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define AS_BOOL(v)      ((v) == TRUE_VAL)

// Can't mention v twice due to macro; so either:
// 1) It was FALSE_VAL and has now been converted to TRUE_VAL.
// 2) It was TRUE_VAL and the | 1 did nothing and it’s still TRUE_VAL.
// 3) It’s some other non-Boolean value.
// This definition catches all three.
#define IS_BOOL(v)      (((v) | 1) == TRUE_VAL)

#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define IS_NIL(v)       ((v) == NIL_VAL)

#define IS_NUMBER(v)    (((v) & QNAN) != QNAN)
#define AS_NUMBER(v)    valueToNum(v)
static inline double valueToNum(Value value) {
  double num;
  // This memcpy will be optimized away by the compiler.
  memcpy(&num, &value, sizeof(Value));
  return num;
}
#define NUMBER_VAL(num) numToValue(num)
static inline Value numToValue(double num) {
  Value value;
  // This memcpy will be optimized away by the compiler.
  memcpy(&value, &num, sizeof(double));
  return value;
}


#else

typedef enum {
  VAL_BOOL,
  VAL_NIL, 
  VAL_NUMBER,
  VAL_OBJ,  // Heap object
} ValueType;

typedef struct {
  ValueType type;
  // A union looks like a struct except that all of its fields overlap in
  // memory, and the size of a union is the size of its largest field
  union {
    bool boolean;
    double number;
    Obj* obj;
  } as;
} Value;

// Type checkers
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

// Takes a Value and returns a cType
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)

// Each one of these takes a C value of the appropriate type and produces a
// Value that has the correct type tag and contains the underlying value.
// TODO: Why is NIL_VAL number = 0?
#define BOOL_VAL(value)   ((Value){ VAL_BOOL, { .boolean = value } })
#define NIL_VAL           ((Value){ VAL_NIL, { .number = 0 } })
#define NUMBER_VAL(value) ((Value){ VAL_NUMBER, { .number = value } })
#define OBJ_VAL(object)   ((Value){ VAL_OBJ, { .obj = (Obj*)object } })

#endif

// An array of Value's
typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
bool valuesEqual(Value a, Value b);
void printValue(Value value);

#endif