#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

// Allocates object of given size on the heap
static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  object->isMarked = false;

  // Make the new object the head of the vm's objects linked list
  object->next = vm.objects;
  vm.objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %ld for %d\n", (void*)object, size, type);
#endif

  return object;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);

  function->arity = 0;
  function->capvarCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjClosure* newClosure(ObjFunction* function) {
  ObjCapvar** capvars = ALLOCATE(ObjCapvar*, function->capvarCount);
  for (int i = 0; i < function->capvarCount; i++) {
    capvars[i] = NULL;
  }
  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->capvars = capvars;
  closure->capvarCount = function->capvarCount;
  return closure;
}

ObjCapvar* newCapvar(Value* slot) {
  ObjCapvar* capvar = ALLOCATE_OBJ(ObjCapvar, OBJ_CAPVAR);
  capvar->heapobj = NIL_VAL;
  capvar->ptr = slot;
  capvar->next = NULL;
  return capvar;
}

// Allocates string on heap
static ObjString* allocateString(char* chars, int length,
                                 uint32_t hash) {
  // super! :)
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;

  push(OBJ_VAL(string));  // For GC's benefit
  // Intern string; keep track of all unique strings
  tableSet(&vm.strings, string, NIL_VAL);  // NIL_VAL; hashset
  pop();  // For GC's benefit

  return string;
}

// FNV-1a hash
static uint32_t hashString(const char* key, int length) {
  uint32_t hash = 2166136261u;

  for (int i = 0; i < length; i++) {
    hash ^= key[i];
    hash *= 16777619;
  }

  return hash;
}

ObjString* takeString(char* chars, int length) {
  uint32_t hash = hashString(chars, length);

  // If string already exists in string table, return that instead
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) {
    // Took ownership of the string `chars`; now free it.
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }
  // Create an sObjString that just points to `chars` directly
  return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
  uint32_t hash = hashString(chars, length);

  // If string already exists in string table, return that instead
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) return interned;

  // Make a copy of `chars` on the heap
  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);

  // Explicitly terminate; the lexeme points at a range of characters inside the
  // monolithic source string and isn’t terminated.
  heapChars[length] = '\0';

  return allocateString(heapChars, length, hash);
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
}

ObjNative* newNative(NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
}

ObjClass* newClass(ObjString* name) {
  ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
  klass->name = name; 
  initTable(&klass->methods);
  return klass;
}

ObjInstance* newInstance(ObjClass* klass) {
  ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
  instance->klass = klass;
  initTable(&instance->fields);
  return instance;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
  ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod,
                                       OBJ_BOUND_METHOD);
  bound->receiver = receiver;
  bound->method = method;
  return bound;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_BOUND_METHOD:
      printFunction(AS_BOUND_METHOD(value)->method->function);
      break;
    case OBJ_CLASS:
      printf("%s", AS_CLASS(value)->name->chars);
      break;
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;
    case OBJ_INSTANCE:
      printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
      break;
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    case OBJ_CLOSURE:
      printFunction(AS_CLOSURE(value)->function);
      break;
    case OBJ_CAPVAR:  // This case will never be reached.
      printf("capvar");
      break;
  }
}