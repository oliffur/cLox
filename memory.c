#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "table.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  vm.bytesAllocated += newSize - oldSize;
  // If stress testing GC, run GC every time new memory is alocated.
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
    collectGarbage();
#endif
    if (vm.bytesAllocated > vm.nextGC) {
      collectGarbage();
    }
  }

  if (newSize == 0) {
    // c-lib function; frees the allocated heap memory
    free(pointer);
    return NULL;
  }

  // realloc is a c-lib function for reallocating memory. Each allocated block
  // on the heap (of which a pointer is definitely one) has add'l bookkeeping
  // information bundled with it, e.g. block size. Many implementations of
  // malloc store this metadata right before the pointer address.
  //
  // - when oldSize = 0, realloc is equivalent to calling malloc
  // - when block can grow, realloc updates block size and returns input ptr
  // - when block can't grow, realloc allocates a new block of desired size,
  //   copies over the old bytes, frees the old block, and returns new block ptr
  void* result = realloc(pointer, newSize);

  // realloc failed because there isn't enough memory left
  if (result == NULL) exit(1);
  return result;
}


static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif
  switch (object->type) {
    case OBJ_BOUND_METHOD: {
      FREE(ObjBoundMethod, object);
      break;
    }
    case OBJ_CLASS: {
      ObjClass* klass = (ObjClass*)object;
      freeTable(&klass->methods);
      FREE(ObjClass, object);
      break;
    } 
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1);
      FREE(ObjString, object);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }
    case OBJ_INSTANCE: {
      ObjInstance* instance = (ObjInstance*)object;
      freeTable(&instance->fields);
      FREE(ObjInstance, object);
      break;
    }
    case OBJ_NATIVE: {
      FREE(ObjNative, object);
      break;
    }
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      FREE_ARRAY(ObjCapvar*, closure->capvars, closure->capvarCount);
      FREE(ObjClosure, object);  // Don't free function; closure doesn't own it.
      break;
    }
    case OBJ_CAPVAR: {
      FREE(ObjCapvar, object);
      break;
    }
  }
}

// TODO
// Tricolor marking to avoid cycles: (1) Unmarked; (2) Marked & in inprogStack;
// (3) Marked & not in inprogStack (finished)

// Adds object to inprogStack.
void markObject(Obj* object) {
  if (object == NULL) return;
  if (object->isMarked) return;  // avoid cycles

#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  object->isMarked = true;

  // This memory is not managed by the GC; otherwise the GC could trigger a GC.
  if (vm.inprogCapacity < vm.inprogCount + 1) {
    vm.inprogCapacity = GROW_CAPACITY(vm.inprogCapacity);
    vm.inprogStack = realloc(vm.inprogStack, sizeof(Obj*) * vm.inprogCapacity);
  }

  vm.inprogStack[vm.inprogCount++] = object;
}

void markValue(Value value) {
  if (!IS_OBJ(value)) return;  // Not on heap
  markObject(AS_OBJ(value));
}
static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}
void markTable(Table* table) {
  for (int i = 0; i <= table->capacity; i++) {
    Entry* entry = &table->entries[i];
    markObject((Obj*)entry->key);
    markValue(entry->value);
  }
}

// Mark objects that VM can directly reach, without intermediate references
static void markRoots() {
  // Mark objects on the vm stack
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) { markValue(*slot); }

  // Mark closures pointed to by call frames on vm
  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }

  // Mark stack capvars.
  for (ObjCapvar* cv = vm.stackCapvars; cv != NULL; cv = cv->next) {
    markObject((Obj*)cv);
  }

  // Mark global variables
  markTable(&vm.globals);

  // Compiler may be running while roots are being marked. Mark compiler roots.
  markCompilerRoots();

  // Mark `init`, which is set aside as a special string.
  markObject((Obj*)vm.initString);
}

// Mark references of `object`
static void markReferences(Obj* object) {

#ifdef DEBUG_LOG_GC
  printf("%p finish ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  switch (object->type) {
    case OBJ_BOUND_METHOD: {
      ObjBoundMethod* bound = (ObjBoundMethod*)object;
      markValue(bound->receiver);
      markObject((Obj*)bound->method);
      break;
    }
    case OBJ_CLASS: {
      ObjClass* klass = (ObjClass*)object;
      markObject((Obj*)klass->name);
      markTable(&klass->methods);
      break;
    }
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      markObject((Obj*)closure->function);
      for (int i = 0; i < closure->capvarCount; i++) {
        markObject((Obj*)closure->capvars[i]);
      }
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      markObject((Obj*)function->name);
      markArray(&function->chunk.constants);
      break;
    }
    case OBJ_INSTANCE: {
      ObjInstance* instance = (ObjInstance*)object;
      markObject((Obj*)instance->klass);
      markTable(&instance->fields);
      break;
    }
    case OBJ_CAPVAR:
      markValue(((ObjCapvar*)object)->heapobj);
      break;
    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
  }
}

// Walk entire object linked list and free any unmarked
static void sweep() {
  Obj* prev = NULL;
  Obj* curr = vm.objects;
  while (curr != NULL) {
    if (curr->isMarked) {  // Keep, but unmark
      curr->isMarked = false;
      prev = curr;
      curr = curr->next;
    } else {  // Free, and patch linked list
      Obj* unreached = curr;
      curr = curr->next;
      if (prev != NULL) { prev->next = curr; } else { vm.objects = curr; }
      freeObject(unreached);
    }
  }
}

// Run garbage collection
void collectGarbage() {

#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
  size_t before = vm.bytesAllocated;
#endif

  markRoots();
  while (vm.inprogCount > 0) {
    // Pop (finish) top object, then mark its references
    Obj* object = vm.inprogStack[--vm.inprogCount];
    markReferences(object);
  }

  // The VM has a hash table with every string value that it encounters. If the
  // entire string table is marked, then no string will ever be collected. So
  // instead, a special function is required to remove weak references.
  for (int i = 0; i <= vm.strings.capacity; i++) {
    Entry* entry = &vm.strings.entries[i];
    if (entry->key != NULL && !entry->key->obj.isMarked) {
      tableDelete(&vm.strings, entry->key);
    }
  }

  sweep();

  vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
  printf("   collected %ld bytes (from %ld to %ld) next at %ld\n",
         before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}


void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }

  free(vm.inprogStack);
}