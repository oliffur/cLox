#ifndef clox_memory_h
#define clox_memory_h

/* Functions and macros for memory management */

#include "common.h"
#include "object.h"

// Allocate memory for `count` object instances of `type`
#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

// Release memory for a single object instance of `type`
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

// Doubles capacity, floored at 8
#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

// Grows array, and makes a copy if there isn't enough space on the heap
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

// Release memory for entire array
#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

// Reminder: A void pointer is a pointer that has no associated data type
// with it. A void pointer can hold addresses of any type and can be 
// typcasted to any type.

// Reallocates data associated with a pointer
//
// oldSize	  newSize	    Operation
// ---------|-----------|----------------------------
// 0          Non‑zero	  Allocate new block.
// Non‑zero	  0	          Free allocation.
// Non‑zero	  < oldSize   Shrink existing allocation.
// Non‑zero	  > oldSize	  Grow existing allocation.
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

void markObject(Obj* object);
void markValue(Value value);

// Remove all values on the heap that are not actively used.
void collectGarbage();


// Objects on the heap are stored as a linked-list; this function walks the
// linked list and frees memory for all objects.
void freeObjects();

#endif