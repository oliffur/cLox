#ifndef clox_table_h
#define clox_table_h

/* A hash table, with string keys and `Value` values*/

#include "common.h"
#include "value.h"

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);

// Looks up `key`; if not found, return false. If found, set value to `value`
// and return true.
bool tableGet(Table* table, ObjString* key, Value* value);
// Setter
bool tableSet(Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(Table* from, Table* to);
// Retrieve a string key if present; otherwise return null. Because strings are
// interned, this function acts as hashset.contains(...).
ObjString* tableFindString(Table* table, const char* chars, int length,
                           uint32_t hash);

#endif