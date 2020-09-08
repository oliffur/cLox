#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = -1;  // True capacity = `capacity` + 1. Convenience for mask
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity + 1);
  initTable(table);
}

// Start from the modulus of the hash, walk forward until it hits an empty
// entry--defined as one with BOTH a NULL key and a NULL value.
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  // & instead of % because capacity is a power of 2 minus 1; % is very slow
  uint32_t index = key->hash & capacity;
  // Entries with NULL key and non-NULL values are `tombstones` (removed values)
  // if encountered, keep walking because the key may still be ahead. However,
  // if a key is not found, return the first found tombstone to conserve space.
  Entry* tombstone = NULL;
  for (;;) {
    Entry* entry = &entries[index];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {  // empty entry.
        return tombstone != NULL ? tombstone : entry;
      } else {  // found a tombstone.
        if (tombstone == NULL) tombstone = entry;
      }
    } else if (entry->key == key) {  // found the key.
      return entry;
    }
    index = (index + 1) & capacity;  // Walk forward from hash
  }
}

static void adjustCapacity(Table* table, int capacity) {
  Entry* entries = ALLOCATE(Entry, capacity + 1);
  for (int i = 0; i <= capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  // Recalculate buckets
  table->count = 0;
  for (int i = 0; i <= table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) continue;  // ignore empty and tombstones

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity + 1);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  if (table->count == 0) return false;
  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  *value = entry->value;
  return true;
}

bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count + 1 > (table->capacity + 1) * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity + 1) - 1;
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);

  bool isNewKey = entry->key == NULL;

  // Remember: table->count is percentage of memory (including dead memory, e.g.
  // tombstones) that is being used in the table
  if (isNewKey && IS_NIL(entry->value)) table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  // Place a tombstone in the entry. The node can't just be reset because if a
  // a collision is reached, the code will stop, which is wrong.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);

  return true;
}

void tableAddAll(Table* from, Table* to) {
  for (int i = 0; i <= from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}

ObjString* tableFindString(Table* table, const char* chars, int length,
                           uint32_t hash) {
  if (table->count == 0) return NULL;

  uint32_t index = hash & table->capacity;

  for (;;) {
    Entry* entry = &table->entries[index];

    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) return NULL;  // Empty entry
    } else if (entry->key->length == length &&
        entry->key->hash == hash &&
        memcmp(entry->key->chars, chars, length) == 0) {
      return entry->key;
    }

    index = (index + 1) & table->capacity;
  }
}