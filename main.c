#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"

/*
 * For a full understanding, read the docstrings of the following modules in
 * order, starting with the headers, and then read the docstrings in the .cc
 * files in the same order: (1) chunk (2) object (3) scanner (4) compiler (5) vm
 */

static void repl() {
  char line[1024];
  for (;;) {
    printf("> ");
    /* * * * * * * * * * * * * * *  Language  Hint  * * * * * * * * * * * * * *
    char *fgets(char *str, int n, FILE *stream) reads a line from the stream
    and stores it into str. It stops when either (n-1) characters are read,
    the newline character is read, or the end-of-file is reached.
    */
    if (!fgets(line, sizeof(line), stdin)) { printf("\n"); break; }
    interpret(line);
  }
}

static char* readFile(const char* path) {
  FILE* file = fopen(path, "rb");
  if (file == NULL) {
    fprintf(stderr, "Couldn't open \"%s\".\n", path); exit(74);
  }

  // Some c functions to get file size
  fseek(file, 0L, SEEK_END);
  size_t fileSize = ftell(file);
  // Reset file position
  rewind(file);

  char* buffer = (char*)malloc(fileSize + 1);
  if (buffer == NULL) {
    fprintf(stderr, "Not enough memory to read \"%s\".\n", path); exit(74);
  }

  size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
  if (bytesRead < fileSize) {
    fprintf(stderr, "Could not read file \"%s\".\n", path); exit(74);
  }
  
  buffer[bytesRead] = '\0';

  fclose(file);
  return buffer;
}

static void runFile(const char* path) {
  char* source = readFile(path);
  InterpretResult result = interpret(source);
  free(source); 
  if (result == INTERPRET_COMPILE_ERROR) exit(65);
  if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char* argv[]) {
  initVM();

  if (argc == 1) {  // clox
    repl();
  } else if (argc == 2) {  // clox filename
    runFile(argv[1]);
  } else {
    fprintf(stderr, "Usage: clox [path]\n");
    exit(64);
  }

  freeVM();
  return 0;
}