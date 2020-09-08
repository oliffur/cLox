#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

/* The compiler compiles source code into bytecode 
 *
 * The input to the compiler is the a C-string representing source code. The
 * compiler holds a Scanner object to help it scan as it compiles. The output
 * of the compiler is a `chunk` of bytecode. Recall that chunks consist of a
 * sequence of OP_CODEs together with their operands.
 *
 * Here, the return value of compile() is a pointer to an ObjFunction object.
 * This is purely for convenience, to make function compilation easier. Each
 * function requires its own compiler to create its own `chunk` which it stores
 * in its ObjFunction object. So here- it's as if the global namespace is itself
 * wrapped in a big `void main()` function as it executes.
 *
 * The compiler uses the following tools:
 * - a Parser object, which is a wrapper object to call scanner functions
 * - a `chunk` object to write to, which is encapsulated in an ObjFunction class
 *   (remember that each chunk is packaged with its own constants table)
 * - an array of `Local`s comprising non-global (scoped) variables, functions,
 *   and classes; each Local object contains information about scope and naming.
 * - a `scopeDepth` variable to keep track of nestedness
 * - an array of `Capvar`s comprising captured variables from enclosing scopes
 *   which allows for closures
 */

ObjFunction* compile(const char* source);

void markCompilerRoots();

#endif