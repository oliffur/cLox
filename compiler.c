#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/* Functions beginning with parse... will consume instructions from the chunk */

static void parseDeclaration();  // fwd decl
static void parseStatement();  // fwd decl
static void parseExpression();  // fwd decl
static void parseBlock();  // fwd decl



/* =============================================================================
 *                              s: PARSER FUNCTIONS
 * =============================================================================
 */

typedef struct {
  Token curr;
  Token prev;  // Some functions use lookbacks, e.g. unary.
  bool hadError;
  // In the middle of erroring code; signals not to throw excess errors
  bool panicMode;
} Parser;
Parser parser;

static void errorAt(Token* token, const char* message) {
  if (parser.panicMode) return;  // already in panic mode; don't inundate user
  parser.panicMode = true;  // enter panic mode for the first time

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}
static void errorAtCurr(const char* message) { errorAt(&parser.curr, message); }
static void errorAtPrev(const char* message) { errorAt(&parser.prev, message); }

// Advances the parser, catching TOKEN_ERRORs. After this finishes, prev points
// to the last good token and curr points to the current good token.
static void advance() {
  parser.prev = parser.curr;

  for (;;) {
    parser.curr = scanToken();
    if (parser.curr.type != TOKEN_ERROR) break;
    errorAtCurr(parser.curr.start);
  }
}

static void synchronize() {
  parser.panicMode = false;

  while (parser.curr.type != TOKEN_EOF) {
    if (parser.prev.type == TOKEN_SEMICOLON) return;
    switch (parser.curr.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;
      default:
        // Do nothing.
        ;
    }
    advance();
  }
}

// Expects TokenType `type`; otherwise throws error. If matches, calls advance()
static void consume(TokenType type, const char* message) {
  if (parser.curr.type == type) { advance(); return; }
  errorAtCurr(message);
}

static bool check(TokenType type) { return parser.curr.type == type; }

// If match, advance
static bool tryConsume(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

static bool lexemesEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}



/* =============================================================================
 *                            s: COMPILER FUNCTIONS
 * =============================================================================
 */

// A local variable; contains identifier name and scope depth. The value of the
// local is already on the vm stack, and is not stored.
typedef struct {
  Token name;
  int depth;  // the scope depth of the block where local was declared
  bool isCaptured;  // is this captured by a closure?
} Local;

typedef struct {
  uint8_t index;
  bool isLocal; // a local variable or a (recursive) capvar from enclosing
} Capvar;

typedef enum {
  TYPE_FUNCTION,
  TYPE_INITIALIZER,
  TYPE_METHOD,
  TYPE_SCRIPT  // Top level script
} FunctionType;

// A named struct at the beginning is needed here for the recursive pointer.
typedef struct FnCompiler {
  struct FnCompiler* enclosing;

  // Compiling inside a function, or compiling at the top level (pointer = NULL)
  ObjFunction* function;
  FunctionType type;

  int scopeDepth;  // nestedness; 0 is global scope, 1 is top level block, etc.
  int localCount;
  Local locals[UINT8_COUNT];
  
  // Captured variables for closures.
  Capvar capvars[UINT8_COUNT];
} FnCompiler;
FnCompiler* compiler = NULL;

Chunk* compilingChunk;

static Chunk* currentChunk() { return &compiler->function->chunk; }

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.prev.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void beginScope() { compiler->scopeDepth++; }

static void endScope() {
  compiler->scopeDepth--;
  // Delete local variables from stack
  while (compiler->localCount > 0 &&
         compiler->locals[compiler->localCount - 1].depth > 
         compiler->scopeDepth) {
    if (compiler->locals[compiler->localCount - 1].isCaptured) {
      // Keep track of captured variables; see discussion in Functions section
      emitByte(OP_MOVE_CAPVAR);
    } else {
      emitByte(OP_POP);
    }
    compiler->localCount--;
  }
}

// Adds a constant to the constant pool and return the index
static uint8_t makeConstant(Value value) {
  int cons = addConstant(currentChunk(), value);
  if (cons > UINT8_MAX) { errorAtPrev("Too many constants in one chunk."); return 0; }
  return (uint8_t)cons;
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static uint8_t makeStringConsFromToken(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}



/* =============================================================================
 *                                 s: VARIABLES
 * =============================================================================
 *
 * Compiling global variables only takes a few easy steps:
 * 1. convert the identifier to a string, and store it in the constants table
 * 2. resolve the rhs, and compile that into the chunk
 * 3. push OP_DEFINE_GLOBAL and the identifier index into the chunk
 *
 * When the vm encounters a global var, its stack looks like:
 *
 *      OP_CODE           INDEX   VALUE
 *      ----------------  -----   --------
 *      OP_CONSTANT       8       123
 *      OP_DEFINE_GLOBAL  7       'my_var'
 *
 * where INDEX is into the constants table. From this it can easily set and
 * retrieve global variables.
 *
 *
 * Compiling local variables comes with two problems:
 * 1) there needs to be some notion of scope
 * 2) local variables are only temporary, and stuffing them in the constants
 *    table is a waste of memory
 *
 * To find a solution, consider what the VM's stack looks like after entering a
 * local scope, assuming they are processed similarly to globals:
 *
 * a = 1; { b = 2 + 3; c = 4; doSomething(); { d = 5; }}
 *
 * Right after d = 5; is executed, the stack looks like:
 *   stack:  | 1 | 5 | 4 | 5 |
 *   scope:    g   1   1   2
 *
 * The key observation here is that after entering a local scope (after `g`), all
 * following variables are local variables! So, instead of the constants table,
 * they can be kept track of using a simple array. To be referenced later, the
 * name also needs to be stored, and to solve (1), the scope needs to be stored
 * also.
 *
 *   stack:  | 1 | 5 | 4 | 5 |
 *   scope:    g   1   1   2
 *   array:        0   1   2
 *                `b` `c` `d`
 *                 1   1   2
 *
 * When compiling to bytecode, it's important to remember that the Locals array
 * is discarded at the end of every compilation; that is, the VM has no idea what
 * a `local` is. For a simple example `var a = 1; {var a = 2; var b = 3; a;}`,
 * the chunk looks like:
 *
 *      OP_CODE           INDEX   VALUE
 *      ----------------  -----   --------
 *      OP_CONSTANT       1       '1'
 *      OP_DEFINE_GLOBAL  0       'a'
 *      OP_CONSTANT       2       '2'
 *      OP_CONSTANT       3       '3'
 *      OP_GET_LOCAL      1
 *      OP_POP
 *      OP_POP
 *
 * Note two things: (1) `a` and `b` inside the local scope aren't defined by
 * name! They're just referenced by the index of OP_GET_LOCAL. (2) There are 2
 * OP_POPS; since `a` and `b` are nameless, they need to be cleaned up when
 * exiting the scope.
 */

static int resolveCapvar(FnCompiler* fc, Token* name);  // fwd decl

// If global, just return; if local, check that there isn't a conflicting local
// variable name in the current scope, and then declare (but not define) `name`.
static void declareLocal(Token* name) {
  // Global variables are implicitly declared.
  if (compiler->scopeDepth == 0) return;

  // Check for double-definition of local variable in same scope.
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (local->depth != -1 && local->depth < compiler->scopeDepth) { break; }

    if (lexemesEqual(name, &local->name)) {
      errorAtPrev("Variable with this name already declared in this scope.");
    }
  }

  if (compiler->localCount == UINT8_COUNT) {
    errorAtPrev("Too many local variables in function."); return;
  }
  Local* local = &compiler->locals[compiler->localCount++];
  local->name = *name;
  local->depth = -1;  // Mark unitialized for now, to avoid `var a = a` bug
  local->isCaptured = false;
}

// Scanner:
// - consume TOKEN_IDENTIFIER and operand
//
// Compiler:
// - for local variables: add entry to locals table, return 0
// - for global variables: add entry to constants table, return index
static uint8_t parseVariableName(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  // If in local scope, exit function (locals aren't looked up by name at
  // runtime, so no need to stuff variable name in constants table)
  if (compiler->scopeDepth > 0) {
    // Locals aren't looked up by name; don't add anything to constants table.
    declareLocal(&parser.prev);
    return 0;
  } else{
    return makeStringConsFromToken(&parser.prev);
  }
}

// Marks last element of `locals` as defined (able to use) by fixing its depth.
static void defineLocal() {
  if (compiler->scopeDepth == 0) return;
  compiler->locals[compiler->localCount - 1].depth = compiler->scopeDepth;
}

// Walk up scopes to find a local variable with given name
static int resolveLocal(FnCompiler* fc, Token* name) {
  for (int i = fc->localCount - 1; i >= 0; i--) {
    Local* local = &fc->locals[i];
    if (lexemesEqual(name, &local->name)) {
      if (local->depth == -1) {  // `var a = a`
        errorAtPrev("Cannot read local variable in its own initializer.");
      }
      return i; 
    }
  }

  return -1;
}

// Compiles code for accessing an already-declared variable.
static void compileGetVar(Token name) {
  uint8_t op;
  int arg = resolveLocal(compiler, &name);  // Try to find a local var with name
  if (arg != -1) {  // Local
    op = OP_GET_LOCAL;
  } else if ((arg = resolveCapvar(compiler, &name)) != -1) {  // closure variable
    op = OP_GET_CAPVAR;
  } else {  // Global
    arg = makeStringConsFromToken(&name);
    op = OP_GET_GLOBAL;
  }
  emitBytes(op, (uint8_t)arg);
}

// Parses and compiles code for getting/setting an already-declared variable.
// 
// Scanner:
// - assumes curr is just after the variable token name
// - if there is a `=`, consume that and parse the following expression
//
// Compiler:
// - emit bytecode for either getting or setting a variable.
static void parseVariable(bool canAssign) {
  uint8_t getOp, setOp;
  Token* name = &parser.prev;
  int arg = resolveLocal(compiler, name);  // Try to find a local var with name
  if (arg != -1) {  // Local
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveCapvar(compiler, name)) != -1) {  // closure variable
    getOp = OP_GET_CAPVAR;
    setOp = OP_SET_CAPVAR;
  } else {  // Global
    arg = makeStringConsFromToken(name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && tryConsume(TOKEN_EQUAL)) {  // definition
    parseExpression();
    emitBytes(setOp, (uint8_t)arg);
  } else {  // declaration
    emitBytes(getOp, (uint8_t)arg);
  }

  // The error case !canAssign && tryConsume(TOKEN_EQUAL), and trailing error
  // chars, e.g. `var a 3;`, are caught back at the end of parsePrecedence.
}

// Parses and compiles a variable declaration statement.
//
// Scanner:
// - assumes `var` keyword has just been consumed
// - consumes the rest of the declaration
//
// Compiler:
// - add the variable to locals or global table
// - parse the expression on the rhs of the = sign (if exists)
// - define the variable
static void parseVarDecl() {
  uint8_t possiblyglobal = parseVariableName("Expect variable name.");

  if (tryConsume(TOKEN_EQUAL)) {
    parseExpression();
  } else {
    emitByte(OP_NIL);  // desugars `var a;` to `var a = nil;`
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  if (compiler->scopeDepth > 0) {  // Local
    defineLocal();
  } else {  // Global
    emitBytes(OP_DEFINE_GLOBAL, possiblyglobal);
  }
}



/* =============================================================================
 *                                 s: FUNCTIONS
 * =============================================================================
 *
 * In clox, each function is compiled by a separate compiler into an ObjFunction
 * object, which contains (1) its own bytecode chunk, and (2) some metadata 
 * about the function, e.g. its name and arity.
 *
 * When a function is defined, it is added as a Value (ObjClosure) to the global
 * (or local, if nested scope) tables, like any other value. Bytecode example:
 *
 *      OP_CLOSURE          1 '<fn my_function>'
 *      OP_DEFINE_GLOBAL    0 'a'
 *
 * When a function is called, the compiler adds all the parameters as constants
 * to the chunk, and an OP_CALL with # of params as operand. Bytecode example:
 *
 *      OP_GET_GLOBAL       2 'my_function'
 *      OP_CONSTANT         3 'arg1'
 *      OP_CONSTANT         4 'arg2'
 *      OP_CALL             2
 *
 * One major problem is that it's unclear how to index local variables inside
 * functions: they are called at runtime, but at compile time the position of 
 * the local variable on the vm stack is unknown. So-a pointer to the beginning
 * of the function bytecode is stored in the call frame, and local variables
 * are indexed from that point on. In the above example:
 *
 *      [ <script> ][ <fn my_function> ][ arg1 ][ arg2 ][ local1 ]
 *                 ^^
 *            stored pointer
 *
 * Here, `local1` would be referred to by bytecode `OP_GET_LOCAL 3`, since it is
 * 3 steps removed from the stored function pointer. The pointer is stored on a
 * separate call frame struct object, which also makes recursion easy.
 *
 * At the end of every function, OP_RETURN is injected into the bytecode which
 * triggers a stack cleanup and a jump back to the original call frame. For
 * explicit blank returns (`return;`) NIL is added implicitly. NIL and OP_RETURN
 * are also conservatively added at the end of every function block. Therefore,
 * a function `fun a() {return 1;}` is encoded into
 * 
 *      OP_CONSTANT         0 '1'
 *      OP_RETURN
 *      OP_NIL
 *      OP_RETURN
 *
 * The second OP_RETURN will never be reached, since after the first OP_RETURN,
 * the current call frame will be popped and execution returns to a higher level
 * call frame.
 *
 * If the function is a closure and captures any surrounding variables, when
 * those variables are compiled Capvar's will be created to help the function
 * find the enclosed variable. The first step in doing this is to walk up the
 * scopes until the function scope that actually defines the variable is found.
 * Then, that function and *every child down to the calling function* must add
 * a value to the Capvar array. This is the only way to store the enclosed
 * variable for the inner function to find at runtime. Here's a simple example:
 *
 *   fun outer() {
 *     var x = 1; var y = 2; var z = 3;
 *     fun middle() {
 *       print y;
 *       fun inner() { print x; }
 *     }
 *   }
 *   == inner ==
 *   OP_GET_CAPVAR       0
 *   OP_PRINT
 *   OP_NIL
 *   OP_RETURN
 *   == end inner ==
 *   == middle ==
 *   OP_GET_CAPVAR       0
 *   OP_PRINT
 *   OP_CLOSURE          0 <fn inner>
 *                         capvar 1
 *   OP_NIL
 *   OP_RETURN
 *   == end middle ==
 *   == outer ==
 *   OP_CONSTANT         0 '1'
 *   OP_CONSTANT         1 '2'
 *   OP_CONSTANT         2 '3'
 *   OP_CLOSURE          3 <fn middle>
 *                         local 2
 *                         local 1
 *   OP_NIL
 *   OP_RETURN
 *   == end outer ==
 *
 * First, notice that OP_CLOSURE has a variably-sized operand, which is unique.
 * `outer` needs to capture `x` and `y` because they are referred to later.
 * `middle` needs to capture `x`, which is the second (1th) capvar of `outer`,
 * so that `inner` can refer to it. When `middle` refers to `y`, it asks for the
 * first (0th) capvar of its parent function. When `inner` refers to `x`, it
 * asks for the first (0th) capvar of its parent function, which is recursively
 * the second (1th) capvar of `outer`.
 *
 * One issue is that captured variables may already be popped off of the stack
 * by the time they are referred to. Take for example:
 *
 *   fun outer() {
 *     var x = "outside";
 *     fun inner() { print x; }
 *     return inner;
 *   }
 *   var closure = outer();  // "outside" gets popped off here!
 *   closure();
 *
 * To keep the variable around, two notions of captured variables are developed:
 * stack capvar are living on the vm stack, and heap capvars are stored on the
 * heap. In this case, OP_RETURN looks for variables marked as `isCaptured` (in
 * this case, `x`), and converts them from stack capvars to heap capvars before
 * popping them. However, there is another subtlety; consider:
 *
 *   var f1;
 *   {
 *     var i = 1;
 *     fun f() { print i; }
 *     f1 = f;
 *   }
 *   f1();
 * 
 * Here, there is no OP_RETURN to migrate `i` from a stack capvar to a heap
 * capvar, so a `OP_MOVE_CAPVAR` instruction is manually generated to help here:
 *
 *   == <script> ==
 *   ...
 *   OP_CONSTANT         1 '1'
 *   OP_CLOSURE          2 <fn f>
 *                       local 1
 *   OP_GET_LOCAL        2
 *   OP_SET_GLOBAL       3 'f1'
 *   OP_POP
 *   OP_POP
 *   OP_MOVE_CAPVAR
 *   ...
 *   == end <script> ==
 */


static void initFnCompiler(FnCompiler* fc, FunctionType type) {
  // Set up a new compiler to replace `current`, and store a pointer to the
  // prev `current`.
  fc->enclosing = compiler;
  fc->function = NULL;
  fc->type = type;
  fc->localCount = 0;
  fc->scopeDepth = 0;
  fc->function = newFunction();
  compiler = fc;

  if (type != TYPE_SCRIPT) {
    // Copy; function object needs to persist until runtime.
    compiler->function->name = copyString(parser.prev.start,
                                         parser.prev.length);
  }

  // Set aside first locals slot, to store `this`.
  Local* local = &compiler->locals[compiler->localCount++];
  local->depth = 0;
  local->isCaptured = false;
  if (type != TYPE_FUNCTION) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}

static void emitReturn() {
  if (compiler->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0);  // returns the instance itself
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN);
}

// Emit NIL and OP_RETURN, and return the compiled function. Update `enclosing`.
static ObjFunction* endFnCompiler() {
  emitReturn();
  ObjFunction* function = compiler->function;

#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(),
        function->name != NULL ? function->name->chars : "<script>");
  }
#endif

  compiler = compiler->enclosing;
  return function;
}

// Parses and compiles function code after the function name. For example, for
// `fun a() { return 0; }`, this function ingests `() { return 0; }`.
// 
// Scanner:
// - consume entirety of a function body; assumes `fun` keyword was already
//   consumed before this function is called.
//
// Compiler:
// - create another nested FnCompiler object to compile the function and create 
//   a ObjFunction in the constants table. Then, emit OP_CONSTANT and the index
static void parseFunction(FunctionType type) {
  FnCompiler fc;  // One FnCompiler per function
  initFnCompiler(&fc, type);

  // This beginScope() doesn’t need a corresponding endScope() call, because
  // endFnCompiler() is called at the end of the function body.
  beginScope(); 

  // Compile the parameter list.
  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      compiler->function->arity++;
      if (compiler->function->arity > 255) {
        errorAtCurr("Cannot have more than 255 parameters.");
      }

      // Since beginScope() was called, these are local variables.
      uint8_t _ = parseVariableName("Expect parameter name.");
      defineLocal();

    } while (tryConsume(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  parseBlock();

  // This compiler's work is done. Get the newly minted ObjFunction
  ObjFunction* function = endFnCompiler();

  // Create a closure in case the function encapsulates variables.
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  // Captured variables, whether local variables of this function, or recursive
  // captured variables from parent scopes.
  for (int i = 0; i < function->capvarCount; i++) {
    emitByte(fc.capvars[i].isLocal ? 1 : 0);
    emitByte(fc.capvars[i].index);
  }
}

static void parseFunDecl() {
  uint8_t possiblyglobal = parseVariableName("Expect function name.");
  defineLocal();  // Immediately mark initialized, to allow recursion.
  parseFunction(TYPE_FUNCTION);
  
  if (compiler->scopeDepth == 0) {  // Global function
    emitBytes(OP_DEFINE_GLOBAL, possiblyglobal);
  }
}

static uint8_t parseArgList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      parseExpression();
      if (argCount == 255) {  // stuff arguments into 1 byte
        errorAtPrev("Cannot have more than 255 arguments.");
      }
      argCount++;
    } while (tryConsume(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void parseCall(bool canAssign) {
  uint8_t argCount = parseArgList();
  emitBytes(OP_CALL, argCount);
}

static void parseReturnStmt() {
  if (compiler->type == TYPE_SCRIPT) {
    errorAtPrev("Cannot return from top-level code.");
  }
  if (tryConsume(TOKEN_SEMICOLON)) {
    emitReturn();  // Implicitly return nil.
  } else {
    if (compiler->type == TYPE_INITIALIZER) {
      errorAtPrev("Cannot return a value from an initializer.");
    }
    // Return value
    parseExpression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

// Adds a new Capvar <isLocal, index> to `compiler`'s capvar array, and return
// its position; if this pair already exists, return the existing position.
static int addCapvar(FnCompiler* fc, uint8_t index, bool isLocal) {
  int capvarCount = fc->function->capvarCount;

  // Check if this capvar has already been added
  for (int i = 0; i < capvarCount; i++) {
    Capvar* capvar = &fc->capvars[i];
    if (capvar->index == index && capvar->isLocal == isLocal) { return i; }
  }

  if (capvarCount == UINT8_COUNT) {
    errorAtPrev("Too many closure variables in function."); return 0;
  }

  fc->capvars[capvarCount].isLocal = isLocal;
  fc->capvars[capvarCount].index = index;
  return fc->function->capvarCount++;
}

static int resolveCapvar(FnCompiler* fc, Token* name) {
  if (fc->enclosing == NULL) return -1;  // Base case; hit global scope.

  // Try to find `name` in locals array of this compiler. If found, add Capvar
  // and return.
  int local = resolveLocal(fc->enclosing, name);
  if (local != -1) {
    fc->enclosing->locals[local].isCaptured = true;
    return addCapvar(fc, (uint8_t)local, true);
  }

  // Recursive call for parent functions.
  int capvar = resolveCapvar(fc->enclosing, name);
  if (capvar != -1) {
    // Adding addCapvar() here triggers a sequence of recursive Capvar additions
    // down from the function that contains `name` down to the caller's parent.
    return addCapvar(fc, (uint8_t)capvar, false);
  }

  return -1;
}



/* =============================================================================
 *                                 s: OOP
 * =============================================================================
 * 
 * When a class is defined, a bunch of things happen in sequence:
 * 1) Declare the name of the class
 * 2) Emit OP_CLASS followed by the index of the class name
 * 3) Define the name (OP_DEFINE_GLOBAL, unless in local scope)
 * 4) For inheritance:
 *      i) Create a ClassCompiler that keeps track of parent class, and call
 *         beginScope(). A new scope is required so that class local variables
 *         (such as `super`!) don't collide
 *     ii) Load parent class onto the stack (OP_GET_GLOBAL), and add `super` to
 *         refer to it as a local variable to the Locals table
 *     ii) Load this class onto the stack (OP_GET_GLOBAL)
 *    iii) Emit OP_INHERIT, which copies over superclass methods
 * 5) For methods:
 *      i) Load the class name back onto the stack (OP_GET_GLOBAL)
 *     ii) For each method, first parse its function (OP_CLOSURE), then emit
 *         OP_METHOD to bind the function to the class
 *    iii) Emit OP_POP to remove the class name from the stack
 * 6) If there was inheritance, call endScope()
 *
 * It looks something like this:
 *
 *   OP_CLASS            4 'SubClass'
 *   OP_DEFINE_GLOBAL    4 'SubClass'
 *   OP_GET_GLOBAL       5 'SuperClass'
 *   OP_GET_GLOBAL       6 'SubClass'
 *   OP_INHERIT
 *   OP_GET_GLOBAL       7 'SubClass'
 *   OP_CLOSURE          9 <fn someMethod>
 *   OP_METHOD           8 'someMethod'
 *   OP_POP
 *   OP_POP
 *
 * When setting a field in a class:
 * 1) First the class is retrieved with OP_GET_GLOBAL
 * 2) Then, the RHS is evaluated and pushed onto the stack
 * 3) Finally, OP_SET_PROPERTY is pushed onto the stack.
 * When getting a field in a class:
 * 1) First the class is retrieved with OP_GET_GLOBAL
 * 2) Then, OP_GET_PROPERTY is pushed onto the stack
 *
 * When calling a method in a class, it's possible to use above semantics to
 * OP_GET_PROPERTY followed by OP_CALL. However, this is slow. Instead, an
 * optimization is done:
 * 1) First the class is retrieved with OP_GET_GLOBAL
 * 2) The method is called directly with OP_INVOKE
 *
 * `this` is stored as a local variable that points to the function itself, but
 * `super` works similar to GET/SET semantics. When super is called as a getter:
 * 1) Push `this` and `super` onto the stack
 * 2) Push OP_GET_SUPER with the index of the gettee
 * Similarly to the above, if a method is called, an optimization is made and
 * OP_SUPER_INVOKE is pushed instead to call the method directly.
 */

// Compiles one method, emitting OP_CLOSURE followed by OP_METHOD.
static void parseMethod() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = makeStringConsFromToken(&parser.prev);

  FunctionType type = TYPE_METHOD;
  if (parser.prev.length == 4 &&
      memcmp(parser.prev.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }
  parseFunction(type);  // methods don't have `fun` keyword

  emitBytes(OP_METHOD, constant);
}

// Create a dummy token that points to `text`
static Token syntheticToken(const char* text) {
  Token token;
  token.start = text;
  token.length = (int)strlen(text);
  return token;
}

typedef struct ClassCompiler {
  struct ClassCompiler* enclosing;
  Token name;
  bool hasSuperclass;
} ClassCompiler;
ClassCompiler* currentClass = NULL;

// Declares a class: parses everything after `class` keyword until end of body
static void parseClassDecl() {
  // Define class name
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.prev;  // Copy class name for future reference.
  uint8_t nameConstant = makeStringConsFromToken(&parser.prev);
  declareLocal(&parser.prev);  // Declare class name.
  emitBytes(OP_CLASS, nameConstant);
  if (compiler->scopeDepth > 0) {  // Local class (rare)
    defineLocal();
  } else {  // Global class
    emitBytes(OP_DEFINE_GLOBAL, nameConstant);
  }

  // Initialize ClassCompiler
  ClassCompiler classCompiler;
  classCompiler.name = parser.prev;
  classCompiler.hasSuperclass = false;
  classCompiler.enclosing = currentClass;
  currentClass = &classCompiler;

  // Inheritance
  if (tryConsume(TOKEN_LESS)) {
    consume(TOKEN_IDENTIFIER, "Expect superclass name.");
    // Looks up superclass by name and pushes it onto the stack.
    parseVariable(/* canAssign = */false);

    if (lexemesEqual(&className, &parser.prev)) {
      errorAtPrev("A class cannot inherit from itself.");
    }

    beginScope();

    // Add `super` as a local variable.
    // TODO: is this check necessary?
    if (compiler->localCount == UINT8_COUNT) {
      errorAtPrev("Too many local variables in function."); return;
    }
    Local* super = &compiler->locals[compiler->localCount++];
    super->name = syntheticToken("super");
    super->depth = -1;
    super->isCaptured = false;
    defineLocal();  // Initialize

    // Push current class onto stack
    compileGetVar(className);
    emitByte(OP_INHERIT);

    classCompiler.hasSuperclass = true;
  }

  // Methods
  compileGetVar(className);  // push class name back onto stack
  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) { parseMethod(); }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  emitByte(OP_POP);  // pop class name

  if (classCompiler.hasSuperclass) { endScope(); }
  currentClass = currentClass->enclosing;
}

static void parseDot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t nameidx = makeStringConsFromToken(&parser.prev);

  if (canAssign && tryConsume(TOKEN_EQUAL)) {
    parseExpression();
    emitBytes(OP_SET_PROPERTY, nameidx);
  } else if (tryConsume(TOKEN_LEFT_PAREN)) {
    // OPTIMIZATION: calling a method; invoke it directly
    uint8_t argCount = parseArgList();
    emitBytes(OP_INVOKE, nameidx);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, nameidx);
  }

  // For errant equal signs (canAssign = False), silently return and let the
  // compiler error when it reaches the `=`.
}

static void parseThis(bool canAssign) {
  if (currentClass == NULL) {
    errorAtPrev("Cannot use 'this' outside of a class."); return;
  }
  // Look up `this`; remember that `this` was stored in local slot 0.
  parseVariable(/* canAssign = */ false);
} 

static void parseSuper(bool canAssign) {
  if (currentClass == NULL) {
    errorAtPrev("Cannot use 'super' outside of a class.");
  } else if (!currentClass->hasSuperclass) {
    errorAtPrev("Cannot use 'super' in a class with no superclass.");
  }

  consume(TOKEN_DOT, "Expect '.' after 'super'.");
  consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
  uint8_t name = makeStringConsFromToken(&parser.prev);

  // Look up current class object and push onto the class
  compileGetVar(syntheticToken("this"));
  if (tryConsume(TOKEN_LEFT_PAREN)) {
    // OPTIMIZATION: calling super.method(). Invoke directly.
    uint8_t argCount = parseArgList();
    compileGetVar(syntheticToken("super"));
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
  } else {
    compileGetVar(syntheticToken("super"));
    emitBytes(OP_GET_SUPER, name);
  }
}



/* =============================================================================
 *                             s: PARSING STATEMENTS
 * =============================================================================
 *
 * Statement parsing is generally straightforward, except for branching. When
 * the parser encounters a location where it may need to jump past some code 
 * (e.g. if statements), at first it does not know by how much to jump. So, it
 * first emits an operation to jump but with a dummy operand. Then, after a bit
 * more parsing, when the parser is ready it will patch in the dummy with an
 * integer for how much to jump. In code, this looks like:
 *
 * int dummyOperandLocation = emitJump(OP_JUMP);
 * ... (more parsing)
 * patchJump(dummyOperandLocation);  // OP_JUMP will jump here.
 *
 * An if statement looks like:
 * 
 *     OP_TRUE / OP_FALSE  `result of condition`
 *     OP_JUMP_IF_FALSE    -> a
 *     OP_POP
 *     ... (if statement stuff)
 *     OP_JUMP             -> b
 *   a OP_POP
 *     ... (else statement stuff, empty if no else)
 *   b OP_NIL  // Stuff after the if statement
 *
 * A while statement looks like:
 *
 *     OP_TRUE / OP_FALSE  `result of condition`
 *     OP_JUMP_IF_FALSE    -> a
 *     OP_POP
 *     ... (while statement stuff)
 *     OP_LOOP             -> b
 *   a  OP_POP
 *     OP_NIL
 *     OP_RETURN
 *
 * A for statement looks like:
 *
 *     ... (for initializer code)
 *   c OP_TRUE / OP_FALSE  `result of condition`
 *     OP_JUMP_IF_FALSE    -> a
 *     OP_POP
 *     OP_JUMP             -> b
 *   d ... (increment code)
 *     OP_POP
 *     OP_LOOP             -> c
 *   b ... (body code)
 *     OP_LOOP             -> d
 *   a ... (after for statement code)
 */

static void parseBlock() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) { parseDeclaration(); }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void parsePrintStmt() {
  parseExpression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void parseExpressionStmt() {
  parseExpression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

// Here `instruction` is either OP_JUMP or OP_JUMP_IF_FALSE.
static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  emitByte(0xff);  // 2 dummy bytes that will be patched later, allowing a jump
  emitByte(0xff);  // of up to 65,536 bytes of code.
  return currentChunk()->count - 2;  // Return location of dummy bytes
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) { errorAtPrev("Too much code to jump over."); }

  // Fill in the 2 bytes of emitJump with jump
  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void parseIfStmt() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  parseExpression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition."); 

  // If the expression is false, jump to the else block
  int thenJump = emitJump(OP_JUMP_IF_FALSE);

  // True block
  emitByte(OP_POP);  // pop truthiness expression
  parseStatement();
  int elseJump = emitJump(OP_JUMP);  // end of True block; jump past end of If
  patchJump(thenJump);

  // Else block
  emitByte(OP_POP);  // pop truthiness expression
  if (tryConsume(TOKEN_ELSE)) parseStatement();
  patchJump(elseJump);
}

// Similar to OP_JUMP, but OP_LOOK jumps backwards to `loopStart`.
static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) errorAtPrev("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static void parseWhileStmt() {
  // Loop needs to include re-parsing the while truthiness expression
  int loopStart = currentChunk()->count;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  parseExpression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);  // If false, exit while loop
  emitByte(OP_POP);  // pop truthiness expression
  
  parseStatement();
  emitLoop(loopStart);  // End of while statement; jump back to beginning
  patchJump(exitJump);
  emitByte(OP_POP);
}

static void parseForStmt() {
  // Initializer
  beginScope();  // In case initializer declares a variable
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (tryConsume(TOKEN_SEMICOLON)) {
    // None
  } else if (tryConsume(TOKEN_VAR)) {
    parseVarDecl();
  } else {
    parseExpressionStmt();
  }

  // Condition; needs to be recalculated every loop
  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!tryConsume(TOKEN_SEMICOLON)) {
    parseExpression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");
    exitJump = emitJump(OP_JUMP_IF_FALSE);  // if False, exit for loop
    emitByte(OP_POP);  // pop truthiness expression
  }

  // Increment; belongs at the end of the loop block, but to parse sequentially,
  // jump over the increment, run the body, then jump back, run the increment,
  // then loop back to start the entire for loop again.
  if (!tryConsume(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    parseExpression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    emitLoop(loopStart);  // after run increment, start entire for loop again
    loopStart = incrementStart;
    patchJump(bodyJump);  // the bodyJump jumps past the increment
  }

  parseStatement();  // body
  emitLoop(loopStart);  // jump back to run the increment

  if (exitJump != -1) {  // only jump back if there is a condition clause
    patchJump(exitJump);
    emitByte(OP_POP);  // pop truthiness expression
  }

  endScope();
}

static void parseStatement() {
  if (tryConsume(TOKEN_PRINT)) {
    parsePrintStmt();
  } else if (tryConsume(TOKEN_FOR)) {
    parseForStmt();
  } else if (tryConsume(TOKEN_IF)) {
    parseIfStmt();
  } else if (tryConsume(TOKEN_RETURN)) {
    parseReturnStmt();
  } else if (tryConsume(TOKEN_WHILE)) {
    parseWhileStmt();
  } else if (tryConsume(TOKEN_LEFT_BRACE)) {
    beginScope();
    parseBlock();
    endScope();
  } else {
    parseExpressionStmt();
  }
}

static void parseDeclaration() {
  if (tryConsume(TOKEN_CLASS)) {
    parseClassDecl();
  } else if (tryConsume(TOKEN_FUN)) {
    parseFunDecl();
  } else if (tryConsume(TOKEN_VAR)) {
    parseVarDecl();
  } else {
    parseStatement();
  }

  // Hit an error. Synchronize.
  if (parser.panicMode) synchronize();
}



/* =============================================================================
 *                            s: PRECEDENCE PARSING
 * =============================================================================
 *
 * Every token is assigned three attributes:
 * 1) a prefix-rule: what function is called to parse this token as a prefix?
 *    for example, for `-`, the prefix-rule is `unary`.
 * 2) an infix-rule: what function is called to parse this token as an infix?
 *    for example, for `-`, the infix-rule is `binary`.
 * 3) a precedence level, which puts the token in relation to other tokens; i.e.
 *    the PEMDAS of clox.
 *
 * parsePrecedence() is the workhorse of parsing; it parses a prefix expression 
 * and then any number of infix expressions of a certain precedence or higher
 * (i.e. stricter). It does this by:
 * 1) Parsing a prefix expression based on the first token
 * 2) parse infix expressions of `precedence` or more stringent, and return if
 *    it encounters a less stringent infix expression.
 *
 * Example: `1 - 2 / 1 + 4`
 * 1) parsePrecedence(PREC_TERM) (or higher) is called to parse the expression
 * 2) the parser parses 1 as a prefix, and then finds `-` and calls parseBinary
 * 3) parseBinary passes `2 / 1 - 4` recursively back to parsePrecedence, but
 *    with precedence PREC_FACTOR
 * 4) this nested parsePrecedence parses `2 / 1` correctly, and hands back to
 *    parseBinary
 * 5) parseBinary adds `+` and hands back to the original parsePrecedence
 * 6) parsePrecedence continues with an infix for `+` and parses the rest
 *
 * This example demonstrates 2 things. First, it demonstrates why precedence is
 * needed; parsed with precedence PREC_TERM, `2 / 1 - 4` is parsed as -2, which
 * isn't correct in that context. Parsed with precedence PREC_FACTOR, it is
 * parsed correctly as 2, returning to let the original parsePrecedence call
 * handle the dangling `- 4`. Second, it demonstrates that parseBinary needs to
 * call parsePrecedence with a stricter precedence because it makes addition / 
 * subtraction left-associative; `(1 - 2 / 1) + 4`, not `1 - (2 / 1 + 4)`.
 */

typedef enum {  // In order of stringency
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

/* * * * * * * * * * * * * * * *  Language Hint  * * * * * * * * * * * * * * * *
In C typedefs are used to create function pointers; something like:
  typedef void (*fnr)(int x);
  void f(int x){ printf("x=%d\n", x); }
  fnr t = f;
  t(0)
*/
typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;  // called if the expression starts with this token
  ParseFn infix;  // called if this token is in the middle of the expression
  Precedence precedence;
} ParseRule;
static ParseRule* getRule(TokenType type);  // fwd decl

static void parsePrecedence(Precedence precedence) {
  advance();
  // Every expression starts with a prefix (e.g. no expression starts with `+`).
  ParseFn prefixRule = getRule(parser.prev.type)->prefix;
  if (prefixRule == NULL) { errorAtPrev("Expect expression."); return; }
  bool canAssign = precedence <= PREC_ASSIGNMENT;  // Don't assign to non-lvalue
  prefixRule(canAssign);

  // The prefix has been parsed, but it's possible that the prefix is the lhs
  // of an infix rule. Only allow this if the infix has higher precedence.
  while (precedence <= getRule(parser.curr.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.prev.type)->infix;
    infixRule(canAssign);
  }

  if (canAssign && tryConsume(TOKEN_EQUAL)) {
    errorAtPrev("Invalid assignment target.");
  }
}

static void parseExpression() { parsePrecedence(PREC_ASSIGNMENT); }

static void parseGrouping(bool canAssign) {
  // Initial ( has already been consumed
  parseExpression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void parseOr(bool canAssign) {
  // Short circuit; if false, jump over the following OP_JUMP (i.e., don't jump)
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void parseAnd(bool canAssign) {
  //  Short-circuit; if lhs is false, jump past rhs.
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);

  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

static void parseBinary(bool canAssign) {
  // LHS was just parsed; next parse RHS and then emit the operator itself
  TokenType operatorType = parser.prev.type;

  // Compile the right operand, using one level more stringent
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));  

  // Emit operator itself
  switch (operatorType) {
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    // resolve >= as !(<)
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    // resolve <= as !(>)
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    default:                  return; // Unreachable.
  }
}

static void parseUnary(bool canAssign) {
  TokenType operatorType = parser.prev.type;

  // Compile the operand.
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default:
      return; // Unreachable.
  }
}

static void parseNumber(bool canAssign) {
  double value = strtod(parser.prev.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void parseString(bool canAssign) {
  // Copies values directly from lexeme, trimming "s
  emitConstant(OBJ_VAL(copyString(
    parser.prev.start + 1, parser.prev.length - 2)));
}

static void parseLiteral(bool canAssign) {
  switch (parser.prev.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
}

ParseRule rules[] = {
  // A function call is kind of like an infix `(` operator: high precedence on
  // the left identifier, argument expressions on the right
  [TOKEN_LEFT_PAREN]    = { parseGrouping,  parseCall,    PREC_CALL },
  [TOKEN_RIGHT_PAREN]   = { NULL,           NULL,         PREC_NONE },
  [TOKEN_LEFT_BRACE]    = { NULL,           NULL,         PREC_NONE }, 
  [TOKEN_RIGHT_BRACE]   = { NULL,           NULL,         PREC_NONE },
  [TOKEN_COMMA]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_DOT]           = { NULL,           parseDot,     PREC_CALL },
  [TOKEN_MINUS]         = { parseUnary,     parseBinary,  PREC_TERM },
  [TOKEN_PLUS]          = { NULL,           parseBinary,  PREC_TERM },
  [TOKEN_SEMICOLON]     = { NULL,           NULL,         PREC_NONE },
  [TOKEN_SLASH]         = { NULL,           parseBinary,  PREC_FACTOR },
  [TOKEN_STAR]          = { NULL,           parseBinary,  PREC_FACTOR },
  [TOKEN_BANG]          = { parseUnary,     NULL,         PREC_NONE },
  [TOKEN_BANG_EQUAL]    = { NULL,           parseBinary,  PREC_EQUALITY },
  [TOKEN_EQUAL]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_EQUAL_EQUAL]   = { NULL,           parseBinary,  PREC_EQUALITY },
  [TOKEN_GREATER]       = { NULL,           parseBinary,  PREC_COMPARISON },
  [TOKEN_GREATER_EQUAL] = { NULL,           parseBinary,  PREC_COMPARISON },
  [TOKEN_LESS]          = { NULL,           parseBinary,  PREC_COMPARISON },
  [TOKEN_LESS_EQUAL]    = { NULL,           parseBinary,  PREC_COMPARISON },
  [TOKEN_IDENTIFIER]    = { parseVariable,  NULL,         PREC_NONE },
  [TOKEN_STRING]        = { parseString,    NULL,         PREC_NONE },
  [TOKEN_NUMBER]        = { parseNumber,    NULL,         PREC_NONE },
  [TOKEN_AND]           = { NULL,           parseAnd,     PREC_AND },
  [TOKEN_CLASS]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_ELSE]          = { NULL,           NULL,         PREC_NONE },
  [TOKEN_FALSE]         = { parseLiteral,   NULL,         PREC_NONE },
  [TOKEN_FOR]           = { NULL,           NULL,         PREC_NONE },
  [TOKEN_FUN]           = { NULL,           NULL,         PREC_NONE },
  [TOKEN_IF]            = { NULL,           NULL,         PREC_NONE },
  [TOKEN_NIL]           = { parseLiteral,   NULL,         PREC_NONE },
  [TOKEN_OR]            = { NULL,           parseOr,      PREC_OR },
  [TOKEN_PRINT]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_RETURN]        = { NULL,           NULL,         PREC_NONE },
  [TOKEN_SUPER]         = { parseSuper,     NULL,         PREC_NONE },
  [TOKEN_THIS]          = { parseThis,      NULL,         PREC_NONE },
  [TOKEN_TRUE]          = { parseLiteral,   NULL,         PREC_NONE },
  [TOKEN_VAR]           = { NULL,           NULL,         PREC_NONE },
  [TOKEN_WHILE]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_ERROR]         = { NULL,           NULL,         PREC_NONE },
  [TOKEN_EOF]           = { NULL,           NULL,         PREC_NONE },
};
static ParseRule* getRule(TokenType type) { return &rules[type]; }



/* =============================================================================
 *                                s: COMPILE
 * =============================================================================
 */

// For GC, because only the compiler has access to global `compiler` variable.
void markCompilerRoots() {
  FnCompiler* fc = compiler;
  while (fc != NULL) {
    markObject((Obj*)fc->function);
    fc = fc->enclosing;
  }
}

ObjFunction* compile(const char* source) {
  initScanner(source);
  FnCompiler fc;
  initFnCompiler(&fc, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!tryConsume(TOKEN_EOF)) { parseDeclaration(); }

  ObjFunction* function = endFnCompiler();
  return parser.hadError ? NULL : function;
}