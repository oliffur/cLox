#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

VM vm;



/*** STACK ***/

void push(Value value) { *vm.stackTop = value; vm.stackTop++; }
Value pop() { vm.stackTop--; return *vm.stackTop; }
static Value peek(int distance) { return vm.stackTop[-1 - distance]; }
static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
  vm.stackCapvars = NULL;
}



/*** NATIVE CLOCK ***/

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}
static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}



/*** INIT & FREE ***/

void initVM() {
  resetStack();
  vm.objects = NULL;

  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;

  vm.inprogCount = 0;
  vm.inprogCapacity = 0;
  vm.inprogStack = NULL;
  
  initTable(&vm.globals);
  initTable(&vm.strings);
  vm.initString = NULL;  // for GC sake
  vm.initString = copyString("init", 4);

  defineNative("clock", clockNative);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  vm.initString = NULL;
  freeObjects();
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;
    // -1 because the IP is sitting on the next instruction to be executed.
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ",
            function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}



// False and nil are `falsey`; notably, 0 is not.
static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
  ObjString* b = AS_STRING(peek(0));  // Peek instead of pop for GC's benefit
  ObjString* a = AS_STRING(peek(1));

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  // Take ownership; this string was newly created on the heap.
  ObjString* result = takeString(chars, length);
  pop();
  pop();
  push(OBJ_VAL(result));
}


// Create a new call frame with the bytecode of the function intended to be 
// called and then pass back to run() to switch to that frame and execute.
static bool call(ObjClosure* closure, int argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.",
                 closure->function->arity, argCount);
    return false;
  }
  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");  // :)
    return false;
  }
  
  // Initialize a new call frame.
  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;

  // At this point, a value representing the function, and `argCount` arg values
  // have been pushed onto the stack. Set `start` to before the function value.
  // `start` will be used to resolve local variables.
  frame->start = vm.stackTop - argCount - 1;
  return true;
}

// Calls `callee`, switching on callee type.
static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      
      case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
        // Take `var bnd_fn = instance.fn; bnd_fn('foo');` At OP_CALL, the top
        // of the stack looks like:
        // ... [ <fn bnd_fn> ][ foo ]
        // First, replace it with:
        // ... [ instance ][ foo ]
        // then pass to call().
        vm.stackTop[-argCount - 1] = bound->receiver;
        return call(bound->method, argCount);
      }

      case OBJ_CLASS: {
        ObjClass* klass = AS_CLASS(callee);
        vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
        Value initializer;
        if (tableGet(&klass->methods, vm.initString, &initializer)) {
          // Arguments are already on top of the stack
          return call(AS_CLOSURE(initializer), argCount);
        } else if (argCount != 0) {
          runtimeError("Expected 0 arguments but got %d.", argCount);
          return false;
        }
        return true;
      }
      
      case OBJ_CLOSURE: return call(AS_CLOSURE(callee), argCount);
      
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
      
      default:
        // Non-callable object type.
        break;
    }
  }

  runtimeError("Can only call functions and classes.");
  return false;
}

// Invokes a method from a class
static bool invokeMethod(ObjClass* klass, ObjString* name, int argCount) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars); return false;
  }

  return call(AS_CLOSURE(method), argCount);
}

// Invokes a call from an instance.
static bool invoke(ObjString* name, int argCount) {
  Value invoker = peek(argCount);  // Get the instance that is invoking

  if (!IS_INSTANCE(invoker)) {
    runtimeError("Only instances have methods."); return false;
  }
  ObjInstance* instance = AS_INSTANCE(invoker);

  // 1) it's a field in the instance that stores a function (e.g. instance.field
  // = someFunction)
  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    vm.stackTop[-argCount - 1] = value;
    return callValue(value, argCount);
  }

  // 2) it's a method
  return invokeMethod(instance->klass, name, argCount);
}

// If already captured, return that ObjCapvar; else, create a new ObjCapvar to
// live on the heap and return it.
static ObjCapvar* capture(Value* local) {
  // Walk the linked list to find the correct ptr given the index
  ObjCapvar* prevCapvar = NULL;
  ObjCapvar* capvar = vm.stackCapvars;
  while (capvar != NULL && capvar->ptr > local) {
    prevCapvar = capvar;
    capvar = capvar->next;
  }

  // Is there a match?
  if (capvar != NULL && capvar->ptr == local) return capvar;

  // No match; create new capvar and insert in the right place
  ObjCapvar* createdCapvar = newCapvar(local);
  createdCapvar->next = capvar;
  if (prevCapvar == NULL) {
    vm.stackCapvars = createdCapvar;
  } else {
    prevCapvar->next = createdCapvar;
  }
  return createdCapvar;
}

// Moves all captured variables on the stack down to `until` to the heap.
static void moveCapvars(Value* until) {
  while (vm.stackCapvars != NULL && vm.stackCapvars->ptr >= until) {
    ObjCapvar* capvar = vm.stackCapvars;
    capvar->heapobj = *capvar->ptr;  // Move value from stack to ObjCapvar
    capvar->ptr = &capvar->heapobj;  // Adjust pointer
    vm.stackCapvars = capvar->next;
  }
}


// Adds `name` to the class table.
static void defineMethod(ObjString* name) {
  // There is a ClosureObj on top of the stack, and the class is behind it.
  Value method = peek(0);
  ObjClass* klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, name, method);
  pop();
}

// Binds a method to a variable; for e.g. `var bnd_fn = instance.fn;`
static bool bindMethod(ObjClass* klass, ObjString* name) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars); return false;
  }

  // The variable that the method is being bound to is on top of the vm stack.
  ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));
  pop();
  push(OBJ_VAL(bound));
  return true;
}



/*** RUN & INTERPRET ***/

// The most important function of clox! :) clox will spend 90% of its time here
static InterpretResult run() {
  // store the current topmost CallFrame in a local variable
  CallFrame* frame = &vm.frames[vm.frameCount - 1];


/** helper macros **/

// Return the currently pointed-to byte and then increments the pointer
#define READ_BYTE() (*frame->ip++)
// Looks up byte (as a number index) in the constants table
#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])
// Reads 2 bytes
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

// TODO....
#define READ_STRING() AS_STRING(READ_CONSTANT())

/* * * * * * * * * * * * * * * *  Language  Hint  * * * * * * * * * * * * * * *
Using a do...while(false) loop in a macro allows you to perform multiple
semicolon-terminated statements in a block. Otherwise, the macro would finish
after the first semicolon.
*/
// Perform a binary operation on top 2 stack elements.
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)



/*** main run loop ***/

  for (;;) {

// Debug: print the values on the VM stack, and then print out the current
// disassembled instruction.
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk,
        (int)(frame->ip - frame->closure->function->chunk.code));
#endif

    // `Note: This code can be made faster via assembly code injection.

    // First, read 1 byte as an instruction OP_CODE.
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      // Constants & Literals; push value onto stack
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_NIL: push(NIL_VAL); break;
      case OP_TRUE: push(BOOL_VAL(true)); break;
      case OP_FALSE: push(BOOL_VAL(false)); break;

      case OP_POP: pop(); break;

      // Local variable; read `slot` from beginning frame pointer `start`.
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        push(frame->start[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        frame->start[slot] = peek(0);
        break;
      }
      
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name); 
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }

      case OP_GET_CAPVAR: {
        uint8_t slot = READ_BYTE();
        push(*frame->closure->capvars[slot]->ptr);
        break;
      }
      // OP_SET... is an expression so the value is left on top of the stack.
      case OP_SET_CAPVAR: {
        uint8_t slot = READ_BYTE();
        *frame->closure->capvars[slot]->ptr = peek(0);  // No pop.
        break;
      }

      case OP_GET_PROPERTY: {
        // The result of the lhs of . is already on top of the stack
        if (!IS_INSTANCE(peek(0))) {
          runtimeError("Only instances have properties.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(0));
        ObjString* name = READ_STRING();

        Value value;
        if (tableGet(&instance->fields, name, &value)) {
          pop();  // Instance.
          push(value);
          break;
        }
        if (!bindMethod(instance->klass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SET_PROPERTY: {
        // On top of the stack is the instance whose field is being set, and
        // above that the value to be stored in the instance's field.
        if (!IS_INSTANCE(peek(1))) {
          runtimeError("Only instances have fields.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(1));
        tableSet(&instance->fields, READ_STRING(), peek(0));

        Value value = pop();
        pop();
        push(value);
        break;
      }

      case OP_GET_SUPER: {  // At this point `super` is on top of the stack
        ObjString* name = READ_STRING();
        ObjClass* superclass = AS_CLASS(pop());
        if (!bindMethod(superclass, name)) { return INTERPRET_RUNTIME_ERROR; }
        break;
      }

      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }

      case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;

      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError("Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }

      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
      case OP_NOT: push(BOOL_VAL(isFalsey(pop()))); break;

      case OP_NEGATE:
        // Don't pop! Keep operands on the stack for garbage collection.
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }

        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;

      case OP_PRINT: printValue(pop()); printf("\n"); break;

      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }

      // Pass to callValue() to create/add call frame, then switch to that frame
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }

      // Invoke has a 2-bit operand; 1st represents method name, 2nd argCount
      case OP_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        if (!invoke(method, argCount)) { return INTERPRET_RUNTIME_ERROR; }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_SUPER_INVOKE: {  // superclass is on top of stack at this point
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        ObjClass* superclass = AS_CLASS(pop());
        if (!invokeMethod(superclass, method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }

      // Push a new closure onto the call frame and populate capvars block with
      // locals/capvars from current (new closure's enclosing) frame
      case OP_CLOSURE: {
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
        // `closure` points to the new function closure; frame->closure points
        // to the enclosing function closure.
        for (int i = 0; i < closure->capvarCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();
          if (isLocal) {
            // Capture the local var on the vm stack and create a new ObjCapvar
            closure->capvars[i] = capture(frame->start + index);
          } else {
            // If it's a capvar, simply pass the correct pointer down the scope.
            closure->capvars[i] = frame->closure->capvars[index];
          }
        }
        break;
      }

      case OP_MOVE_CAPVAR: moveCapvars(vm.stackTop - 1); pop(); break;

      // When return is called, a couple of things need to happen:
      // (1) the return value needs to be stored
      // (2) captured variables need to be moved to the heap
      // (3) the stack needs to be reset
      // (4) the top call frame needs to be popped off
      case OP_RETURN: {
        Value result = pop();  // (1)

        moveCapvars(frame->start);  // (2)

        vm.frameCount--;  // (4) pt 1
        if (vm.frameCount == 0) { pop(); return INTERPRET_OK; }  // all done?

        // (3); remove all local vars / arguments of current function as well as
        // the function object itself. Then push the return value back on.
        vm.stackTop = frame->start;
        push(result);

        frame = &vm.frames[vm.frameCount - 1];  // (4) pt 2
        break;
      }

      case OP_CLASS: push(OBJ_VAL(newClass(READ_STRING()))); break;

      case OP_INHERIT: {
        Value superclass = peek(1);
        if (!IS_CLASS(superclass)) {
          runtimeError("Superclass must be a class.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjClass* subclass = AS_CLASS(peek(0));
        // Copy inherited class methods into subclass method table. In clox,
        // once a class declaration is finished executing, the set of methods
        // for that class can never change (i.e. clox does not allow monkey
        // patching). Subclass methods can then overwrite the inherited ones.
        tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
        pop();  // Subclass.
        break;
      }

      case OP_METHOD: defineMethod(READ_STRING()); break;
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  ObjFunction* function = compile(source);  // our fake main()
  if (function == NULL) return INTERPRET_COMPILE_ERROR;

  // Push main() onto the stack and create a call frame for it
  push(OBJ_VAL(function));
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  callValue(OBJ_VAL(closure), 0);

  return run();
}

