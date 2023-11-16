#pragma once

#include <cinttypes>

#define OPCODE(op) constexpr std::uint8_t OPCODE_##op = __COUNTER__

/// Unknown instruction
/// Value given when one cannot be interpreted
///
/// Encoding: None
OPCODE(UNKNOWN);
/// Looks up a constant value at the specified location.
///
/// Encoding: | usize |
OPCODE(CONST);
/// Pushes a nil value on to the stack
///
/// Encoding: None
OPCODE(NIL);
/// Pushes true on the stack
///
/// Encoding: None
OPCODE(TRUE);
/// Pushes false on the stack
///
/// Encoding: None
OPCODE(FALSE);
/// Pops a value off the stack
///
/// Encoding: None
OPCODE(POP);
/// Pops N values off the stack.
///
/// Encoding: | usize |
OPCODE(POPN);
/// Store the value on the stack in the given location
///
/// Encoding: | Storage | LongAddr |
OPCODE(STORE);
/// Load a value and push it onto the stack
///
/// Encoding: | Storage | LongAddr |
OPCODE(LOAD);
/// Assigns a value to a member on an object
///
/// \[ Value \] \
  /// \[ Object \]
///
/// Encoding: | usize |
OPCODE(ASSIGN_MEMBER);
/// Initializes a member of an object, keeping the object on the stack for further assignments
///
/// Encoding: | usize |
OPCODE(INITIALIZE_MEMBER);
/// Initializes a method on a class, keeping the class on the stack for further assignments
///
/// Encoding: | usize |
OPCODE(INITIALIZE_METHOD);
/// Initializes the constructor on a class, keeping the class on the stack for further assignments
///
/// Encoding: None
OPCODE(INITIALIZE_CONSTRUCTOR);
/// Looks up the member on the next value on the stack, replacing it with the member's value
///
/// Encoding: | usize |
OPCODE(LOOKUP_MEMBER);
/// Looks up the member of the next value on the stack, pushing the value
///
/// Encoding: | usize |
OPCODE(PEEK_MEMBER);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up, compared, and the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(EQUAL);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(NOT_EQUAL);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(GREATER);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(GREATER_EQUAL);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(LESS);
/// Pops two values off the stack, compares, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(LessEqual);
/// Pops two values off the stack, calculates the sum, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(ADD);
/// Pops two values off the stack, calculates the difference, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(SUB);
/// Pops two values off the stack, calculates the product, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(MUL);
/// Pops two values off the stack, calculates the quotient, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(DIV);
/// Pops two values off the stack, calculates the remainder, then pushes the result back on
///
/// With data two locations are looked up and the operation applied, the result placed back on the stack
///
/// Encoding: [ None ] | [ Storage | ShortAddr | Storage | ShortAddr ]
OPCODE(REM);
/// Peeks at the stack. If the top value is true, the ip in incremented
///
/// Encoding: | usize |
OPCODE(OR);
/// Peeks at the stack. If the top value is false, the ip is incremented
///
/// Encoding: | usize |
OPCODE(AND);
/// Pops a value off the stack, inverts its truthy value, then pushes that back on
///
/// Encoding: None
OPCODE(NOT);
/// Pops a value off the stack, inverts its numerical value, then pushes that back on
///
/// Encoding: None
OPCODE(NEGATE);
/// Pops a value off the stack, and compares it with the peeked value, pushing the new value on
///
/// Encoding: None
OPCODE(CHECK);
/// Pops a value off the stack and prints it to the screen
///
/// Encoding: None
OPCODE(PRINTLN);
/// Jumps the ip forward unconditionally
///
/// Encoding: | usize |
OPCODE(JUMP);
/// Jumps the ip forward if the value on the stack is falsy
///
/// Encoding: | usize |
OPCODE(JUMP_IF_FALSE);
/// Jumps the instruction pointer backwards a number of instructions
///
/// Encoding: | usize |
OPCODE(LOOP);
/// Calls the value on the stack. Number of arguments is specified by the modifying bits
///
/// Encoding: | usize |
OPCODE(INVOKE);
/// Swaps the two locations on the stack
///
/// Encoding: | ShortAddr | ShortAddr |
OPCODE(SWAP);
/// Swaps the last two items on the stack and pops
///
/// Encoding: None
OPCODE(SWAP_POP);
/// Exits from a function, returning nil on the previous frame
///
/// Encoding: None
OPCODE(RET);
/// Load an external file, or pull from the cache if already loaded.
/// The file name is the value on the stack
///
/// Encoding: None
OPCODE(REQ);
/// Create a vec of values and push it on the stack.
/// Items come off the top of the stack.
/// The number of items is specified in the encoding
///
/// Encoding: | usize |
OPCODE(CREATE_VEC);
/// Create a vec of values and push it on the stack.
/// The last item on the stack is copied as many times as the size indicates
///
/// Encoding: | usize |
OPCODE(CREATE_SIZED_VEC);
/// Create a vec of values and push it on the stack.
/// The last item is the size.
/// The next is the item to be copied the amount of times specified
///
/// Encoding: | usize |
OPCODE(CREATE_DYNAMIC_VEC);
/// Create a closure. The first item on the stack is the function itself, the second is the capture list
///
/// Encoding: None
OPCODE(CREATE_CLOSURE);
/// Create a new struct with the number of members as the bits
/// Values are popped off the stack as key values in that order
///
/// Encoding: | usize |
OPCODE(CREATE_STRUCT);
/// Create a new class.
/// The const in the encoding is the name
///
/// Encoding: | usize |
OPCODE(CREATE_CLASS);
/// Create a new module.
/// The const in the encoding is the name
///
/// Encoding: | usize |
OPCODE(CREATE_MODULE);
/// Halt the VM when this instruction is reached and enter the debugger
///
/// Encoding: None
OPCODE(BREAKPOINT);
/// Mark the current value as exported
///
/// Encoding: None
OPCODE(EXPORT);
/// Defines the identifier on the variable
/// The const in the encoding is the name
///
/// Encoding: | usize |
OPCODE(DEFINE);
/// Resolve the specified identifier
/// The const in the encoding is the name
///
/// Encoding: | usize |
OPCODE(RESOLVE);
/// Push a new env
///
/// Encoding: None
OPCODE(ENTER_BLOCK);
/// Pop an env
///
/// Encoding: None
OPCODE(POP_SCOPE);
/// Panic duck style
///
/// Encoding: None
OPCODE(QUACK);
