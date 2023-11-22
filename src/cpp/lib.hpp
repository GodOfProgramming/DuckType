#include <cinttypes>

#define DECL(name, ...) extern "C" void name(__VA_ARGS__)

using Vm          = void*;
using Exp         = void*;
using Instruction = std::uint64_t;

namespace duck_type
{
  enum Opcode : std::uint8_t
  {
    Unknown,
    Const,
    Nil,
    True,
    False,
    Pop,
    PopN,
    Store,
    Load,
    AssignMember,
    InitializeMember,
    InitializeMethod,
    InitializeConstructor,
    LookupMember,
    PeekMember,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Index,
    AssignIndex,
    Or,
    And,
    Not,
    Negate,
    Check,
    Println,
    Jump,
    JumpIfFalse,
    Loop,
    Invoke,
    Swap,
    SwapPop,
    Ret,
    Req,
    CreateVec,
    CreateSizedVec,
    CreateDynamicVec,
    CreateClosure,
    CreateStruct,
    CreateClass,
    CreateModule,
    Breakpoint,
    Export,
    DefineGlobal,
    DefineScope,
    Resolve,
    EnterBlock,
    PopScope,
    Is,
    Quack,
  };

  extern "C" void execute(
   Vm vm, Instruction* instructions, std::size_t* ip, Exp exp);
}  // namespace duck_type

DECL(exec_disasm, Vm, Instruction);

DECL(exec_pop, Vm);

DECL(exec_pop_n, Vm, Instruction);

DECL(exec_const, Vm, Instruction, Exp);

DECL(exec_store, Vm, Instruction);

DECL(exec_load, Vm, Instruction);

DECL(exec_nil, Vm);

DECL(exec_true, Vm);

DECL(exec_false, Vm);

DECL(exec_initialize_member, Vm, Instruction, Exp);

DECL(exec_assign_member, Vm, Instruction, Exp);

DECL(exec_lookup_member, Vm, Instruction, Exp);

DECL(exec_peek_member, Vm, Instruction, Exp);

DECL(exec_initialize_constructor, Vm, Exp);

DECL(exec_initialize_method, Vm, Instruction, Exp);

DECL(exec_create_vec, Vm, Instruction, Exp);

DECL(exec_create_sized_vec, Vm, Instruction, Exp);

DECL(exec_create_dyn_vec, Vm, Exp);

DECL(exec_create_closure, Vm, Exp);

DECL(exec_create_struct, Vm, Instruction, Exp);

DECL(exec_create_class, Vm, Instruction, Exp);

DECL(exec_create_module, Vm, Instruction, Exp);

DECL(exec_check, Vm);

DECL(exec_println, Vm);

DECL(exec_jump, Vm, Instruction);

DECL(exec_jump_if_false, Vm, Instruction);

DECL(exec_loop, Vm, Instruction);

DECL(exec_call, Vm, Instruction);

DECL(exec_req, Vm, Exp);

DECL(exec_dbg, Vm);

DECL(exec_export, Vm, Exp);

DECL(exec_define_global, Vm, Instruction);

DECL(exec_define_scope, Vm, Instruction);

DECL(exec_resolve, Vm, Instruction);

DECL(exec_enter_block, Vm, Exp);

DECL(exec_pop_scope, Vm);

DECL(exec_equal, Vm, Instruction);

DECL(exec_not_equal, Vm, Instruction);

DECL(exec_greater, Vm, Instruction);

DECL(exec_greater_equal, Vm, Instruction);

DECL(exec_less, Vm, Instruction);

DECL(exec_less_equal, Vm, Instruction);

DECL(exec_add, Vm, Instruction);

DECL(exec_sub, Vm, Instruction);

DECL(exec_mul, Vm, Instruction);

DECL(exec_div, Vm, Instruction);

DECL(exec_rem, Vm, Instruction);

DECL(exec_negate, Vm);

DECL(exec_not, Vm);

DECL(exec_index, Vm);

DECL(exec_index_assign, Vm);

DECL(exec_or, Vm, Instruction);

DECL(exec_and, Vm, Instruction);

DECL(exec_swap, Vm, Instruction);

DECL(exec_swap_pop, Vm);

DECL(exec_is, Vm);

DECL(exec_quack, Vm);

DECL(exec_unknown, Vm, Instruction);
