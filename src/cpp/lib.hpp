#include <cinttypes>

#define DECL(name, ...)             extern "C" void name(__VA_ARGS__)
#define DECL_RET(retval, name, ...) extern "C" auto name(__VA_ARGS__)->retval

using Vm          = void*;
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

  extern "C" void execute(Vm vm, Instruction* instructions, std::size_t* ip);
}  // namespace duck_type

DECL(exec_disasm, Vm, Instruction);

DECL(exec_pop, Vm);

DECL(exec_pop_n, Vm, Instruction);

DECL_RET(bool, exec_const, Vm, Instruction);

DECL_RET(bool, exec_store, Vm, Instruction);

DECL_RET(bool, exec_load, Vm, Instruction);

DECL(exec_nil, Vm);

DECL(exec_true, Vm);

DECL(exec_false, Vm);

DECL_RET(bool, exec_add, Vm, Instruction);

DECL_RET(bool, exec_sub, Vm, Instruction);

DECL_RET(bool, exec_mul, Vm, Instruction);

DECL_RET(bool, exec_div, Vm, Instruction);

DECL_RET(bool, exec_rem, Vm, Instruction);

DECL_RET(bool, exec_equal, Vm, Instruction);

DECL_RET(bool, exec_not_equal, Vm, Instruction);

DECL_RET(bool, exec_greater, Vm, Instruction);

DECL_RET(bool, exec_greater_equal, Vm, Instruction);

DECL_RET(bool, exec_less, Vm, Instruction);

DECL_RET(bool, exec_less_equal, Vm, Instruction);

DECL_RET(bool, exec_index, Vm);

DECL_RET(bool, exec_index_assign, Vm);

DECL_RET(bool, exec_negate, Vm);

DECL_RET(bool, exec_not, Vm);

DECL(exec_or, Vm, Instruction);

DECL(exec_and, Vm, Instruction);

DECL_RET(bool, exec_initialize_member, Vm, Instruction);

DECL_RET(bool, exec_assign_member, Vm, Instruction);

DECL_RET(bool, exec_lookup_member, Vm, Instruction);

DECL_RET(bool, exec_peek_member, Vm, Instruction);

DECL_RET(bool, exec_initialize_constructor, Vm);

DECL_RET(bool, exec_initialize_method, Vm, Instruction);

DECL_RET(bool, exec_create_vec, Vm, Instruction);

DECL_RET(bool, exec_create_sized_vec, Vm, Instruction);

DECL_RET(bool, exec_create_dyn_vec, Vm);

DECL_RET(bool, exec_create_closure, Vm);

DECL_RET(bool, exec_create_struct, Vm, Instruction);

DECL_RET(bool, exec_create_class, Vm, Instruction);

DECL_RET(bool, exec_create_module, Vm, Instruction);

DECL_RET(bool, exec_check, Vm);

DECL(exec_println, Vm);

DECL(exec_jump, Vm, Instruction);

DECL(exec_jump_if_false, Vm, Instruction);

DECL(exec_loop, Vm, Instruction);

DECL_RET(bool, exec_call, Vm, Instruction);

DECL_RET(bool, exec_req, Vm);

DECL(exec_export, Vm);

DECL_RET(bool, exec_define_global, Vm, Instruction);

DECL_RET(bool, exec_define_scope, Vm, Instruction);

DECL_RET(bool, exec_resolve, Vm, Instruction);

DECL(exec_enter_block, Vm);

DECL(exec_pop_scope, Vm);

DECL(exec_swap, Vm, Instruction);

DECL(exec_swap_pop, Vm);

DECL_RET(bool, exec_is, Vm);

DECL_RET(bool, exec_quack, Vm);

DECL_RET(bool, exec_unknown, Vm, Instruction);

DECL_RET(bool, exec_dbg, Vm);
