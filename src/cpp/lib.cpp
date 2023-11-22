#include "lib.hpp"

#include <stdexcept>

constexpr std::uint64_t OPCODE_BITMASK = 0b1111111;

#define OPCODE(op) OPCODE_##op
#define CASE(op)                                                               \
  OPCODE(op)                                                                   \
   :
#define CASE_ADDR(op)     &&OPCODE(op)
#define FETCH(insts, ip)  insts[(*ip)]
#define INC_JMP(inst, ip) goto* JUMP_TABLE[inst[++(*ip)] & OPCODE_BITMASK]
#define JMP(inst, ip)     goto* JUMP_TABLE[inst[(*ip)] & OPCODE_BITMASK]

namespace duck_type
{
  extern "C" void execute(
   Vm vm, Instruction* instructions, std::size_t* ip, Exp exp)
  {
    // MUST BE IN SYNC WITH ENUMS
    constexpr void* JUMP_TABLE[] = {
     CASE_ADDR(Unknown),
     CASE_ADDR(Const),
     CASE_ADDR(Nil),
     CASE_ADDR(True),
     CASE_ADDR(False),
     CASE_ADDR(Pop),
     CASE_ADDR(PopN),
     CASE_ADDR(Store),
     CASE_ADDR(Load),
     CASE_ADDR(AssignMember),
     CASE_ADDR(InitializeMember),
     CASE_ADDR(InitializeMethod),
     CASE_ADDR(InitializeConstructor),
     CASE_ADDR(LookupMember),
     CASE_ADDR(PeekMember),
     CASE_ADDR(Equal),
     CASE_ADDR(NotEqual),
     CASE_ADDR(Greater),
     CASE_ADDR(GreaterEqual),
     CASE_ADDR(Less),
     CASE_ADDR(LessEqual),
     CASE_ADDR(Add),
     CASE_ADDR(Sub),
     CASE_ADDR(Mul),
     CASE_ADDR(Div),
     CASE_ADDR(Rem),
     CASE_ADDR(Index),
     CASE_ADDR(AssignIndex),
     CASE_ADDR(Or),
     CASE_ADDR(And),
     CASE_ADDR(Not),
     CASE_ADDR(Negate),
     CASE_ADDR(Check),
     CASE_ADDR(Println),
     CASE_ADDR(Jump),
     CASE_ADDR(JumpIfFalse),
     CASE_ADDR(Loop),
     CASE_ADDR(Invoke),
     CASE_ADDR(Swap),
     CASE_ADDR(SwapPop),
     CASE_ADDR(Ret),
     CASE_ADDR(Req),
     CASE_ADDR(CreateVec),
     CASE_ADDR(CreateSizedVec),
     CASE_ADDR(CreateDynamicVec),
     CASE_ADDR(CreateClosure),
     CASE_ADDR(CreateStruct),
     CASE_ADDR(CreateClass),
     CASE_ADDR(CreateModule),
     CASE_ADDR(Breakpoint),
     CASE_ADDR(Export),
     CASE_ADDR(DefineGlobal),
     CASE_ADDR(DefineScope),
     CASE_ADDR(Resolve),
     CASE_ADDR(EnterBlock),
     CASE_ADDR(PopScope),
     CASE_ADDR(Is),
     CASE_ADDR(Quack),
    };

    JMP(instructions, ip);

    CASE(Pop)
    {
      exec_pop(vm);
      INC_JMP(instructions, ip);
    }
    CASE(PopN)
    {
      exec_pop_n(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Const)
    {
      exec_const(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(Store)
    {
      exec_store(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Load)
    {
      exec_load(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Nil)
    {
      exec_nil(vm);
      INC_JMP(instructions, ip);
    }
    CASE(True)
    {
      exec_true(vm);
      INC_JMP(instructions, ip);
    }
    CASE(False)
    {
      exec_false(vm);
      INC_JMP(instructions, ip);
    }
    CASE(InitializeMember)
    {
      exec_initialize_member(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(AssignMember)
    {
      exec_assign_member(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(LookupMember)
    {
      exec_lookup_member(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(PeekMember)
    {
      exec_peek_member(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(InitializeConstructor)
    {
      exec_initialize_constructor(vm, exp);
      INC_JMP(instructions, ip);
    }
    CASE(InitializeMethod)
    {
      exec_initialize_method(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateVec)
    {
      exec_create_vec(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateSizedVec)
    {
      exec_create_sized_vec(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateDynamicVec)
    {
      exec_create_dyn_vec(vm, exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateClosure)
    {
      exec_create_closure(vm, exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateStruct)
    {
      exec_create_struct(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateClass)
    {
      exec_create_class(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(CreateModule)
    {
      exec_create_module(vm, FETCH(instructions, ip), exp);
      INC_JMP(instructions, ip);
    }
    CASE(Equal)
    {
      exec_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(NotEqual)
    {
      exec_not_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Greater)
    {
      exec_greater(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(GreaterEqual)
    {
      exec_greater_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Less)
    {
      exec_less(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(LessEqual)
    {
      exec_less_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Add)
    {
      exec_add(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Sub)
    {
      exec_sub(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Mul)
    {
      exec_mul(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Div)
    {
      exec_div(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Rem)
    {
      exec_rem(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Index)
    {
      exec_index(vm);
      INC_JMP(instructions, ip);
    }
    CASE(AssignIndex)
    {
      exec_index_assign(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Or)
    {
      exec_or(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(And)
    {
      exec_and(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Not)
    {
      exec_not(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Negate)
    {
      exec_negate(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Check)
    {
      exec_check(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Println)
    {
      exec_println(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Jump)
    {
      exec_jump(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(JumpIfFalse)
    {
      exec_jump_if_false(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Loop)
    {
      exec_loop(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Invoke)
    {
      exec_call(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Swap)
    {
      exec_swap(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(SwapPop)
    {
      exec_swap_pop(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Req)
    {
      exec_req(vm, exp);
      JMP(instructions, ip);
    }
    CASE(Breakpoint)
    {
      exec_dbg(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Export)
    {
      exec_export(vm, exp);
      INC_JMP(instructions, ip);
    }
    CASE(DefineGlobal)
    {
      exec_define_global(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(DefineScope)
    {
      exec_define_scope(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Resolve)
    {
      exec_resolve(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(EnterBlock)
    {
      exec_enter_block(vm, exp);
      INC_JMP(instructions, ip);
    }
    CASE(PopScope)
    {
      exec_pop_scope(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Is)
    {
      exec_is(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Quack)
    {
      exec_quack(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Unknown)
    {
      exec_unknown(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Ret)
    {
      // break
    }
  }
}  // namespace duck_type