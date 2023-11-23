#include "lib.hpp"

#include <stdexcept>

constexpr std::uint64_t OPCODE_BITMASK = 0b1111111;

#define OPCODE(op) OPCODE_##op
#define CASE(op)                                                               \
  OPCODE(op)                                                                   \
   :
#define CASE_ADDR(op)        &&OPCODE(op)
#define FETCH(insts, ip)     insts[(*ip)]
#define INC_JMP(inst, ip)    goto* JUMP_TABLE[inst[++(*ip)] & OPCODE_BITMASK]
#define JMP(inst, ip)        goto* JUMP_TABLE[inst[(*ip)] & OPCODE_BITMASK]
#define DISASM(vm, inst, ip) exec_disasm(vm, FETCH(inst, ip))

#define CHECK(expr)                                                            \
  if (!(expr)) {                                                               \
    return;                                                                    \
  }

namespace duck_type
{
  extern "C" void execute(Vm vm, Instruction* instructions, std::size_t* ip)
  {
    // MUST BE IN SYNC WITH ENUMS
    static const void* JUMP_TABLE[] = {
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
      DISASM(vm, instructions, ip);
      exec_pop(vm);
      INC_JMP(instructions, ip);
    }
    CASE(PopN)
    {
      DISASM(vm, instructions, ip);
      exec_pop_n(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Const)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_const(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(Store)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_store(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(Load)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_load(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(Nil)
    {
      DISASM(vm, instructions, ip);
      exec_nil(vm);
      INC_JMP(instructions, ip);
    }
    CASE(True)
    {
      DISASM(vm, instructions, ip);
      exec_true(vm);
      INC_JMP(instructions, ip);
    }
    CASE(False)
    {
      DISASM(vm, instructions, ip);
      exec_false(vm);
      INC_JMP(instructions, ip);
    }
    CASE(InitializeMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(AssignMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_assign_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(LookupMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_lookup_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(PeekMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_peek_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(InitializeConstructor)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_constructor(vm));
      INC_JMP(instructions, ip);
    }
    CASE(InitializeMethod)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_method(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(CreateVec)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_vec(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(CreateSizedVec)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_sized_vec(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(CreateDynamicVec)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_dyn_vec(vm));
      INC_JMP(instructions, ip);
    }
    CASE(CreateClosure)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_closure(vm));
      INC_JMP(instructions, ip);
    }
    CASE(CreateStruct)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_struct(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    CASE(CreateClass)
    {
      DISASM(vm, instructions, ip);
      exec_create_class(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(CreateModule)
    {
      DISASM(vm, instructions, ip);
      exec_create_module(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Equal)
    {
      DISASM(vm, instructions, ip);
      exec_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(NotEqual)
    {
      DISASM(vm, instructions, ip);
      exec_not_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Greater)
    {
      DISASM(vm, instructions, ip);
      exec_greater(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(GreaterEqual)
    {
      DISASM(vm, instructions, ip);
      exec_greater_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Less)
    {
      DISASM(vm, instructions, ip);
      exec_less(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(LessEqual)
    {
      DISASM(vm, instructions, ip);
      exec_less_equal(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Add)
    {
      DISASM(vm, instructions, ip);
      exec_add(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Sub)
    {
      DISASM(vm, instructions, ip);
      exec_sub(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Mul)
    {
      DISASM(vm, instructions, ip);
      exec_mul(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Div)
    {
      DISASM(vm, instructions, ip);
      exec_div(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Rem)
    {
      DISASM(vm, instructions, ip);
      exec_rem(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Index)
    {
      DISASM(vm, instructions, ip);
      exec_index(vm);
      INC_JMP(instructions, ip);
    }
    CASE(AssignIndex)
    {
      DISASM(vm, instructions, ip);
      exec_index_assign(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Or)
    {
      DISASM(vm, instructions, ip);
      exec_or(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(And)
    {
      DISASM(vm, instructions, ip);
      exec_and(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Not)
    {
      DISASM(vm, instructions, ip);
      exec_not(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Negate)
    {
      DISASM(vm, instructions, ip);
      exec_negate(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Check)
    {
      DISASM(vm, instructions, ip);
      exec_check(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Println)
    {
      DISASM(vm, instructions, ip);
      exec_println(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Jump)
    {
      DISASM(vm, instructions, ip);
      exec_jump(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(JumpIfFalse)
    {
      DISASM(vm, instructions, ip);
      exec_jump_if_false(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Loop)
    {
      DISASM(vm, instructions, ip);
      exec_loop(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Invoke)
    {
      DISASM(vm, instructions, ip);
      exec_call(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    CASE(Swap)
    {
      DISASM(vm, instructions, ip);
      exec_swap(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(SwapPop)
    {
      DISASM(vm, instructions, ip);
      exec_swap_pop(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Ret)
    {
      DISASM(vm, instructions, ip);
      return;
    }
    CASE(Req)
    {
      DISASM(vm, instructions, ip);
      exec_req(vm);
      JMP(instructions, ip);
    }
    CASE(Breakpoint)
    {
      DISASM(vm, instructions, ip);
      exec_dbg(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Export)
    {
      DISASM(vm, instructions, ip);
      exec_export(vm);
      INC_JMP(instructions, ip);
    }
    CASE(DefineGlobal)
    {
      DISASM(vm, instructions, ip);
      exec_define_global(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(DefineScope)
    {
      DISASM(vm, instructions, ip);
      exec_define_scope(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(Resolve)
    {
      DISASM(vm, instructions, ip);
      exec_resolve(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    CASE(EnterBlock)
    {
      DISASM(vm, instructions, ip);
      exec_enter_block(vm);
      INC_JMP(instructions, ip);
    }
    CASE(PopScope)
    {
      DISASM(vm, instructions, ip);
      exec_pop_scope(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Is)
    {
      DISASM(vm, instructions, ip);
      exec_is(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Quack)
    {
      DISASM(vm, instructions, ip);
      exec_quack(vm);
      INC_JMP(instructions, ip);
    }
    CASE(Unknown)
    {
      DISASM(vm, instructions, ip);
      exec_unknown(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
  }
}  // namespace duck_type