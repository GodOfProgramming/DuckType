#include "lib.hpp"

#include <stdexcept>

constexpr std::uint64_t OPCODE_BITMASK = 0x7f;

#define OPCODE(op) OPCODE_##op
#define ADDR(op)                                                                                                               \
  OPCODE(op)                                                                                                                   \
   :
#define JTBL_ADDR(op)     &&OPCODE(op)
#define FETCH(insts, ip)  insts[(*ip)]
#define INC_JMP(inst, ip) goto* JUMP_TABLE[inst[++(*ip)] & OPCODE_BITMASK]
#define JMP(inst, ip)     goto* JUMP_TABLE[inst[(*ip)] & OPCODE_BITMASK]

#if DISASM_ENABLED
#define DISASM(vm, inst, ip) exec_disasm(vm, FETCH(inst, ip))
#else
#define DISASM(vm, inst, ip)
#endif

#define CHECK(expr)                                                                                                            \
  if (!(expr)) {                                                                                                               \
    return;                                                                                                                    \
  }

namespace duck_type
{
  extern "C" void execute(Vm vm, Instruction const* instructions, std::size_t* ip)
  {
    // MUST BE IN SYNC WITH ENUMS
    static void const* JUMP_TABLE[] = {
     JTBL_ADDR(Unknown),
     JTBL_ADDR(Const),
     JTBL_ADDR(Nil),
     JTBL_ADDR(True),
     JTBL_ADDR(False),
     JTBL_ADDR(Pop),
     JTBL_ADDR(PopN),
     JTBL_ADDR(Store),
     JTBL_ADDR(Load),
     JTBL_ADDR(AssignMember),
     JTBL_ADDR(InitializeMember),
     JTBL_ADDR(InitializeMethod),
     JTBL_ADDR(InitializeConstructor),
     JTBL_ADDR(LookupMember),
     JTBL_ADDR(PeekMember),
     JTBL_ADDR(Equal),
     JTBL_ADDR(NotEqual),
     JTBL_ADDR(Greater),
     JTBL_ADDR(GreaterEqual),
     JTBL_ADDR(Less),
     JTBL_ADDR(LessEqual),
     JTBL_ADDR(Add),
     JTBL_ADDR(Sub),
     JTBL_ADDR(Mul),
     JTBL_ADDR(Div),
     JTBL_ADDR(Rem),
     JTBL_ADDR(Index),
     JTBL_ADDR(AssignIndex),
     JTBL_ADDR(Or),
     JTBL_ADDR(And),
     JTBL_ADDR(Not),
     JTBL_ADDR(Negate),
     JTBL_ADDR(Check),
     JTBL_ADDR(Println),
     JTBL_ADDR(Jump),
     JTBL_ADDR(JumpIfFalse),
     JTBL_ADDR(Loop),
     JTBL_ADDR(Invoke),
     JTBL_ADDR(Swap),
     JTBL_ADDR(SwapPop),
     JTBL_ADDR(Ret),
     JTBL_ADDR(Req),
     JTBL_ADDR(CreateVec),
     JTBL_ADDR(CreateSizedVec),
     JTBL_ADDR(CreateDynamicVec),
     JTBL_ADDR(CreateClosure),
     JTBL_ADDR(CreateStruct),
     JTBL_ADDR(CreateClass),
     JTBL_ADDR(CreateModule),
     JTBL_ADDR(Breakpoint),
     JTBL_ADDR(Export),
     JTBL_ADDR(DefineGlobal),
     JTBL_ADDR(DefineScope),
     JTBL_ADDR(Resolve),
     JTBL_ADDR(EnableModule),
     JTBL_ADDR(PopScope),
     JTBL_ADDR(Is),
     JTBL_ADDR(Quack),
    };

    JMP(instructions, ip);

    ADDR(Pop)
    {
      DISASM(vm, instructions, ip);
      exec_pop(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(PopN)
    {
      DISASM(vm, instructions, ip);
      exec_pop_n(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    ADDR(Const)
    {
      DISASM(vm, instructions, ip);
      exec_const(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    ADDR(Store)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_store(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Load)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_load(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Nil)
    {
      DISASM(vm, instructions, ip);
      exec_nil(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(True)
    {
      DISASM(vm, instructions, ip);
      exec_true(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(False)
    {
      DISASM(vm, instructions, ip);
      exec_false(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Equal)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_equal(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(NotEqual)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_not_equal(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Greater)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_greater(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(GreaterEqual)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_greater_equal(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Less)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_less(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(LessEqual)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_less_equal(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Add)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_add(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Sub)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_sub(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Mul)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_mul(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Div)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_div(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Rem)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_rem(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Index)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_index(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(AssignIndex)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_index_assign(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(Not)
    {
      DISASM(vm, instructions, ip);
      exec_not(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Negate)
    {
      DISASM(vm, instructions, ip);
      exec_negate(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Or)
    {
      DISASM(vm, instructions, ip);
      exec_or(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    ADDR(And)
    {
      DISASM(vm, instructions, ip);
      exec_and(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    ADDR(InitializeMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(AssignMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_assign_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(LookupMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_lookup_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(PeekMember)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_peek_member(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(InitializeConstructor)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_constructor(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(InitializeMethod)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_initialize_method(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateVec)
    {
      DISASM(vm, instructions, ip);
      exec_create_vec(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateSizedVec)
    {
      DISASM(vm, instructions, ip);
      exec_create_sized_vec(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateDynamicVec)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_dyn_vec(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateClosure)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_closure(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateStruct)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_struct(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateClass)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_class(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(CreateModule)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_create_module(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Check)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_check(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(Println)
    {
      DISASM(vm, instructions, ip);
      exec_println(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Jump)
    {
      DISASM(vm, instructions, ip);
      exec_jump(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    ADDR(JumpIfFalse)
    {
      DISASM(vm, instructions, ip);
      exec_jump_if_false(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    ADDR(Loop)
    {
      DISASM(vm, instructions, ip);
      exec_loop(vm, FETCH(instructions, ip));
      JMP(instructions, ip);
    }
    ADDR(Invoke)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_call(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Req)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_req(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(Ret)
    {
      DISASM(vm, instructions, ip);
      return;
    }
    ADDR(Export)
    {
      DISASM(vm, instructions, ip);
      exec_export(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(DefineGlobal)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_define_global(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(DefineScope)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_define_scope(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Resolve)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_resolve(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(EnableModule)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_enable_module(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(PopScope)
    {
      DISASM(vm, instructions, ip);
      exec_pop_scope(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Swap)
    {
      DISASM(vm, instructions, ip);
      exec_swap(vm, FETCH(instructions, ip));
      INC_JMP(instructions, ip);
    }
    ADDR(SwapPop)
    {
      DISASM(vm, instructions, ip);
      exec_swap_pop(vm);
      INC_JMP(instructions, ip);
    }
    ADDR(Is)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_is(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(Quack)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_quack(vm));
      INC_JMP(instructions, ip);
    }
    ADDR(Unknown)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_unknown(vm, FETCH(instructions, ip)));
      INC_JMP(instructions, ip);
    }
    ADDR(Breakpoint)
    {
      DISASM(vm, instructions, ip);
      CHECK(exec_dbg(vm));
      INC_JMP(instructions, ip);
    }
  }
}  // namespace duck_type
