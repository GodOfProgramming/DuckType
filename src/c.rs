use super::*;
#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn exec_disasm(vm: &Vm, inst: Instruction) {
  #[cfg(feature = "runtime-disassembly")]
  {
    vm.exec_disasm(inst);
  }
}

#[no_mangle]
pub extern "C" fn exec_pop(vm: &mut Vm) {
  vm.exec_pop();
}

#[no_mangle]
pub extern "C" fn exec_pop_n(vm: &mut Vm, inst: Instruction) {
  vm.exec_pop_n(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_const(vm: &mut Vm, inst: Instruction) {
  vm.exec_const(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_store(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_store(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_load(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_load(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_nil(vm: &mut Vm) {
  vm.exec_nil()
}

#[no_mangle]
pub extern "C" fn exec_true(vm: &mut Vm) {
  vm.exec_true();
}

#[no_mangle]
pub extern "C" fn exec_false(vm: &mut Vm) {
  vm.exec_false();
}

#[no_mangle]
pub extern "C" fn exec_add(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_add(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_sub(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_sub(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_mul(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_mul(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_div(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_div(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_rem(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_rem(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_not_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_not_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_greater(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_greater(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_greater_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_greater_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_less(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_less(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_less_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_less_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_index(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_index();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_index_assign(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_index_assign();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_negate(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_negate();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_not(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_not();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_or(vm: &mut Vm, inst: Instruction) {
  vm.exec_or(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_and(vm: &mut Vm, inst: Instruction) {
  vm.exec_and(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_initialize_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_initialize_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_assign_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_assign_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_lookup_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_lookup_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_peek_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_peek_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_initialize_constructor(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_initialize_constructor();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_initialize_method(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_initialize_method(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_vec(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_vec(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_sized_vec(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_sized_vec(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_dyn_vec(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_create_dyn_vec();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_closure(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_create_closure();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_struct(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_struct(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_class(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_class(inst.data());
  vm.last_error.is_ok()
}

/// Create a module and make it the current
#[no_mangle]
pub extern "C" fn exec_create_module(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_module(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_check(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_check();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_println(vm: &mut Vm) {
  vm.exec_println();
}

#[no_mangle]
pub extern "C" fn exec_jump(vm: &mut Vm, inst: Instruction) {
  vm.exec_jump(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_jump_if_false(vm: &mut Vm, inst: Instruction) {
  vm.exec_jump_if_false(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_loop(vm: &mut Vm, inst: Instruction) {
  vm.exec_loop(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_call(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_call(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_req(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_req();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_export(vm: &mut Vm) {
  vm.exec_export();
}

#[no_mangle]
pub extern "C" fn exec_define_global(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_define_global(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_define_scope(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_define_scope(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_resolve(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_resolve(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_enter_block(vm: &mut Vm) {
  vm.exec_enter_block();
}

#[no_mangle]
pub extern "C" fn exec_pop_scope(vm: &mut Vm) {
  vm.exec_pop_scope();
}

#[no_mangle]
pub extern "C" fn exec_swap(vm: &mut Vm, inst: Instruction) {
  vm.exec_swap(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_swap_pop(vm: &mut Vm) {
  vm.exec_swap_pop();
}

#[no_mangle]
pub extern "C" fn exec_is(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_is();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_quack(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_quack();
  vm.last_error.is_ok()
}

#[cold]
#[no_mangle]
pub extern "C" fn exec_unknown(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_unknown(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_dbg(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_dbg();
  vm.last_error.is_ok()
}
