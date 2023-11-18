use crate::prelude::*;

#[derive(Usertype)]
#[uuid("988a6bd1-4a54-416f-aad5-0d1cc8ce652e")]
pub struct InstanceValue {
  #[trace]
  pub data: Value,
  #[trace]
  pub class: Value,
}

impl InstanceValue {
  pub fn new(data: Value, class: Value) -> Self {
    Self { data, class }
  }

  fn call_binary(&self, vm: &mut Vm, left: Value, op: &str, operand: Value) -> UsageResult<Value> {
    if let Some(mut op) = self.get_method(&mut vm.gc, &left, Field::named(op))? {
      vm.stack_push(operand);
      op.call(vm, 1)
    } else {
      Err(UsageError::Unimplemented(ops::ADD))
    }
  }
}

impl UsertypeFields for InstanceValue {
  /// Discards the error returned from struct when trying to access a method on it that won't exist
  fn get_field(&self, gc: &mut Gc, field: Field) -> UsageResult<Option<Value>> {
    Ok(self.data.get_member(gc, field).unwrap_or(None))
  }

  fn set_field(&mut self, gc: &mut Gc, field: Field, value: Value) -> UsageResult<()> {
    self.data.set_member(gc, field, value)
  }
}

impl UsertypeMethods for InstanceValue {
  fn get_method(&self, gc: &mut Gc, this: &Value, field: Field) -> UsageResult<Option<Value>> {
    if let Some(class) = self.class.cast_to::<ClassValue>() {
      Ok(class.get_method(gc, this, field))
    } else {
      Err(UsageError::Infallible)
    }
  }
}

impl Operators for InstanceValue {
  #[binary(with_vm)]
  fn __add__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::ADD, right)
  }

  #[binary(with_vm)]
  fn __sub__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::SUB, right)
  }

  #[binary(with_vm)]
  fn __mul__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::MUL, right)
  }

  #[binary(with_vm)]
  fn __div__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::DIV, right)
  }

  #[binary(with_vm)]
  fn __rem__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::REM, right)
  }

  #[binary(with_vm)]
  fn __eq__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::EQUALITY, right)
  }

  #[binary(with_vm)]
  fn __neq__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::NOT_EQUAL, right)
  }

  #[binary(with_vm)]
  fn __less__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::LESS, right)
  }

  #[binary(with_vm)]
  fn __leq__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::LESS_EQUAL, right)
  }

  #[binary(with_vm)]
  fn __greater__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::GREATER, right)
  }

  #[binary(with_vm)]
  fn __geq__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::GREATER_EQUAL, right)
  }

  #[binary(with_vm)]
  fn __index__(vm: &mut Vm, left: Value, right: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    lval.call_binary(vm, left, ops::INDEX, right)
  }

  #[ternary(with_vm)]
  fn __idxeq__(vm: &mut Vm, left: Value, index: Value, value: Value) -> UsageResult {
    let lval = left.cast_to::<InstanceValue>().ok_or(UsageError::Infallible)?;
    if let Some(mut op) = lval.get_method(&mut vm.gc, &left, Field::named(ops::INDEX_ASSIGN))? {
      vm.stack_push(index);
      vm.stack_push(value);
      op.call(vm, 2)
    } else {
      Err(UsageError::Unimplemented(ops::ADD))
    }
  }

  fn __ivk__(&mut self, vm: &mut Vm, this: Value, airity: usize) -> UsageResult {
    let mut callable = self
      .get(&mut vm.gc, &this, Field::named("__ivk__"))?
      .map(Ok)
      .unwrap_or(Err(UsageError::UnexpectedNil))?;

    callable.call(vm, airity)
  }

  fn __str__(&self) -> String {
    format!("<instance of {}>", self.class)
  }

  fn __dbg__(&self) -> String {
    self.__str__()
  }
}
