use crate::{code::FunctionConstant, prelude::*};
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields)]
#[uuid("4263e9fa-21fe-420c-b5a9-beca8fe3ca05")]
pub struct FunctionValue {
  pub airity: usize,
  ctx: SmartPtr<Context>,
  #[trace]
  env: Value,
}

impl FunctionValue {
  pub fn from_constant(f: &FunctionConstant, env: Value) -> Self {
    Self {
      airity: f.airity,
      ctx: f.ctx.clone(),
      env,
    }
  }

  pub fn check_args(&self, airity: usize) -> UsageResult<()> {
    if airity == self.airity {
      Ok(())
    } else {
      Err(UsageError::ArgumentError(airity, self.airity))
    }
  }

  pub fn invoke(&mut self, vm: &mut Vm, offset: usize) -> UsageResult {
    let env = UsertypeHandle::new(vm.gc.handle_from(self.env.clone()));
    vm.run_fn(self.ctx.clone(), env, self.airity + offset)
      .map_err(UsageError::Preformatted)
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.ctx
  }

  pub fn context(&self) -> &Context {
    &self.ctx
  }

  pub fn context_mut(&mut self) -> &mut Context {
    &mut self.ctx
  }
}

#[methods]
impl FunctionValue {
  fn __ivk__(&mut self, vm: &mut Vm, _this_fn: Value, airity: usize) -> UsageResult {
    self.check_args(airity)?;
    self.invoke(vm, 0)
  }

  fn __str__(&self) -> String {
    format!("fn {}", self.ctx.meta.name.as_ref().map(|n| n.as_ref()).unwrap_or("<lambda>"))
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
