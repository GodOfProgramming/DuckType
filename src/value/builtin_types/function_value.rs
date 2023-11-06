use crate::{code::FunctionConstant, prelude::*};
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields)]
#[uuid("4263e9fa-21fe-420c-b5a9-beca8fe3ca05")]
pub struct FunctionValue {
  pub airity: BitsRepr,
  locals: usize,
  ctx: SmartPtr<Context>,
  #[trace]
  env: Value,
}

impl FunctionValue {
  pub fn new(airity: BitsRepr, locals: usize, ctx: SmartPtr<Context>, env: Value) -> Self {
    Self {
      airity,
      locals,
      ctx,
      env,
    }
  }

  pub fn from_constant(f: &FunctionConstant, env: Value) -> Self {
    Self {
      airity: f.airity,
      locals: f.locals,
      ctx: f.ctx.clone(),
      env,
    }
  }

  pub fn check_args(&self, args: &Args) -> UsageResult<()> {
    if args.list.len() == self.airity as usize {
      Ok(())
    } else {
      Err(UsageError::ArgumentError(args.list.len(), self.airity as usize))
    }
  }

  pub fn invoke(&mut self, vm: &mut Vm, mut args: Args) -> UsageResult {
    args.list.reserve(self.locals);
    let env = UsertypeHandle::new(Gc::handle_from(&mut vm.gc, self.env.clone()));
    vm.run_fn(self.ctx.clone(), env, args).map_err(UsageError::Preformated)
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
  fn __ivk__(&mut self, vm: &mut Vm, _this_fn: Value, args: Args) -> UsageResult {
    self.check_args(&args)?;
    self.invoke(vm, args)
  }

  fn __str__(&self) -> String {
    format!("fn {}", self.ctx.meta.name.as_ref().map(|n| n.as_ref()).unwrap_or("<lambda>"))
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
