use crate::{code::FunctionConstant, prelude::*};
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields)]
#[uuid("4263e9fa-21fe-420c-b5a9-beca8fe3ca05")]
pub struct FunctionValue {
  pub airity: usize,
  locals: usize,
  ctx: SmartPtr<Context>,
}

impl FunctionValue {
  pub fn new(airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self { airity, locals, ctx }
  }

  pub fn call(&self, vm: &mut Vm, mut args: Vec<Value>) {
    if args.len() > self.airity {
      args.drain(0..self.airity);
    } else {
      while args.len() < self.airity {
        args.push(Value::nil);
      }
    }

    args.reserve(self.locals);
    vm.new_frame(self.ctx.clone());
    vm.set_stack(args);
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
  fn __new__() -> ValueResult {
    Err(ValueError::Infallible)
  }

  fn __str__(&self) -> String {
    format!("fn {}", self.ctx.name.as_ref().map(|n| n.as_ref()).unwrap_or("<lambda>"))
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}

impl From<&FunctionConstant> for FunctionValue {
  fn from(f: &FunctionConstant) -> Self {
    Self {
      airity: f.airity,
      locals: f.locals,
      ctx: f.ctx.clone(),
    }
  }
}
