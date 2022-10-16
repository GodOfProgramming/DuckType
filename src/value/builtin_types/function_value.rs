use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone)]
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

impl Usertype for FunctionValue {
  const ID: UsertypeId = "Function";

  fn stringify(&self) -> String {
    format!("fn {}", self.ctx.name())
  }

  fn debug_string(&self) -> String {
    format!("<{}>", self.stringify())
  }
}
