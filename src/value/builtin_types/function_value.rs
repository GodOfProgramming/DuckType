use crate::{code::FunctionConstant, prelude::*};
use macros::Class;
use ptr::SmartPtr;

#[derive(Clone, Class)]
pub struct FunctionValue {
  name: Option<String>,
  pub airity: usize,
  locals: usize,
  ctx: SmartPtr<Context>,
}

impl FunctionValue {
  pub fn new(name: Option<String>, airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self {
      name,
      airity,
      locals,
      ctx,
    }
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
  const ID: &'static str = "Function";

  fn stringify(&self) -> String {
    format!("fn {}", self.name.as_ref().map(|n| n.as_ref()).unwrap_or("<lambda>"))
  }

  fn debug_string(&self) -> String {
    format!("<{}>", self.stringify())
  }
}

impl From<&FunctionConstant> for FunctionValue {
  fn from(f: &FunctionConstant) -> Self {
    Self {
      name: f.name.clone(),
      airity: f.airity,
      locals: f.locals,
      ctx: f.ctx.clone(),
    }
  }
}
