use super::*;
use crate::Context;
use ptr::SmartPtr;

#[derive(Clone)]
pub struct Function {
  pub airity: usize,
  locals: usize,
  ctx: SmartPtr<Context>,
}

impl Function {
  pub fn new(airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self {
      airity,
      locals,
      ctx,
    }
  }

  pub fn call(
    &mut self,
    thread: &mut crate::ExecutionThread,
    env: &mut crate::Env,
    args: Vec<crate::Value>,
  ) {
    if args.len() > self.airity {
      args.drain(0..self.airity);
    } else {
      while args.len() < self.airity {
        args.push(Value::nil);
      }
    }

    args.reserve(self.locals);
    thread.new_frame(self.ctx.clone());
    thread.set_stack(args);
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    &self.ctx
  }

  pub fn context(&self) -> &Context {
    &self.ctx
  }
}

impl Object for Function {}
