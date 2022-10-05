use super::*;
use crate::Context;
use ptr::SmartPtr;

#[derive(Clone)]
pub struct FunctionValue {
  pub airity: usize,
  locals: usize,
  ctx: SmartPtr<Context>,
}

impl FunctionValue {
  pub fn new(airity: usize, locals: usize, ctx: SmartPtr<Context>) -> Self {
    Self {
      airity,
      locals,
      ctx,
    }
  }

  pub fn call(&self, thread: &mut crate::ExecutionThread, mut args: Args) {
    if args.count() > self.airity {
      args.list.drain(0..self.airity);
    } else {
      while args.count() < self.airity {
        args.list.push(Value::nil);
      }
    }

    args.list.reserve(self.locals);
    thread.new_frame(self.ctx.clone());
    thread.set_stack(args.list);
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

impl ComplexValue for FunctionValue {}
