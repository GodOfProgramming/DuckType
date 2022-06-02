mod code;
pub mod types;

#[cfg(test)]
mod test;

pub use code::Context;
pub use code::Env;
use code::{Compiler, OpCode, OpCodeReflection};
use ptr::SmartPtr;
use std::fs;
use types::Error;
pub use types::{Struct, Value, ValueOpResult};

pub trait New<T> {
  fn new(item: T) -> Self;
}

type ExecResult = Result<(), Vec<Error>>;
type ExecBoolResult = Result<bool, Vec<Error>>;

#[derive(Default)]
pub struct ExecutionThread {
  pub ip: usize,
  stack: Vec<Value>,
  disassemble: bool,
  runtime_disassemble: bool,
}

impl ExecutionThread {
  fn new(disassemble: bool, runtime_disassemble: bool) -> Self {
    Self {
      ip: Default::default(),
      stack: Default::default(),
      disassemble,
      runtime_disassemble,
    }
  }

  fn unary_op<F: FnOnce(&mut Self, &mut Context, Value) -> ExecResult>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    if let Some(v) = self.stack_pop() {
      f(self, ctx, v)
    } else {
      Err(self.error(ctx, opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn binary_op<F: FnOnce(&mut Self, &mut Context, Value, Value) -> ExecResult>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    if let Some(bv) = self.stack_pop() {
      if let Some(av) = self.stack_pop() {
        f(self, ctx, av, bv)
      } else {
        Err(self.error(ctx, opcode, String::from("cannot operate on empty stack")))
      }
    } else {
      Err(self.error(ctx, opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn global_op<F: FnOnce(&mut Self, &mut Context, String) -> ExecResult>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    index: usize,
    f: F,
  ) -> ExecResult {
    if let Some(name) = ctx.global_const_at(index) {
      if let Value::Str(name) = name {
        f(self, ctx, name)
      } else {
        Err(self.error(
          ctx,
          opcode,
          format!("global variable name is not an identifier: {}", name),
        ))
      }
    } else {
      Err(self.error(
        ctx,
        opcode,
        String::from("global variable name does not exist"),
      ))
    }
  }

  fn error(&self, ctx: &Context, opcode: &OpCode, msg: String) -> Vec<Error> {
    vec![self.error_at(ctx, |opcode_ref| Error::from_ref(msg, opcode, opcode_ref))]
  }

  fn run(&mut self, mut ctx: SmartPtr<Context>, env: &mut Env) -> Result<Value, Vec<Error>> {
    self.ip = 0;
    while let Some(opcode) = ctx.next(self.ip) {
      #[cfg(debug_assertions)]
      if self.runtime_disassemble {
        self.stack_display();
        ctx.display_instruction(&opcode, self.ip);
      }

      match opcode {
        OpCode::NoOp => self.exec_noop(&mut ctx)?,
        OpCode::Const(index) => self.exec_const(&mut ctx, &opcode, index)?,
        OpCode::Nil => self.exec_nil(),
        OpCode::True => self.exec_true(),
        OpCode::False => self.exec_false(),
        OpCode::Pop => self.exec_pop(),
        OpCode::PopN(count) => self.exec_pop_n(count),
        OpCode::LookupLocal(index) => self.exec_lookup_local(&mut ctx, &opcode, index)?,
        OpCode::AssignLocal(index) => self.exec_assign_local(&mut ctx, &opcode, index)?,
        OpCode::LookupGlobal(index) => self.exec_lookup_global(&mut ctx, env, &opcode, index)?,
        OpCode::DefineGlobal(index) => self.exec_define_global(&mut ctx, env, &opcode, index)?,
        OpCode::AssignGlobal(index) => self.exec_assign_global(&mut ctx, env, &opcode, index)?,
        OpCode::AssignMember(index) => self.exec_assign_member(&mut ctx, &opcode, index)?,
        OpCode::LookupMember(index) => self.exec_lookup_member(&mut ctx, &opcode, index)?,
        OpCode::Equal => self.exec_equal(&mut ctx, &opcode)?,
        OpCode::NotEqual => self.exec_not_equal(&mut ctx, &opcode)?,
        OpCode::Greater => self.exec_greater(&mut ctx, &opcode)?,
        OpCode::GreaterEqual => self.exec_greater_equal(&mut ctx, &opcode)?,
        OpCode::Less => self.exec_less(&mut ctx, &opcode)?,
        OpCode::LessEqual => self.exec_less_equal(&mut ctx, &opcode)?,
        OpCode::Check => self.exec_check(&mut ctx, &opcode)?,
        OpCode::Add => self.exec_add(&mut ctx, &opcode)?,
        OpCode::Sub => self.exec_sub(&mut ctx, &opcode)?,
        OpCode::Mul => self.exec_mul(&mut ctx, &opcode)?,
        OpCode::Div => self.exec_div(&mut ctx, &opcode)?,
        OpCode::Mod => self.exec_mod(&mut ctx, &opcode)?,
        OpCode::Or(count) => {
          if self.exec_or(&mut ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::And(count) => {
          if self.exec_and(&mut ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::Not => self.exec_not(&mut ctx, &opcode)?,
        OpCode::Negate => self.exec_negate(&mut ctx, &opcode)?,
        OpCode::Print => self.exec_print(&mut ctx, &opcode)?,
        OpCode::Jump(count) => {
          self.jump(count);
          continue;
        }
        OpCode::JumpIfFalse(count) => {
          if self.exec_jump_if_false(&mut ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::Loop(count) => {
          self.loop_back(count);
          continue;
        }
        OpCode::Call(airity) => self.exec_call(&mut ctx, env, &opcode, airity)?,
        OpCode::Ret => return Ok(self.exec_ret()),
        OpCode::Req => self.exec_req(&mut ctx, env, &opcode)?,
        OpCode::Index => self.exec_index(&mut ctx, &opcode)?,
        OpCode::CreateList(num_items) => self.exec_create_list(num_items),
        x => unimplemented!("Unimplemented: {:?}", x),
      }
      self.ip += 1;
    }

    #[cfg(debug_assertions)]
    if self.runtime_disassemble {
      println!("<< EOF >>");
      self.stack_display();
    }

    Ok(Value::Nil)
  }

  /* Operations */

  fn exec_noop(&self, ctx: &mut Context) -> ExecResult {
    Err(self.error(
      ctx,
      &OpCode::NoOp,
      String::from("executed noop opcode, should not happen"),
    ))
  }

  fn exec_const(&mut self, ctx: &mut Context, opcode: &OpCode, index: usize) -> ExecResult {
    if let Some(c) = ctx.const_at(index) {
      self.stack_push(c);
      Ok(())
    } else {
      Err(self.error(ctx, opcode, String::from("could not lookup constant")))
    }
  }

  fn exec_nil(&mut self) {
    self.stack_push(Value::Nil);
  }

  fn exec_true(&mut self) {
    self.stack_push(Value::new(true));
  }

  fn exec_false(&mut self) {
    self.stack_push(Value::new(false));
  }

  fn exec_pop(&mut self) {
    self.stack_pop();
  }

  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  fn exec_lookup_local(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    if let Some(local) = self.stack_index(location) {
      self.stack_push(local);
      Ok(())
    } else {
      Err(self.error(
        ctx,
        opcode,
        format!("could not index stack at pos {}", location),
      ))
    }
  }

  fn exec_assign_local(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      self.stack_assign(location, value.clone());
      Ok(())
    } else {
      Err(self.error(
        ctx,
        opcode,
        format!("could not replace stack value at pos {}", location),
      ))
    }
  }

  fn exec_lookup_global(
    &mut self,
    ctx: &mut Context,
    env: &Env,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    self.global_op(ctx, opcode, location, |this, ctx, name| {
      if let Some(global) = env.lookup(&name) {
        this.stack_push(global);
        Ok(())
      } else {
        Err(this.error(ctx, opcode, String::from("use of undefined variable")))
      }
    })
  }

  fn exec_define_global(
    &mut self,
    ctx: &mut Context,
    env: &mut Env,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    self.global_op(ctx, opcode, location, |this, ctx, name| {
      if let Some(v) = this.stack_peek() {
        if env.define(name, v.clone()) {
          Ok(())
        } else {
          Err(this.error(
            ctx,
            opcode,
            String::from("tried redefining global variable"),
          ))
        }
      } else {
        Err(this.error(
          ctx,
          opcode,
          String::from("can not define global using empty stack"),
        ))
      }
    })
  }

  fn exec_assign_global(
    &mut self,
    ctx: &mut Context,
    env: &mut Env,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    self.global_op(ctx, opcode, location, |this, ctx, name| {
      if let Some(v) = this.stack_peek() {
        if env.assign(name, v) {
          Ok(())
        } else {
          Err(this.error(
            ctx,
            opcode,
            String::from("tried to assign to nonexistent global"),
          ))
        }
      } else {
        Err(this.error(
          ctx,
          opcode,
          String::from("can not assign to global using empty stack"),
        ))
      }
    })
  }

  fn exec_assign_member(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    match ctx.const_at(location) {
      Some(name) => match self.stack_pop() {
        Some(value) => match self.stack_peek() {
          Some(obj) => match obj {
            Value::Struct(mut obj) => match name {
              Value::Str(name) => {
                obj.set(name, value);
                Ok(())
              }
              _ => Err(self.error(ctx, opcode, String::from("member name is not a string"))),
            },
            _ => Err(self.error(
              ctx,
              opcode,
              String::from("tried to assigning to non object"),
            )),
          },
          None => Err(self.error(ctx, opcode, String::from("no object on stack to assign to"))),
        },
        None => Err(self.error(
          ctx,
          opcode,
          String::from("no value on stack to assign to member"),
        )),
      },
      None => Err(self.error(ctx, opcode, String::from("no member ident to assign to"))),
    }
  }

  fn exec_lookup_member(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    match ctx.const_at(location) {
      Some(name) => match self.stack_pop() {
        Some(obj) => match obj {
          Value::Struct(obj) => match name {
            Value::Str(ident) => {
              self.stack_push(obj.get(ident));
              Ok(())
            }
            _ => Err(self.error(ctx, opcode, String::from("invalid member name"))),
          },
          _ => Err(self.error(ctx, opcode, String::from("invalid type for member access"))),
        },
        None => Err(self.error(
          ctx,
          opcode,
          String::from("no object to for member access on the stack"),
        )),
      },
      None => Err(self.error(ctx, opcode, String::from("no constant specified by index"))),
    }
  }

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    self.binary_op(ctx, opcode, |this, _ctx, a, b| {
      this.stack_push(Value::new(f(a, b)));
      Ok(())
    })
  }

  fn exec_equal(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a == b)
  }

  fn exec_not_equal(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a != b)
  }

  fn exec_greater(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a > b)
  }

  fn exec_greater_equal(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a >= b)
  }

  fn exec_less(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a < b)
  }

  fn exec_less_equal(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a <= b)
  }

  fn exec_check(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(a) => match self.stack_peek() {
        Some(b) => {
          self.stack_push(Value::new(a == b));
          Ok(())
        }
        None => Err(self.error(ctx, opcode, String::from("stack peek failed"))),
      },
      None => Err(self.error(ctx, opcode, String::from("stack pop failed"))),
    }
  }

  fn exec_arith<F: FnOnce(Value, Value) -> ValueOpResult>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    self.binary_op(ctx, opcode, |this, ctx, a, b| match f(a, b) {
      Ok(v) => {
        this.stack_push(v);
        Ok(())
      }
      Err(e) => Err(this.error(ctx, opcode, e)),
    })
  }

  fn exec_add(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a + b)
  }

  fn exec_sub(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a - b)
  }

  fn exec_mul(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a * b)
  }

  fn exec_div(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a / b)
  }

  fn exec_mod(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a % b)
  }

  fn exec_logical<F: FnOnce(Value) -> bool>(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    offset: usize,
    f: F,
  ) -> ExecBoolResult {
    match self.stack_peek() {
      Some(v) => {
        if f(v) {
          self.jump(offset);
          Ok(true)
        } else {
          self.stack_pop();
          Ok(false)
        }
      }
      None => Err(self.error(ctx, opcode, String::from("no item on the stack to peek"))),
    }
  }

  fn exec_or(&mut self, ctx: &mut Context, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(ctx, opcode, offset, |v| v.truthy())
  }

  fn exec_and(&mut self, ctx: &mut Context, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(ctx, opcode, offset, |v| !v.truthy())
  }

  fn exec_not(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |this, _ctx, v| {
      this.stack_push(!v);
      Ok(())
    })
  }

  fn exec_negate(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |this, ctx, v| match -v {
      Ok(n) => {
        this.stack_push(n);
        Ok(())
      }
      Err(e) => Err(this.error(ctx, opcode, e)),
    })
  }

  fn exec_print(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |_this, _ctx, v| {
      println!("{}", v);
      Ok(())
    })
  }

  fn exec_jump_if_false(
    &mut self,
    ctx: &mut Context,
    opcode: &OpCode,
    offset: usize,
  ) -> ExecBoolResult {
    match self.stack_pop() {
      Some(v) => {
        if !v.truthy() {
          self.jump(offset);
          Ok(true)
        } else {
          Ok(false)
        }
      }
      None => Err(self.error(ctx, opcode, String::from("no item on the stack to pop"))),
    }
  }

  fn exec_call(
    &mut self,
    ctx: &mut Context,
    env: &mut Env,
    opcode: &OpCode,
    airity: usize,
  ) -> ExecResult {
    if let Some(mut value) = self.stack_pop() {
      let args = self.stack_drain_from(airity);
      match value.call(self, env, args) {
        Ok(ret) => {
          self.stack_push(ret);
          Ok(())
        }
        Err(errors) => Err(
          errors
            .into_iter()
            .map(|e| self.error_at(ctx, |opcode_ref| Error::from_ref(e, opcode, opcode_ref)))
            .collect(),
        ),
      }
    } else {
      Err(self.error(ctx, opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn exec_ret(&mut self) -> Value {
    self.stack_pop().unwrap_or(Value::Nil)
  }

  fn exec_req(&mut self, ctx: &mut Context, env: &mut Env, opcode: &OpCode) -> ExecResult {
    if let Some(file) = self.stack_pop() {
      if let Value::Str(file) = file {
        match fs::read_to_string(&file) {
          Ok(data) => {
            let ip = self.ip;
            let mut stack = self.stack_move(Vec::default());

            let new_ctx = Compiler::compile(&file, &data)?;

            #[cfg(debug_assertions)]
            if self.disassemble {
              new_ctx.disassemble();
            }

            let result = match self.run(new_ctx, env) {
              Ok(v) => {
                stack.push(v);
                Ok(())
              }
              Err(mut errors) => {
                let req_err = self.error(
                  ctx,
                  opcode,
                  format!("errors detected while loading file {}", file),
                );
                errors.extend(req_err);
                Err(errors)
              }
            };

            self.ip = ip;
            self.stack = stack;
            result
          }
          Err(e) => Err(self.error(ctx, opcode, format!("unable to read file {}: {}", file, e))),
        }
      } else {
        Err(self.error(
          ctx,
          opcode,
          format!(
            "can only load files specified by strings or objects convertible to strings, got {}",
            file
          ),
        ))
      }
    } else {
      Err(self.error(
        ctx,
        opcode,
        String::from("cannot operate with an empty stack"),
      ))
    }
  }

  fn exec_index(&mut self, ctx: &mut Context, opcode: &OpCode) -> ExecResult {
    if let Some(index) = self.stack_pop() {
      if let Some(indexable) = self.stack_pop() {
        match indexable.index(index) {
          Ok(value) => {
            self.stack_push(value);
            Ok(())
          }
          Err(err) => Err(self.error(
            ctx,
            opcode,
            format!("unable to index item {}: {}", indexable, err),
          )),
        }
      } else {
        Err(self.error(ctx, opcode, String::from("no item to index")))
      }
    } else {
      Err(self.error(ctx, opcode, String::from("no item to use as an index")))
    }
  }

  fn exec_create_list(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    self.stack_push(Value::new(list));
  }

  /* Utility Functions */

  pub fn stack_push(&mut self, value: Value) {
    self.stack.push(value);
  }

  pub fn stack_pop(&mut self) -> Option<Value> {
    self.stack.pop()
  }

  pub fn stack_pop_n(&mut self, count: usize) {
    self.stack.truncate(self.stack.len().saturating_sub(count));
  }

  pub fn stack_drain_from(&mut self, index: usize) -> Vec<Value> {
    self.stack.drain(self.stack_size() - index..).collect()
  }

  pub fn stack_index(&self, index: usize) -> Option<Value> {
    self.stack.get(index).cloned()
  }

  pub fn stack_index_rev(&self, index: usize) -> Option<Value> {
    self.stack.get(self.stack.len() - 1 - index).cloned()
  }

  pub fn stack_peek(&self) -> Option<Value> {
    self.stack.last().cloned()
  }

  pub fn stack_assign(&mut self, index: usize, value: Value) {
    self.stack[index] = value;
  }

  pub fn stack_move(&mut self, mut other: Vec<Value>) -> Vec<Value> {
    std::mem::swap(&mut self.stack, &mut other);
    other
  }

  pub fn stack_size(&self) -> usize {
    self.stack.len()
  }

  fn jump(&mut self, count: usize) {
    self.ip = self.ip.saturating_add(count);
  }

  fn loop_back(&mut self, count: usize) {
    self.ip = self.ip.saturating_sub(count);
  }

  fn error_at<F: FnOnce(OpCodeReflection) -> Error>(&self, ctx: &Context, f: F) -> Error {
    if let Some(opcode_ref) = ctx.meta.get(self.ip) {
      f(opcode_ref)
    } else {
      Error {
        msg: format!("could not fetch info for instruction {:04X}", self.ip),
        file: ctx.meta.file.access().clone(),
        line: 0,
        column: 0,
      }
    }
  }

  pub fn stack_display(&self) {
    print!("          | ");
    if self.stack.is_empty() {
      print!("[ ]");
    } else {
      for item in self.stack.iter() {
        print!("[ {} ]", item);
      }
    }
    println!();
  }
}

#[derive(Default)]
pub struct Vm {
  main: ExecutionThread,
}

impl Vm {
  pub fn new(disassemble: bool, runtime_disassemble: bool) -> Self {
    Vm {
      main: ExecutionThread::new(disassemble, runtime_disassemble),
    }
  }

  pub fn load(&self, file: String, code: &str) -> Result<SmartPtr<Context>, Vec<Error>> {
    Compiler::compile(&file, code)
  }

  pub fn run(&mut self, ctx: SmartPtr<Context>, env: &mut Env) -> Result<Value, Vec<Error>> {
    self.main.run(ctx, env)
  }
}
