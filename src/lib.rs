mod code;
pub mod types;

#[cfg(test)]
mod test;

pub use code::Context;
pub use code::Env;
use code::{Compiler, OpCode, OpCodeReflection};
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  fs,
  path::{Path, PathBuf},
};
pub use types::{Class, Struct, Value, ValueOpResult};
use types::{Closure, Error};

pub trait New<T> {
  fn new(item: T) -> Self;
}

type ExecResult = Result<(), Vec<Error>>;
type ExecBoolResult = Result<bool, Vec<Error>>;

#[derive(Default)]
pub struct ExecutionThread {
  pub ip: usize,
  stack: Vec<Value>,

  libs: BTreeMap<&'static str, Value>,
}

impl ExecutionThread {
  pub fn new() -> Self {
    Self {
      ip: Default::default(),
      stack: Default::default(),
      libs: Default::default(),
    }
  }

  fn new_with_libs(libs: BTreeMap<&'static str, Value>) -> Self {
    Self {
      ip: Default::default(),
      stack: Default::default(),
      libs,
    }
  }

  fn unary_op<F: FnOnce(&mut Self, &Context, Value) -> ExecResult>(
    &mut self,
    ctx: &Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    if let Some(v) = self.stack_pop() {
      f(self, ctx, v)
    } else {
      Err(self.error(ctx, opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn binary_op<F: FnOnce(&mut Self, &Context, Value, Value) -> ExecResult>(
    &mut self,
    ctx: &Context,
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

  fn global_op<F: FnOnce(&mut Self, &Context, String) -> ExecResult>(
    &mut self,
    ctx: &Context,
    opcode: &OpCode,
    index: usize,
    f: F,
  ) -> ExecResult {
    if let Some(name) = ctx.global_const_at(index) {
      if let Value::String(name) = name {
        f(self, ctx, name.clone())
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

  #[cold]
  fn error<M: ToString>(&self, ctx: &Context, opcode: &OpCode, msg: M) -> Vec<Error> {
    vec![self.error_at(ctx, |opcode_ref| Error::from_ref(msg, opcode, opcode_ref))]
  }

  #[inline]
  fn run(&mut self, ctx: SmartPtr<Context>, env: &mut Env) -> Result<Value, Vec<Error>> {
    self.ip = 0;
    #[cfg(feature = "runtime-disassembly")]
    {
      println!("<< {} >>", ctx.display_str());
    }

    while let Some(opcode) = ctx.next(self.ip) {
      #[cfg(feature = "runtime-disassembly")]
      {
        self.stack_display();
        ctx.display_instruction(&opcode, self.ip);
      }

      match opcode {
        OpCode::NoOp => self.exec_noop(&ctx)?,
        OpCode::Const(index) => self.exec_const(&ctx, &opcode, index)?,
        OpCode::Nil => self.exec_nil(),
        OpCode::True => self.exec_true(),
        OpCode::False => self.exec_false(),
        OpCode::Pop => self.exec_pop(),
        OpCode::PopN(count) => self.exec_pop_n(count),
        OpCode::ForceAssignGlobal(index) => {
          self.exec_force_assign_global(&ctx, env, &opcode, index)?
        }
        OpCode::DefineGlobal(index) => self.exec_define_global(&ctx, env, &opcode, index)?,
        OpCode::LookupGlobal(index) => self.exec_lookup_global(&ctx, env, &opcode, index)?,
        OpCode::AssignGlobal(index) => self.exec_assign_global(&ctx, env, &opcode, index)?,
        OpCode::LookupLocal(index) => self.exec_lookup_local(&ctx, &opcode, index)?,
        OpCode::AssignLocal(index) => self.exec_assign_local(&ctx, &opcode, index)?,
        OpCode::AssignMember(index) => self.exec_assign_member(&ctx, &opcode, index)?,
        OpCode::LookupMember(index) => self.exec_lookup_member(&ctx, &opcode, index)?,
        OpCode::AssignInitializer => self.exec_assign_initializer(&ctx, &opcode)?,
        OpCode::Equal => self.exec_equal(&ctx, &opcode)?,
        OpCode::NotEqual => self.exec_not_equal(&ctx, &opcode)?,
        OpCode::Greater => self.exec_greater(&ctx, &opcode)?,
        OpCode::GreaterEqual => self.exec_greater_equal(&ctx, &opcode)?,
        OpCode::Less => self.exec_less(&ctx, &opcode)?,
        OpCode::LessEqual => self.exec_less_equal(&ctx, &opcode)?,
        OpCode::Check => self.exec_check(&ctx, &opcode)?,
        OpCode::Add => self.exec_add(&ctx, &opcode)?,
        OpCode::Sub => self.exec_sub(&ctx, &opcode)?,
        OpCode::Mul => self.exec_mul(&ctx, &opcode)?,
        OpCode::Div => self.exec_div(&ctx, &opcode)?,
        OpCode::Mod => self.exec_mod(&ctx, &opcode)?,
        OpCode::Or(count) => {
          if self.exec_or(&ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::And(count) => {
          if self.exec_and(&ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::Not => self.exec_not(&ctx, &opcode)?,
        OpCode::Negate => self.exec_negate(&ctx, &opcode)?,
        OpCode::Print => self.exec_print(&ctx, &opcode)?,
        OpCode::Jump(count) => {
          self.jump(count);
          continue;
        }
        OpCode::JumpIfFalse(count) => {
          if self.exec_jump_if_false(&ctx, &opcode, count)? {
            continue;
          }
        }
        OpCode::Loop(count) => {
          self.loop_back(count);
          continue;
        }
        OpCode::Call(airity) => self.exec_call(&ctx, env, &opcode, airity)?,
        OpCode::Ret => return Ok(self.exec_ret()),
        OpCode::Req => self.exec_req(&ctx, env, &opcode)?,
        OpCode::Index => self.exec_index(&ctx, &opcode, env)?,
        OpCode::CreateList(num_items) => self.exec_create_list(num_items),
        OpCode::CreateClosure => self.exec_create_closure(&ctx, &opcode)?,
      }
      self.ip += 1;
    }

    #[cfg(feature = "runtime-disassembly")]
    {
      println!("<< END >>");
    }

    Ok(Value::Nil)
  }

  /* Operations */

  #[inline]
  fn exec_noop(&self, ctx: &Context) -> ExecResult {
    Err(self.error(
      ctx,
      &OpCode::NoOp,
      String::from("executed noop opcode, should not happen"),
    ))
  }

  #[inline]
  fn exec_const(&mut self, ctx: &Context, opcode: &OpCode, index: usize) -> ExecResult {
    if let Some(c) = ctx.const_at(index) {
      self.stack_push(c.clone());
      Ok(())
    } else {
      Err(self.error(ctx, opcode, String::from("could not lookup constant")))
    }
  }

  #[inline]
  fn exec_nil(&mut self) {
    self.stack_push(Value::Nil);
  }

  #[inline]
  fn exec_true(&mut self) {
    self.stack_push(Value::new(true));
  }

  #[inline]
  fn exec_false(&mut self) {
    self.stack_push(Value::new(false));
  }

  #[inline]
  fn exec_pop(&mut self) {
    self.stack_pop();
  }

  #[inline]
  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  #[inline]
  fn exec_lookup_local(&mut self, ctx: &Context, opcode: &OpCode, location: usize) -> ExecResult {
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

  #[inline]
  fn exec_assign_local(&mut self, ctx: &Context, opcode: &OpCode, location: usize) -> ExecResult {
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

  #[inline]
  fn exec_lookup_global(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_force_assign_global(
    &mut self,
    ctx: &Context,
    env: &mut Env,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    self.global_op(ctx, opcode, location, |this, ctx, name| {
      // used with functions & classes only, so pop
      if let Some(v) = this.stack_pop() {
        env.assign(name, v.clone());
        Ok(())
      } else {
        Err(this.error(
          ctx,
          opcode,
          String::from("can not define global using empty stack"),
        ))
      }
    })
  }

  #[inline]
  fn exec_define_global(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_assign_global(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_assign_member(&mut self, ctx: &Context, opcode: &OpCode, location: usize) -> ExecResult {
    match ctx.const_at(location) {
      Some(name) => match name {
        Value::String(name) => match self.stack_pop() {
          Some(value) => match self.stack_peek() {
            Some(obj) => match obj {
              Value::Struct(mut obj) => {
                obj.set(name, value);
                Ok(())
              }
              Value::Class(mut class) => {
                class.set_method(name, value);
                Ok(())
              }
              Value::Instance(mut instance) => instance
                .set(name, value)
                .map_err(|e| self.error(ctx, opcode, e)),
              _ => Err(self.error(
                ctx,
                opcode,
                format!("cannot assign member to invalid type {}", obj),
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
        _ => Err(self.error(
          ctx,
          opcode,
          String::from("tried to assigning to non object"),
        )),
      },
      None => Err(self.error(ctx, opcode, String::from("no member ident to assign to"))),
    }
  }

  #[inline]
  fn exec_lookup_member(&mut self, ctx: &Context, opcode: &OpCode, location: usize) -> ExecResult {
    match ctx.const_at(location) {
      Some(name) => match self.stack_pop() {
        Some(obj) => match obj {
          Value::Struct(obj) => match name {
            Value::String(ident) => {
              self.stack_push(obj.get(ident));
              Ok(())
            }
            v => Err(self.error(ctx, opcode, format!("invalid member name {}", v))),
          },
          Value::Instance(obj) => match name {
            Value::String(ident) => {
              self.stack_push(obj.get(ident));
              Ok(())
            }
            v => Err(self.error(ctx, opcode, format!("invalid member name {}", v))),
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

  fn exec_assign_initializer(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(initializer) => match self.stack_peek() {
        Some(value) => match value {
          Value::Class(mut class) => {
            class.set_initializer(initializer);
            Ok(())
          }
          _ => Err(self.error(
            ctx,
            opcode,
            String::from("tried to assign initializer to non class"),
          )),
        },
        None => Err(self.error(ctx, opcode, String::from("no class on stack to assign to"))),
      },
      None => Err(self.error(
        ctx,
        opcode,
        String::from("no value on stack to assign to class initializer"),
      )),
    }
  }

  #[inline]
  fn exec_bool<F: FnOnce(Value, Value) -> bool>(
    &mut self,
    ctx: &Context,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    self.binary_op(ctx, opcode, |this, _ctx, a, b| {
      this.stack_push(Value::new(f(a, b)));
      Ok(())
    })
  }

  #[inline]
  fn exec_equal(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a == b)
  }

  #[inline]
  fn exec_not_equal(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a != b)
  }

  #[inline]
  fn exec_greater(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a > b)
  }

  #[inline]
  fn exec_greater_equal(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a >= b)
  }

  #[inline]
  fn exec_less(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a < b)
  }

  #[inline]
  fn exec_less_equal(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_bool(ctx, opcode, |a, b| a <= b)
  }

  #[inline]
  fn exec_check(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
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

  #[inline]
  fn exec_arith<F: FnOnce(Value, Value) -> ValueOpResult>(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_add(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a + b)
  }

  #[inline]
  fn exec_sub(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a - b)
  }

  #[inline]
  fn exec_mul(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a * b)
  }

  #[inline]
  fn exec_div(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a / b)
  }

  #[inline]
  fn exec_mod(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.exec_arith(ctx, opcode, |a, b| a % b)
  }

  #[inline]
  fn exec_logical<F: FnOnce(Value) -> bool>(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_or(&mut self, ctx: &Context, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(ctx, opcode, offset, |v| v.truthy())
  }

  #[inline]
  fn exec_and(&mut self, ctx: &Context, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(ctx, opcode, offset, |v| !v.truthy())
  }

  #[inline]
  fn exec_not(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |this, _ctx, v| {
      this.stack_push(!v);
      Ok(())
    })
  }

  #[inline]
  fn exec_negate(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |this, ctx, v| match -v {
      Ok(n) => {
        this.stack_push(n);
        Ok(())
      }
      Err(e) => Err(this.error(ctx, opcode, e)),
    })
  }

  #[inline]
  fn exec_print(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    self.unary_op(ctx, opcode, |_this, _ctx, v| {
      println!("{}", v);
      Ok(())
    })
  }

  #[inline]
  fn exec_jump_if_false(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_call(
    &mut self,
    ctx: &Context,
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

  #[inline]
  fn exec_ret(&mut self) -> Value {
    self.stack_pop().unwrap_or(Value::Nil)
  }

  #[inline]
  fn exec_req(&mut self, ctx: &Context, env: &mut Env, opcode: &OpCode) -> ExecResult {
    if let Some(file) = self.stack_pop() {
      if let Value::String(file) = file {
        if let Some(lib) = self.libs.get(file.as_str()).cloned() {
          self.stack_push(lib);
          Ok(())
        } else {
          let mut p = PathBuf::from(file.as_str());

          if !Path::exists(&p) {
            if let Some(Value::Struct(library_mod)) = env.lookup("$LIBRARY") {
              if let Value::List(list) = library_mod.get(&"path") {
                for item in list.iter() {
                  if let Value::String(path) = item {
                    let base = PathBuf::from(path);
                    let whole = base.join(&p);
                    if Path::exists(&whole) {
                      p = whole;
                      break;
                    }
                  }
                }
              }
            }
          }

          match fs::read_to_string(p) {
            Ok(data) => {
              let ip = self.ip;
              let mut stack = self.stack_move(Vec::default());

              let new_ctx = Compiler::compile(&file, &data)?;

              #[cfg(feature = "disassemble")]
              {
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

  #[inline]
  fn exec_index(&mut self, ctx: &Context, opcode: &OpCode, env: &mut Env) -> ExecResult {
    if let Some(index) = self.stack_pop() {
      if let Some(mut indexable) = self.stack_pop() {
        match indexable.index(self, env, index) {
          Ok(value) => {
            self.stack_push(value);
            Ok(())
          }
          Err(err) => Err(self.error(ctx, opcode, format!("unable to index value: {}", err))),
        }
      } else {
        Err(self.error(ctx, opcode, String::from("no item to index")))
      }
    } else {
      Err(self.error(ctx, opcode, String::from("no item to use as an index")))
    }
  }

  #[inline]
  fn exec_create_list(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    self.stack_push(Value::new(list));
  }

  #[inline]
  fn exec_create_closure(&mut self, ctx: &Context, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(function) => match self.stack_pop() {
        Some(captures) => match function {
          Value::Function(function) => match captures {
            Value::List(captures) => {
              self.stack_push(Value::new(Closure::new(captures, function)));
              Ok(())
            }
            _ => Err(self.error(ctx, opcode, String::from("capture list must be a struct"))),
          },
          _ => Err(self.error(ctx, opcode, String::from("closure must be a function"))),
        },
        None => Err(self.error(
          ctx,
          opcode,
          String::from("no item on the stack to pop for closure captures"),
        )),
      },
      None => Err(self.error(
        ctx,
        opcode,
        String::from("no item on the stack to pop for closure function"),
      )),
    }
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

  pub fn stack_append(&mut self, other: Vec<Value>) {
    self.stack.extend(other);
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

  #[cold]
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

  #[cfg(debug_assertions)]
  #[cfg(feature = "runtime-disassembly")]
  pub fn stack_display(&self) {
    if self.stack.is_empty() {
      println!("          | [ ]");
    } else {
      for (index, item) in self.stack.iter().enumerate() {
        println!("{:#10}| [ {} ]", index, item);
      }
    }
  }
}

pub enum Library {
  Std,
  Env,
  Time,
  String,
  Console,
  Ptr,
}

#[derive(Default)]
pub struct Vm {
  main: ExecutionThread,
}

impl Vm {
  pub fn new() -> Self {
    Self {
      main: Default::default(),
    }
  }

  pub fn new_with_libs(args: &[String], libs: &[Library]) -> Self {
    let mut loaded_libs = BTreeMap::default();

    for lib in libs {
      let (key, value) = Self::load_lib(args, lib);
      loaded_libs.insert(key, value);
    }

    Self {
      main: ExecutionThread::new_with_libs(loaded_libs),
    }
  }

  pub fn load(&self, file: String, code: &str) -> Result<SmartPtr<Context>, Vec<Error>> {
    Compiler::compile(&file, code)
  }

  pub fn run(&mut self, ctx: SmartPtr<Context>, env: &mut Env) -> Result<Value, Vec<Error>> {
    #[cfg(feature = "disassemble")]
    {
      ctx.disassemble();

      #[cfg(feature = "quit-after-disassembled")]
      {
        std::process::exit(0);
      }
    }

    self.main.run(ctx, env)
  }

  fn load_lib(args: &[String], lib: &Library) -> (&'static str, Value) {
    match lib {
      Library::Std => ("std", Self::load_std()),
      Library::Env => ("env", Self::load_env(args)),
      Library::Time => ("time", Self::load_time()),
      Library::String => ("string", Self::load_string()),
      Library::Console => ("console", Self::load_console()),
      Library::Ptr => ("ptr", Self::load_ptr()),
    }
  }

  fn load_std() -> Value {
    let mut obj = Struct::default();

    let mut array = Class::new("Array");

    array.set_initializer(Value::native("Array.new", |_thread, _env, args| {
      Ok(Value::new(args))
    }));

    array.set_method(
      "push",
      Value::native("push", |_thread, _env, mut args| {
        if args.len() > 1 {
          let this = args.get(0).cloned().unwrap();
          let rest = args.drain(1..);

          match this {
            Value::Instance(mut instance) => match &mut instance.data {
              Value::List(list) => list.extend(rest.into_iter()),
              v => return Err(format!("somehow called push on non array type {}", v)),
            },
            v => return Err(format!("somehow called push on a primitive type {}", v)),
          }
        }

        Ok(Value::Nil)
      }),
    );

    array.set_method(
      "__index__",
      Value::native("__index__", |thread, env, mut args| {
        if args.len() == 2 {
          let value = args.swap_remove(1);
          let this = args.get_mut(0).unwrap();

          match this {
            Value::Instance(instance) => instance.data.index(thread, env, value),
            c => Err(format!(
              "somehow called index method for non array instance {}",
              c
            )),
          }
        } else {
          Err(String::from("invalid number of arguments for index"))
        }
      }),
    );

    array.set_method(
      "len",
      Value::native("len", |_thread, _env, args| {
        let this = args.get(0).unwrap();

        match this {
          Value::Instance(instance) => match &instance.data {
            Value::List(list) => Ok(Value::new(list.len() as f64)),
            c => Err(format!("somehow called len on non instance of array {}", c)),
          },
          c => Err(format!(
            "somehow called index method for non array instance {}",
            c
          )),
        }
      }),
    );

    obj.set("Array", Value::new(array));

    Value::new(obj)
  }

  fn load_env(args: &[String]) -> Value {
    let mut obj = Struct::default();
    obj.set(
      "ARGV",
      Value::new(
        args
          .iter()
          .map(|arg| Value::new(arg.clone()))
          .collect::<Vec<Value>>(),
      ),
    );
    Value::new(obj)
  }

  fn load_time() -> Value {
    let mut obj = Struct::default();

    let clock = Value::native(String::from("clock"), |_thread, _env, _args: Vec<Value>| {
      use std::time::{SystemTime, UNIX_EPOCH};
      let now = SystemTime::now();
      let since = now.duration_since(UNIX_EPOCH).expect("time went backwards");
      Ok(Value::new(since.as_nanos()))
    });

    let clock_diff = Value::native(
      String::from("clock_diff"),
      |_thread, _env, args: Vec<Value>| {
        if let Some(Value::U128(before)) = args.get(0) {
          if let Some(Value::U128(after)) = args.get(1) {
            let diff = std::time::Duration::from_nanos((after - before) as u64);
            return Ok(Value::new(diff.as_secs_f64()));
          }
        }
        Err(String::from(
          "clock_diff called with wrong number of arguments or invalid types",
        ))
      },
    );

    obj.set("clock", clock);
    obj.set("clock_diff", clock_diff);

    Value::new(obj)
  }

  fn load_string() -> Value {
    let mut obj = Struct::default();

    let parse_number = Value::native(
      String::from("parse_number"),
      |_thread, _env, args: Vec<Value>| {
        if let Some(arg) = args.get(0) {
          match arg {
            Value::String(string) => Ok(Value::new(
              string.parse::<f64>().map_err(|e| format!("{}", e))?,
            )),
            v => Err(format!("can not convert {} to a number", v)),
          }
        } else {
          Err(String::from("expected 1 argument"))
        }
      },
    );

    obj.set("parse_number", parse_number);

    Value::new(obj)
  }

  fn load_console() -> Value {
    let mut obj = Struct::default();

    let write = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
      for arg in args {
        print!("{}", arg);
      }
      println!();
      Ok(Value::Nil)
    });

    obj.set("write", write);

    Value::new(obj)
  }

  fn load_ptr() -> Value {
    let mut obj = Struct::default();

    let deref = Value::native(
      String::from("deref"),
      |_thread, _env, mut args: Vec<Value>| {
        fn deref_value(v: Value) -> Value {
          if let Value::Instance(v) = v {
            (***v).clone()
          } else {
            v
          }
        }

        if !args.is_empty() {
          if args.len() == 1 {
            Ok(deref_value(args.swap_remove(0)))
          } else {
            let mut derefs = Vec::with_capacity(args.len());
            for arg in args {
              derefs.push(deref_value(arg))
            }
            Ok(Value::new(derefs))
          }
        } else {
          Err(String::from("deref called with no arguments"))
        }
      },
    );

    obj.set("deref", deref);

    Value::new(obj)
  }
}
