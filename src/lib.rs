mod code;
pub mod types;

#[cfg(test)]
mod test;

pub use code::Context;
pub use code::Env;
use code::{Compiler, OpCode, OpCodeReflection, StackFrame, Yield};
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

// yield must store ip and stack and return ctx
// or better yet, have ctx have an Option<usize> and option<Vec<Value>>
// and when running if present, then set the current thread's ip & stack to those values

pub enum RunResult {
  Value(Value),
  Yield(Yield),
}

#[derive(Default)]
pub struct ExecutionThread {
  current_frame: StackFrame,
  stack_frames: Vec<StackFrame>,

  libs: BTreeMap<&'static str, Value>,
}

impl ExecutionThread {
  fn new_with_libs(libs: BTreeMap<&'static str, Value>) -> Self {
    Self {
      current_frame: Default::default(),
      stack_frames: Vec::with_capacity(256),
      libs,
    }
  }

  fn unary_op<F: FnOnce(&mut Self, Value) -> ExecResult>(
    &mut self,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    if let Some(v) = self.stack_pop() {
      f(self, v)
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn binary_op<F: FnOnce(&mut Self, Value, Value) -> ExecResult>(
    &mut self,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    if let Some(bv) = self.stack_pop() {
      if let Some(av) = self.stack_pop() {
        f(self, av, bv)
      } else {
        Err(self.error(opcode, String::from("cannot operate on empty stack")))
      }
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn global_op<F: FnOnce(&mut Self, String) -> ExecResult>(
    &mut self,
    opcode: &OpCode,
    index: usize,
    f: F,
  ) -> ExecResult {
    let name = if let Some(name) = self.current_frame.ctx.global_const_at(index) {
      if let Value::String(name) = name {
        Ok(name.clone())
      } else {
        Err(self.error(
          opcode,
          format!("global variable name is not an identifier: {}", name),
        ))
      }
    } else {
      Err(self.error(opcode, String::from("global variable name does not exist")))
    }?;

    f(self, name)
  }

  #[cold]
  fn error<M: ToString>(&self, opcode: &OpCode, msg: M) -> Vec<Error> {
    vec![self.error_at(|opcode_ref| Error::from_ref(msg, opcode, opcode_ref))]
  }

  #[inline]
  fn run(&mut self, ctx: SmartPtr<Context>, env: &mut Env) -> Result<RunResult, Vec<Error>> {
    self.current_frame.ctx = ctx;
    self.execute(env)
  }

  pub fn resume(&mut self, y: Yield, env: &mut Env) -> Result<RunResult, Vec<Error>> {
    self.current_frame = y.current_frame;
    self.stack_frames = y.stack_frames;
    self.execute(env)
  }

  fn execute(&mut self, env: &mut Env) -> Result<RunResult, Vec<Error>> {
    loop {
      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< {} >>", self.current_frame.ctx.display_str());
      }

      let mut returned_value = false;
      let mut return_self = false;

      while let Some(opcode) = self.current_frame.ctx.next(self.current_frame.ip) {
        #[cfg(feature = "runtime-disassembly")]
        {
          self.stack_display();
          self
            .current_frame
            .ctx
            .display_instruction(&opcode, self.current_frame.ip);
        }

        match opcode {
          OpCode::NoOp => self.exec_noop()?,
          OpCode::Const(index) => self.exec_const(&opcode, index)?,
          OpCode::Nil => self.exec_nil(),
          OpCode::True => self.exec_true(),
          OpCode::False => self.exec_false(),
          OpCode::Pop => self.exec_pop(),
          OpCode::PopN(count) => self.exec_pop_n(count),
          OpCode::ForceAssignGlobal(index) => self.exec_force_assign_global(env, &opcode, index)?,
          OpCode::DefineGlobal(index) => self.exec_define_global(env, &opcode, index)?,
          OpCode::LookupGlobal(index) => self.exec_lookup_global(env, &opcode, index)?,
          OpCode::AssignGlobal(index) => self.exec_assign_global(env, &opcode, index)?,
          OpCode::LookupLocal(index) => self.exec_lookup_local(&opcode, index)?,
          OpCode::AssignLocal(index) => self.exec_assign_local(&opcode, index)?,
          OpCode::AssignMember(index) => self.exec_assign_member(&opcode, index)?,
          OpCode::LookupMember(index) => self.exec_lookup_member(&opcode, index)?,
          OpCode::AssignInitializer => self.exec_assign_initializer(&opcode)?,
          OpCode::Deref => self.exec_deref(&opcode)?,
          OpCode::DerefAssignLocal(index) => self.exec_deref_assign_local(&opcode, index)?,
          OpCode::DerefAssignGlobal(index) => self.exec_deref_assign_global(&opcode, env, index)?,
          OpCode::Equal => self.exec_equal(&opcode)?,
          OpCode::NotEqual => self.exec_not_equal(&opcode)?,
          OpCode::Greater => self.exec_greater(&opcode)?,
          OpCode::GreaterEqual => self.exec_greater_equal(&opcode)?,
          OpCode::Less => self.exec_less(&opcode)?,
          OpCode::LessEqual => self.exec_less_equal(&opcode)?,
          OpCode::Check => self.exec_check(&opcode)?,
          OpCode::Add => self.exec_add(&opcode)?,
          OpCode::Sub => self.exec_sub(&opcode)?,
          OpCode::Mul => self.exec_mul(&opcode)?,
          OpCode::Div => self.exec_div(&opcode)?,
          OpCode::Mod => self.exec_mod(&opcode)?,
          OpCode::Or(count) => {
            if self.exec_or(&opcode, count)? {
              continue;
            }
          }
          OpCode::And(count) => {
            if self.exec_and(&opcode, count)? {
              continue;
            }
          }
          OpCode::Not => self.exec_not(&opcode)?,
          OpCode::Negate => self.exec_negate(&opcode)?,
          OpCode::Print => self.exec_print(&opcode)?,
          OpCode::Jump(count) => {
            self.jump(count);
            continue;
          }
          OpCode::JumpIfFalse(count) => {
            if self.exec_jump_if_false(&opcode, count)? {
              continue;
            }
          }
          OpCode::Loop(count) => {
            self.loop_back(count);
            continue;
          }
          OpCode::Call(airity) => {
            self.current_frame.ip += 1;
            self.exec_call(env, &opcode, airity)?;
            continue;
          }
          OpCode::Ret => {
            break;
          }
          OpCode::RetValue => {
            returned_value = true;
            break;
          }
          OpCode::Req => {
            self.current_frame.ip += 1;
            self.exec_req(env, &opcode)?;
            continue;
          }
          OpCode::Index => self.exec_index(&opcode, env)?,
          OpCode::CreateList(num_items) => self.exec_create_list(num_items),
          OpCode::CreateClosure => self.exec_create_closure(&opcode)?,
          OpCode::DefaultConstructorRet => return_self = true,
        }

        self.current_frame.ip += 1;
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< END >>");
      }

      let ret_val = if return_self {
        // return self value, failure here is a logic error so unwrap
        self.stack_index_0().unwrap()
      } else if returned_value {
        // return actual value, failure here is a logic error so unwrap
        self.stack_pop().unwrap()
      } else {
        // if implicit/void return nil
        Value::Nil
      };

      if let Some(stack_frame) = self.stack_frames.pop() {
        if stack_frame.ip < stack_frame.ctx.num_instructions() {
          self.current_frame = stack_frame;
          self.stack_push(ret_val);
        } else {
          break Ok(RunResult::Value(ret_val));
        }
      } else {
        break Ok(RunResult::Value(ret_val));
      }
    }
  }

  /* Operations */

  #[inline]
  fn exec_noop(&self) -> ExecResult {
    Err(self.error(
      &OpCode::NoOp,
      String::from("executed noop opcode, should not happen"),
    ))
  }

  #[inline]
  fn exec_const(&mut self, opcode: &OpCode, index: usize) -> ExecResult {
    let value = if let Some(c) = self.current_frame.ctx.const_at(index) {
      Ok(c.clone())
    } else {
      Err(self.error(opcode, String::from("could not lookup constant")))
    }?;

    self.stack_push(value);
    Ok(())
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
  fn exec_lookup_local(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(local) = self.stack_index(location) {
      self.stack_push(local);
      Ok(())
    } else {
      Err(self.error(opcode, format!("could not index stack at pos {}", location)))
    }
  }

  #[inline]
  fn exec_assign_local(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      self.stack_assign(location, value.clone());
      Ok(())
    } else {
      Err(self.error(
        opcode,
        format!("could not replace stack value at pos {}", location),
      ))
    }
  }

  #[inline]
  fn exec_lookup_global(&mut self, env: &Env, opcode: &OpCode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(global) = env.lookup(&name) {
        this.stack_push(global);
        Ok(())
      } else {
        Err(this.error(opcode, String::from("use of undefined variable")))
      }
    })
  }

  #[inline]
  fn exec_force_assign_global(
    &mut self,
    env: &mut Env,
    opcode: &OpCode,
    location: usize,
  ) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      // used with functions & classes only, so pop
      if let Some(v) = this.stack_pop() {
        env.assign(name, v.clone());
        Ok(())
      } else {
        Err(this.error(
          opcode,
          String::from("can not define global using empty stack"),
        ))
      }
    })
  }

  #[inline]
  fn exec_define_global(&mut self, env: &mut Env, opcode: &OpCode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if env.define(name, v.clone()) {
          Ok(())
        } else {
          Err(this.error(opcode, String::from("tried redefining global variable")))
        }
      } else {
        Err(this.error(
          opcode,
          String::from("can not define global using empty stack"),
        ))
      }
    })
  }

  #[inline]
  fn exec_assign_global(&mut self, env: &mut Env, opcode: &OpCode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if env.assign(name, v) {
          Ok(())
        } else {
          Err(this.error(
            opcode,
            String::from("tried to assign to nonexistent global"),
          ))
        }
      } else {
        Err(this.error(
          opcode,
          String::from("can not assign to global using empty stack"),
        ))
      }
    })
  }

  #[inline]
  fn exec_assign_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    match self.stack_pop() {
      Some(value) => match self.current_frame.ctx.const_at(location) {
        Some(name) => match name {
          Value::String(name) => match self.stack_peek() {
            Some(obj) => match obj {
              Value::Struct(mut obj) => {
                obj.set(name, value);
                Ok(())
              }
              Value::Class(mut class) => {
                class.set_method(name, value);
                Ok(())
              }
              Value::Instance(mut instance) => {
                instance.set(name, value).map_err(|e| self.error(opcode, e))
              }
              _ => Err(self.error(
                opcode,
                format!("cannot assign member to invalid type {}", obj),
              )),
            },
            None => Err(self.error(opcode, String::from("no object on stack to assign to"))),
          },
          _ => Err(self.error(opcode, String::from("tried to assigning to non object"))),
        },
        None => Err(self.error(opcode, String::from("no member ident to assign to"))),
      },
      None => Err(self.error(
        opcode,
        String::from("no value on stack to assign to member"),
      )),
    }
  }

  #[inline]
  fn exec_lookup_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    let value = match self.stack_pop() {
      Some(obj) => match self.current_frame.ctx.const_at(location) {
        Some(name) => match obj {
          Value::Struct(obj) => match name {
            Value::String(ident) => Ok(obj.get(ident)),
            v => Err(self.error(opcode, format!("invalid member name {}", v))),
          },
          Value::Instance(obj) => match name {
            Value::String(ident) => Ok(obj.get(ident)),
            v => Err(self.error(opcode, format!("invalid member name {}", v))),
          },
          _ => Err(self.error(opcode, String::from("invalid type for member access"))),
        },
        None => Err(self.error(opcode, String::from("no constant specified by index"))),
      },
      None => Err(self.error(
        opcode,
        String::from("no object to for member access on the stack"),
      )),
    }?;

    self.stack_push(value);

    Ok(())
  }

  fn exec_assign_initializer(&mut self, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(initializer) => match self.stack_peek() {
        Some(value) => match value {
          Value::Class(mut class) => {
            class.set_initializer(initializer);
            Ok(())
          }
          _ => Err(self.error(
            opcode,
            String::from("tried to assign initializer to non class"),
          )),
        },
        None => Err(self.error(opcode, String::from("no class on stack to assign to"))),
      },
      None => Err(self.error(
        opcode,
        String::from("no value on stack to assign to class initializer"),
      )),
    }
  }

  #[inline]
  fn exec_deref(&mut self, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(instance) => match instance {
        Value::Instance(instance) => {
          self.stack_push(instance.deref());
          Ok(())
        }
        v => Err(self.error(opcode, format!("cannot deref value type {}", v))),
      },
      None => Err(self.error(opcode, String::from("no item on stack to dereference"))),
    }
  }

  #[inline]
  fn exec_deref_assign_local(&mut self, opcode: &OpCode, index: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      if let Some(obj) = self.stack_index(index) {
        match obj {
          Value::Instance(mut instance) => {
            instance.assign(value);
            Ok(())
          }
          v => Err(self.error(opcode, format!("cannot assign to non instance {}", v))),
        }
      } else {
        Err(self.error(opcode, format!("no item on stack at location {}", index)))
      }
    } else {
      Err(self.error(
        opcode,
        format!("could not replace stack value at pos {}", index),
      ))
    }
  }

  #[inline]
  fn exec_deref_assign_global(&mut self, opcode: &OpCode, env: &Env, index: usize) -> ExecResult {
    self.global_op(opcode, index, |this, name| {
      if let Some(value) = this.stack_peek() {
        if let Some(instance) = env.lookup(name) {
          match instance {
            Value::Instance(mut instance) => {
              instance.assign(value);
              Ok(())
            }
            v => Err(this.error(opcode, format!("cannot assign to value type {}", v))),
          }
        } else {
          Err(this.error(
            opcode,
            String::from("tried to assign to nonexistent global"),
          ))
        }
      } else {
        Err(this.error(
          opcode,
          String::from("can not assign to global using empty stack"),
        ))
      }
    })
  }

  #[inline]
  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: &OpCode, f: F) -> ExecResult {
    self.binary_op(opcode, |this, a, b| {
      this.stack_push(Value::new(f(a, b)));
      Ok(())
    })
  }

  #[inline]
  fn exec_equal(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a == b)
  }

  #[inline]
  fn exec_not_equal(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a != b)
  }

  #[inline]
  fn exec_greater(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a > b)
  }

  #[inline]
  fn exec_greater_equal(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a >= b)
  }

  #[inline]
  fn exec_less(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a < b)
  }

  #[inline]
  fn exec_less_equal(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a <= b)
  }

  #[inline]
  fn exec_check(&mut self, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(a) => match self.stack_peek() {
        Some(b) => {
          self.stack_push(Value::new(a == b));
          Ok(())
        }
        None => Err(self.error(opcode, "stack peek failed")),
      },
      None => Err(self.error(opcode, "stack pop failed")),
    }
  }

  #[inline]
  fn exec_arith<F: FnOnce(Value, Value) -> ValueOpResult>(
    &mut self,
    opcode: &OpCode,
    f: F,
  ) -> ExecResult {
    self.binary_op(opcode, |this, a, b| match f(a, b) {
      Ok(v) => {
        this.stack_push(v);
        Ok(())
      }
      Err(e) => Err(this.error(opcode, e)),
    })
  }

  #[inline]
  fn exec_add(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a + b)
  }

  #[inline]
  fn exec_sub(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a - b)
  }

  #[inline]
  fn exec_mul(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a * b)
  }

  #[inline]
  fn exec_div(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a / b)
  }

  #[inline]
  fn exec_mod(&mut self, opcode: &OpCode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a % b)
  }

  #[inline]
  fn exec_logical<F: FnOnce(Value) -> bool>(
    &mut self,

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
      None => Err(self.error(opcode, "no item on the stack to peek")),
    }
  }

  #[inline]
  fn exec_or(&mut self, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(opcode, offset, |v| v.truthy())
  }

  #[inline]
  fn exec_and(&mut self, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    self.exec_logical(opcode, offset, |v| !v.truthy())
  }

  #[inline]
  fn exec_not(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |this, v| {
      this.stack_push(!v);
      Ok(())
    })
  }

  #[inline]
  fn exec_negate(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |this, v| match -v {
      Ok(n) => {
        this.stack_push(n);
        Ok(())
      }
      Err(e) => Err(this.error(opcode, e)),
    })
  }

  #[inline]
  fn exec_print(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |_this, v| {
      println!("{}", v);
      Ok(())
    })
  }

  #[inline]
  fn exec_jump_if_false(&mut self, opcode: &OpCode, offset: usize) -> ExecBoolResult {
    match self.stack_pop() {
      Some(v) => {
        if !v.truthy() {
          self.jump(offset);
          Ok(true)
        } else {
          Ok(false)
        }
      }
      None => Err(self.error(opcode, "no item on the stack to pop")),
    }
  }

  #[inline]
  fn exec_call(&mut self, env: &mut Env, opcode: &OpCode, airity: usize) -> ExecResult {
    if let Some(mut callable) = self.stack_pop() {
      let args = self.stack_drain_from(airity);

      if cfg!(feature = "runtime-disassembly") {
        let res = callable.call(self, env, args).map_err(|errors| {
          errors
            .into_iter()
            .map(|e| self.error_at(|opcode_ref| Error::from_ref(e, opcode, opcode_ref)))
            .collect()
        });
        println!("<< {} >>", self.current_frame.ctx.display_str());
        res
      } else {
        callable.call(self, env, args).map_err(|errors| {
          errors
            .into_iter()
            .map(|e| self.error_at(|opcode_ref| Error::from_ref(e, opcode, opcode_ref)))
            .collect()
        })
      }
    } else {
      Err(self.error(opcode, "cannot operate on empty stack"))
    }
  }

  #[inline]
  fn exec_req(&mut self, env: &mut Env, opcode: &OpCode) -> ExecResult {
    if let Some(file) = self.stack_pop() {
      if let Value::String(file) = file {
        // load global libs only once

        if !env.is_defined(&file) {
          if let Some(lib) = self.libs.get(file.as_str()).cloned() {
            env.define(file, lib);
            return Ok(());
          }
        }

        let mut p = PathBuf::from(file.as_str());

        if !Path::exists(&p) {
          if let Some(Value::Struct(library_mod)) = env.lookup("$LIBRARY") {
            if let Value::List(list) = library_mod.get(&"path") {
              for item in list.iter() {
                if let Value::String(path) = item {
                  let base = PathBuf::from(path);
                  let whole = base.join(&p);
                  if Path::exists(&whole) {
                    println!("found {:?}", whole);
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
            let new_ctx = Compiler::compile(&file, &data)?;

            #[cfg(debug_assertions)]
            #[cfg(feature = "disassemble")]
            {
              new_ctx.disassemble();
            }

            self.new_frame(new_ctx);

            Ok(())
          }
          Err(e) => Err(self.error(opcode, format!("unable to read file {}: {}", file, e))),
        }
      } else {
        Err(self.error(
          opcode,
          format!(
            "can only load files specified by strings or objects convertible to strings, got {}",
            file
          ),
        ))
      }
    } else {
      Err(self.error(opcode, "cannot operate with an empty stack"))
    }
  }

  #[inline]
  fn exec_index(&mut self, opcode: &OpCode, env: &mut Env) -> ExecResult {
    if let Some(index) = self.stack_pop() {
      if let Some(mut indexable) = self.stack_pop() {
        indexable
          .index(self, env, index)
          .map_err(|err| self.error(opcode, format!("unable to index value: {}", err)))
      } else {
        Err(self.error(opcode, "no item to index"))
      }
    } else {
      Err(self.error(opcode, "no item to use as an index"))
    }
  }

  #[inline]
  fn exec_create_list(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    self.stack_push(Value::new(list));
  }

  #[inline]
  fn exec_create_closure(&mut self, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(function) => match self.stack_pop() {
        Some(captures) => match function {
          Value::Function(function) => match captures {
            Value::List(captures) => {
              self.stack_push(Value::new(Closure::new(captures, function)));
              Ok(())
            }
            _ => Err(self.error(opcode, "capture list must be a struct")),
          },
          _ => Err(self.error(opcode, "closure must be a function")),
        },
        None => Err(self.error(opcode, "no item on the stack to pop for closure captures")),
      },
      None => Err(self.error(opcode, "no item on the stack to pop for closure function")),
    }
  }

  /* Utility Functions */

  pub fn new_frame(&mut self, ctx: SmartPtr<Context>) {
    let mut frame = StackFrame::new(ctx);
    std::mem::swap(&mut self.current_frame, &mut frame);
    self.stack_frames.push(frame);
  }

  pub fn set_stack(&mut self, stack: Vec<Value>) {
    self.current_frame.stack = stack;
  }

  pub fn stack_push(&mut self, value: Value) {
    self.current_frame.stack.push(value);
  }

  pub fn stack_pop(&mut self) -> Option<Value> {
    self.current_frame.stack.pop()
  }

  pub fn stack_pop_n(&mut self, count: usize) {
    self
      .current_frame
      .stack
      .truncate(self.current_frame.stack.len().saturating_sub(count));
  }

  pub fn stack_drain_from(&mut self, index: usize) -> Vec<Value> {
    self
      .current_frame
      .stack
      .drain(self.stack_size() - index..)
      .collect()
  }

  pub fn stack_index(&self, index: usize) -> Option<Value> {
    self.current_frame.stack.get(index).cloned()
  }

  pub fn stack_index_0(&self) -> Option<Value> {
    self.current_frame.stack.first().cloned()
  }

  pub fn stack_index_rev(&self, index: usize) -> Option<Value> {
    self
      .current_frame
      .stack
      .get(self.current_frame.stack.len() - 1 - index)
      .cloned()
  }

  pub fn stack_peek(&self) -> Option<Value> {
    self.current_frame.stack.last().cloned()
  }

  pub fn stack_assign(&mut self, index: usize, value: Value) {
    self.current_frame.stack[index] = value;
  }

  pub fn stack_append(&mut self, other: Vec<Value>) {
    self.current_frame.stack.extend(other);
  }

  pub fn stack_size(&self) -> usize {
    self.current_frame.stack.len()
  }

  fn jump(&mut self, count: usize) {
    self.current_frame.ip = self.current_frame.ip.saturating_add(count);
  }

  fn loop_back(&mut self, count: usize) {
    self.current_frame.ip = self.current_frame.ip.saturating_sub(count);
  }

  #[cold]
  fn error_at<F: FnOnce(OpCodeReflection) -> Error>(&self, f: F) -> Error {
    if let Some(opcode_ref) = self.current_frame.ctx.meta.get(self.current_frame.ip) {
      f(opcode_ref)
    } else {
      Error {
        msg: format!(
          "could not fetch info for instruction {:04X}",
          self.current_frame.ip
        ),
        file: self.current_frame.ctx.meta.file.access().clone(),
        line: 0,
        column: 0,
      }
    }
  }

  #[cfg(debug_assertions)]
  #[cfg(feature = "runtime-disassembly")]
  pub fn stack_display(&self) {
    if self.current_frame.stack.is_empty() {
      println!("          | [ ]");
    } else {
      for (index, item) in self.current_frame.stack.iter().enumerate() {
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
  Ps,
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

  pub fn run(&mut self, ctx: SmartPtr<Context>, env: &mut Env) -> Result<RunResult, Vec<Error>> {
    #[cfg(debug_assertions)]
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
      Library::String => ("str", Self::load_string()),
      Library::Console => ("console", Self::load_console()),
      Library::Ps => ("ps", Self::load_ps()),
    }
  }

  fn load_std() -> Value {
    let mut obj = Struct::default();

    let mut array = Class::new("Array");

    array.set_initializer(Value::native("Array.new", |thread, _env, args| {
      thread.stack_push(Value::new(args));
      Ok(())
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

        Ok(())
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
      Value::native("len", |thread, _env, args| {
        let this = args.get(0).unwrap();

        match this {
          Value::Instance(instance) => match &instance.data {
            Value::List(list) => {
              thread.stack_push(Value::new(list.len() as f64));
              Ok(())
            }
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

    let clock = Value::native(String::from("clock"), |thread, _env, _args: Vec<Value>| {
      use std::time::{SystemTime, UNIX_EPOCH};
      let now = SystemTime::now();
      let since = now.duration_since(UNIX_EPOCH).expect("time went backwards");
      thread.stack_push(Value::new(since.as_nanos()));
      Ok(())
    });

    let clock_diff = Value::native(
      String::from("clock_diff"),
      |thread, _env, args: Vec<Value>| {
        if let Some(Value::U128(before)) = args.get(0) {
          if let Some(Value::U128(after)) = args.get(1) {
            let diff = std::time::Duration::from_nanos((after - before) as u64);
            thread.stack_push(Value::new(diff.as_secs_f64()));
            return Ok(());
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
      |thread, _env, args: Vec<Value>| {
        if let Some(arg) = args.get(0) {
          match arg {
            Value::String(string) => {
              let value = string.parse::<f64>().map_err(|e| format!("{}", e))?;
              thread.stack_push(Value::new(value));
              Ok(())
            }
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

      Ok(())
    });

    let writeln = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
      for arg in args {
        print!("{}", arg);
      }
      println!();

      Ok(())
    });

    obj.set("write", write);
    obj.set("writeln", writeln);

    Value::new(obj)
  }

  fn load_ps() -> Value {
    let mut obj = Struct::default();

    let exit = Value::native(String::from("write"), |_thread, _env, args: Vec<Value>| {
      let exit_code = args
        .get(0)
        .map(|v| match v {
          Value::Num(n) => *n as i32,
          _ => 0,
        })
        .unwrap_or(0);

      std::process::exit(exit_code);
    });

    obj.set("exit", exit);

    Value::new(obj)
  }
}
