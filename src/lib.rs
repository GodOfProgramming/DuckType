mod code;
mod dbg;
mod stdlib;
mod value;

#[cfg(test)]
mod test;

pub use code::Context;
pub use code::Env;
use code::{Compiler, OpCode, OpCodeReflection, StackFrame, Yield};
use dbg::here;
use dbg::RuntimeError;
use ptr::SmartPtr;
pub use stdlib::Library;
pub use value::prelude::*;

use std::ops::Deref;
use std::ops::Index;
use std::{
  fs,
  path::{Path, PathBuf},
};

type ExecResult = Result<(), Vec<RuntimeError>>;
type ExecBoolResult = Result<bool, Vec<RuntimeError>>;

pub mod prelude {
  pub use super::value::prelude::*;
  pub use super::{Context, Env, ExecutionThread, Library, RunResult, Vm};
}

// yield must store ip and stack and return ctx
// or better yet, have ctx have an Option<usize> and option<Vec<Value>>
// and when running if present, then set the current thread's ip & stack to those values

#[derive(Debug)]
pub enum RunResult {
  Value(Value),
  Yield(Yield),
}

#[derive(Default)]
pub struct ExecutionThread {
  current_frame: StackFrame,
  stack_frames: Vec<StackFrame>,
  // usize for what frame to pop on, string for file path
  opened_files: Vec<(usize, PathBuf)>,
}

impl ExecutionThread {
  fn reinitialize(&mut self, ctx: SmartPtr<Context>) {
    self.current_frame = StackFrame::new(ctx);
    self.stack_frames = Default::default();
  }

  fn unary_op<F>(&mut self, opcode: &OpCode, f: F) -> ExecResult
  where
    F: FnOnce(&mut Self, Value),
  {
    if let Some(v) = self.stack_pop() {
      f(self, v);
      Ok(())
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn binary_op<F>(&mut self, opcode: &OpCode, f: F) -> ExecResult
  where
    F: FnOnce(&mut Self, Value, Value),
  {
    if let Some(bv) = self.stack_pop() {
      if let Some(av) = self.stack_pop() {
        f(self, av, bv);
        Ok(())
      } else {
        Err(self.error(opcode, String::from("cannot operate on empty stack")))
      }
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn global_op<F>(&mut self, opcode: &OpCode, index: usize, f: F) -> ExecResult
  where
    F: FnOnce(&mut Self, String) -> ExecResult,
  {
    let name = if let Some(name) = self.current_frame.ctx.global_const_at(index) {
      if let Ok(name) = name.as_str() {
        Ok(name.deref().clone())
      } else {
        Err(self.error(opcode, format!("global variable name is not an identifier: {}", name)))
      }
    } else {
      Err(self.error(opcode, String::from("global variable name does not exist")))
    }?;

    f(self, name)
  }

  #[cold]
  fn error<M: ToString>(&self, opcode: &OpCode, msg: M) -> Vec<RuntimeError> {
    vec![self.error_at(|opcode_ref| RuntimeError::from_ref(msg, opcode, opcode_ref))]
  }

  #[inline]
  fn run(&mut self, file: PathBuf, ctx: SmartPtr<Context>, env: &mut Env) -> Result<RunResult, Vec<RuntimeError>> {
    self.opened_files = vec![(0, file)];
    self.reinitialize(ctx);
    self.execute(env)
  }

  pub fn resume(&mut self, y: Yield, env: &mut Env) -> Result<RunResult, Vec<RuntimeError>> {
    self.current_frame = y.current_frame;
    self.stack_frames = y.stack_frames;
    self.opened_files = y.opened_files;
    self.execute(env)
  }

  fn execute(&mut self, env: &mut Env) -> Result<RunResult, Vec<RuntimeError>> {
    loop {
      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< {} >>", self.current_frame.ctx.display_str());
      }

      let mut returned_value = false;

      while let Some(opcode) = self.current_frame.ctx.next(self.current_frame.ip) {
        #[cfg(feature = "runtime-disassembly")]
        {
          self.stack_display();
          self.current_frame.ctx.display_instruction(&opcode, self.current_frame.ip);
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
          OpCode::InitializeMember(index) => self.exec_initialize_member(&opcode, index)?,
          OpCode::AssignMember(index) => self.exec_assign_member(&opcode, index)?,
          OpCode::LookupMember(index) => self.exec_lookup_member(&opcode, index)?,
          OpCode::PeekMember(index) => self.exec_peek_member(&opcode, index)?,
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
          OpCode::Yield => {
            self.current_frame.ip += 1;

            let current_frame = self.current_frame.clear_out();

            let mut stack_frames = Vec::default();
            std::mem::swap(&mut stack_frames, &mut self.stack_frames);

            let mut opened_files = Vec::default();
            std::mem::swap(&mut opened_files, &mut self.opened_files);

            return Ok(RunResult::Yield(Yield::new(current_frame, stack_frames, opened_files)));
          }
        }

        self.current_frame.ip += 1;
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< END >>");
      }

      let ret_val = if returned_value {
        // return actual value, failure here is a logic error so unwrap
        self.stack_pop().unwrap()
      } else {
        // if implicit/void return nil
        Value::nil
      };

      let last_file = self.opened_files.last().unwrap();

      // if we're at the stack frame that required this file, pop it as we're exiting the file
      if last_file.0 == self.stack_frames.len() {
        self.opened_files.pop();
      }

      if let Some(stack_frame) = self.stack_frames.pop() {
        if stack_frame.ip < stack_frame.ctx.num_instructions() {
          self.current_frame = stack_frame;
          self.stack_push(ret_val);
        } else {
          break Ok(RunResult::Value(ret_val));
        }
      } else {
        // TODO is this even possible to reach?
        break Ok(RunResult::Value(ret_val));
      }
    }
  }

  /* Operations */

  #[inline]
  fn exec_noop(&self) -> ExecResult {
    Err(self.error(&OpCode::NoOp, String::from("executed noop opcode, should not happen")))
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
    self.stack_push(Value::nil);
  }

  #[inline]
  fn exec_true(&mut self) {
    self.stack_push(Value::from(true));
  }

  #[inline]
  fn exec_false(&mut self) {
    self.stack_push(Value::from(false));
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
      Err(self.error(opcode, format!("could not replace stack value at pos {}", location)))
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
  fn exec_force_assign_global(&mut self, env: &mut Env, opcode: &OpCode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      // used with functions & classes only, so pop
      if let Some(v) = this.stack_pop() {
        env.assign(name, v.clone());
        Ok(())
      } else {
        Err(this.error(opcode, String::from("can not define global using empty stack")))
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
        Err(this.error(opcode, String::from("can not define global using empty stack")))
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
          Err(this.error(opcode, String::from("tried to assign to nonexistent global")))
        }
      } else {
        Err(this.error(opcode, String::from("can not assign to global using empty stack")))
      }
    })
  }

  #[inline]
  fn exec_initialize_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let Ok(name) = name.as_str() {
            obj.set(name, value).map_err(|e| self.error(opcode, format!("{}", e)))
          } else {
            Err(self.error(opcode, String::from("invalid name for member")))
          }
        } else {
          Err(self.error(opcode, String::from("no identifier found at index")))
        }
      } else {
        Err(self.error(opcode, String::from("no value on stack to assign to member")))
      }
    } else {
      Err(self.error(opcode, String::from("no value on stack to assign a member to")))
    }
  }

  #[inline]
  fn exec_assign_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_pop() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let Ok(name) = name.as_str() {
            obj
              .set(name, value.clone())
              .map_err(|e| self.error(opcode, format!("{}", e)))?;
            self.stack_push(value);
            Ok(())
          } else {
            Err(self.error(opcode, String::from("constant at index is not an identifier")))
          }
        } else {
          Err(self.error(opcode, String::from("no ident found at index")))
        }
      } else {
        Err(self.error(opcode, String::from("no object to assign to")))
      }
    } else {
      Err(self.error(opcode, String::from("no value on stack to assign to member")))
    }
  }

  #[inline]
  fn exec_lookup_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(obj) = self.stack_pop() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let Ok(name) = name.as_str() {
          if obj.is_ptr() {
            self.stack_push(obj.get(name));
            self.current_frame.last_lookup = obj;
            Ok(())
          } else {
            Err(self.error(opcode, format!("cannot access member of primitive {}", obj)))
          }
        } else {
          Err(self.error(opcode, format!("member identifier is not a string")))
        }
      } else {
        Err(self.error(opcode, format!("no identifier at index")))
      }
    } else {
      Err(self.error(opcode, format!("no value on stack to perform lookup on")))
    }
  }

  #[inline]
  fn exec_peek_member(&mut self, opcode: &OpCode, location: usize) -> ExecResult {
    if let Some(obj) = self.stack_peek() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let Ok(name) = name.as_str() {
          if obj.is_ptr() {
            self.stack_push(obj.get(name));
            Ok(())
          } else {
            Err(self.error(opcode, format!("invalid type for member access: {}", obj)))
          }
        } else {
          Err(self.error(opcode, format!("invalid lookup for member access: {}", obj)))
        }
      } else {
        Err(self.error(opcode, format!("no name for member access: {}", obj)))
      }
    } else {
      Err(self.error(opcode, "no object to lookup on"))
    }
  }

  #[inline]
  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: &OpCode, f: F) -> ExecResult {
    self.binary_op(opcode, |this, a, b| {
      this.stack_push(Value::from(f(a, b)));
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
          self.stack_push(Value::from(a == b));
          Ok(())
        }
        None => Err(self.error(opcode, "stack peek failed")),
      },
      None => Err(self.error(opcode, "stack pop failed")),
    }
  }

  #[inline]
  fn exec_arith<F: FnOnce(Value, Value) -> Value>(&mut self, opcode: &OpCode, f: F) -> ExecResult {
    self.binary_op(opcode, |this, a, b| {
      this.stack_push(f(a, b));
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

  /// when f evaluates to true, short circuit
  #[inline]
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, opcode: &OpCode, offset: usize, f: F) -> ExecBoolResult {
    match self.stack_peek() {
      Some(v) => {
        if f(v) {
          here!();
          self.jump(offset);
          Ok(true)
        } else {
          here!();
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
    self.exec_logical(opcode, offset, |v| v.falsy())
  }

  #[inline]
  fn exec_not(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |this, v| {
      this.stack_push(!v);
    })
  }

  #[inline]
  fn exec_negate(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |this, v| {
      this.stack_push(-v);
    })
  }

  #[inline]
  fn exec_print(&mut self, opcode: &OpCode) -> ExecResult {
    self.unary_op(opcode, |_this, v| {
      println!("{}", v);
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

      let res = if let Ok(f) = callable.as_fn() {
        f.call(self, args.into());
        Ok(())
      } else if let Ok(f) = callable.as_closure() {
        f.call(self, args.into());
        Ok(())
      } else if let Ok(f) = callable.as_method() {
        let mut this = Value::nil;
        std::mem::swap(&mut this, &mut self.current_frame.last_lookup);
        let args = Args::from((this, args));
        f.call(self, args.into());
        Ok(())
      } else if let Ok(f) = callable.as_native_fn() {
        let v = f(self, env, args.into());
        self.stack_push(v);
        Ok(())
      } else if let Ok(f) = callable.as_native_closure_mut() {
        let v = f.call(self, env, args.into());
        self.stack_push(v);
        Ok(())
      } else if let Ok(f) = callable.as_native_method_mut() {
        let mut this = Value::nil;
        std::mem::swap(&mut this, &mut self.current_frame.last_lookup);
        let args = Args::from((this, args));
        let v = f.call(self, env, args.into());
        self.stack_push(v);
        Ok(())
      } else if callable.is_class() {
        ClassValue::call_constructor(callable, self, env, args.into());
        Ok(())
      } else {
        Err(vec![format!("unable to call non callable '{}'", callable)])
      }
      .map_err(|errors| {
        errors
          .into_iter()
          .map(|e| self.error_at(|opcode_ref| RuntimeError::from_ref(e, opcode, opcode_ref)))
          .collect()
      });

      if cfg!(feature = "runtime-disassembly") {
        println!("<< entering {} >>", self.current_frame.ctx.display_str());
      }

      res
    } else {
      Err(self.error(opcode, "cannot operate on empty stack"))
    }
  }

  #[inline]
  fn exec_req(&mut self, env: &mut Env, opcode: &OpCode) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      let mut attempts = Vec::with_capacity(10);

      let file = value.to_string();
      let this_file = &self.opened_files.last().unwrap().1; // must exist by program logic
      let required_file = PathBuf::from(file.as_str());
      let required_file_with_ext = if required_file.extension().is_none() {
        Some(required_file.with_extension("ss"))
      } else {
        // if already given ext, assume it was intentional to not follow convention
        None
      };

      let mut found_file = None;

      // find relative first, if not already at cwd
      if let Some(this_dir) = this_file.parent() {
        let relative_path = this_dir.join(&required_file);
        if Path::exists(&relative_path) {
          found_file = Some(relative_path);
        } else if let Some(required_file_with_ext) = &required_file_with_ext {
          attempts.push(relative_path);
          // then try with the .ss extension
          let relative_path = this_dir.join(required_file_with_ext);
          if Path::exists(&relative_path) {
            found_file = Some(relative_path);
          } else {
            attempts.push(relative_path);
          }
        }
      }

      if found_file.is_none() {
        // then try to find from cwd
        if Path::exists(&required_file) {
          found_file = Some(required_file);
        } else {
          attempts.push(required_file.clone());
          // then try with the .ss extension
          if let Some(required_file_with_ext) = &required_file_with_ext {
            if Path::exists(&required_file_with_ext) {
              found_file = Some(required_file_with_ext.clone());
            } else {
              attempts.push(required_file_with_ext.to_path_buf());
            }
          }

          // if still not found, try searching library paths
          if found_file.is_none() {
            if let Some(library_mod) = env.lookup("$LIBRARY") {
              if let Ok(library_mod) = library_mod.as_struct() {
                if let Ok(list) = library_mod.get("path").as_array() {
                  for item in list.iter() {
                    let base = PathBuf::from(item.to_string());
                    let path = base.join(&required_file);

                    if Path::exists(&path) {
                      found_file = Some(path);
                      break;
                    } else if let Some(required_file_with_ext) = &required_file_with_ext {
                      attempts.push(path);
                      let path = base.join(required_file_with_ext);

                      if Path::exists(&path) {
                        found_file = Some(path);
                        break;
                      } else {
                        attempts.push(path);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      if let Some(found_file) = found_file {
        match fs::read_to_string(&found_file) {
          Ok(data) => {
            let new_ctx = Compiler::compile(&file, &data)?;

            #[cfg(debug_assertions)]
            #[cfg(feature = "disassemble")]
            {
              new_ctx.disassemble();
            }

            self.new_frame(new_ctx);

            self.opened_files.push((self.stack_frames.len(), found_file));

            Ok(())
          }
          Err(e) => Err(self.error(opcode, format!("unable to read file '{}': {}", file, e,))),
        }
      } else {
        Err(self.error(opcode, format!("unable to find file, tried: {:#?}", attempts)))
      }
    } else {
      Err(self.error(opcode, "no item on stack to require (logic error)"))
    }
  }

  #[inline]
  fn exec_index(&mut self, opcode: &OpCode, env: &mut Env) -> ExecResult {
    if let Some(index) = self.stack_pop() {
      if let Some(mut indexable) = self.stack_pop() {
        self.stack_push(indexable.index(index));
        Ok(())
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
    self.stack_push(Value::from(list));
  }

  #[inline]
  fn exec_create_closure(&mut self, opcode: &OpCode) -> ExecResult {
    match self.stack_pop() {
      Some(function) => match self.stack_pop() {
        Some(captures) => {
          if let Ok(f) = function.as_fn() {
            if let Ok(captures) = captures.as_array() {
              self.stack_push(Value::from(ClosureValue::new(captures, f.clone())));
              Ok(())
            } else {
              Err(self.error(opcode, "capture list must be a struct"))
            }
          } else {
            Err(self.error(opcode, "closure must be a function"))
          }
        }
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
    self.current_frame.stack.drain(self.stack_size() - index..).collect()
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
  fn error_at<F: FnOnce(OpCodeReflection) -> RuntimeError>(&self, f: F) -> RuntimeError {
    if let Some(opcode_ref) = self.current_frame.ctx.meta.get(self.current_frame.ip) {
      f(opcode_ref)
    } else {
      RuntimeError {
        msg: format!("could not fetch info for instruction {:04X}", self.current_frame.ip),
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
      println!("               | [ ]");
    } else {
      for (index, item) in self.current_frame.stack.iter().enumerate() {
        println!("{:#15}| [ {} ]", index, item);
      }
    }
  }
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

  pub fn load<T: ToString>(&self, file: T, code: &str) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    Compiler::compile(&file.to_string(), code)
  }

  pub fn resume(&mut self, y: Yield, env: &mut Env) -> Result<RunResult, Vec<RuntimeError>> {
    self.main.resume(y, env)
  }

  pub fn run<T: ToString>(&mut self, file: T, ctx: SmartPtr<Context>, env: &mut Env) -> Result<RunResult, Vec<RuntimeError>> {
    #[cfg(debug_assertions)]
    #[cfg(feature = "disassemble")]
    {
      ctx.disassemble();

      #[cfg(feature = "quit-after-disassembled")]
      {
        std::process::exit(0);
      }
    }
    self.main.run(PathBuf::from(file.to_string()), ctx, env)
  }
}
