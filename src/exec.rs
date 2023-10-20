use crate::{
  code::{Compiler, ConstantValue, Context, Env, OpCodeReflection, Opcode, StackFrame, Yield},
  dbg::{Cli, RuntimeError},
  memory::{Allocation, Gc},
  prelude::Library,
  value::prelude::*,
  UnwrapAnd,
};
use clap::Parser;
use dlopen2::wrapper::{Container, WrapperApi};
use ptr::SmartPtr;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::BTreeMap,
  env, fs,
  path::{Path, PathBuf},
  rc::Rc,
  time::{Duration, Instant},
};

#[derive(WrapperApi)]
struct NativeApi {
  simple_script_load_module: fn(vm: &mut Vm) -> ValueResult<()>,
}

pub mod prelude {
  pub use super::{Return, Vm};
}

type ExecResult<'file> = Result<(), Vec<RuntimeError>>;
type ExecBoolResult<'file> = Result<bool, Vec<RuntimeError>>;

// yield must store ip and stack and return ctx
// or better yet, have ctx have an Option<usize> and option<Vec<Value>>
// and when running if present, then set the current thread's ip & stack to those values

pub enum Return {
  Value(Value),
  Yield(Yield),
}

const DEFAULT_GC_FREQUENCY: Duration = Duration::from_nanos(100);

pub struct Vm {
  pub(crate) current_frame: StackFrame,
  pub(crate) stack_frames: Vec<StackFrame>,
  pub gc: Gc,

  args: Vec<String>,
  libs: Library,

  // usize for what frame to pop on, string for file path
  opened_files: Vec<(usize, PathBuf)>,

  opened_libs: BTreeMap<PathBuf, Container<NativeApi>>,

  next_gc: Instant,
}

impl Vm {
  pub fn new(args: impl Into<Vec<String>>, libs: Library) -> Self {
    Self {
      args: args.into(),
      libs,
      current_frame: Default::default(),
      stack_frames: Default::default(),
      gc: Default::default(),
      opened_files: Default::default(),
      opened_libs: Default::default(),
      next_gc: Instant::now() + DEFAULT_GC_FREQUENCY,
    }
  }

  pub fn ssdb(&mut self) -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = DefaultEditor::new()?;
    loop {
      match rl.readline("dbg> ") {
        Ok(line) => match shellwords::split(&format!("dbg {}", line)) {
          Ok(words) => match Cli::try_parse_from(words) {
            Ok(cli) => match cli.exec(self) {
              Ok(output) => {
                rl.add_history_entry(line).ok();
                output.response.unwrap_and(|response| println!("=> {}", response));
                if output.quit {
                  return Ok(());
                }
              }
              Err(e) => println!("{}", e),
            },
            Err(e) => println!("{}", e),
          },
          Err(e) => println!("{}", e),
        },
        Err(ReadlineError::Interrupted) => {
          println!("Use repl.quit instead of CTRL-C");
        }
        Err(e) => Err(e)?,
      }
    }
  }

  pub fn load(&self, file: impl Into<PathBuf>, code: &str, env: Env) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    Compiler::compile(file.into(), code, SmartPtr::new(env))
  }

  pub fn resume(&mut self, y: Yield) -> Result<Return, Vec<RuntimeError>> {
    self.current_frame = y.current_frame;
    self.stack_frames = y.stack_frames;
    self.opened_files = y.opened_files;
    self.execute()
  }

  pub fn run(&mut self, file: impl Into<PathBuf>, ctx: SmartPtr<Context>) -> Result<Return, Vec<RuntimeError>> {
    #[cfg(feature = "disassemble")]
    {
      ctx.disassemble();

      #[cfg(feature = "quit-after-disassembled")]
      {
        std::process::exit(0);
      }
    }

    self.opened_files = vec![(0, file.into())];
    self.reinitialize(ctx);
    self.execute()
  }

  fn reinitialize(&mut self, ctx: SmartPtr<Context>) {
    self.current_frame = StackFrame::new(ctx);
    self.stack_frames = Default::default();
  }

  fn unary_op<F>(&mut self, opcode: &Opcode, f: F) -> ExecResult
  where
    F: FnOnce(Value) -> ValueResult,
  {
    if let Some(v) = self.stack_pop() {
      if v.is_ptr() {
        let key = match opcode {
          Opcode::Negate => ops::NEG,
          Opcode::Not => ops::NOT,
          op => Err(self.error(op, format!("invalid unary operation {:?}", op)))?,
        };

        let callable = v.lookup(&mut self.gc, key).map_err(|e| self.error(opcode, e))?;
        self.call_value(opcode, callable, [])
      } else {
        self.stack_push(f(v).map_err(|e| self.error(opcode, e))?);
        Ok(())
      }
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn binary_op<F>(&mut self, opcode: &Opcode, f: F) -> ExecResult
  where
    F: FnOnce(Value, Value) -> ValueResult,
  {
    if let Some(bv) = self.stack_pop() {
      if let Some(av) = self.stack_pop() {
        if av.is_ptr() {
          let key = match opcode {
            Opcode::Add => ops::ADD,
            Opcode::Sub => ops::SUB,
            Opcode::Mul => ops::MUL,
            Opcode::Div => ops::DIV,
            Opcode::Rem => ops::REM,
            Opcode::Equal => ops::EQUALITY,
            Opcode::NotEqual => ops::NOT_EQUAL,
            Opcode::Less => ops::LESS,
            Opcode::LessEqual => ops::LESS_EQUAL,
            Opcode::Greater => ops::GREATER,
            Opcode::GreaterEqual => ops::GREATER_EQUAL,
            op => Err(self.error(op, format!("invalid binary operation {:?}", op)))?,
          };

          let callable = av.lookup(&mut self.gc, key).map_err(|e| self.error(opcode, e))?;
          self.call_value(opcode, callable, [bv])
        } else {
          self.stack_push(f(av, bv).map_err(|e| self.error(opcode, e))?);
          Ok(())
        }
      } else {
        Err(self.error(opcode, String::from("cannot operate on empty stack")))
      }
    } else {
      Err(self.error(opcode, String::from("cannot operate on empty stack")))
    }
  }

  fn global_op<F>(&mut self, opcode: &Opcode, index: usize, f: F) -> ExecResult
  where
    F: FnOnce(&mut Self, String) -> ExecResult,
  {
    let name = if let Some(name) = self.current_frame.ctx.global_const_at(index) {
      if let ConstantValue::String(name) = name {
        Ok((*name).clone())
      } else {
        Err(self.error(opcode, format!("global variable name is not an identifier: {}", name)))
      }
    } else {
      Err(self.error(opcode, String::from("global variable name does not exist")))
    }?;

    f(self, name)
  }

  #[cold]
  fn error<M: ToString>(&self, opcode: &Opcode, msg: M) -> Vec<RuntimeError> {
    vec![self.error_at(|opcode_ref| RuntimeError::from_ref(msg, opcode, opcode_ref))]
  }

  fn execute(&mut self) -> Result<Return, Vec<RuntimeError>> {
    self.next_gc = Instant::now() + DEFAULT_GC_FREQUENCY;

    loop {
      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< {} >>", self.current_frame.ctx.id);
      }

      let mut should_return_from_stack = false;
      let mut export = None;

      while let Some(opcode) = self.current_frame.ctx.next(self.current_frame.ip) {
        let now = Instant::now();
        if now > self.next_gc {
          self.next_gc = now + DEFAULT_GC_FREQUENCY;
          self.gc.clean(&self.current_frame, &self.stack_frames);
        }

        #[cfg(feature = "runtime-disassembly")]
        {
          self.stack_display();
          self.current_frame.ctx.display_instruction(&opcode, self.current_frame.ip);
        }

        match opcode {
          Opcode::NoOp => self.exec_noop()?,
          Opcode::Const(index) => self.exec_const(&opcode, index)?,
          Opcode::Nil => self.exec_nil(),
          Opcode::True => self.exec_true(),
          Opcode::False => self.exec_false(),
          Opcode::Pop => self.exec_pop(),
          Opcode::PopN(count) => self.exec_pop_n(count),
          Opcode::ForceAssignGlobal(index) => self.exec_force_assign_global(&opcode, index)?,
          Opcode::DefineGlobal(index) => self.exec_define_global(&opcode, index)?,
          Opcode::LookupGlobal(index) => self.exec_lookup_global(&opcode, index)?,
          Opcode::AssignGlobal(index) => self.exec_assign_global(&opcode, index)?,
          Opcode::LookupLocal(index) => self.exec_lookup_local(&opcode, index)?,
          Opcode::AssignLocal(index) => self.exec_assign_local(&opcode, index)?,
          Opcode::InitializeMember(index) => self.exec_initialize_member(&opcode, index)?,
          Opcode::AssignMember(index) => self.exec_assign_member(&opcode, index)?,
          Opcode::LookupMember(index) => self.exec_lookup_member(&opcode, index)?,
          Opcode::PeekMember(index) => self.exec_peek_member(&opcode, index)?,
          Opcode::Equal => self.exec_equal(&opcode)?,
          Opcode::NotEqual => self.exec_not_equal(&opcode)?,
          Opcode::Greater => self.exec_greater(&opcode)?,
          Opcode::GreaterEqual => self.exec_greater_equal(&opcode)?,
          Opcode::Less => self.exec_less(&opcode)?,
          Opcode::LessEqual => self.exec_less_equal(&opcode)?,
          Opcode::Check => self.exec_check(&opcode)?,
          Opcode::Add => self.exec_add(&opcode)?,
          Opcode::Sub => self.exec_sub(&opcode)?,
          Opcode::Mul => self.exec_mul(&opcode)?,
          Opcode::Div => self.exec_div(&opcode)?,
          Opcode::Rem => self.exec_rem(&opcode)?,
          Opcode::Or(count) => {
            if self.exec_or(&opcode, count)? {
              continue;
            }
          }
          Opcode::And(count) => {
            if self.exec_and(&opcode, count)? {
              continue;
            }
          }
          Opcode::Not => self.exec_not(&opcode)?,
          Opcode::Negate => self.exec_negate(&opcode)?,
          Opcode::Print => self.exec_print(&opcode)?,
          Opcode::Jump(count) => {
            self.jump(count);
            continue;
          }
          Opcode::JumpIfFalse(count) => {
            if self.exec_jump_if_false(&opcode, count)? {
              continue;
            }
          }
          Opcode::Loop(count) => {
            self.loop_back(count);
            continue;
          }
          Opcode::Call(airity) => {
            self.current_frame.ip += 1;
            self.exec_call(&opcode, airity)?;
            continue;
          }
          Opcode::Ret => {
            break;
          }
          Opcode::RetValue => {
            should_return_from_stack = true;
            break;
          }
          Opcode::Req => {
            self.current_frame.ip += 1;
            self.exec_req(&opcode)?;
            continue;
          }
          Opcode::CreateList(num_items) => self.exec_create_list(num_items),
          Opcode::CreateClosure => self.exec_create_closure(&opcode)?,
          Opcode::CreateStruct => self.exec_create_struct(),
          Opcode::CreateModule => self.exec_create_module(),
          Opcode::Lock => self.exec_lock()?,
          Opcode::Yield => {
            self.current_frame.ip += 1;

            let current_frame = self.current_frame.clear_out();

            let mut stack_frames = Vec::default();
            std::mem::swap(&mut stack_frames, &mut self.stack_frames);

            let mut opened_files = Vec::default();
            std::mem::swap(&mut opened_files, &mut self.opened_files);

            return Ok(Return::Yield(Yield::new(current_frame, stack_frames, opened_files)));
          }
          Opcode::Breakpoint => {
            self.ssdb().ok();
          }
          Opcode::Export => {
            if let Some(value) = self.stack_pop() {
              export = Some(value);
            } else {
              self.error(&opcode, "no value on stack to export");
            }
          }
        }

        self.current_frame.ip += 1;
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        println!("<< END >>");
      }

      let output = export
        // if there's an export, use this, only possible when exiting a file
        .take()
        // return from stack, not possible to hit outside of functions
        // failure here is a logic error so unwrap
        .or_else(|| should_return_from_stack.then(|| self.stack_pop().unwrap()))
        // if implicit/void return nil
        .unwrap_or_default();

      // if executing at the stack frame that required this file, pop it as we're exiting the file
      // repl isn't an opened file therefore have to check for Some
      if let Some(last_file) = self.opened_files.last() {
        if last_file.0 == self.stack_frames.len() {
          self.opened_files.pop();
        }
      }

      if let Some(stack_frame) = self.stack_frames.pop() {
        if stack_frame.ip < stack_frame.ctx.num_instructions() {
          self.current_frame = stack_frame;
          self.stack_push(output);
        } else {
          break Ok(Return::Value(output));
        }
      } else {
        // possible to reach with repl
        // since it will run out of instructions
        // but keep the stack/ip
        break Ok(Return::Value(output));
      }
    }
  }

  /* Operations */

  fn exec_noop(&self) -> ExecResult {
    Err(self.error(&Opcode::NoOp, String::from("executed noop opcode, should not happen")))
  }

  fn exec_const(&mut self, opcode: &Opcode, index: usize) -> ExecResult {
    if let Some(c) = self.current_frame.ctx.const_at(index) {
      let value = Value::from_constant(&mut self.gc, c);
      self.stack_push(value);
    } else {
      Err(self.error(opcode, String::from("could not lookup constant")))?;
    }

    Ok(())
  }

  fn exec_nil(&mut self) {
    self.stack_push(Value::nil);
  }

  fn exec_true(&mut self) {
    self.stack_push(Value::from(true));
  }

  fn exec_false(&mut self) {
    self.stack_push(Value::from(false));
  }

  fn exec_pop(&mut self) {
    self.stack_pop();
  }

  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  fn exec_lookup_local(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(local) = self.stack_index(location) {
      self.stack_push(local);
      Ok(())
    } else {
      Err(self.error(opcode, format!("could not index stack at pos {}", location)))
    }
  }

  fn exec_assign_local(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      self.stack_assign(location, value);
      Ok(())
    } else {
      Err(self.error(opcode, format!("could not replace stack value at pos {}", location)))
    }
  }

  fn exec_lookup_global(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(global) = this.env().lookup(&name) {
        this.stack_push(global);
        Ok(())
      } else {
        Err(this.error(opcode, String::from("use of undefined variable")))
      }
    })
  }

  fn exec_force_assign_global(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      // used with functions & classes only, so pop
      if let Some(v) = this.stack_pop() {
        this.env().assign(name, v);
        Ok(())
      } else {
        Err(this.error(opcode, String::from("can not define global using empty stack")))
      }
    })
  }

  fn exec_define_global(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if this.env().define(name, v) {
          Ok(())
        } else {
          Err(this.error(opcode, String::from("tried redefining global variable")))
        }
      } else {
        Err(this.error(opcode, String::from("can not define global using empty stack")))
      }
    })
  }

  fn exec_assign_global(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    self.global_op(opcode, location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if this.env().assign(name, v) {
          Ok(())
        } else {
          Err(this.error(opcode, String::from("tried to assign to nonexistent global")))
        }
      } else {
        Err(this.error(opcode, String::from("can not assign to global using empty stack")))
      }
    })
  }

  fn exec_initialize_member(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            if let Some(obj) = obj.as_struct_mut() {
              obj.set(name, value);
              Ok(())
            } else if let Some(obj) = obj.as_instance_mut() {
              obj.set(&mut self.gc, name, value).map_err(|e| self.error(opcode, e))
            } else if let Some(obj) = obj.as_module_mut() {
              obj.set(&mut self.gc, name, value).map_err(|e| self.error(opcode, e))
            } else {
              Err(self.error(opcode, String::from("invalid type for member initialization")))
            }
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

  fn exec_assign_member(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_pop() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            obj
              .assign(&mut self.gc, &name, value.clone())
              .map_err(|e| self.error(opcode, e))?;
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

  fn exec_lookup_member(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(obj) = self.stack_pop() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let ConstantValue::String(name) = name {
          let value = obj.lookup(&mut self.gc, name).map_err(|e| self.error(opcode, e))?;
          self.stack_push(value);
          Ok(())
        } else {
          Err(self.error(opcode, "member identifier is not a string"))
        }
      } else {
        Err(self.error(opcode, "no identifier at index"))
      }
    } else {
      Err(self.error(opcode, "no value on stack to perform lookup on"))
    }
  }

  fn exec_peek_member(&mut self, opcode: &Opcode, location: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let ConstantValue::String(name) = name {
          if let Some(obj) = value.as_struct() {
            let member = obj.get(&mut self.gc, &value, name).map_err(|e| self.error(opcode, e))?;
            self.stack_push(member);
            Ok(())
          } else if let Some(obj) = value.as_instance() {
            let member = obj.get(&mut self.gc, &value, name).map_err(|e| self.error(opcode, e))?;
            self.stack_push(member);
            Ok(())
          } else {
            Err(self.error(opcode, format!("invalid type for member access: {}", value)))
          }
        } else {
          Err(self.error(opcode, format!("invalid lookup for member access: {}", value)))
        }
      } else {
        Err(self.error(opcode, format!("no name for member access: {}", value)))
      }
    } else {
      Err(self.error(opcode, "no object to lookup on"))
    }
  }

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: &Opcode, f: F) -> ExecResult {
    self.binary_op(opcode, |a, b| Ok(Value::from(f(a, b))))
  }

  fn exec_equal(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a == b)
  }

  fn exec_not_equal(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a != b)
  }

  fn exec_greater(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a > b)
  }

  fn exec_greater_equal(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a >= b)
  }

  fn exec_less(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a < b)
  }

  fn exec_less_equal(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_bool(opcode, |a, b| a <= b)
  }

  fn exec_check(&mut self, opcode: &Opcode) -> ExecResult {
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

  fn exec_arith<F: FnOnce(Value, Value) -> ValueResult>(&mut self, opcode: &Opcode, f: F) -> ExecResult {
    self.binary_op(opcode, |a, b| f(a, b))
  }

  fn exec_add(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a + b)
  }

  fn exec_sub(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a - b)
  }

  fn exec_mul(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a * b)
  }

  fn exec_div(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a / b)
  }

  fn exec_rem(&mut self, opcode: &Opcode) -> ExecResult {
    self.exec_arith(opcode, |a, b| a % b)
  }

  /// when f evaluates to true, short circuit

  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, opcode: &Opcode, offset: usize, f: F) -> ExecBoolResult {
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

  fn exec_or(&mut self, opcode: &Opcode, offset: usize) -> ExecBoolResult {
    self.exec_logical(opcode, offset, |v| v.truthy())
  }

  fn exec_and(&mut self, opcode: &Opcode, offset: usize) -> ExecBoolResult {
    self.exec_logical(opcode, offset, |v| v.falsy())
  }

  fn exec_not(&mut self, opcode: &Opcode) -> ExecResult {
    self.unary_op(opcode, |v| Ok(!v))
  }

  fn exec_negate(&mut self, opcode: &Opcode) -> ExecResult {
    self.unary_op(opcode, |v| -v)
  }

  fn exec_print(&mut self, opcode: &Opcode) -> ExecResult {
    if let Some(v) = self.stack_pop() {
      println!("{}", v);
      Ok(())
    } else {
      Err(self.error(opcode, "no value to print"))
    }
  }

  fn exec_jump_if_false(&mut self, opcode: &Opcode, offset: usize) -> ExecBoolResult {
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

  fn exec_call(&mut self, opcode: &Opcode, airity: usize) -> ExecResult {
    let args = self.stack_drain_from(airity);
    if let Some(callable) = self.stack_pop() {
      self.call_value(opcode, callable, args)
    } else {
      Err(self.error(opcode, "cannot operate on empty stack"))
    }
  }

  fn exec_req(&mut self, opcode: &Opcode) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      let mut attempts = Vec::with_capacity(10);

      let file_str = value.to_string();
      let this_file = self.opened_files.last().map(|f| f.1.clone());

      let required_file = PathBuf::from(file_str.as_str());

      let mut found_file = None;

      fn try_to_find_file(root: &Path, desired: &PathBuf, attempts: &mut Vec<PathBuf>) -> Option<PathBuf> {
        let direct = root.join(&desired);
        if direct.exists() {
          return Some(direct);
        }
        attempts.push(direct.clone());

        let direct_with_native_extension = direct.with_extension(dlopen2::utils::PLATFORM_FILE_EXTENSION);
        if direct_with_native_extension.exists() {
          return Some(direct_with_native_extension);
        }
        attempts.push(direct_with_native_extension);

        let direct_with_script_extension = direct.with_extension("ss");
        if direct_with_script_extension.exists() {
          return Some(direct_with_script_extension);
        }
        attempts.push(direct_with_script_extension);

        None
      }

      // find relative first, skip if None, None will be during repl so go to cwd
      if let Some(this_dir) = this_file
        .map(|this_file| this_file.parent().map(|p| p.to_path_buf()))
        .flatten()
      {
        found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
      }

      // then try to find from cwd
      if found_file.is_none() {
        let this_dir = env::current_dir().map_err(|e| self.error(opcode, e))?;
        found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
      }

      // if still not found, try searching library paths
      if found_file.is_none() {
        if let Some(library_mod) = self.env().lookup("$LIBRARY") {
          if let Some(library_mod) = library_mod.as_struct() {
            if let Ok(Some(list)) = library_mod.get_field(&mut self.gc, "path").map(|l| l.map(|l| l.as_array())) {
              for item in list.iter() {
                let base = PathBuf::from(item.to_string());
                found_file = try_to_find_file(&base, &required_file, &mut attempts);
                if found_file.is_some() {
                  break;
                }
              }
            }
          }
        }
      }

      if let Some(found_file) = found_file {
        match found_file.extension().and_then(|s| s.to_str()) {
          Some(dlopen2::utils::PLATFORM_FILE_EXTENSION) => {
            let lib: Container<NativeApi> =
              unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

            lib.simple_script_load_module(self).map_err(|e| self.error(opcode, e))?;

            self.opened_libs.insert(found_file, lib);
            Ok(())
          }
          _ => match fs::read_to_string(&found_file) {
            Ok(data) => {
              let env = SmartPtr::new(Env::initialize(&mut self.gc, &self.args, self.libs.clone()));
              let new_ctx = Compiler::compile(found_file.clone(), &data, env)?;

              #[cfg(feature = "disassemble")]
              {
                println!("!!!!! ENTERING {} !!!!!", found_file.display());
                new_ctx.disassemble();
                println!("!!!!! LEAVING  {} !!!!!", found_file.display());
              }

              self.new_frame(new_ctx);

              self.opened_files.push((self.stack_frames.len(), found_file));

              Ok(())
            }
            Err(e) => Err(self.error(opcode, format!("unable to read file '{}': {}", file_str, e,))),
          },
        }
      } else {
        Err(self.error(opcode, format!("unable to find file, tried: {:#?}", attempts)))
      }
    } else {
      Err(self.error(opcode, "no item on stack to require (logic error)"))
    }
  }

  fn exec_create_list(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    let list = self.gc.allocate(list);
    self.stack_push(list);
  }

  fn exec_create_closure(&mut self, opcode: &Opcode) -> ExecResult {
    match self.stack_pop() {
      Some(function) => match self.stack_pop() {
        Some(captures) => {
          if let Some(f) = function.as_fn() {
            if let Some(captures) = captures.as_array() {
              let closure = self.gc.allocate(ClosureValue::new(captures, f.clone()));
              self.stack_push(closure);
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

  fn exec_create_struct(&mut self) {
    let v = self.gc.allocate(StructValue::default());
    self.stack_push(v);
  }

  fn exec_create_module(&mut self) {
    let v = self.gc.allocate(ModuleValue::new());
    self.stack_push(v);
  }

  fn exec_lock(&mut self) -> ExecResult {
    if let Some(mut value) = self.stack_peek() {
      value.lock();
      Ok(())
    } else {
      Err(self.error(&Opcode::Lock, "no item on the stack to lock"))
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

  fn call_value(&mut self, opcode: &Opcode, mut callable: Value, args: impl Into<Vec<Value>>) -> ExecResult {
    let args = args.into();
    let res = if let Some(f) = callable.as_fn() {
      let args = Args::new(args);
      f.call(self, args);
      Ok(())
    } else if let Some(f) = callable.as_closure() {
      let args = Args::new(args);
      f.call(self, args);
      Ok(())
    } else if let Some(f) = callable.as_method() {
      let args = Args::new_with_this(f.this.clone(), args);
      f.call(self, args);
      Ok(())
    } else if let Some(f) = callable.as_native_fn() {
      let args = Args::new(args);
      let v = f(self, args).map_err(|e| self.error(opcode, e))?;
      self.stack_push(v);
      Ok(())
    } else if let Some(f) = callable.as_native_closure_mut() {
      let args = Args::new(args);
      let v = f.call(self, args).map_err(|e| self.error(opcode, e))?;
      self.stack_push(v);
      Ok(())
    } else if let Some(f) = callable.as_native_method_mut() {
      let args = Args::new_with_this(f.this.clone(), args);
      let v = f.call(self, args).map_err(|e| self.error(opcode, e))?;
      self.stack_push(v);
      Ok(())
    } else if callable.is_class() {
      let args = Args::new(args);
      ClassValue::construct(self, callable, args).map_err(|e| self.error(opcode, e))?;
      Ok(())
    } else {
      Err(vec![format!(
        "unable to call non callable '{}', perhaps an operator overload was undefined?",
        callable
      )])
    }
    .map_err(|errors| {
      errors
        .into_iter()
        .map(|e| self.error_at(|opcode_ref| RuntimeError::from_ref(e, opcode, opcode_ref)))
        .collect()
    });

    if cfg!(feature = "runtime-disassembly") {
      println!("<< entering {} >>", self.current_frame.ctx.id);
    }

    res
  }

  pub fn ctx(&mut self) -> &Context {
    &self.current_frame.ctx
  }

  pub fn ctx_mut(&mut self) -> &mut Context {
    &mut self.current_frame.ctx
  }

  pub fn env(&mut self) -> &mut Env {
    &mut self.ctx_mut().env
  }

  #[cold]
  fn error_at<F: FnOnce(OpCodeReflection) -> RuntimeError>(&self, f: F) -> RuntimeError {
    if let Some(opcode_ref) = self.current_frame.ctx.meta.get(self.current_frame.ip) {
      f(opcode_ref)
    } else {
      RuntimeError {
        msg: format!("could not fetch info for instruction {:04X}", self.current_frame.ip),
        file: Rc::clone(&self.current_frame.ctx.meta.file),
        line: 0,
        column: 0,
      }
    }
  }

  pub fn stack_display(&self) {
    if self.current_frame.stack.is_empty() {
      println!("               | [ ]");
    } else {
      for (index, item) in self.current_frame.stack.iter().enumerate() {
        println!("{:#15}| [ {:?} ]", index, item);
      }
    }
  }
}
