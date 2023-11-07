mod env;
pub mod memory;

use crate::{
  code::{self, ConstantValue, FileMap, OpcodeReflection},
  dbg::Cli,
  prelude::*,
  util::{FileIdType, FileMetadata, PlatformMetadata},
  UnwrapAnd,
};
use clap::Parser;
use dlopen2::wrapper::{Container, WrapperApi};
use enum_map::{Enum, EnumMap};
use memory::{Allocation, Gc};
use ptr::SmartPtr;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::BTreeMap,
  fmt::{self, Debug, Display, Formatter},
  fs,
  path::{Path, PathBuf},
  time::{Duration, Instant},
};
use strum_macros::{EnumCount, EnumIter};

pub mod prelude {
  pub use super::env::prelude::*;
  pub use super::memory::*;
  pub use super::{Opcode, Vm};
}

#[cfg(test)]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_nanos(100);

#[cfg(not(test))]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_millis(16);

type ExecResult<T = ()> = Result<T, Error>;

type OpcodeResult<T = ()> = Result<T, UsageError>;

#[derive(WrapperApi)]
struct NativeApi {
  simple_script_load_module: fn(vm: &mut Vm) -> Result<Value, Error>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Storage {
  Local(BitsRepr),
  Global(BitsRepr),
  Reg(Register),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
  /** No operation instruction */
  NoOp,
  /** Looks up a constant value at the specified location. Location is specified by the tuple */
  Const(BitsRepr),
  /** Pushes a nil value on to the stack */
  Nil,
  /** Pushes a true value on to the stack */
  True,
  /** Pushes a false value on to the stack */
  False,
  /** Pops a value off the stack */
  Pop,
  /** Pops N values off the stack. N is specified by tuple */
  PopN(BitsRepr),
  /** Store a value in the specified register */
  Store(Storage),
  /** Load a value in the specified register */
  Load(Storage),
  /** Defines a member on an object type. The first item popped off the stack is the value. The object is next which is left on for further assignments. The member name is specified by the modifying bits */
  AssignMember(BitsRepr),
  /** Initializes a member of an object, keeping the object on the stack for further assignments */
  InitializeMember(BitsRepr),
  /** Initializes a method on a class, keeping the class on the stack for further assignments */
  InitializeMethod(BitsRepr),
  /** Initializes the constructor on a class, keeping the class on the stack for further assignments */
  InitializeConstructor,
  /** Uses the constant pointed to by the modifying bits to lookup a value on the next item on the stack */
  LookupMember(BitsRepr),
  /** Uses the constant pointed to by the modifying bits to peek at a value on the next item on the stack */
  PeekMember(BitsRepr),
  /** Pops two values off the stack, compares, then pushes the result back on */
  Equal,
  /** Pops two values off the stack, compares, then pushes the result back on */
  NotEqual,
  /** Pops two values off the stack, compares, then pushes the result back on */
  Greater,
  /** Pops two values off the stack, compares, then pushes the result back on */
  GreaterEqual,
  /** Pops two values off the stack, compares, then pushes the result back on */
  Less,
  /** Pops two values off the stack, compares, then pushes the result back on */
  LessEqual,
  /** Pops a value off the stack, and compares it with the peeked value, pushing the new value on */
  Check,
  /** Pops two values off the stack, calculates the sum, then pushes the result back on */
  Add,
  /** Pops two values off the stack, calculates the difference, then pushes the result back on */
  Sub,
  /** Pops two values off the stack, calculates the product, then pushes the result back on */
  Mul,
  /** Pops two values off the stack, calculates the quotient, then pushes the result back on */
  Div,
  /** Pops two values off the stack, calculates the remainder, then pushes the result back on */
  Rem,
  /** Peeks at the stack, if the top value is true short circuits to the instruction pointed to by the tuple */
  Or(BitsRepr),
  /** Peeks at the stack, if the top value is false short circuits to the instruction pointed to by the tuple */
  And(BitsRepr),
  /** Pops a value off the stack, inverts its truthy value, then pushes that back on */
  Not,
  /** Pops a value off the stack, inverts its numerical value, then pushes that back on */
  Negate,
  /** Pops a value off the stack and prints it to the screen */
  Println,
  /** Jumps to a code location indicated by the tuple */
  Jump(BitsRepr),
  /** Jumps to a code location indicated by the tuple */
  JumpIfFalse(BitsRepr),
  /** Jumps the instruction pointer backwards N instructions. N specified by the tuple */
  Loop(BitsRepr),
  /** Calls the value on the stack. Number of arguments is specified by the modifying bits */
  Invoke(BitsRepr),
  /** Exits from a function, returning nil on the previous frame */
  Ret,
  /** Returns the first item on the stack which is self */
  RetSelf,
  /** Require an external file. The file name is the top of the stack. Must be a string or convertible to */
  Req,
  /** Create a vec of values and push it on the stack. Items come off the top of the stack and the number is specified by the modifying bits */
  CreateVec(BitsRepr),
  /** Create a vec of values and push it on the stack. The last item on the stack is copied as many times as the param indicates */
  CreateSizedVec(BitsRepr),
  /** Create a vec of values and push it on the stack. The last item is the number of times and the next is the item to be copied the times specified */
  CreateDynamicVec,
  /** Create a closure. The first item on the stack is the function itself, the second is the capture list  */
  CreateClosure,
  /** Create a new struct with the number of members as the bits */
  CreateStruct(BitsRepr),
  /** Create a new class. Name is from the bits */
  CreateClass(BitsRepr),
  /** Create a new module. Name is from the bits */
  CreateModule(BitsRepr),
  /** Halt the VM when this instruction is reached and enter repl mode */
  Breakpoint,
  /** Mark the current value as exported */
  Export,
  /** Defines the identifier on the variable */
  Define(BitsRepr),
  /** Resolve the specified identifier */
  Resolve(BitsRepr),
  /** Push a new env */
  EnterBlock,
  /** Pop an env */
  PopScope,
  /**  */
  PushRegCtx,
  /**  */
  PopRegCtx,
}

pub struct Vm {
  // Don't get rid of current_frame in favor of the vec of stack frames
  // current_frame will be needed for repl so locals at the 0 depth scope
  // stay alive
  pub(crate) current_frame: StackFrame,
  pub(crate) stack_frames: Vec<StackFrame>,
  pub gc: SmartPtr<Gc>,

  program: Program,
  pub(crate) envs: EnvStack,

  args: Vec<String>,

  filemap: FileMap,
  pub(crate) lib_cache: BTreeMap<FileIdType, Value>,

  // usize for what frame to pop on, string for file path
  opened_files: Vec<FileInfo>,
  opened_native_libs: BTreeMap<PathBuf, Container<NativeApi>>,

  next_gc: Instant,
}

impl Vm {
  pub fn new(gc: SmartPtr<Gc>, args: impl Into<Vec<String>>) -> Self {
    Self {
      current_frame: Default::default(),
      stack_frames: Default::default(),
      gc,
      program: Default::default(),
      envs: Default::default(),
      args: args.into(),
      filemap: Default::default(),
      lib_cache: Default::default(),
      opened_files: Default::default(),
      opened_native_libs: Default::default(),
      next_gc: Instant::now() + DEFAULT_GC_FREQUENCY,
    }
  }

  pub fn run_file(&mut self, file: impl Into<PathBuf>, env: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    let file = file.into();
    let file_id = PlatformMetadata::id_of(&file).unwrap_or(0);

    self.filemap.add(file_id, &file);
    self.opened_files = vec![FileInfo::new(&file, file_id)];

    let source = fs::read_to_string(&file).map_err(Error::other_system_err)?;
    let ctx = code::compile_file(&mut self.program, file_id, source).map_err(|e| e.with_filename(&self.filemap))?;

    self.current_frame = StackFrame::new(ctx);
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::File(env));

    self.execute(ExecType::File)
  }

  pub fn run_string(&mut self, source: impl AsRef<str>, env: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    self.opened_files = vec![];

    let ctx = code::compile_string(&mut self.program, source)?;

    self.current_frame = StackFrame::new(ctx);
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::String(env));

    self.execute(ExecType::String)
  }

  pub fn run_fn(&mut self, ctx: SmartPtr<Context>, env: UsertypeHandle<ModuleValue>, args: Args) -> ExecResult<Value> {
    self.new_frame(ctx);
    self.set_stack(args.list);
    self.envs.push(EnvEntry::Fn(env));

    self.execute(ExecType::Fn)
  }

  pub(crate) fn exec_op<F, T>(&mut self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&mut Self) -> OpcodeResult<T>,
  {
    f(self).map_err(|e| self.error(e))
  }

  pub(crate) fn execute(&mut self, exec_type: ExecType) -> ExecResult<Value> {
    let mut export = None;

    'ctx: while let Some(opcode) = self.current_frame.ctx.next(self.current_frame.ip) {
      let now = Instant::now();
      if now > self.next_gc {
        self.run_gc()?;
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        self.stack_display();
        self
          .current_frame
          .ctx
          .display_instruction(&self.program, &opcode, self.current_frame.ip);
      }

      match opcode {
        Opcode::NoOp => self.exec_op(|this| this.exec_noop())?,
        Opcode::Const(index) => self.exec_op(|this| this.exec_const(index))?,
        Opcode::Nil => self.exec_nil(),
        Opcode::True => self.exec_true(),
        Opcode::False => self.exec_false(),
        Opcode::Pop => self.exec_pop(),
        Opcode::PopN(count) => self.exec_pop_n(count),
        Opcode::Store(storage) => match storage {
          Storage::Local(index) => self.exec_op(|this| this.exec_store_local(index))?,
          Storage::Global(index) => self.exec_op(|this| this.exec_store_global(index))?,
          Storage::Reg(reg) => {
            let value = self.exec_op(|this| this.stack_peek().ok_or(UsageError::EmptyStack))?;
            self.current_frame.reg_store(reg, value);
          }
        },
        Opcode::Load(storage) => match storage {
          Storage::Local(index) => self.exec_op(|this| this.exec_load_local(index))?,
          Storage::Global(index) => self.exec_op(|this| this.exec_load_global(index))?,
          Storage::Reg(reg) => self.stack_push(self.current_frame.reg_load(reg)),
        },
        Opcode::InitializeMember(index) => self.exec_op(|this| this.exec_initialize_member(index))?,
        Opcode::InitializeMethod(index) => self.exec_op(|this| this.exec_initialize_method(index))?,
        Opcode::InitializeConstructor => self.exec_op(|this| this.exec_initialize_constructor())?,
        Opcode::AssignMember(index) => self.exec_op(|this| this.exec_assign_member(index))?,
        Opcode::LookupMember(index) => self.exec_op(|this| this.exec_lookup_member(index))?,
        Opcode::PeekMember(index) => self.exec_op(|this| this.exec_peek_member(index))?,
        Opcode::Check => self.exec_op(|this| this.exec_check())?,
        Opcode::Println => self.exec_op(|this| this.exec_print())?,
        Opcode::Jump(count) => {
          self.jump(count as usize);
          continue 'ctx;
        }
        Opcode::JumpIfFalse(count) => {
          let val = self.exec_op(|this| this.exec_jump_if_false(count as usize))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Loop(count) => {
          self.loop_back(count as usize);
          continue 'ctx;
        }
        Opcode::Invoke(airity) => {
          self.current_frame.ip += 1;
          self.exec_op(|this| this.exec_call(airity as usize))?;
          continue 'ctx;
        }
        Opcode::Ret => {
          break 'ctx;
        }
        Opcode::RetSelf => {
          let this = self.exec_op(|this| this.exec_retself())?;
          export = Some(this);
          break 'ctx;
        }
        Opcode::Req => {
          self.current_frame.ip += 1;
          self.exec_req()?;
          continue 'ctx;
        }
        Opcode::CreateVec(num_items) => self.exec_create_vec(num_items as usize),
        Opcode::CreateSizedVec(repeat) => self.exec_op(|this| this.exec_sized_vec(repeat as usize))?,
        Opcode::CreateDynamicVec => self.exec_op(|this| this.exec_dyn_vec())?,
        Opcode::CreateClosure => self.exec_op(|this| this.exec_create_closure())?,
        Opcode::CreateStruct(size) => self.exec_op(|this| this.exec_create_struct(size as usize))?,
        Opcode::CreateClass(name) => self.exec_op(|this| this.exec_create_class(name))?,
        Opcode::CreateModule(name) => self.exec_op(|this| this.exec_create_module(name))?,
        Opcode::Breakpoint => {
          self.ssdb()?;
        }
        Opcode::Export => {
          let value = self.exec_op(|this| this.stack_pop().ok_or(UsageError::EmptyStack))?;
          export = Some(value);
        }
        Opcode::Define(ident) => self.exec_op(|this| this.exec_define_global(ident))?,
        Opcode::Resolve(ident) => self.exec_op(|this| this.exec_scope_resolution(ident))?,
        Opcode::EnterBlock => self.push_scope(),
        Opcode::PopScope => self.pop_scope(),
        Opcode::Equal => self.exec_op(|this| this.exec_equal(&opcode))?,
        Opcode::NotEqual => self.exec_op(|this| this.exec_not_equal(&opcode))?,
        Opcode::Greater => self.exec_op(|this| this.exec_greater(&opcode))?,
        Opcode::GreaterEqual => self.exec_op(|this| this.exec_greater_equal(&opcode))?,
        Opcode::Less => self.exec_op(|this| this.exec_less(&opcode))?,
        Opcode::LessEqual => self.exec_op(|this| this.exec_less_equal(&opcode))?,
        Opcode::Add => self.exec_op(|this| this.exec_add(&opcode))?,
        Opcode::Sub => self.exec_op(|this| this.exec_sub(&opcode))?,
        Opcode::Mul => self.exec_op(|this| this.exec_mul(&opcode))?,
        Opcode::Div => self.exec_op(|this| this.exec_div(&opcode))?,
        Opcode::Rem => self.exec_op(|this| this.exec_rem(&opcode))?,
        Opcode::Or(count) => {
          let val = self.exec_op(|this| this.exec_or(count as usize))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::And(count) => {
          let val = self.exec_op(|this| this.exec_and(count as usize))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Not => self.exec_op(|this| this.exec_not(&opcode))?,
        Opcode::Negate => self.exec_op(|this| this.exec_negate(&opcode))?,
        Opcode::PushRegCtx => self.current_frame.new_reg_ctx(),
        Opcode::PopRegCtx => self.current_frame.pop_reg_ctx(),
      }

      self.current_frame.ip += 1;
    }

    match exec_type {
      ExecType::File => {
        let info = self.opened_files.pop().expect("file must be popped when leaving a file");
        if let Some(export) = &export {
          self.lib_cache.insert(info.id, export.clone());
        }

        // pop until a file env is found
        while !matches!(self.envs.pop(), EnvEntry::File(_)) {}
      }
      ExecType::Fn => {
        // pop until a fn env is found
        while !matches!(self.envs.pop(), EnvEntry::Fn(_)) {}
      }
      ExecType::String => while !matches!(self.envs.pop(), EnvEntry::String(_)) {},
    }

    if let Some(stack_frame) = self.stack_frames.pop() {
      // the vm is returning from a function call or req
      self.current_frame = stack_frame;
    }

    Ok(export.unwrap_or_default())
  }

  /* Operations */

  #[cold]
  fn exec_noop(&self) -> OpcodeResult {
    Err(UsageError::Infallible)
  }

  fn exec_const(&mut self, index: BitsRepr) -> OpcodeResult {
    let c = self.program.const_at(index).ok_or(UsageError::InvalidConst(index))?;

    let env = self.current_env().into();
    let value = Value::from_constant(&mut self.gc, env, c);
    self.stack_push(value);
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

  fn exec_pop_n(&mut self, count: BitsRepr) {
    self.stack_pop_n(count);
  }

  fn exec_load_local(&mut self, loc: BitsRepr) -> OpcodeResult {
    let local = self.stack_index(loc as usize).ok_or(UsageError::InvalidStackIndex(loc))?;
    self.stack_push(local);
    Ok(())
  }

  fn exec_store_local(&mut self, loc: BitsRepr) -> OpcodeResult {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_assign(loc as usize, value);
    Ok(())
  }

  fn exec_load_global(&mut self, loc: BitsRepr) -> OpcodeResult {
    self.global_op(loc, |this, name| {
      let global = this.current_env().lookup(&name).ok_or(UsageError::UndefinedVar(name))?;
      this.stack_push(global);
      Ok(())
    })
  }

  fn exec_store_global(&mut self, loc: BitsRepr) -> OpcodeResult {
    self.global_op(loc, |this, name| {
      let value = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().assign(&name, value) {
        Ok(())
      } else {
        Err(UsageError::UndefinedVar(name))
      }
    })
  }

  fn exec_define_global(&mut self, loc: BitsRepr) -> OpcodeResult {
    self.global_op(loc, |this, name| {
      let v = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().define(name.clone(), v) {
        Ok(())
      } else {
        let level = this.current_env().search_for(0, &name);
        Err(UsageError::Redefine { level, name })
      }
    })
  }

  fn exec_initialize_member(&mut self, loc: BitsRepr) -> OpcodeResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      obj.set_member(&mut self.gc, Field::new(loc, name), value)?;
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_initialize_method(&mut self, loc: BitsRepr) -> OpcodeResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_peek().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    let class = obj.as_class_mut().ok_or(UsageError::MethodAssignment)?;

    let f = value.as_fn().ok_or(UsageError::MethodType)?;

    if let ConstantValue::String(name) = name {
      class.set_method(name, f.clone());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_initialize_constructor(&mut self) -> OpcodeResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    let class = obj.as_class_mut().ok_or(UsageError::MethodAssignment)?;
    class.set_constructor(value);
    Ok(())
  }

  fn exec_assign_member(&mut self, loc: BitsRepr) -> OpcodeResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      obj.set_member(&mut self.gc, Field::new(loc, name), value.clone())?;
      self.stack_push(value);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_lookup_member(&mut self, loc: BitsRepr) -> OpcodeResult {
    let obj = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let value = obj.get_member(&mut self.gc, Field::new(loc, name))?.unwrap_or_default();
      self.stack_push(value);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_peek_member(&mut self, loc: BitsRepr) -> OpcodeResult {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let member = value.get_member(&mut self.gc, Field::new(loc, name))?.unwrap_or_default();
      self.stack_push(member);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: &Opcode, f: F) -> OpcodeResult {
    self.binary_op(opcode, |a, b| Ok(Value::from(f(a, b))))
  }

  fn exec_equal(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a == b)
  }

  fn exec_not_equal(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a != b)
  }

  fn exec_greater(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a > b)
  }

  fn exec_greater_equal(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a >= b)
  }

  fn exec_less(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a < b)
  }

  fn exec_less_equal(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a <= b)
  }

  fn exec_check(&mut self) -> OpcodeResult {
    let b = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let a = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_push(Value::from(a == b));
    Ok(())
  }

  fn exec_add(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a + b)
  }

  fn exec_sub(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a - b)
  }

  fn exec_mul(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a * b)
  }

  fn exec_div(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a / b)
  }

  fn exec_rem(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a % b)
  }

  /// when f evaluates to true, short circuit
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) -> OpcodeResult<bool> {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    if f(value) {
      self.jump(offset);
      Ok(true)
    } else {
      self.stack_pop();
      Ok(false)
    }
  }

  fn exec_or(&mut self, offset: usize) -> OpcodeResult<bool> {
    self.exec_logical(offset, |v| v.truthy())
  }

  fn exec_and(&mut self, offset: usize) -> OpcodeResult<bool> {
    self.exec_logical(offset, |v| v.falsy())
  }

  fn exec_not(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.unary_op(opcode, |v| Ok(!v))
  }

  fn exec_negate(&mut self, opcode: &Opcode) -> OpcodeResult {
    self.unary_op(opcode, |v| -v)
  }

  fn exec_print(&mut self) -> OpcodeResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    println!("{value}");
    Ok(())
  }

  fn exec_jump_if_false(&mut self, offset: usize) -> OpcodeResult<bool> {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    if !value.truthy() {
      self.jump(offset);
      Ok(true)
    } else {
      Ok(false)
    }
  }

  fn exec_call(&mut self, airity: usize) -> OpcodeResult {
    let args = self.stack_drain_from(airity);
    let callable = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    self.call_value(callable, args)
  }

  fn exec_retself(&mut self) -> OpcodeResult<Value> {
    self.stack_index_0().ok_or(UsageError::EmptyStack)
  }

  fn exec_create_vec(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    let list = self.gc.allocate(list);
    self.stack_push(list);
  }

  fn exec_sized_vec(&mut self, repeats: usize) -> OpcodeResult {
    let item = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let vec = vec![item; repeats];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
  }

  fn exec_dyn_vec(&mut self) -> OpcodeResult {
    let repeats = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let repeats = repeats.as_i32().ok_or(UsageError::CoercionError(repeats, "i32"))? as usize;
    self.exec_sized_vec(repeats)
  }

  fn exec_create_closure(&mut self) -> OpcodeResult {
    let function = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let captures = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let f = function.as_fn().ok_or(UsageError::ClosureType)?;
    let c = captures.as_vec().ok_or(UsageError::CaptureType)?;

    let closure = self.gc.allocate(ClosureValue::new(c, f.clone()));
    self.stack_push(closure);
    Ok(())
  }

  fn exec_create_struct(&mut self, size: usize) -> OpcodeResult {
    let mut members = Vec::with_capacity(size);

    for _ in 0..size {
      let key = self.stack_pop().ok_or(UsageError::EmptyStack)?;
      let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
      if let Some(key) = key.as_str() {
        let id = self
          .program
          .strings
          .get_by_right(&**key)
          .cloned()
          .ok_or(UsageError::InvalidIdentifier(key.to_string()))?;
        members.push((((**key).clone(), id), value));
      } else {
        Err(UsageError::InvalidIdentifier(key.to_string()))?;
      }
    }

    let struct_value = self.gc.allocate(StructValue::new(members));

    self.stack_push(struct_value);

    Ok(())
  }

  fn exec_create_class(&mut self, loc: BitsRepr) -> OpcodeResult {
    let creator = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let v = self.gc.allocate(ClassValue::new(name, creator));
      self.stack_push(v);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  /// Create a module and make it the current env
  fn exec_create_module(&mut self, loc: BitsRepr) -> OpcodeResult {
    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let leaf = self.current_env();
      let module = ModuleValue::new_child(name, leaf.handle.value.clone());
      let uhandle = Gc::allocate_handle(&mut self.gc, module);
      self.envs.push(EnvEntry::Mod(uhandle.clone()));
      self.stack_push(uhandle.handle.value.clone());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_scope_resolution(&mut self, ident: BitsRepr) -> OpcodeResult {
    let obj = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(ident).ok_or(UsageError::InvalidConst(ident))?;

    if let ConstantValue::String(name) = name {
      let value = obj.resolve(name)?;
      self.stack_push(value);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn push_scope(&mut self) {
    let mut gc = self.gc.clone();
    let leaf = self.current_env();
    let handle = Gc::allocate_handle(&mut gc, ModuleValue::new_scope(leaf.handle.value.clone()));
    self.envs.push(EnvEntry::Block(handle));
  }

  fn pop_scope(&mut self) {
    self.envs.pop();
  }

  fn exec_req(&mut self) -> ExecResult {
    fn try_to_find_file(root: &Path, desired: &PathBuf, attempts: &mut Vec<PathBuf>) -> Option<PathBuf> {
      let direct = root.join(desired);
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

    let value = self.stack_pop().ok_or_else(|| self.error(UsageError::EmptyStack))?;

    let mut attempts = Vec::with_capacity(10);

    let file_str = value.to_string();
    let this_file = self.opened_files.last().map(|f| f.path.clone());

    let required_file = PathBuf::from(file_str.as_str());

    let mut found_file = None;

    // find relative first, skip if None, None will be during repl so go to cwd
    if let Some(this_dir) = this_file.and_then(|this_file| this_file.parent().map(|p| p.to_path_buf())) {
      found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
    }

    // then try to find from cwd
    if found_file.is_none() {
      let this_dir = std::env::current_dir().map_err(Error::other_system_err)?;
      found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
    }

    // if still not found, try searching library paths
    if found_file.is_none() {
      if let Some(library_mod) = self.current_env().lookup(module_value::LIB_GLOBAL) {
        if let Some(library_mod) = library_mod.as_struct() {
          if let Ok(Some(list)) = library_mod
            .get_field(&mut self.gc, Field::named(module_value::PATHS_MEMBER))
            .map(|l| l.map(|l| l.as_vec()))
          {
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

    let found_file = found_file.ok_or_else(|| self.error(UsageError::BadReq(attempts)))?;

    let id = PlatformMetadata::id_of(&found_file).map_err(Error::other_system_err)?;

    match found_file.extension().and_then(|s| s.to_str()) {
      Some(dlopen2::utils::PLATFORM_FILE_EXTENSION) => {
        let value = if let Some(value) = self.lib_cache.get(&id) {
          value.clone()
        } else {
          let lib: Container<NativeApi> =
            unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

          let value = lib.simple_script_load_module(self)?;
          self.opened_native_libs.insert(found_file, lib);
          self.lib_cache.insert(id, value.clone());
          value
        };

        self.stack_push(value);

        Ok(())
      }
      _ => {
        let data = fs::read_to_string(&found_file).map_err(Error::other_system_err)?;

        if let Some(value) = self.lib_cache.get(&id) {
          self.stack_push(value.clone());
        } else {
          self.filemap.add(id, &found_file);
          let new_ctx = code::compile(&mut self.program, Some(id), data)?;
          let gmod = ModuleBuilder::initialize(&mut self.gc, id, None, |gc, mut lib| {
            lib.env = stdlib::enable_std(gc, lib.handle.value.clone(), &self.args);
          });

          self.new_frame(new_ctx);
          self.envs.push(EnvEntry::File(gmod));
          self.opened_files.push(FileInfo::new(found_file, id));
          let output = self.execute(ExecType::File)?;
          self.stack_push(output);
        }

        Ok(())
      }
    }
  }

  /* Utility Functions */

  fn unary_op<F>(&mut self, opcode: &Opcode, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value) -> Result<Value, UsageError>,
  {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    if value.is_ptr() {
      let key = match opcode {
        Opcode::Negate => ops::NEG,
        Opcode::Not => ops::NOT,
        _ => Err(UsageError::InvalidUnary)?,
      };

      let callable = value
        .get_member(&mut self.gc, Field::named(key))?
        .map(Ok)
        .unwrap_or(Err(UsageError::UnexpectedNil))?;
      self.call_value(callable, [])
    } else {
      self.stack_push(f(value)?);
      Ok(())
    }
  }

  fn binary_op<F>(&mut self, opcode: &Opcode, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    let bv = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let av = self.stack_pop().ok_or(UsageError::EmptyStack)?;

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
        _ => Err(UsageError::InvalidBinary)?,
      };

      let callable = av
        .get_member(&mut self.gc, Field::named(key))?
        .map(Ok)
        .unwrap_or(Err(UsageError::UnexpectedNil))?;

      self.call_value(callable, [bv])
    } else {
      self.stack_push(f(av, bv)?);
      Ok(())
    }
  }

  fn global_op<F>(&mut self, index: BitsRepr, f: F) -> OpcodeResult
  where
    F: FnOnce(&mut Self, String) -> OpcodeResult,
  {
    match self.program.const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      None => Err(UsageError::InvalidConst(index))?,
    }
  }

  pub fn ssdb(&mut self) -> Result<(), Error> {
    let mut rl = DefaultEditor::new().map_err(Error::other_system_err)?;
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
        Err(e) => Err(Error::other_system_err(e))?,
      }
    }
  }
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

  pub fn stack_pop_n(&mut self, count: BitsRepr) {
    self
      .current_frame
      .stack
      .truncate(self.current_frame.stack.len().saturating_sub(count as usize));
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

  fn call_value(&mut self, mut callable: Value, args: impl Into<Vec<Value>>) -> Result<(), UsageError> {
    let value = callable.call(self, Args::new(args))?;
    self.stack_push(value);

    Ok(())
  }

  pub fn ctx(&mut self) -> &Context {
    &self.current_frame.ctx
  }

  pub fn ctx_mut(&mut self) -> &mut Context {
    &mut self.current_frame.ctx
  }

  pub fn current_env(&self) -> &UsertypeHandle<ModuleValue> {
    self.envs.last()
  }

  pub fn current_env_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    self.envs.last_mut()
  }

  pub fn run_gc(&mut self) -> ExecResult {
    let now = Instant::now();
    self.next_gc = now + DEFAULT_GC_FREQUENCY;
    self
      .gc
      .clean(&self.current_frame, &self.stack_frames, self.lib_cache.values())?;
    Ok(())
  }

  #[cold]
  fn error(&self, e: UsageError) -> Error {
    let opcode = self.current_frame.ctx.instructions[self.current_frame.ip].clone();
    Error::runtime_error(self.error_at(opcode, |opcode_ref| RuntimeError::new(e, &self.filemap, opcode_ref)))
  }

  #[cold]
  fn error_at<F>(&self, opcode: Opcode, f: F) -> RuntimeError
  where
    F: FnOnce(OpcodeReflection) -> RuntimeError,
  {
    self
      .current_frame
      .ctx
      .meta
      .reflect(opcode, self.current_frame.ip)
      .map(f)
      .unwrap_or_else(|| RuntimeError {
        msg: UsageError::IpOutOfBounds(self.current_frame.ip).to_string(),
        file: self.filemap.get(self.current_frame.ctx.meta.file_id),
        line: 0,
        column: 0,
        nested: false,
      })
  }

  pub fn stack_display(&self) {
    print!("{}", self.current_frame);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumCount, EnumIter, Enum)]
pub enum Register {
  A,
  B,
  C,
  D,
  E,
  F,
  G,
  H,
}

#[derive(Default)]
pub struct StackFrame {
  pub ip: usize,
  pub ctx: SmartPtr<Context>,
  pub stack: Vec<Value>,
  pub registers: Vec<EnumMap<Register, Value>>,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ip: Default::default(),
      ctx,
      stack: Default::default(),
      registers: Default::default(),
    }
  }

  /**
   * Clear the current stack frame, returning the previous
   */
  pub fn clear_out(&mut self) -> Self {
    let mut old = Self::default();
    std::mem::swap(&mut old, self);
    old
  }

  pub fn reg_store(&mut self, reg: Register, value: Value) {
    let sz = self.registers.len();
    self.registers[sz - 1][reg] = value;
  }

  pub fn reg_load(&self, reg: Register) -> Value {
    self.registers[self.registers.len() - 1][reg].clone()
  }

  pub fn new_reg_ctx(&mut self) {
    self.registers.push(Default::default());
  }

  pub fn pop_reg_ctx(&mut self) {
    self.registers.pop();
  }
}

impl Display for StackFrame {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    if self.stack.is_empty() {
      writeln!(f, "               | [ ]")
    } else {
      for (index, item) in self.stack.iter().enumerate() {
        writeln!(f, "{:#15}| [ {:?} ]", index, item)?;
      }
      Ok(())
    }
  }
}

pub(crate) struct FileInfo {
  path: PathBuf,
  id: FileIdType,
}

impl FileInfo {
  fn new(path: impl Into<PathBuf>, id: FileIdType) -> Self {
    Self { path: path.into(), id }
  }
}

#[derive(Default)]
pub(crate) struct EnvStack {
  envs: Vec<EnvEntry>,
}

impl EnvStack {
  pub(crate) fn len(&self) -> usize {
    self.envs.len()
  }

  pub(crate) fn push(&mut self, entry: EnvEntry) {
    self.envs.push(entry);
  }

  fn pop(&mut self) -> EnvEntry {
    self.envs.pop().expect("pop: the env stack should never be empty")
  }

  fn last(&self) -> &UsertypeHandle<ModuleValue> {
    match self.envs.last().expect("last: the env stack should never be empty") {
      EnvEntry::Fn(e) => e,
      EnvEntry::Mod(e) => e,
      EnvEntry::File(e) => e,
      EnvEntry::Block(e) => e,
      EnvEntry::String(e) => e,
    }
  }

  fn last_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    match self.envs.last_mut().expect("last_mut: the env stack should never be empty") {
      EnvEntry::Fn(e) => e,
      EnvEntry::Mod(e) => e,
      EnvEntry::File(e) => e,
      EnvEntry::Block(e) => e,
      EnvEntry::String(e) => e,
    }
  }
}

pub(crate) enum EnvEntry {
  Fn(UsertypeHandle<ModuleValue>),
  Mod(UsertypeHandle<ModuleValue>),
  File(UsertypeHandle<ModuleValue>),
  Block(UsertypeHandle<ModuleValue>),
  String(UsertypeHandle<ModuleValue>),
}

impl Debug for EnvEntry {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fn(_) => f.debug_tuple("Fn").finish(),
      Self::Mod(_) => f.debug_tuple("Mod").finish(),
      Self::File(_) => f.debug_tuple("File").finish(),
      Self::Block(_) => f.debug_tuple("Block").finish(),
      Self::String(_) => f.debug_tuple("String").finish(),
    }
  }
}

pub(crate) enum ExecType {
  String,
  File,
  Fn,
}
