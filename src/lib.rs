pub(crate) mod code;
pub(crate) mod dbg;
pub mod error;
pub(crate) mod exec;
pub mod stdlib;
mod util;
pub(crate) mod value;

pub mod prelude {
  pub use super::error::*;
  pub use super::exec::prelude::*;
  pub use super::stdlib;
  pub use super::value::prelude::*;
  pub use super::Vm;
  pub use macros::*;
  pub use ptr::SmartPtr;
}

pub mod macro_requirements {
  pub use crate::prelude::{
    methods, native, Args, DebugValue, DisplayValue, Fields, MaybeFrom, ModuleBuilder, TryUnwrapArg, UsageError, UsageResult,
    Usertype, UsertypeFields, UsertypeMethods, Value, Vm,
  };
  pub use uuid;
}

use crate::{
  code::{ConstantValue, FileMap, InstructionReflection},
  dbg::Cli,
  exec::*,
  prelude::*,
  util::{FileIdType, FileMetadata, PlatformMetadata, UnwrapAnd},
};
use clap::Parser;
use dlopen2::wrapper::Container;
use exec::memory::{Allocation, Gc};
use ptr::SmartPtr;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::BTreeMap,
  fs, mem,
  path::{Path, PathBuf},
  time::{Duration, Instant},
};

pub const EXTENSION: &str = "dk";

#[cfg(test)]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_nanos(100);

#[cfg(not(test))]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_millis(16);

const INITIAL_STACK_SIZE: usize = 400;

type ExecResult<T = ()> = Result<T, Error>;

type OpcodeResult<T = ()> = Result<T, UsageError>;

pub(crate) enum RunMode {
  String,
  File,
  Fn,
}

#[cfg(debug_assertions)]
macro_rules! data {
  ($inst:ident) => {
    $inst.data().ok_or(UsageError::InvalidInstruction($inst))?
  };
}

#[cfg(not(debug_assertions))]
macro_rules! data {
  ($inst:ident) => {
    $inst.data()
  };
}

pub struct Vm {
  pub(crate) stack: Stack,
  pub(crate) stack_frame: StackFrame,
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
      stack_frame: Default::default(),
      stack_frames: Default::default(),
      stack: Stack::with_capacity(INITIAL_STACK_SIZE),
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

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::File(env));

    self.execute(RunMode::File)
  }

  pub fn run_string(&mut self, source: impl AsRef<str>, env: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    self.opened_files = vec![];

    let ctx = code::compile_string(&mut self.program, source)?;

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::String(env));

    self.execute(RunMode::String)
  }

  pub fn run_fn(&mut self, ctx: SmartPtr<Context>, env: UsertypeHandle<ModuleValue>, airity: usize) -> ExecResult<Value> {
    self.new_frame(ctx, airity);
    self.envs.push(EnvEntry::Fn(env));

    self.execute(RunMode::Fn)
  }

  pub(crate) fn exec<F, T>(&mut self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&mut Self) -> OpcodeResult<T>,
  {
    f(self).map_err(|e| self.error(e))
  }

  pub(crate) fn execute(&mut self, exec_type: RunMode) -> ExecResult<Value> {
    let mut export = None;

    'ctx: while let Some(inst) = self.stack_frame.ctx.next(self.stack_frame.ip) {
      let now = Instant::now();
      if now > self.next_gc {
        self.run_gc()?;
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        self.stack_display();
        self
          .stack_frame
          .ctx
          .display_instruction(&self.program, inst, self.stack_frame.ip);
      }

      let opcode = inst
        .opcode()
        .ok_or_else(|| self.error(UsageError::InvalidInstruction(inst)))?;

      match opcode {
        Opcode::Unknown => Err(self.error(UsageError::InvalidInstruction(inst)))?,
        Opcode::NoOp => self.exec(|this| this.exec_noop())?,
        Opcode::Const => self.exec(|this| this.exec_const(data!(inst)))?,
        Opcode::Nil => self.exec_nil(),
        Opcode::True => self.exec_true(),
        Opcode::False => self.exec_false(),
        Opcode::Pop => self.exec_pop(),
        Opcode::PopN => self.exec(|this| Ok(this.exec_pop_n(data!(inst))))?,
        Opcode::Store => {
          let (storage, addr) = self.exec(|_| Ok(data!(inst)))?;
          match storage {
            Storage::Local => self.exec(|this| this.exec_store_local(addr))?,
            Storage::Global => self.exec(|this| this.exec_store_global(addr))?,
            Storage::Reg => {
              let (_, reg): (Storage, Register) = self.exec(|_| Ok(data!(inst)))?;
              let value = self.exec(|this| this.stack_peek().ok_or(UsageError::EmptyStack))?;
              self.stack_frame.reg_store(reg, value);
            }
          }
        }
        Opcode::Load => {
          let (storage, index): (Storage, LongAddr) = self.exec(|_| Ok(data!(inst)))?;
          match storage {
            Storage::Local => self.exec(|this| this.exec_load_local(index))?,
            Storage::Global => self.exec(|this| this.exec_load_global(index))?,
            Storage::Reg => {
              let (_, reg): (Storage, Register) = self.exec(|_| Ok(data!(inst)))?;
              self.stack_push(self.stack_frame.reg_load(reg))
            }
          }
        }
        Opcode::InitializeMember => self.exec(|this| this.exec_initialize_member(data!(inst)))?,
        Opcode::InitializeMethod => self.exec(|this| this.exec_initialize_method(data!(inst)))?,
        Opcode::InitializeConstructor => self.exec(|this| this.exec_initialize_constructor())?,
        Opcode::AssignMember => self.exec(|this| this.exec_assign_member(data!(inst)))?,
        Opcode::LookupMember => self.exec(|this| this.exec_lookup_member(data!(inst)))?,
        Opcode::PeekMember => self.exec(|this| this.exec_peek_member(data!(inst)))?,
        Opcode::Check => self.exec(|this| this.exec_check())?,
        Opcode::Println => self.exec(|this| this.exec_println())?,
        Opcode::Jump => {
          self.exec(|this| Ok(this.jump(data!(inst))))?;
          continue 'ctx;
        }
        Opcode::JumpIfFalse => {
          let val = self.exec(|this| this.exec_jump_if_false(data!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Loop => {
          self.exec(|this| Ok(this.loop_back(data!(inst))))?;
          continue 'ctx;
        }
        Opcode::Invoke => {
          self.stack_frame.ip += 1;
          self.exec(|this| this.exec_call(data!(inst)))?;
          continue 'ctx;
        }
        Opcode::Ret => {
          break 'ctx;
        }
        Opcode::Req => {
          self.stack_frame.ip += 1;
          self.exec_req()?;
          continue 'ctx;
        }
        Opcode::CreateVec => self.exec(|this| Ok(this.exec_create_vec(data!(inst))))?,
        Opcode::CreateSizedVec => self.exec(|this| this.exec_sized_vec(data!(inst)))?,
        Opcode::CreateDynamicVec => self.exec(|this| this.exec_dyn_vec())?,
        Opcode::CreateClosure => self.exec(|this| this.exec_create_closure())?,
        Opcode::CreateStruct => self.exec(|this| this.exec_create_struct(data!(inst)))?,
        Opcode::CreateClass => self.exec(|this| this.exec_create_class(data!(inst)))?,
        Opcode::CreateModule => self.exec(|this| this.exec_create_module(data!(inst)))?,
        Opcode::Breakpoint => {
          self.dbg()?;
        }
        Opcode::Export => {
          let value = self.exec(|this| this.stack_pop().ok_or(UsageError::EmptyStack))?;
          export = Some(value);
        }
        Opcode::Define => self.exec(|this| this.exec_define_global(data!(inst)))?,
        Opcode::Resolve => self.exec(|this| this.exec_scope_resolution(data!(inst)))?,
        Opcode::EnterBlock => self.push_scope(),
        Opcode::PopScope => self.pop_scope(),
        Opcode::Equal => self.exec(|this| this.exec_equal(opcode))?,
        Opcode::NotEqual => self.exec(|this| this.exec_not_equal(opcode))?,
        Opcode::Greater => self.exec(|this| this.exec_greater(opcode))?,
        Opcode::GreaterEqual => self.exec(|this| this.exec_greater_equal(opcode))?,
        Opcode::Less => self.exec(|this| this.exec_less(opcode))?,
        Opcode::LessEqual => self.exec(|this| this.exec_less_equal(opcode))?,
        Opcode::Add => self.exec(|this| this.exec_add(opcode))?,
        Opcode::Sub => self.exec(|this| this.exec_sub(opcode))?,
        Opcode::Mul => self.exec(|this| this.exec_mul(opcode))?,
        Opcode::Div => self.exec(|this| this.exec_div(opcode))?,
        Opcode::Rem => self.exec(|this| this.exec_rem(opcode))?,
        Opcode::Or => {
          let val = self.exec(|this| this.exec_or(data!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::And => {
          let val = self.exec(|this| this.exec_and(data!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Not => self.exec(|this| this.exec_not(opcode))?,
        Opcode::Negate => self.exec(|this| this.exec_negate(opcode))?,
        Opcode::PushRegCtx => self.stack_frame.new_reg_ctx(),
        Opcode::PopRegCtx => self.stack_frame.pop_reg_ctx(),
        Opcode::SwapPop => {
          let idx = self.stack_size() - 2;
          self.stack.swap_remove(idx);
        }
        Opcode::Quack => {
          let value = self.exec(|this| this.stack_pop().ok_or(UsageError::EmptyStack))?;
          panic!("{value}");
        }
      }

      self.stack_frame.ip += 1;
    }

    match exec_type {
      RunMode::File => {
        let info = self.opened_files.pop().expect("file must be popped when leaving a file");
        if let Some(export) = &export {
          self.lib_cache.insert(info.id, export.clone());
        }

        // pop until a file env is found
        while !matches!(self.envs.pop(), EnvEntry::File(_)) {}
      }
      RunMode::Fn => {
        // pop until a fn env is found
        while !matches!(self.envs.pop(), EnvEntry::Fn(_)) {}
      }
      RunMode::String => while !matches!(self.envs.pop(), EnvEntry::String(_)) {},
    }

    if let Some(stack_frame) = self.stack_frames.pop() {
      // the vm is returning from a function call or req
      self.stack_frame = stack_frame;
    }

    Ok(export.unwrap_or_default())
  }

  /* Operations */

  #[cold]
  fn exec_noop(&self) -> OpcodeResult {
    Err(UsageError::Infallible)
  }

  fn exec_const(&mut self, index: LongAddr) -> OpcodeResult {
    let c = self.program.const_at(index).ok_or(UsageError::InvalidConst(index.into()))?;

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

  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  fn exec_load_local(&mut self, loc: LongAddr) -> OpcodeResult {
    let local = self
      .stack_index(self.stack_frame.sp + loc.0)
      .ok_or(UsageError::InvalidStackIndex(loc.0))?;
    self.stack_push(local);
    Ok(())
  }

  fn exec_store_local(&mut self, loc: LongAddr) -> OpcodeResult {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_assign(self.stack_frame.sp + loc.0, value);
    Ok(())
  }

  fn exec_load_global(&mut self, loc: LongAddr) -> OpcodeResult {
    self.global_op(loc.into(), |this, name| {
      let global = this.current_env().lookup(&name).ok_or(UsageError::UndefinedVar(name))?;
      this.stack_push(global);
      Ok(())
    })
  }

  fn exec_store_global(&mut self, loc: LongAddr) -> OpcodeResult {
    self.global_op(loc.into(), |this, name| {
      let value = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().assign(&name, value) {
        Ok(())
      } else {
        Err(UsageError::UndefinedVar(name))
      }
    })
  }

  fn exec_define_global(&mut self, loc: LongAddr) -> OpcodeResult {
    self.global_op(loc.into(), |this, name| {
      let v = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().define(name.clone(), v) {
        Ok(())
      } else {
        let level = this.current_env().search_for(0, &name);
        Err(UsageError::Redefine { level, name })
      }
    })
  }

  fn exec_initialize_member(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_initialize_method(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_assign_member(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_lookup_member(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_peek_member(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: Opcode, f: F) -> OpcodeResult {
    self.binary_op(opcode, |a, b| Ok(Value::from(f(a, b))))
  }

  fn exec_equal(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a == b)
  }

  fn exec_not_equal(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a != b)
  }

  fn exec_greater(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a > b)
  }

  fn exec_greater_equal(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a >= b)
  }

  fn exec_less(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a < b)
  }

  fn exec_less_equal(&mut self, opcode: Opcode) -> OpcodeResult {
    self.exec_bool(opcode, |a, b| a <= b)
  }

  fn exec_check(&mut self) -> OpcodeResult {
    let b = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let a = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_push(Value::from(a == b));
    Ok(())
  }

  fn exec_add(&mut self, opcode: Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a + b)
  }

  fn exec_sub(&mut self, opcode: Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a - b)
  }

  fn exec_mul(&mut self, opcode: Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a * b)
  }

  fn exec_div(&mut self, opcode: Opcode) -> OpcodeResult {
    self.binary_op(opcode, |a, b| a / b)
  }

  fn exec_rem(&mut self, opcode: Opcode) -> OpcodeResult {
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

  fn exec_not(&mut self, opcode: Opcode) -> OpcodeResult {
    self.unary_op(opcode, |v| Ok(!v))
  }

  fn exec_negate(&mut self, opcode: Opcode) -> OpcodeResult {
    self.unary_op(opcode, |v| -v)
  }

  fn exec_println(&mut self) -> OpcodeResult {
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
    let callable = self.stack_index_rev(airity).ok_or(UsageError::EmptyStack)?;
    self.call_value(callable, airity)
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
    let repeats = repeats.as_i32().ok_or(UsageError::CoercionError(repeats, "i32"))?;
    let item = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let vec = vec![item; repeats as usize];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
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

  fn exec_create_class(&mut self, loc: usize) -> OpcodeResult {
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
  fn exec_create_module(&mut self, loc: usize) -> OpcodeResult {
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

  fn exec_scope_resolution(&mut self, ident: usize) -> OpcodeResult {
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

      let direct_with_script_extension = direct.with_extension(EXTENSION);
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

          self.new_frame(new_ctx, 0);
          self.envs.push(EnvEntry::File(gmod));
          self.opened_files.push(FileInfo::new(found_file, id));
          let output = self.execute(RunMode::File)?;
          self.stack_push(output);
        }

        Ok(())
      }
    }
  }

  /* Utility Functions */

  fn unary_op<F>(&mut self, opcode: Opcode, f: F) -> Result<(), UsageError>
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
      self.call_value(callable, 0)
    } else {
      self.stack_push(f(value)?);
      Ok(())
    }
  }

  fn binary_op<F>(&mut self, opcode: Opcode, f: F) -> Result<(), UsageError>
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

      self.stack_push(bv);
      self.call_value(callable, 1)
    } else {
      self.stack_push(f(av, bv)?);
      Ok(())
    }
  }

  fn global_op<F>(&mut self, index: usize, f: F) -> OpcodeResult
  where
    F: FnOnce(&mut Self, String) -> OpcodeResult,
  {
    match self.program.const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      None => Err(UsageError::InvalidConst(index))?,
    }
  }

  pub fn dbg(&mut self) -> Result<(), Error> {
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
  pub fn new_frame(&mut self, ctx: SmartPtr<Context>, offset: usize) {
    let mut frame = StackFrame::new(ctx, self.stack_size() - offset);
    mem::swap(&mut self.stack_frame, &mut frame);
    self.stack_frames.push(frame);
  }

  pub fn stack_push(&mut self, value: Value) {
    self.stack.push(value);
  }

  pub fn stack_pop(&mut self) -> Option<Value> {
    self.stack.pop()
  }

  pub fn stack_pop_n(&mut self, count: usize) {
    let pops = self.stack.len().saturating_sub(count);
    self.stack.truncate(pops);
  }

  pub fn stack_drain_from(&mut self, index: usize) -> Vec<Value> {
    let size = self.stack_size();
    self.stack.drain(size - index..).collect()
  }

  pub fn stack_index(&self, index: usize) -> Option<Value> {
    self.stack.get(index).cloned()
  }

  pub fn stack_index_0(&self) -> Option<Value> {
    self.stack.first().cloned()
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

  pub fn stack_append(&mut self, other: Vec<Value>) {
    self.stack.extend(other);
  }

  pub fn stack_size(&self) -> usize {
    self.stack.len()
  }

  fn jump(&mut self, count: usize) {
    self.stack_frame.ip = self.stack_frame.ip.saturating_add(count);
  }

  fn loop_back(&mut self, count: usize) {
    self.stack_frame.ip = self.stack_frame.ip.saturating_sub(count);
  }

  fn call_value(&mut self, mut callable: Value, airity: usize) -> Result<(), UsageError> {
    let value = callable.call(self, airity)?;
    self.stack_push(value);

    Ok(())
  }

  pub fn ctx(&mut self) -> &Context {
    &self.stack_frame.ctx
  }

  pub fn ctx_mut(&mut self) -> &mut Context {
    &mut self.stack_frame.ctx
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
      .clean(&self.stack, &self.stack_frame, &self.stack_frames, self.lib_cache.values())?;
    Ok(())
  }

  #[cold]
  fn error(&self, e: UsageError) -> Error {
    let opcode = self.stack_frame.ctx.instructions[self.stack_frame.ip];
    Error::runtime_error(self.error_at(opcode, |opcode_ref| RuntimeError::new(e, &self.filemap, opcode_ref)))
  }

  #[cold]
  fn error_at<F>(&self, inst: Instruction, f: F) -> RuntimeError
  where
    F: FnOnce(InstructionReflection) -> RuntimeError,
  {
    self
      .stack_frame
      .ctx
      .meta
      .reflect(inst, self.stack_frame.ip)
      .map(f)
      .unwrap_or_else(|| RuntimeError {
        msg: UsageError::IpOutOfBounds(self.stack_frame.ip).to_string(),
        file: self.filemap.get(self.stack_frame.ctx.meta.file_id),
        line: 0,
        column: 0,
        nested: false,
      })
  }

  pub fn stack_display(&self) {
    println!("{}", self.stack);
    println!(
      "               | ip: {ip} sp: {sp}",
      ip = self.stack_frame.ip,
      sp = self.stack_frame.sp
    );
  }
}

#[cfg(test)]
mod test;
