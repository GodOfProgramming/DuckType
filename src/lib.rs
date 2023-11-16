pub(crate) mod bindings;
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
use code::CompileOpts;
use dlopen2::wrapper::Container;
use dlopen2::wrapper::WrapperApi;
use exec::memory::{Allocation, Gc};
use prelude::module_value::ModuleType;
use ptr::SmartPtr;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::BTreeMap,
  fs, mem,
  path::{Path, PathBuf},
};

pub const EXTENSION: &str = "dk";

const INITIAL_STACK_SIZE: usize = 400;

type ExecResult<T = ()> = Result<T, Error>;

type OpResult<T = ()> = Result<T, UsageError>;

pub(crate) enum RunMode {
  String,
  File,
  Fn,
}

#[cfg(debug_assertions)]
macro_rules! decode {
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

  opt: bool,
  args: Vec<String>,

  filemap: FileMap,
  pub(crate) lib_cache: BTreeMap<FileIdType, Value>,

  // usize for what frame to pop on, string for file path
  opened_files: Vec<FileInfo>,
  opened_native_libs: BTreeMap<PathBuf, Container<NativeApi>>,
}

impl Vm {
  pub fn new(gc: SmartPtr<Gc>, opt: bool, args: impl Into<Vec<String>>) -> Self {
    Self {
      stack_frame: Default::default(),
      stack_frames: Default::default(),
      stack: Stack::with_capacity(INITIAL_STACK_SIZE),
      gc,
      program: Default::default(),
      envs: Default::default(),
      opt,
      args: args.into(),
      filemap: Default::default(),
      lib_cache: Default::default(),
      opened_files: Default::default(),
      opened_native_libs: Default::default(),
    }
  }

  pub fn run_file(&mut self, file: impl Into<PathBuf>, env: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    let file = file.into();
    let file_id = PlatformMetadata::id_of(&file).unwrap_or(0);

    self.filemap.add(file_id, &file);
    self.opened_files = vec![FileInfo::new(&file, file_id)];

    let source = fs::read_to_string(&file).map_err(Error::other_system_err)?;
    let opts = CompileOpts { optimize: self.opt };
    let ctx = code::compile_file(&mut self.program, file_id, source, opts).map_err(|e| e.with_filename(&self.filemap))?;

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::File(env));

    let res = self.run(RunMode::File);

    res
  }

  pub fn run_string(&mut self, source: impl AsRef<str>, env: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    self.opened_files = vec![];

    let opts = CompileOpts { optimize: self.opt };
    let ctx = code::compile_string(&mut self.program, source, opts)?;

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.envs.push(EnvEntry::String(env));

    self.run(RunMode::String)
  }

  pub fn run_fn(&mut self, ctx: SmartPtr<Context>, env: UsertypeHandle<ModuleValue>, airity: usize) -> ExecResult<Value> {
    self.new_frame(ctx, airity);
    self.envs.push(EnvEntry::Fn(env));

    self.run(RunMode::Fn)
  }

  fn run(&mut self, exec_type: RunMode) -> ExecResult<Value> {
    let mut export = None;

    // Execute instructions in the current stack frame's context
    //
    // The GC is checked at every instruction that does or could involve an allocation
    'ctx: while let Some(inst) = self.stack_frame.ctx.fetch(self.stack_frame.ip) {
      #[cfg(feature = "runtime-disassembly")]
      {
        self.stack_display();

        println!(
          "{}",
          InstructionDisassembler {
            ctx: &ContextDisassembler {
              ctx: &self.stack_frame.ctx,
              stack: &self.stack,
              program: &self.program,
            },
            inst,
            offset: self.stack_frame.ip,
          }
        );
      }

      let opcode = inst
        .opcode()
        .ok_or_else(|| self.error(UsageError::InvalidInstruction(inst)))?;

      match opcode {
        Opcode::Pop => self.exec_pop(),
        Opcode::PopN => self.execute(|this| Ok(this.exec_pop_n(decode!(inst))))?,
        Opcode::Const => {
          self.execute(|this| this.exec_const(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::Store => {
          let (storage, addr) = self.execute(|_| Ok(decode!(inst)))?;
          match storage {
            Storage::Stack => self.execute(|this| this.exec_store_stack(addr))?,
            Storage::Local => self.execute(|this| this.exec_store_local(addr))?,
            Storage::Global => self.execute(|this| this.exec_store_global(addr))?,
          }
        }
        Opcode::Load => {
          let (storage, addr): (Storage, LongAddr) = self.execute(|_| Ok(decode!(inst)))?;
          match storage {
            Storage::Stack => self.execute(|this| this.exec_load_stack(addr))?,
            Storage::Local => self.execute(|this| this.exec_load_local(addr))?,
            Storage::Global => self.execute(|this| this.exec_load_global(addr))?,
          }
        }
        Opcode::Nil => self.exec_nil(),
        Opcode::True => self.exec_true(),
        Opcode::False => self.exec_false(),
        Opcode::InitializeMember => {
          self.execute(|this| this.exec_initialize_member(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::AssignMember => {
          self.execute(|this| this.exec_assign_member(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::LookupMember => {
          self.execute(|this| this.exec_lookup_member(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::PeekMember => {
          self.execute(|this| this.exec_peek_member(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::InitializeConstructor => {
          self.execute(|this| this.exec_initialize_constructor())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::InitializeMethod => {
          self.execute(|this| this.exec_initialize_method(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateVec => {
          self.execute(|this| Ok(this.exec_create_vec(decode!(inst))))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateSizedVec => {
          self.execute(|this| this.exec_sized_vec(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateDynamicVec => {
          self.execute(|this| this.exec_dyn_vec())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateClosure => {
          self.execute(|this| this.exec_create_closure())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateStruct => {
          self.execute(|this| this.exec_create_struct(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateClass => {
          self.execute(|this| this.exec_create_class(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateModule => {
          self.execute(|this| this.exec_create_module(decode!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::Check => self.execute(|this| this.exec_check())?,
        Opcode::Println => self.execute(|this| this.exec_println())?,
        Opcode::Jump => {
          self.execute(|this| Ok(this.jump(decode!(inst))))?;
          continue 'ctx;
        }
        Opcode::JumpIfFalse => {
          let val = self.execute(|this| this.exec_jump_if_false(decode!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Loop => {
          self.execute(|this| Ok(this.loop_back(decode!(inst))))?;
          continue 'ctx;
        }
        Opcode::Invoke => {
          self.stack_frame.ip += 1;
          self.execute(|this| this.exec_call(decode!(inst)))?;
          continue 'ctx;
        }
        Opcode::Ret => {
          break 'ctx;
        }
        Opcode::Req => {
          self.stack_frame.ip += 1;
          self.exec_req()?;
          self.check_gc(export.as_ref())?;
          continue 'ctx;
        }
        Opcode::Breakpoint => {
          self.dbg()?;
        }
        Opcode::Export => {
          let value = self.execute(|this| this.stack_pop().ok_or(UsageError::EmptyStack))?;
          export = Some(value);
        }
        Opcode::Define => self.execute(|this| this.exec_define_global(decode!(inst)))?,
        Opcode::Resolve => self.execute(|this| this.exec_scope_resolution(decode!(inst)))?,
        Opcode::EnterBlock => {
          self.push_scope();
          self.check_gc(export.as_ref())?;
        }
        Opcode::PopScope => self.pop_scope(),
        Opcode::Equal => {
          if inst.has_data() {
            self.execute(|this| this.exec_equal_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_equal(opcode))?;
          }
        }
        Opcode::NotEqual => {
          if inst.has_data() {
            self.execute(|this| this.exec_not_equal_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_not_equal(opcode))?;
          }
        }
        Opcode::Greater => {
          if inst.has_data() {
            self.execute(|this| this.exec_greater_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_greater(opcode))?;
          }
        }
        Opcode::GreaterEqual => {
          if inst.has_data() {
            self.execute(|this| this.exec_greater_equal_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_greater_equal(opcode))?;
          }
        }
        Opcode::Less => {
          if inst.has_data() {
            self.execute(|this| this.exec_less_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_less(opcode))?;
          }
        }
        Opcode::LessEqual => {
          if inst.has_data() {
            self.execute(|this| this.exec_less_equal_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_less_equal(opcode))?;
          }
        }
        Opcode::Add => {
          if inst.has_data() {
            self.execute(|this| this.exec_add_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_add(opcode))?;
          }
        }
        Opcode::Sub => {
          if inst.has_data() {
            self.execute(|this| this.exec_sub_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_sub(opcode))?;
          }
        }
        Opcode::Mul => {
          if inst.has_data() {
            self.execute(|this| this.exec_mul_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_mul(opcode))?;
          }
        }
        Opcode::Div => {
          if inst.has_data() {
            self.execute(|this| this.exec_div_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_div(opcode))?;
          }
        }
        Opcode::Rem => {
          if inst.has_data() {
            self.execute(|this| this.exec_rem_fast(opcode, decode!(inst)))?;
          } else {
            self.execute(|this| this.exec_rem(opcode))?;
          }
        }
        Opcode::Negate => {
          self.execute(|this| this.exec_negate(opcode))?;
        }
        Opcode::Not => {
          self.execute(|this| this.exec_not(opcode))?;
        }
        Opcode::Or => {
          let val = self.execute(|this| this.exec_or(decode!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::And => {
          let val = self.execute(|this| this.exec_and(decode!(inst)))?;
          if val {
            continue 'ctx;
          }
        }
        Opcode::Swap => self.execute(|this| this.exec_swap(decode!(inst)))?,
        Opcode::SwapPop => {
          let idx = self.stack_size() - 2;
          self.stack.swap_remove(idx);
        }
        Opcode::Quack => {
          let value = self.execute(|this| this.stack_pop().ok_or(UsageError::EmptyStack))?;
          Err(self.error(UsageError::Quack(value)))?;
        }
        Opcode::Unknown => self.exec_unknown(inst)?,
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
  fn exec_unknown(&self, inst: Instruction) -> ExecResult {
    Err(self.error(UsageError::InvalidInstruction(inst)))
  }

  fn exec_const(&mut self, index: LongAddr) -> OpResult {
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

  fn exec_load_stack(&mut self, loc: LongAddr) -> OpResult {
    let value = self.stack_load_rev(loc.0).ok_or(UsageError::InvalidStackIndex(loc.0))?;
    self.stack_push(value);
    Ok(())
  }

  fn exec_load_local(&mut self, loc: LongAddr) -> OpResult {
    let local = self
      .stack_load(self.stack_frame.sp + loc.0)
      .ok_or(UsageError::InvalidStackIndex(loc.0))?;
    self.stack_push(local);
    Ok(())
  }

  fn exec_load_global(&mut self, loc: LongAddr) -> OpResult {
    self.global_op_mut(loc.into(), |this, name| {
      let global = this.current_env().lookup(&name).ok_or(UsageError::UndefinedVar(name))?;
      this.stack_push(global);
      Ok(())
    })
  }
  fn exec_store_stack(&mut self, loc: LongAddr) -> OpResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    self.stack_store_rev(loc.0, value);
    Ok(())
  }

  fn exec_store_local(&mut self, loc: LongAddr) -> OpResult {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_store(self.stack_frame.sp + loc.0, value);
    Ok(())
  }

  fn exec_define_global(&mut self, loc: LongAddr) -> OpResult {
    self.global_op_mut(loc.into(), |this, name| {
      let v = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().define(name.clone(), v) {
        Ok(())
      } else {
        let level = this.current_env().search_for(0, &name);
        Err(UsageError::Redefine { level, name })
      }
    })
  }

  fn exec_store_global(&mut self, loc: LongAddr) -> OpResult {
    self.global_op_mut(loc.into(), |this, name| {
      let value = this.stack_peek().ok_or(UsageError::EmptyStack)?;
      if this.current_env_mut().assign(&name, value) {
        Ok(())
      } else {
        Err(UsageError::UndefinedVar(name))
      }
    })
  }

  fn exec_initialize_member(&mut self, loc: usize) -> OpResult {
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

  fn exec_initialize_method(&mut self, loc: usize) -> OpResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_peek().ok_or(UsageError::EmptyStack)?;

    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment)?;

    let f = value.as_fn().ok_or(UsageError::MethodType)?;

    if let ConstantValue::String(name) = name {
      class.set_method(name, f.clone());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_initialize_constructor(&mut self) -> OpResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let mut obj = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment)?;
    class.set_constructor(value);
    Ok(())
  }

  fn exec_assign_member(&mut self, loc: usize) -> OpResult {
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

  fn exec_lookup_member(&mut self, loc: usize) -> OpResult {
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

  fn exec_peek_member(&mut self, loc: usize) -> OpResult {
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

  fn exec_swap(&mut self, (addr_a, addr_b): (ShortAddr, ShortAddr)) -> OpResult {
    let st_sz = self.stack_size() - 1;
    self.stack.swap(st_sz - addr_a.0, st_sz - addr_b.0);
    Ok(())
  }

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: Opcode, f: F) -> OpResult {
    self.binary_op(opcode, |a, b| Ok(Value::from(f(a, b))))
  }

  fn do_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> OpResult {
    self.do_binary_op(opcode, av, bv, |a, b| Ok(Value::from(f(a, b))))
  }

  fn exec_equal(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a == b)
  }

  fn exec_equal_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a == b)
  }

  fn exec_not_equal(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a != b)
  }

  fn exec_not_equal_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a != b)
  }

  fn exec_greater(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a > b)
  }

  fn exec_greater_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a > b)
  }

  fn exec_greater_equal(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a >= b)
  }

  fn exec_greater_equal_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a >= b)
  }

  fn exec_less(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a < b)
  }

  fn exec_less_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a < b)
  }

  fn exec_less_equal(&mut self, opcode: Opcode) -> OpResult {
    self.exec_bool(opcode, |a, b| a <= b)
  }

  fn exec_less_equal_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(opcode, av, bv, |a, b| a <= b)
  }

  fn exec_add(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |a, b| a + b)
  }

  fn exec_add_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(opcode, av, bv, |a, b| a + b)
  }

  fn exec_sub(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |a, b| a - b)
  }

  fn exec_sub_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(opcode, av, bv, |a, b| a - b)
  }

  fn exec_mul(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |a, b| a * b)
  }

  fn exec_mul_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(opcode, av, bv, |a, b| a * b)
  }

  fn exec_div(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |a, b| a / b)
  }

  fn exec_div_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(opcode, av, bv, |a, b| a / b)
  }

  fn exec_rem(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |a, b| a % b)
  }

  fn exec_rem_fast(
    &mut self,
    opcode: Opcode,
    (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr),
  ) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(opcode, av, bv, |a, b| a % b)
  }

  /// when f evaluates to true, short circuit
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) -> OpResult<bool> {
    let value = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    if f(value) {
      self.jump(offset);
      Ok(true)
    } else {
      self.stack_pop();
      Ok(false)
    }
  }

  fn exec_or(&mut self, offset: usize) -> OpResult<bool> {
    self.exec_logical(offset, |v| v.truthy())
  }

  fn exec_and(&mut self, offset: usize) -> OpResult<bool> {
    self.exec_logical(offset, |v| v.falsy())
  }

  fn exec_not(&mut self, opcode: Opcode) -> OpResult {
    self.unary_op(opcode, |v| Ok(!v))
  }

  fn exec_negate(&mut self, opcode: Opcode) -> OpResult {
    self.unary_op(opcode, |v| -v)
  }

  fn exec_check(&mut self) -> OpResult {
    let b = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let a = self.stack_peek().ok_or(UsageError::EmptyStack)?;
    self.stack_push(Value::from(a == b));
    Ok(())
  }

  fn exec_println(&mut self) -> OpResult {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    println!("{value}");
    Ok(())
  }

  fn exec_jump_if_false(&mut self, offset: usize) -> OpResult<bool> {
    let value = self.stack_pop().ok_or(UsageError::EmptyStack)?;

    if !value.truthy() {
      self.jump(offset);
      Ok(true)
    } else {
      Ok(false)
    }
  }

  fn exec_call(&mut self, airity: usize) -> OpResult {
    let callable = self.stack_load_rev(airity).ok_or(UsageError::EmptyStack)?;
    self.call_value(callable, airity)
  }

  fn exec_create_vec(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    let list = self.gc.allocate(list);
    self.stack_push(list);
  }

  fn exec_sized_vec(&mut self, repeats: usize) -> OpResult {
    let item = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let vec = vec![item; repeats];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
  }

  fn exec_dyn_vec(&mut self) -> OpResult {
    let repeats = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let repeats = repeats.as_i32().ok_or(UsageError::CoercionError(repeats, "i32"))?;
    let item = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let vec = vec![item; repeats as usize];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
  }

  fn exec_create_closure(&mut self) -> OpResult {
    let function = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let captures = self.stack_pop().ok_or(UsageError::EmptyStack)?;
    let f = function.as_fn().ok_or(UsageError::ClosureType)?;
    let c = captures.cast_to::<VecValue>().ok_or(UsageError::CaptureType)?;

    let closure = self.gc.allocate(ClosureValue::new(c, f.clone()));
    self.stack_push(closure);
    Ok(())
  }

  fn exec_create_struct(&mut self, size: usize) -> OpResult {
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

  fn exec_create_class(&mut self, loc: usize) -> OpResult {
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
  fn exec_create_module(&mut self, loc: usize) -> OpResult {
    let name = self.program.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let leaf = self.current_env();
      let module = ModuleValue::new_child(name, leaf.handle.value.clone());
      let handle = self.gc.allocate_typed_handle(module);
      self.envs.push(EnvEntry::Mod(handle.clone()));
      self.stack_push(handle.value());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_scope_resolution(&mut self, ident: usize) -> OpResult {
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
    let leaf = self.current_env().value();
    let handle = self.gc.allocate_typed_handle(ModuleValue::new_scope(leaf));
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
      use stdlib::names::{env::*, *};
      if let Some(paths) = self
        .current_env()
        .lookup_path(&[STD, ENV, PATHS])
        .map(|l| l.and_then(|l| l.cast_to::<VecValue>()))
        .map_err(|e| self.error(e))?
      {
        for path in paths.iter() {
          let base = PathBuf::from(path.to_string());
          found_file = try_to_find_file(&base, &required_file, &mut attempts);
          if found_file.is_some() {
            break;
          }
        }
      }
    }

    let found_file = found_file.ok_or_else(|| self.error(UsageError::BadReq(attempts)))?;

    let file_id = PlatformMetadata::id_of(&found_file).map_err(Error::other_system_err)?;

    match found_file.extension().and_then(|s| s.to_str()) {
      Some(dlopen2::utils::PLATFORM_FILE_EXTENSION) => {
        let value = if let Some(value) = self.lib_cache.get(&file_id) {
          value.clone()
        } else {
          let lib: Container<NativeApi> =
            unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

          let value: Value = lib.duck_type_load_module(self).into();
          self.opened_native_libs.insert(found_file, lib);
          self.lib_cache.insert(file_id, value.clone());
          value
        };

        self.stack_push(value);

        Ok(())
      }
      _ => {
        let source = fs::read_to_string(&found_file).map_err(Error::other_system_err)?;

        if let Some(value) = self.lib_cache.get(&file_id) {
          self.stack_push(value.clone());
        } else {
          self.filemap.add(file_id, &found_file);

          let opts = CompileOpts { optimize: self.opt };
          let new_ctx =
            code::compile_file(&mut self.program, file_id, source, opts).map_err(|e| e.with_filename(&self.filemap))?;
          let gmod = ModuleBuilder::initialize(
            &mut self.gc,
            ModuleType::new_global(format!("<file export {}>", found_file.display())),
            |gc, mut lib| {
              let libval = lib.handle.value.clone();
              lib.env.extend(stdlib::enable_std(gc, libval, &self.args));
            },
          );

          self.new_frame(new_ctx, 0);
          self.envs.push(EnvEntry::File(gmod));
          self.opened_files.push(FileInfo::new(found_file, file_id));
          let output = self.run(RunMode::File)?;
          self.stack_push(output);
        }

        Ok(())
      }
    }
  }

  /* Utility Functions */

  fn execute<F, T>(&mut self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&mut Self) -> OpResult<T>,
  {
    let r = f(self).map_err(|e| self.error(e));

    r
  }

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
    self.do_binary_op(opcode, av, bv, f)
  }

  fn do_binary_op<F>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
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

  fn global_op<F, T>(&self, index: usize, f: F) -> OpResult<T>
  where
    F: FnOnce(&Self, String) -> OpResult<T>,
  {
    match self.program.const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      None => Err(UsageError::InvalidConst(index))?,
    }
  }

  fn global_op_mut<F, T>(&mut self, index: usize, f: F) -> OpResult<T>
  where
    F: FnOnce(&mut Self, String) -> OpResult<T>,
  {
    match self.program.const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      None => Err(UsageError::InvalidConst(index))?,
    }
  }

  fn load_from_storage(&mut self, st: Storage, addr: impl Into<usize>) -> OpResult<Value> {
    let addr = addr.into();
    match st {
      Storage::Stack => self.stack_pop().ok_or(UsageError::EmptyStack),
      Storage::Local => self
        .stack_load(self.stack_frame.sp + addr)
        .ok_or(UsageError::InvalidStackIndex(addr)),
      Storage::Global => self.global_op(addr, |this, name| {
        this.current_env().lookup(&name).ok_or(UsageError::UndefinedVar(name))
      }),
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

  pub fn stack_load(&self, index: usize) -> Option<Value> {
    self.stack.get(index).cloned()
  }

  pub fn stack_load_rev(&self, index: usize) -> Option<Value> {
    self.stack.get(self.stack_size() - 1 - index).cloned()
  }

  pub fn stack_store(&mut self, index: usize, value: Value) {
    self.stack[index] = value;
  }

  pub fn stack_store_rev(&mut self, index: usize, value: Value) {
    let st_len = self.stack_size();
    self.stack[st_len - 1 - index] = value;
  }

  pub fn stack_peek(&self) -> Option<Value> {
    self.stack.last().cloned()
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

  pub fn current_env(&self) -> &UsertypeHandle<ModuleValue> {
    self.envs.last()
  }

  pub fn current_env_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    self.envs.last_mut()
  }

  pub fn check_gc(&mut self, export: Option<&Value>) -> ExecResult {
    self
      .gc
      .clean_if_time(&self.stack, &self.envs, self.lib_cache.values(), export)?;
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

struct FileInfo {
  path: PathBuf,
  id: FileIdType,
}

impl FileInfo {
  fn new(path: impl Into<PathBuf>, id: FileIdType) -> Self {
    Self { path: path.into(), id }
  }
}

#[derive(WrapperApi)]
pub(crate) struct NativeApi {
  duck_type_load_module: fn(vm: &mut Vm) -> UsertypeHandle<ModuleValue>,
}

#[cfg(test)]
mod tests;
