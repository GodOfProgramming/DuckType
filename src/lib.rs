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
    methods, native, Args, Fields, MaybeFrom, ModuleBuilder, Operators, TryUnwrapArg, UsageError, UsageResult, Usertype,
    UsertypeFields, UsertypeMethods, Value, Vm,
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
use ahash::RandomState;
use clap::Parser;
use code::CompileOpts;
use dlopen2::wrapper::Container;
use dlopen2::wrapper::WrapperApi;
use exec::memory::{Allocation, Gc};
use prelude::module_value::ModuleType;
use ptr::{MutPtr, SmartPtr};
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::{BTreeMap, HashMap, HashSet},
  fs, mem,
  path::{Path, PathBuf},
};

type FastHashSet<T> = HashSet<T, RandomState>;
type FastHashMap<K, V> = HashMap<K, V, RandomState>;

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

macro_rules! current_module {
  ($this:ident) => {
    $this.modules.last()
  };
}

macro_rules! current_module_mut {
  ($this:ident) => {
    $this.modules.last_mut()
  };
}

#[cfg(feature = "runtime-disassembly")]
macro_rules! cache_hit {
  () => {
    println!("{:>28}", "| [cache hit]");
  };
}

#[cfg(not(feature = "runtime-disassembly"))]
macro_rules! cache_hit {
  () => {};
}

#[cfg(not(debug_assertions))]
macro_rules! stack_pop {
  ($this:ident) => {
    $this.stack_pop()
  };
}

#[cfg(debug_assertions)]
macro_rules! stack_pop {
  ($this:ident) => {
    $this.stack_pop()?
  };
}

pub struct Vm {
  pub(crate) stack: Stack,
  pub(crate) stack_frame: StackFrame,
  pub(crate) stack_frames: Vec<StackFrame>,
  globals: FastHashMap<String, Value>,

  pub gc: SmartPtr<Gc>,

  cache: Cache,
  pub(crate) modules: ModuleStack,

  opt: bool,
  args: Vec<String>,

  filemap: FileMap,

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
      globals: Default::default(),
      gc,
      cache: Default::default(),
      modules: Default::default(),
      opt,
      args: args.into(),
      filemap: Default::default(),
      opened_files: Default::default(),
      opened_native_libs: Default::default(),
    }
  }

  pub fn run_file(&mut self, file: impl Into<PathBuf>, module: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    let file = file.into();
    let file_id = PlatformMetadata::id_of(&file).unwrap_or(0);

    self.filemap.add(file_id, &file);
    self.opened_files = vec![FileInfo::new(&file, file_id)];

    let source = fs::read_to_string(&file).map_err(Error::other_system_err)?;
    let opts = CompileOpts { optimize: self.opt };
    let ctx = code::compile_file(&mut self.cache, file_id, source, opts).map_err(|e| e.with_filename(&self.filemap))?;

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.modules.push(ModuleEntry::File(module));

    let res = self.execute(RunMode::File);

    res
  }

  pub fn run_string(&mut self, source: impl AsRef<str>, module: UsertypeHandle<ModuleValue>) -> Result<Value, Error> {
    self.opened_files = vec![];

    let opts = CompileOpts { optimize: self.opt };
    let ctx = code::compile_string(&mut self.cache, source, opts)?;

    self.stack_frame = StackFrame::new(ctx, self.stack_size());
    self.stack_frames = Default::default();
    self.modules.push(ModuleEntry::String(module));

    self.execute(RunMode::String)
  }

  pub fn run_fn(&mut self, ctx: SmartPtr<Context>, module: UsertypeHandle<ModuleValue>, airity: usize) -> ExecResult<Value> {
    self.new_frame(ctx, airity);
    self.modules.push(ModuleEntry::Fn(module));

    self.execute(RunMode::Fn)
  }

  pub(crate) fn execute(&mut self, exec_type: RunMode) -> ExecResult<Value> {
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
              ctx: self.ctx(),
              stack: &self.stack,
              cache: &self.cache,
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
        Opcode::Pop => self.exec(|this| this.exec_pop())?,
        Opcode::PopN => self.exec(|this| Ok(this.exec_pop_n(data!(inst))))?,
        Opcode::Const => {
          self.exec(|this| this.exec_const(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::Store => {
          let (storage, addr) = self.exec(|_| Ok(data!(inst)))?;
          match storage {
            Storage::Stack => self.exec(|this| this.exec_store_stack(addr))?,
            Storage::Local => self.exec(|this| this.exec_store_local(addr))?,
            Storage::Global => self.exec(|this| this.exec_store_global(addr))?,
          }
        }
        Opcode::Load => {
          let (storage, addr): (Storage, LongAddr) = self.exec(|_| Ok(data!(inst)))?;
          match storage {
            Storage::Stack => self.exec(|this| this.exec_load_stack(addr))?,
            Storage::Local => self.exec(|this| this.exec_load_local(addr))?,
            Storage::Global => self.exec(|this| this.exec_load_global(addr))?,
          }
        }
        Opcode::Nil => self.exec_nil(),
        Opcode::True => self.exec_true(),
        Opcode::False => self.exec_false(),
        Opcode::InitializeMember => {
          self.exec(|this| this.exec_initialize_member(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::AssignMember => {
          self.exec(|this| this.exec_assign_member(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::LookupMember => {
          self.exec(|this| this.exec_lookup_member(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::PeekMember => {
          self.exec(|this| this.exec_peek_member(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::InitializeConstructor => {
          self.exec(|this| this.exec_initialize_constructor())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::InitializeMethod => {
          self.exec(|this| this.exec_initialize_method(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateVec => {
          self.exec(|this| Ok(this.exec_create_vec(data!(inst))))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateSizedVec => {
          self.exec(|this| this.exec_sized_vec(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateDynamicVec => {
          self.exec(|this| this.exec_dyn_vec())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateClosure => {
          self.exec(|this| this.exec_create_closure())?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateStruct => {
          self.exec(|this| this.exec_create_struct(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateClass => {
          self.exec(|this| this.exec_create_class(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
        Opcode::CreateModule => {
          self.exec(|this| this.exec_create_module(data!(inst)))?;
          self.check_gc(export.as_ref())?;
        }
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
          self.check_gc(export.as_ref())?;
          continue 'ctx;
        }
        Opcode::Breakpoint => {
          self.dbg()?;
        }
        Opcode::Export => self.exec(|this| this.exec_export(&mut export))?,
        Opcode::DefineGlobal => self.exec(|this| this.exec_define_global(data!(inst)))?,
        Opcode::DefineScope => self.exec(|this| this.exec_define_scope(data!(inst)))?,
        Opcode::Resolve => self.exec(|this| this.exec_resolve(data!(inst)))?,
        Opcode::EnterBlock => {
          self.push_scope();
          self.check_gc(export.as_ref())?;
        }
        Opcode::PopScope => self.pop_scope(),
        Opcode::Equal => {
          if inst.has_data() {
            self.exec(|this| this.exec_equal_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_equal(opcode))?;
          }
        }
        Opcode::NotEqual => {
          if inst.has_data() {
            self.exec(|this| this.exec_not_equal_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_not_equal(opcode))?;
          }
        }
        Opcode::Greater => {
          if inst.has_data() {
            self.exec(|this| this.exec_greater_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_greater(opcode))?;
          }
        }
        Opcode::GreaterEqual => {
          if inst.has_data() {
            self.exec(|this| this.exec_greater_equal_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_greater_equal(opcode))?;
          }
        }
        Opcode::Less => {
          if inst.has_data() {
            self.exec(|this| this.exec_less_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_less(opcode))?;
          }
        }
        Opcode::LessEqual => {
          if inst.has_data() {
            self.exec(|this| this.exec_less_equal_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_less_equal(opcode))?;
          }
        }
        Opcode::Add => {
          if inst.has_data() {
            self.exec(|this| this.exec_add_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_add(opcode))?;
          }
        }
        Opcode::Sub => {
          if inst.has_data() {
            self.exec(|this| this.exec_sub_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_sub(opcode))?;
          }
        }
        Opcode::Mul => {
          if inst.has_data() {
            self.exec(|this| this.exec_mul_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_mul(opcode))?;
          }
        }
        Opcode::Div => {
          if inst.has_data() {
            self.exec(|this| this.exec_div_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_div(opcode))?;
          }
        }
        Opcode::Rem => {
          if inst.has_data() {
            self.exec(|this| this.exec_rem_fast(opcode, data!(inst)))?;
          } else {
            self.exec(|this| this.exec_rem(opcode))?;
          }
        }
        Opcode::Negate => {
          self.exec(|this| this.exec_negate(opcode))?;
        }
        Opcode::Not => {
          self.exec(|this| this.exec_not(opcode))?;
        }
        Opcode::Index => {
          self.exec(|this| this.exec_index(opcode))?;
        }
        Opcode::AssignIndex => {
          self.exec(|this| this.exec_index_assign())?;
        }
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
        Opcode::Swap => self.exec(|this| this.exec_swap(data!(inst)))?,
        Opcode::SwapPop => {
          let idx = self.stack_size() - 2;
          self.stack.swap_remove(idx);
        }
        Opcode::Is => self.exec(|this| this.exec_is())?,
        Opcode::Quack => self.exec(|this| this.exec_quack())?,
        Opcode::Unknown => self.exec_unknown(inst)?,
      }

      self.stack_frame.ip += 1;
    }

    match exec_type {
      RunMode::File => {
        let info = self.opened_files.pop().expect("file must be popped when leaving a file");
        if let Some(export) = &export {
          self.cache.add_lib(info.id, export.clone());
        }

        // pop until a file module is found
        while !matches!(self.modules.pop(), ModuleEntry::File(_)) {}
      }
      RunMode::Fn => {
        // pop until a fn module is found
        while !matches!(self.modules.pop(), ModuleEntry::Fn(_)) {}
      }
      RunMode::String => while !matches!(self.modules.pop(), ModuleEntry::String(_)) {},
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
    let c = self.cache.const_at(index).ok_or(UsageError::InvalidConst(index.into()))?;

    let module = current_module!(self).into();
    let value = Value::from_constant(&mut self.gc, module, c);
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

  fn exec_pop(&mut self) -> OpResult {
    stack_pop!(self);
    Ok(())
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
    let var = self.value_of_ident(loc)?;
    self.stack_push(var);
    Ok(())
  }

  fn exec_store_stack(&mut self, loc: LongAddr) -> OpResult {
    let value = stack_pop!(self);
    self.stack_store_rev(loc.0, value);
    Ok(())
  }

  fn exec_store_local(&mut self, loc: LongAddr) -> OpResult {
    let value = self.stack_peek();
    self.stack_store(self.stack_frame.sp + loc.0, value);
    Ok(())
  }

  fn exec_define_global(&mut self, loc: LongAddr) -> OpResult {
    self.validate_ident_and(loc, |this, name| {
      let v = this.stack_peek();
      if this.globals.insert(name.clone(), v.clone()).is_none() {
        this.cache.add_global(loc, v);
        Ok(())
      } else {
        let level = current_module!(this).search_for(0, &name);
        Err(UsageError::Redefine { level, name })
      }
    })
  }

  fn exec_define_scope(&mut self, loc: LongAddr) -> OpResult {
    self.validate_ident_and(loc, |this, name| {
      let v = this.stack_peek();
      let module = current_module_mut!(this);
      if module.define(name.clone(), v.clone()) {
        this.cache.add_to_mod(module.value(), loc, v);
        Ok(())
      } else {
        let level = current_module!(this).search_for(0, &name);
        Err(UsageError::Redefine { level, name })
      }
    })
  }

  fn exec_store_global(&mut self, loc: LongAddr) -> OpResult {
    self.validate_ident_and(loc, |this, name| {
      let value = this.stack_peek();

      if this.globals.insert(name.clone(), value.clone()).is_some() {
        this.cache.add_global(loc, value);
        Ok(())
      } else {
        Err(UsageError::UndefinedVar(name))
      }
    })
  }

  fn exec_initialize_member(&mut self, loc: usize) -> OpResult {
    let value = stack_pop!(self);
    let mut obj = self.stack_peek();
    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      obj.set_member(&mut self.gc, Field::new(loc, name), value)?;
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_initialize_method(&mut self, loc: usize) -> OpResult {
    let value = stack_pop!(self);
    let mut obj = self.stack_peek();

    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment)?;

    let f = value.cast_to::<FunctionValue>().ok_or(UsageError::MethodType)?;

    if let ConstantValue::String(name) = name {
      class.set_method(name, f.clone());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_initialize_constructor(&mut self) -> OpResult {
    let value = stack_pop!(self);
    let mut obj = self.stack_peek();
    let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment)?;
    class.set_constructor(value);
    Ok(())
  }

  fn exec_assign_member(&mut self, loc: usize) -> OpResult {
    let value = stack_pop!(self);
    let mut obj = stack_pop!(self);

    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      obj.set_member(&mut self.gc, Field::new(loc, name), value.clone())?;

      self.stack_push(value);

      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_lookup_member(&mut self, loc: usize) -> OpResult {
    let obj = stack_pop!(self);

    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let value = obj.get_member(&mut self.gc, Field::new(loc, name))?.unwrap_or_default();
      self.stack_push(value);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_peek_member(&mut self, loc: usize) -> OpResult {
    let value = self.stack_peek();

    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

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
    let value = self.stack_peek();
    if f(value) {
      self.jump(offset);
      Ok(true)
    } else {
      stack_pop!(self);
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

  fn exec_index(&mut self, opcode: Opcode) -> OpResult {
    self.binary_op(opcode, |_, _| Err(UsageError::InvalidBinary))
  }

  fn exec_index_assign(&mut self) -> OpResult {
    let value = stack_pop!(self);
    let index = stack_pop!(self);
    let indexable = stack_pop!(self);
    (indexable.vtable().assign_index)(MutPtr::new(self), indexable, index, value.clone())?;
    self.stack_push(value);
    Ok(())
  }

  fn exec_check(&mut self) -> OpResult {
    let b = stack_pop!(self);
    let a = self.stack_peek();
    self.stack_push(Value::from(a == b));
    Ok(())
  }

  fn exec_println(&mut self) -> OpResult {
    let value = stack_pop!(self);
    println!("{value}");
    Ok(())
  }

  fn exec_jump_if_false(&mut self, offset: usize) -> OpResult<bool> {
    let value = stack_pop!(self);

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
    let item = stack_pop!(self);
    let vec = vec![item; repeats];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
  }

  fn exec_dyn_vec(&mut self) -> OpResult {
    let repeats = stack_pop!(self);
    let repeats = repeats.cast_to::<i32>().ok_or(UsageError::CoercionError(repeats, "i32"))?;
    let item = stack_pop!(self);
    let vec = vec![item; repeats as usize];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    Ok(())
  }

  fn exec_create_closure(&mut self) -> OpResult {
    let function = stack_pop!(self);
    let captures = stack_pop!(self);
    let f = function.cast_to::<FunctionValue>().ok_or(UsageError::ClosureType)?;
    let c = captures.cast_to::<VecValue>().ok_or(UsageError::CaptureType)?;

    let closure = self.gc.allocate(ClosureValue::new(c, f.clone()));
    self.stack_push(closure);
    Ok(())
  }

  fn exec_create_struct(&mut self, size: usize) -> OpResult {
    let mut members = Vec::with_capacity(size);

    for _ in 0..size {
      let key = stack_pop!(self);
      let value = stack_pop!(self);
      if let Some(key) = key.cast_to::<StringValue>() {
        let id = self
          .cache
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
    let creator = stack_pop!(self);
    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let v = self.gc.allocate(ClassValue::new(name, creator));
      self.stack_push(v);
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  /// Create a module and make it the current
  fn exec_create_module(&mut self, loc: usize) -> OpResult {
    let name = self.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc))?;

    if let ConstantValue::String(name) = name {
      let leaf = current_module!(self);
      let module = ModuleValue::new_child(name, leaf.handle.value.clone());
      let handle = self.gc.allocate_typed_handle(module);
      self.modules.push(ModuleEntry::Mod(handle.clone()));
      self.stack_push(handle.value());
      Ok(())
    } else {
      Err(UsageError::InvalidIdentifier(name.to_string()))
    }
  }

  fn exec_resolve(&mut self, ident: usize) -> OpResult {
    let obj = stack_pop!(self);

    let hit = self.cache.resolve_mod(obj.clone(), ident);

    match hit {
      Some(value) => {
        cache_hit!();
        self.stack_push(value);
        Ok(())
      }
      None => {
        let name = self.cache.const_at(ident).ok_or(UsageError::InvalidConst(ident))?;

        if let ConstantValue::String(name) = name {
          let value = obj.resolve(name)?;
          self.cache.add_to_mod(obj, ident, value.clone());
          self.stack_push(value);
          Ok(())
        } else {
          Err(UsageError::InvalidIdentifier(name.to_string()))
        }
      }
    }
  }

  fn push_scope(&mut self) {
    let leaf = current_module!(self).value();
    let handle = self.gc.allocate_typed_handle(ModuleValue::new_scope(leaf));
    self.modules.push(ModuleEntry::Block(handle));
  }

  fn pop_scope(&mut self) {
    self.modules.pop();
  }

  fn exec_export(&mut self, export: &mut Option<Value>) -> OpResult {
    let value = stack_pop!(self);
    *export = Some(value);
    Ok(())
  }

  fn exec_is(&mut self) -> OpResult {
    let right = stack_pop!(self);
    let left = stack_pop!(self);

    let is_type = if let Some(instance) = left.cast_to::<InstanceValue>() {
      instance.class.bits == right.bits
    } else if left.is_ptr() {
      if let Some(right) = right.cast_to::<IdValue>() {
        left.type_id() == &right.id
      } else {
        left.type_id() == right.type_id()
      }
    } else if left.is::<NativeFn>() {
      if right.pointer() == std::ptr::null() {
        left.tag() == right.tag()
      } else {
        left.bits == right.bits
      }
    } else {
      left.tag() == right.tag()
    };

    self.stack_push(Value::new(is_type));
    Ok(())
  }

  fn exec_quack(&mut self) -> OpResult {
    let value = stack_pop!(self);
    Err(UsageError::Quack(value))
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

    // TODO really need to figure out how to turn usage errors into regular ones without the passthrough
    let value = (|| {
      let value = stack_pop!(self);
      Ok(value)
    })()
    .map_err(|err| self.error(err))?;

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
      if let Some(paths) = current_module!(self)
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
        let value = if let Some(value) = self.cache.get_lib(file_id) {
          value.clone()
        } else {
          let lib: Container<NativeApi> =
            unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

          let value: Value = lib.duck_type_load_module(self).into();
          self.opened_native_libs.insert(found_file, lib);
          self.cache.add_lib(file_id, value.clone());
          value
        };

        self.stack_push(value);

        Ok(())
      }
      _ => {
        let source = fs::read_to_string(&found_file).map_err(Error::other_system_err)?;

        if let Some(value) = self.cache.get_lib(file_id) {
          self.stack_push(value.clone());
        } else {
          self.filemap.add(file_id, &found_file);

          let opts = CompileOpts { optimize: self.opt };
          let new_ctx =
            code::compile_file(&mut self.cache, file_id, source, opts).map_err(|e| e.with_filename(&self.filemap))?;
          let gmod = ModuleBuilder::initialize(
            &mut self.gc,
            ModuleType::new_global(format!("<file export {}>", found_file.display())),
            |gc, mut lib| {
              let libval = lib.handle.value.clone();
              lib.env.extend(stdlib::enable_std(gc, libval, &self.args));
            },
          );

          self.new_frame(new_ctx, 0);
          self.modules.push(ModuleEntry::File(gmod));
          self.opened_files.push(FileInfo::new(found_file, file_id));
          let output = self.execute(RunMode::File)?;
          self.stack_push(output);
        }

        Ok(())
      }
    }
  }

  /* Utility Functions */

  fn exec<F, T>(&mut self, f: F) -> ExecResult<T>
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
    let value = stack_pop!(self);

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
    let bv = stack_pop!(self);
    let av = stack_pop!(self);
    self.do_binary_op(opcode, av, bv, f)
  }

  fn do_binary_op<F>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    if av.is_ptr() {
      let vtable = av.vtable();
      let bin_op = match opcode {
        Opcode::Add => vtable.add,
        Opcode::Sub => vtable.sub,
        Opcode::Mul => vtable.mul,
        Opcode::Div => vtable.div,
        Opcode::Rem => vtable.rem,
        Opcode::Equal => vtable.eq,
        Opcode::NotEqual => vtable.neq,
        Opcode::Less => vtable.less,
        Opcode::LessEqual => vtable.leq,
        Opcode::Greater => vtable.greater,
        Opcode::GreaterEqual => vtable.geq,
        Opcode::Index => vtable.index,
        _ => Err(UsageError::InvalidBinary)?,
      };

      let output = bin_op(MutPtr::new(self), av, bv)?;
      self.stack_push(output);
      Ok(())
    } else {
      self.stack_push(f(av, bv)?);
      Ok(())
    }
  }

  fn load_from_storage(&mut self, st: Storage, addr: impl Into<usize>) -> OpResult<Value> {
    let addr = addr.into();
    match st {
      Storage::Stack => Ok(stack_pop!(self)),
      Storage::Local => self
        .stack_load(self.stack_frame.sp + addr)
        .ok_or(UsageError::InvalidStackIndex(addr)),
      Storage::Global => self.value_of_ident(addr),
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

  #[cfg(debug_assertions)]
  pub fn stack_pop(&mut self) -> UsageResult {
    self.stack.pop().ok_or(UsageError::EmptyStack)
  }

  #[cfg(not(debug_assertions))]
  pub fn stack_pop(&mut self) -> Value {
    self.stack.pop().unwrap()
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

  pub fn stack_peek(&self) -> Value {
    self.stack.last().cloned().unwrap()
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

  /// Tries to find the cached value at the given location and if not performs a slow lookup
  ///
  /// Do not attempt to optimize with caching the current module's value
  ///
  /// Scopes are created and destroyed rapidly at the moment,
  /// which explodes the size of the cache map
  fn value_of_ident(&self, ident_loc: impl Into<usize>) -> OpResult<Value> {
    let ident = ident_loc.into();
    let module = current_module!(self);
    let hit = self.cache.find_var(module, ident);
    match hit {
      Some(hit) => Ok(hit),
      None => match self.cache.const_at(ident) {
        Some(ConstantValue::String(name)) => current_module!(self)
          .lookup(&name)
          .ok_or_else(|| UsageError::UndefinedVar(name.clone())),
        Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
        None => Err(UsageError::InvalidConst(ident))?,
      },
    }
  }

  fn validate_ident_and<F, T>(&mut self, index: impl Into<usize>, f: F) -> OpResult<T>
  where
    F: FnOnce(&mut Self, String) -> OpResult<T>,
  {
    let index = index.into();
    match self.cache.const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      None => Err(UsageError::InvalidConst(index))?,
    }
  }

  pub fn ctx(&self) -> &Context {
    &self.stack_frame.ctx
  }

  pub fn ctx_mut(&mut self) -> &mut Context {
    &mut self.stack_frame.ctx
  }

  pub fn current_module_value(&self) -> Value {
    self.modules.last().value()
  }

  pub fn check_gc(&mut self, export: Option<&Value>) -> ExecResult {
    self.gc.clean_if_time(&self.stack, &self.modules, &mut self.cache, export)?;
    Ok(())
  }

  pub fn get_global(&self, name: &str) -> Option<Value> {
    self.globals.get(name).cloned()
  }

  pub fn set_global(&mut self, name: impl ToString, value: impl Into<Value>) {
    let name = name.to_string();
    self.cache.invalidate_global(&name);
    self.globals.insert(name, value.into());
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
