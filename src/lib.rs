mod bindings;
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
  ffi::c_void,
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

// .ok_or(UsageError::InvalidInstruction($inst))?
#[cfg(debug_assertions)]
macro_rules! decode {
  ($inst:ident) => {
    $inst.data().unwrap()
  };
}

#[cfg(not(debug_assertions))]
macro_rules! decode {
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

//?
#[cfg(debug_assertions)]
macro_rules! stack_pop {
  ($this:ident) => {
    $this.stack_pop().unwrap()
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

    self.execute(RunMode::File)
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
    let mut export: Option<Value> = None;

    unsafe {
      bindings::duck_type_execute(
        self as *mut Vm as *mut c_void,
        self.ctx_mut().instructions.as_mut_ptr() as *mut u64,
        &mut self.stack_frame.ip as *mut usize,
        &mut export as *mut Option<Value> as *mut c_void,
      )
    };

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

  fn jump(&mut self, offset: usize) {
    self.stack_frame.ip += offset;
  }

  fn jump_back(&mut self, offset: usize) {
    self.stack_frame.ip -= offset;
  }

  /// when f evaluates to true, short circuit
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) {
    let value = self.stack_peek();
    if f(value) {
      self.jump(offset);
    } else {
      stack_pop!(self);
      self.stack_frame.ip += 1;
    }
  }

  fn exec_or(&mut self, offset: usize) {
    self.exec_logical(offset, |v| v.truthy())
  }

  fn exec_and(&mut self, offset: usize) {
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

  fn push_scope(&mut self) {
    let leaf = current_module!(self).value();
    let handle = self.gc.allocate_typed_handle(ModuleValue::new_scope(leaf));
    self.modules.push(ModuleEntry::Block(handle));
  }

  fn pop_scope(&mut self) {
    self.modules.pop();
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
      if right.pointer().is_null() {
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

  /* Utility Functions */

  fn exec<F, T>(&mut self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&mut Self) -> OpResult<T>,
  {
    f(self).map_err(|e| self.error(e))
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
          .lookup(name)
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

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn exec_disasm(vm: &Vm, inst: Instruction) {
  #[cfg(feature = "runtime-disassembly")]
  {
    vm.stack_display();

    println!(
      "{}",
      InstructionDisassembler {
        ctx: &ContextDisassembler {
          ctx: vm.ctx(),
          stack: &vm.stack,
          cache: &vm.cache,
        },
        inst,
        offset: vm.stack_frame.ip,
      }
    );
  }
}

#[no_mangle]
pub extern "C" fn exec_pop(vm: &mut Vm) {
  stack_pop!(vm);
}

#[no_mangle]
pub extern "C" fn exec_pop_n(vm: &mut Vm, inst: Instruction) {
  let count: usize = decode!(inst);
  vm.stack_pop_n(count);
}

#[no_mangle]
pub extern "C" fn exec_const(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let index: LongAddr = decode!(inst);
  let c = vm
    .cache
    .const_at(index)
    .ok_or(UsageError::InvalidConst(index.into()))
    .unwrap();

  let module = current_module!(vm).into();
  let value = Value::from_constant(&mut vm.gc, module, c);
  vm.stack_push(value);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_store(vm: &mut Vm, inst: Instruction) {
  let (storage, addr) = decode!(inst);
  match storage {
    Storage::Stack => vm.exec(|this| this.exec_store_stack(addr)).unwrap(),
    Storage::Local => vm.exec(|this| this.exec_store_local(addr)).unwrap(),
    Storage::Global => vm.exec(|this| this.exec_store_global(addr)).unwrap(),
  }
}

#[no_mangle]
pub extern "C" fn exec_load(vm: &mut Vm, inst: Instruction) {
  let (storage, addr): (Storage, LongAddr) = decode!(inst);
  match storage {
    Storage::Stack => vm.exec(|this| this.exec_load_stack(addr)).unwrap(),
    Storage::Local => vm.exec(|this| this.exec_load_local(addr)).unwrap(),
    Storage::Global => vm.exec(|this| this.exec_load_global(addr)).unwrap(),
  }
}

#[no_mangle]
pub extern "C" fn exec_nil(vm: &mut Vm) {
  vm.stack_push(Value::nil);
}

#[no_mangle]
pub extern "C" fn exec_true(vm: &mut Vm) {
  vm.stack_push(Value::from(true));
}

#[no_mangle]
pub extern "C" fn exec_false(vm: &mut Vm) {
  vm.stack_push(Value::from(false));
}

#[no_mangle]
pub extern "C" fn exec_initialize_member(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let value = stack_pop!(vm);
  let mut obj = vm.stack_peek();
  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    obj.set_member(&mut vm.gc, Field::new(loc, name), value).unwrap();
  } else {
    Err(UsageError::InvalidIdentifier(name.to_string())).unwrap()
  }

  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_assign_member(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let value = stack_pop!(vm);
  let mut obj = stack_pop!(vm);

  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    obj.set_member(&mut vm.gc, Field::new(loc, name), value.clone()).unwrap();

    vm.stack_push(value);
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_lookup_member(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let obj = stack_pop!(vm);

  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    let value = obj.get_member(&mut vm.gc, Field::new(loc, name)).unwrap().unwrap_or_default();
    vm.stack_push(value);
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_peek_member(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let value = vm.stack_peek();

  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    let member = value
      .get_member(&mut vm.gc, Field::new(loc, name))
      .unwrap()
      .unwrap_or_default();
    vm.stack_push(member);
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_initialize_constructor(vm: &mut Vm, export: &mut Option<Value>) {
  let value = stack_pop!(vm);
  let mut obj = vm.stack_peek();
  let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment).unwrap();
  class.set_constructor(value);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_initialize_method(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let value = stack_pop!(vm);
  let mut obj = vm.stack_peek();

  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment).unwrap();

  let f = value.cast_to::<FunctionValue>().ok_or(UsageError::MethodType).unwrap();

  if let ConstantValue::String(name) = name {
    class.set_method(name, f.clone());
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_vec(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let num_items: usize = decode!(inst);
  let list = vm.stack_drain_from(num_items);
  let list = vm.gc.allocate(list);
  vm.stack_push(list);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_sized_vec(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let repeats: usize = decode!(inst);
  let item = stack_pop!(vm);
  let vec = vec![item; repeats];
  let vec = vm.gc.allocate(vec);
  vm.stack_push(vec);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_dyn_vec(vm: &mut Vm, export: &mut Option<Value>) {
  let repeats = stack_pop!(vm);
  let repeats = repeats
    .cast_to::<i32>()
    .ok_or(UsageError::CoercionError(repeats, "i32"))
    .unwrap();
  let item = stack_pop!(vm);
  let vec = vec![item; repeats as usize];
  let vec = vm.gc.allocate(vec);
  vm.stack_push(vec);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_closure(vm: &mut Vm, export: &mut Option<Value>) {
  let function = stack_pop!(vm);
  let captures = stack_pop!(vm);
  let f = function.cast_to::<FunctionValue>().ok_or(UsageError::ClosureType).unwrap();
  let c = captures.cast_to::<VecValue>().ok_or(UsageError::CaptureType).unwrap();

  let closure = vm.gc.allocate(ClosureValue::new(c, f.clone()));
  vm.stack_push(closure);
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_struct(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let size: usize = decode!(inst);
  let mut members = Vec::with_capacity(size);

  for _ in 0..size {
    let key = stack_pop!(vm);
    let value = stack_pop!(vm);
    if let Some(key) = key.cast_to::<StringValue>() {
      let id = vm
        .cache
        .strings
        .get_by_right(&**key)
        .cloned()
        .ok_or(UsageError::InvalidIdentifier(key.to_string()))
        .unwrap();
      members.push((((**key).clone(), id), value));
    } else {
      panic!("{}", UsageError::InvalidIdentifier(key.to_string()));
    }
  }

  let struct_value = vm.gc.allocate(StructValue::new(members));

  vm.stack_push(struct_value);

  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_create_class(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let creator = stack_pop!(vm);
  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    let v = vm.gc.allocate(ClassValue::new(name, creator));
    vm.stack_push(v);
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

/// Create a module and make it the current
#[no_mangle]
pub extern "C" fn exec_create_module(vm: &mut Vm, inst: Instruction, export: &mut Option<Value>) {
  let loc: usize = decode!(inst);
  let name = vm.cache.const_at(loc).ok_or(UsageError::InvalidConst(loc)).unwrap();

  if let ConstantValue::String(name) = name {
    let leaf = current_module!(vm);
    let module = ModuleValue::new_child(name, leaf.handle.value.clone());
    let handle = vm.gc.allocate_typed_handle(module);
    vm.modules.push(ModuleEntry::Mod(handle.clone()));
    vm.stack_push(handle.value());
  } else {
    panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
  }
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_check(vm: &mut Vm) {
  let b = stack_pop!(vm);
  let a = vm.stack_peek();
  vm.stack_push(Value::from(a == b)); // TODO actual binary eq op
}

#[no_mangle]
pub extern "C" fn exec_println(vm: &mut Vm) {
  let value = stack_pop!(vm);
  println!("{value}");
}

#[no_mangle]
pub extern "C" fn exec_jump(vm: &mut Vm, inst: Instruction) {
  let offset: usize = decode!(inst);
  vm.jump(offset);
}

/// Returns true if the jump was performed
#[no_mangle]
pub extern "C" fn exec_jump_if_false(vm: &mut Vm, inst: Instruction) {
  let offset: usize = decode!(inst);
  let value = stack_pop!(vm);

  if !value.truthy() {
    vm.jump(offset);
  } else {
    vm.stack_frame.ip += 1;
  }
}

#[no_mangle]
pub extern "C" fn exec_loop(vm: &mut Vm, inst: Instruction) {
  let offset: usize = decode!(inst);
  vm.jump_back(offset);
}

#[no_mangle]
pub extern "C" fn exec_call(vm: &mut Vm, inst: Instruction) {
  let airity: usize = decode!(inst);
  vm.stack_frame.ip += 1;
  let callable = vm.stack_load_rev(airity).ok_or(UsageError::EmptyStack).unwrap();
  vm.call_value(callable, airity).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_req(vm: &mut Vm, export: &mut Option<Value>) {
  vm.stack_frame.ip += 1;
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
    let value = stack_pop!(vm);
    Ok(value)
  })()
  .map_err(|err| vm.error(err))
  .unwrap();

  let mut attempts = Vec::with_capacity(10);

  let file_str = value.to_string();
  let this_file = vm.opened_files.last().map(|f| f.path.clone());

  let required_file = PathBuf::from(file_str.as_str());

  let mut found_file = None;

  // find relative first, skip if None, None will be during repl so go to cwd
  if let Some(this_dir) = this_file.and_then(|this_file| this_file.parent().map(|p| p.to_path_buf())) {
    found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
  }

  // then try to find from cwd
  if found_file.is_none() {
    let this_dir = std::env::current_dir().map_err(Error::other_system_err).unwrap();
    found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
  }

  // if still not found, try searching library paths
  if found_file.is_none() {
    use stdlib::names::{env::*, *};
    if let Some(paths) = current_module!(vm)
      .lookup_path(&[STD, ENV, PATHS])
      .map(|l| l.and_then(|l| l.cast_to::<VecValue>()))
      .map_err(|e| vm.error(e))
      .unwrap()
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

  let found_file = found_file.ok_or_else(|| vm.error(UsageError::BadReq(attempts))).unwrap();

  let file_id = PlatformMetadata::id_of(&found_file).map_err(Error::other_system_err).unwrap();

  match found_file.extension().and_then(|s| s.to_str()) {
    Some(dlopen2::utils::PLATFORM_FILE_EXTENSION) => {
      let value = if let Some(value) = vm.cache.get_lib(file_id) {
        value.clone()
      } else {
        let lib: Container<NativeApi> =
          unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

        let value: Value = lib.duck_type_load_module(vm).into();
        vm.opened_native_libs.insert(found_file, lib);
        vm.cache.add_lib(file_id, value.clone());
        value
      };

      vm.stack_push(value);
    }
    _ => {
      let source = fs::read_to_string(&found_file).map_err(Error::other_system_err).unwrap();

      if let Some(value) = vm.cache.get_lib(file_id) {
        vm.stack_push(value.clone());
      } else {
        vm.filemap.add(file_id, &found_file);

        let opts = CompileOpts { optimize: vm.opt };
        let new_ctx = code::compile_file(&mut vm.cache, file_id, source, opts)
          .map_err(|e| e.with_filename(&vm.filemap))
          .unwrap();
        let gmod = ModuleBuilder::initialize(
          &mut vm.gc,
          ModuleType::new_global(format!("<file export {}>", found_file.display())),
          |gc, mut lib| {
            let libval = lib.handle.value.clone();
            lib.env.extend(stdlib::enable_std(gc, libval, &vm.args));
          },
        );

        vm.new_frame(new_ctx, 0);
        vm.modules.push(ModuleEntry::File(gmod));
        vm.opened_files.push(FileInfo::new(found_file, file_id));
        let output = vm.execute(RunMode::File).unwrap();
        vm.stack_push(output);
      }
    }
  }

  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_dbg(vm: &mut Vm) {
  vm.dbg().unwrap();
}

#[no_mangle]
pub extern "C" fn exec_export(vm: &mut Vm, export: &mut Option<Value>) {
  let value = stack_pop!(vm);
  *export = Some(value);
}

#[no_mangle]
pub extern "C" fn exec_define_global(vm: &mut Vm, inst: Instruction) {
  let loc: LongAddr = decode!(inst);
  vm.validate_ident_and(loc, |this, name| {
    let v = this.stack_peek();
    if this.globals.insert(name.clone(), v.clone()).is_none() {
      this.cache.add_global(loc, v);
      Ok(())
    } else {
      let level = current_module!(this).search_for(0, &name);
      Err(UsageError::Redefine { level, name })
    }
  })
  .unwrap();
}

#[no_mangle]
pub extern "C" fn exec_define_scope(vm: &mut Vm, inst: Instruction) {
  let loc: LongAddr = decode!(inst);
  vm.validate_ident_and(loc, |this, name| {
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
  .unwrap();
}

#[no_mangle]
pub extern "C" fn exec_resolve(vm: &mut Vm, inst: Instruction) {
  let ident: usize = decode!(inst);
  let obj = stack_pop!(vm);

  let hit = vm.cache.resolve_mod(obj.clone(), ident);

  match hit {
    Some(value) => {
      cache_hit!();
      vm.stack_push(value);
    }
    None => {
      let name = vm.cache.const_at(ident).ok_or(UsageError::InvalidConst(ident)).unwrap();

      if let ConstantValue::String(name) = name {
        let value = obj.resolve(name).unwrap();
        vm.cache.add_to_mod(obj, ident, value.clone());
        vm.stack_push(value);
      } else {
        panic!("{}", UsageError::InvalidIdentifier(name.to_string()));
      }
    }
  }
}

#[no_mangle]
pub extern "C" fn exec_enter_block(vm: &mut Vm, export: &mut Option<Value>) {
  vm.push_scope();
  vm.check_gc(export.as_ref()).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_pop_scope(vm: &mut Vm) {
  vm.pop_scope();
}

#[no_mangle]
pub extern "C" fn exec_equal(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_equal_fast(Opcode::Equal, decode!(inst)).unwrap();
  } else {
    vm.exec_equal(Opcode::Equal).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_not_equal(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_not_equal_fast(Opcode::NotEqual, decode!(inst)).unwrap();
  } else {
    vm.exec_not_equal(Opcode::NotEqual).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_greater(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_greater_fast(Opcode::Greater, decode!(inst)).unwrap();
  } else {
    vm.exec_greater(Opcode::Greater).unwrap()
  }
}

#[no_mangle]
pub extern "C" fn exec_greater_equal(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_greater_equal_fast(Opcode::GreaterEqual, decode!(inst)).unwrap();
  } else {
    vm.exec_greater_equal(Opcode::GreaterEqual).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_less(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_less_fast(Opcode::Less, decode!(inst)).unwrap();
  } else {
    vm.exec_less(Opcode::Less).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_less_equal(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_less_equal_fast(Opcode::LessEqual, decode!(inst)).unwrap();
  } else {
    vm.exec_less_equal(Opcode::LessEqual).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_add(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_add_fast(Opcode::Add, decode!(inst)).unwrap();
  } else {
    vm.exec_add(Opcode::Add).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_sub(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_sub_fast(Opcode::Sub, decode!(inst)).unwrap();
  } else {
    vm.exec_sub(Opcode::Sub).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_mul(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_mul_fast(Opcode::Mul, decode!(inst)).unwrap();
  } else {
    vm.exec_mul(Opcode::Mul).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_div(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_div_fast(Opcode::Div, decode!(inst)).unwrap();
  } else {
    vm.exec_div(Opcode::Div).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_rem(vm: &mut Vm, inst: Instruction) {
  if inst.has_data() {
    vm.exec_rem_fast(Opcode::Rem, decode!(inst)).unwrap();
  } else {
    vm.exec_rem(Opcode::Rem).unwrap();
  }
}

#[no_mangle]
pub extern "C" fn exec_negate(vm: &mut Vm) {
  vm.exec_negate(Opcode::Negate).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_not(vm: &mut Vm) {
  vm.exec_not(Opcode::Not).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_index(vm: &mut Vm) {
  vm.exec_index(Opcode::Index).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_index_assign(vm: &mut Vm) {
  vm.exec_index_assign().unwrap();
}

#[no_mangle]
pub extern "C" fn exec_or(vm: &mut Vm, inst: Instruction) {
  vm.exec_or(decode!(inst));
}

#[no_mangle]
pub extern "C" fn exec_and(vm: &mut Vm, inst: Instruction) {
  vm.exec_and(decode!(inst));
}

#[no_mangle]
pub extern "C" fn exec_swap(vm: &mut Vm, inst: Instruction) {
  vm.exec_swap(decode!(inst)).unwrap();
}

#[no_mangle]
pub extern "C" fn exec_swap_pop(vm: &mut Vm) {
  let idx = vm.stack_size() - 2;
  vm.stack.swap_remove(idx);
}

#[no_mangle]
pub extern "C" fn exec_is(vm: &mut Vm) {
  vm.exec_is().unwrap();
}

#[no_mangle]
pub extern "C" fn exec_quack(vm: &mut Vm) {
  vm.exec_quack().unwrap();
}

#[cold]
#[no_mangle]
pub extern "C" fn exec_unknown(vm: &Vm, inst: Instruction) {
  panic!("{}", vm.error(UsageError::InvalidInstruction(inst)));
}
