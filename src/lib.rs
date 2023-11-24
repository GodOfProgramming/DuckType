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

  last_error: ExecResult,
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

      last_error: Ok(()),
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
    #[cfg(feature = "jump-table")]
    {
      unsafe {
        bindings::duck_type_execute(
          self as *mut Vm as *mut c_void,
          self.ctx_mut().instructions.as_mut_ptr() as *mut u64,
          &mut *self.stack_frame.ip as *mut usize,
        )
      };

      let mut result = Ok(());
      mem::swap(&mut result, &mut self.last_error);
      result?;
    }

    #[cfg(not(feature = "jump-table"))]
    {
      'fetch_cycle: loop {
        let inst = self.stack_frame.ctx.fetch(*self.stack_frame.ip);

        let opcode = inst.opcode();

        match opcode {
          Opcode::Pop => self.exec_pop(),
          Opcode::PopN => self.exec_pop_n(inst.data()),
          Opcode::Const => self.exec_const(inst.data())?,
          Opcode::Store => self.exec_store(inst.data())?,
          Opcode::Load => self.exec_load(inst.data())?,
          Opcode::Nil => self.exec_nil(),
          Opcode::True => self.exec_true(),
          Opcode::False => self.exec_false(),
          Opcode::Add => self.exec_add(inst)?,
          Opcode::Sub => self.exec_sub(inst)?,
          Opcode::Mul => self.exec_mul(inst)?,
          Opcode::Div => self.exec_div(inst)?,
          Opcode::Rem => self.exec_rem(inst)?,
          Opcode::Equal => self.exec_equal(inst)?,
          Opcode::NotEqual => self.exec_not_equal(inst)?,
          Opcode::Greater => self.exec_greater(inst)?,
          Opcode::GreaterEqual => self.exec_greater_equal(inst)?,
          Opcode::Less => self.exec_less(inst)?,
          Opcode::LessEqual => self.exec_less_equal(inst)?,
          Opcode::Index => self.exec_index()?,
          Opcode::AssignIndex => self.exec_index_assign()?,
          Opcode::Negate => self.exec_negate()?,
          Opcode::Not => self.exec_not()?,
          Opcode::Or => {
            self.exec_or(inst.data());
            continue 'fetch_cycle;
          }
          Opcode::And => {
            self.exec_and(inst.data());
            continue 'fetch_cycle;
          }
          Opcode::InitializeMember => self.exec_initialize_member(inst.data())?,
          Opcode::AssignMember => self.exec_assign_member(inst.data())?,
          Opcode::LookupMember => self.exec_lookup_member(inst.data())?,
          Opcode::PeekMember => self.exec_peek_member(inst.data())?,
          Opcode::InitializeConstructor => self.exec_initialize_constructor()?,
          Opcode::InitializeMethod => self.exec_initialize_method(inst.data())?,
          Opcode::CreateVec => self.exec_create_vec(inst.data())?,
          Opcode::CreateSizedVec => self.exec_create_sized_vec(inst.data())?,
          Opcode::CreateDynamicVec => self.exec_create_dyn_vec()?,
          Opcode::CreateClosure => self.exec_create_closure()?,
          Opcode::CreateStruct => self.exec_create_struct(inst.data())?,
          Opcode::CreateClass => self.exec_create_class(inst.data())?,
          Opcode::CreateModule => self.exec_create_module(inst.data())?,
          Opcode::Check => self.exec_check()?,
          Opcode::Println => self.exec_println(),
          Opcode::Jump => {
            self.exec_jump(inst.data());
            continue 'fetch_cycle;
          }
          Opcode::JumpIfFalse => {
            self.exec_jump_if_false(inst.data());
            continue 'fetch_cycle;
          }
          Opcode::Loop => {
            self.exec_loop(inst.data());
            continue 'fetch_cycle;
          }
          Opcode::Invoke => self.exec_call(inst.data())?,
          Opcode::Req => self.exec_req()?,
          Opcode::Ret => break 'fetch_cycle,
          Opcode::Export => self.exec_export(),
          Opcode::DefineGlobal => self.exec_define_global(inst.data())?,
          Opcode::DefineScope => self.exec_define_scope(inst.data())?,
          Opcode::Resolve => self.exec_resolve(inst.data())?,
          Opcode::EnterBlock => self.exec_enter_block(),
          Opcode::PopScope => self.pop_scope(),
          Opcode::Swap => self.exec_swap(inst.data()),
          Opcode::SwapPop => self.exec_swap_pop(),
          Opcode::Is => self.exec_is()?,
          Opcode::Quack => self.exec_quack()?,
          Opcode::Unknown => self.exec_unknown(inst)?,
          Opcode::Breakpoint => self.exec_dbg()?,
        }
        *self.stack_frame.ip += 1;
      }
    }

    match exec_type {
      RunMode::File => {
        let info = self.opened_files.pop().expect("file must be popped when leaving a file");
        if let Some(export) = &self.stack_frame.export {
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

    let export = self.stack_frame.export.take();

    if let Some(stack_frame) = self.stack_frames.pop() {
      // the vm is returning from a function call or req
      self.stack_frame = stack_frame;
    }

    Ok(export.unwrap_or_default())
  }

  /* Operations */

  fn exec_load_stack(&mut self, loc: LongAddr) {
    self.stack_push(self.stack_load_rev(loc));
  }

  fn exec_load_local(&mut self, loc: LongAddr) {
    self.stack_push(self.stack_load(self.stack_frame.sp + loc.0));
  }

  fn exec_load_global(&mut self, loc: LongAddr) -> OpResult {
    let var = self.value_of_ident(loc)?;
    self.stack_push(var);
    Ok(())
  }

  fn exec_store_stack(&mut self, loc: LongAddr) {
    let value = self.stack_pop();
    self.stack_store_rev(loc.0, value);
  }

  fn exec_store_local(&mut self, loc: LongAddr) {
    let value = self.stack_peek();
    self.stack_store(self.stack_frame.sp + loc.0, value);
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

  fn exec_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: Opcode, f: F) -> OpResult {
    self.binary_op(opcode, |a, b| Ok(Value::from(f(a, b))))
  }

  fn do_bool<F: FnOnce(Value, Value) -> bool>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> OpResult {
    self.do_binary_op(opcode, av, bv, |a, b| Ok(Value::from(f(a, b))))
  }

  fn exec_equal_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_not_equal_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_greater_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_greater_equal_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_less_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_less_equal_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_add_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_sub_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_mul_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_div_slow(&mut self, opcode: Opcode) -> OpResult {
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

  fn exec_rem_slow(&mut self, opcode: Opcode) -> OpResult {
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
    *self.stack_frame.ip += offset;
  }

  fn jump_back(&mut self, offset: usize) {
    *self.stack_frame.ip -= offset;
  }

  /// when f evaluates to true, short circuit
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) {
    let value = self.stack_peek();
    if f(value) {
      self.jump(offset);
    } else {
      self.stack_pop();
      *self.stack_frame.ip += 1;
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

  /* Utility Functions */

  fn wrap_err<F, T>(&self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&Self) -> OpResult<T>,
  {
    f(self).map_err(|e| self.error(e))
  }

  fn wrap_err_mut<F, T>(&mut self, f: F) -> ExecResult<T>
  where
    F: FnOnce(&mut Self) -> OpResult<T>,
  {
    f(self).map_err(|e| self.error(e))
  }

  fn unary_op<F>(&mut self, opcode: Opcode, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value) -> Result<Value, UsageError>,
  {
    let value = self.stack_pop();

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
    let bv = self.stack_pop();
    let av = self.stack_pop();
    self.do_binary_op(opcode, av, bv, f)
  }

  fn invoke_binary_op<F>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> Result<Value, UsageError>
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

      bin_op(MutPtr::new(self), av, bv)
    } else {
      f(av, bv)
    }
  }

  fn do_binary_op<F>(&mut self, opcode: Opcode, av: Value, bv: Value, f: F) -> Result<(), UsageError>
  where
    F: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    let value = self.invoke_binary_op(opcode, av, bv, f)?;
    self.stack_push(value);
    Ok(())
  }

  fn load_from_storage(&mut self, st: Storage, addr: impl Into<usize>) -> OpResult<Value> {
    let addr = addr.into();
    match st {
      Storage::Stack => Ok(self.stack_pop()),
      Storage::Local => Ok(self.stack_load(self.stack_frame.sp + addr)),
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
  pub fn stack_pop(&mut self) -> Value {
    self.stack.pop().unwrap()
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

  pub fn stack_load(&self, index: impl Into<usize>) -> Value {
    self.stack[index.into()]
  }

  pub fn stack_load_rev(&self, index: impl Into<usize>) -> Value {
    self.stack[self.stack_size() - 1 - index.into()]
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
        ConstantValue::String(name) => current_module!(self)
          .lookup(name)
          .ok_or_else(|| UsageError::UndefinedVar(name.clone())),
        name => Err(UsageError::InvalidIdentifier(name.to_string()))?,
      },
    }
  }

  fn validate_ident_and<F, T>(&mut self, index: impl Into<usize>, f: F) -> OpResult<T>
  where
    F: FnOnce(&mut Self, String) -> OpResult<T>,
  {
    let index = index.into();
    match self.cache.const_at(index) {
      ConstantValue::String(name) => f(self, name.clone()),
      name => Err(UsageError::InvalidIdentifier(name.to_string()))?,
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

  pub fn check_gc(&mut self) -> ExecResult {
    self.gc.clean_if_time(
      &self.stack,
      &self.modules,
      &mut self.cache,
      &self.stack_frame,
      &self.stack_frames,
    )?;
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
    let instruction = self.stack_frame.ctx.instructions[*self.stack_frame.ip];
    Error::runtime_error(self.error_at(instruction, |opcode_ref| RuntimeError::new(e, &self.filemap, opcode_ref)))
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
      .reflect(inst, *self.stack_frame.ip)
      .map(f)
      .unwrap_or_else(|| RuntimeError {
        msg: UsageError::IpOutOfBounds(*self.stack_frame.ip).to_string(),
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

// ops

impl Vm {
  #[allow(unused)]
  fn exec_disasm(&self, inst: Instruction) {
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
        offset: *self.stack_frame.ip,
      }
    );
  }

  fn exec_pop(&mut self) {
    self.stack_pop();
  }

  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  fn exec_const(&mut self, index: LongAddr) -> ExecResult {
    let c = self.cache.const_at(index);

    let module = current_module!(self).into();
    let value = Value::from_constant(&mut self.gc, module, c);
    self.stack_push(value);
    self.check_gc()?;

    Ok(())
  }

  fn exec_store(&mut self, (storage, addr): (Storage, LongAddr)) -> ExecResult {
    match storage {
      Storage::Stack => self.exec_store_stack(addr),
      Storage::Local => self.exec_store_local(addr),
      Storage::Global => self.wrap_err_mut(|this| this.exec_store_global(addr))?,
    }
    Ok(())
  }

  fn exec_load(&mut self, (storage, addr): (Storage, LongAddr)) -> ExecResult {
    match storage {
      Storage::Stack => self.exec_load_stack(addr),
      Storage::Local => self.exec_load_local(addr),
      Storage::Global => self.wrap_err_mut(|this| this.exec_load_global(addr))?,
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

  fn exec_initialize_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_pop();
      let mut obj = this.stack_peek();
      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        obj.set_member(&mut this.gc, Field::new(loc, name), value)?;
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })?;
    self.check_gc()
  }

  fn exec_assign_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_pop();
      let mut obj = this.stack_pop();

      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        obj.set_member(&mut this.gc, Field::new(loc, name), value.clone())?;
        this.stack_push(value);
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })?;

    self.check_gc()
  }

  fn exec_lookup_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let obj = this.stack_pop();

      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        let member = obj.get_member(&mut this.gc, Field::new(loc, name))?.unwrap_or_default();
        this.stack_push(member);
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })
  }

  fn exec_peek_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_peek();

      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        let member = value.get_member(&mut this.gc, Field::new(loc, name))?.unwrap_or_default();
        this.stack_push(member);
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })?;

    self.check_gc()
  }

  fn exec_initialize_constructor(&mut self) -> ExecResult {
    let value = self.stack_pop();
    let mut obj = self.stack_peek();
    let class = self.wrap_err_mut(|_| obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment))?;
    class.set_constructor(value);
    self.check_gc()
  }

  fn exec_initialize_method(&mut self, loc: usize) -> ExecResult {
    let value = self.stack_pop();
    let mut obj = self.stack_peek();

    let name = self.cache.const_at(loc);

    if let ConstantValue::String(name) = name {
      self.wrap_err(|_| {
        let class = obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment)?;
        let method = value.cast_to::<FunctionValue>().ok_or(UsageError::MethodType)?;
        class.set_method(name, method.clone());
        Ok(())
      })?;
    } else {
      Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
    }

    self.check_gc()
  }

  fn exec_create_vec(&mut self, num_items: usize) -> ExecResult {
    let list = self.stack_drain_from(num_items);
    let list = self.gc.allocate(list);
    self.stack_push(list);
    self.check_gc()
  }

  fn exec_create_sized_vec(&mut self, repeats: usize) -> ExecResult {
    let item = self.stack_pop();
    let vec = vec![item; repeats];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    self.check_gc()
  }

  fn exec_create_dyn_vec(&mut self) -> ExecResult {
    let repeats = self.stack_pop();
    let repeats = self.wrap_err_mut(|_| repeats.cast_to::<i32>().ok_or(UsageError::CoercionError(repeats, "i32")))?;
    let item = self.stack_pop();
    let vec = vec![item; repeats as usize];
    let vec = self.gc.allocate(vec);
    self.stack_push(vec);
    self.check_gc()
  }

  fn exec_create_closure(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| {
      let function = this.stack_pop();
      let captures = this.stack_pop();

      let captures = captures.cast_to::<VecValue>().ok_or(UsageError::CaptureType)?;
      let function = function.cast_to::<FunctionValue>().ok_or(UsageError::ClosureType)?;

      let closure = this.gc.allocate(ClosureValue::new(captures, function.clone()));
      this.stack_push(closure);
      Ok(())
    })?;

    self.check_gc()
  }

  fn exec_create_struct(&mut self, nmem: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let mut members = Vec::with_capacity(nmem);

      for _ in 0..nmem {
        let key = this.stack_pop();
        let value = this.stack_pop();
        let key = key
          .cast_to::<StringValue>()
          .ok_or_else(|| UsageError::InvalidIdentifier(key.to_string()))?;
        let id = this
          .cache
          .strings
          .get_by_right(&**key)
          .cloned()
          .ok_or(UsageError::InvalidIdentifier(key.to_string()))?;
        members.push((((**key).clone(), id), value));
      }

      let struct_value = this.gc.allocate(StructValue::new(members));

      this.stack_push(struct_value);

      Ok(())
    })?;

    self.check_gc()
  }

  fn exec_create_class(&mut self, loc: usize) -> ExecResult {
    let creator = self.stack_pop();
    let name = self.cache.const_at(loc);

    if let ConstantValue::String(name) = name {
      let v = self.gc.allocate(ClassValue::new(name, creator));
      self.stack_push(v);
    } else {
      Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
    }

    self.check_gc()
  }

  fn exec_create_module(&mut self, loc: usize) -> ExecResult {
    let name = self.cache.const_at(loc);

    if let ConstantValue::String(name) = name {
      let leaf = current_module!(self);
      let module = ModuleValue::new_child(name, leaf.handle.value.clone());
      let handle = self.gc.allocate_typed_handle(module);
      self.modules.push(ModuleEntry::Mod(handle.clone()));
      self.stack_push(handle.value());
    } else {
      Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
    }

    self.check_gc()
  }

  fn exec_check(&mut self) -> ExecResult {
    let b = self.stack_pop();
    let a = self.stack_peek();
    self.wrap_err_mut(|this| this.do_binary_op(Opcode::Equal, a, b, |a, b| Ok(Value::from(a == b))))
  }

  fn exec_println(&mut self) {
    let value = self.stack_pop();
    println!("{value}");
  }

  fn exec_jump(&mut self, offset: usize) {
    self.jump(offset);
  }

  fn exec_jump_if_false(&mut self, offset: usize) {
    let value = self.stack_pop();

    if value.truthy() {
      *self.stack_frame.ip += 1;
    } else {
      self.jump(offset);
    }
  }

  fn exec_loop(&mut self, offset: usize) {
    self.jump_back(offset);
  }

  // TODO change calls back to not use recursion
  fn exec_call(&mut self, airity: usize) -> ExecResult {
    let callable = self.stack_load_rev(airity);
    self.wrap_err_mut(|this| this.call_value(callable, airity))
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

    let value = self.stack_pop();

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
      }
    }

    self.check_gc()
  }

  fn exec_dbg(&mut self) -> ExecResult {
    self.dbg()
  }

  fn exec_export(&mut self) {
    let value = self.stack_pop();
    self.stack_frame.export = Some(value);
  }

  fn exec_define_global(&mut self, loc: LongAddr) -> ExecResult {
    self.wrap_err_mut(|this| {
      this.validate_ident_and(loc, |this, name| {
        let v = this.stack_peek();
        if this.globals.insert(name.clone(), v.clone()).is_none() {
          this.cache.add_global(loc, v);
          Ok(())
        } else {
          let level = current_module!(this).search_for(0, &name);
          Err(UsageError::Redefine { level, name })
        }
      })
    })
  }

  fn exec_define_scope(&mut self, loc: LongAddr) -> ExecResult {
    self.wrap_err_mut(|this| {
      this.validate_ident_and(loc, |this, name| {
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
    })
  }

  fn exec_resolve(&mut self, ident: usize) -> ExecResult {
    let obj = self.stack_pop();

    let hit = self.cache.resolve_mod(obj.clone(), ident);

    match hit {
      Some(value) => {
        cache_hit!();
        self.stack_push(value);
      }
      None => {
        let name = self.cache.const_at(ident);

        if let ConstantValue::String(name) = name {
          let value = obj.resolve(name).unwrap();
          self.cache.add_to_mod(obj, ident, value.clone());
          self.stack_push(value);
        } else {
          Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
        }
      }
    }
    Ok(())
  }

  fn exec_enter_block(&mut self) {
    self.push_scope();
  }

  fn exec_pop_scope(&mut self) {
    self.pop_scope();
  }

  fn exec_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_equal_fast(Opcode::Equal, inst.data())
      } else {
        this.exec_equal_slow(Opcode::Equal)
      }
    })
  }

  fn exec_not_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_not_equal_fast(Opcode::NotEqual, inst.data())
      } else {
        this.exec_not_equal_slow(Opcode::NotEqual)
      }
    })
  }

  fn exec_greater(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_greater_fast(Opcode::Greater, inst.data())
      } else {
        this.exec_greater_slow(Opcode::Greater)
      }
    })
  }

  fn exec_greater_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_greater_equal_fast(Opcode::GreaterEqual, inst.data())
      } else {
        this.exec_greater_equal_slow(Opcode::GreaterEqual)
      }
    })
  }

  fn exec_less(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_less_fast(Opcode::Less, inst.data())
      } else {
        this.exec_less_slow(Opcode::Less)
      }
    })
  }

  fn exec_less_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_less_equal_fast(Opcode::LessEqual, inst.data())
      } else {
        this.exec_less_equal_slow(Opcode::LessEqual)
      }
    })
  }

  fn exec_add(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_add_fast(Opcode::Add, inst.data())
      } else {
        this.exec_add_slow(Opcode::Add)
      }
    })
  }

  fn exec_sub(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_sub_fast(Opcode::Sub, inst.data())
      } else {
        this.exec_sub_slow(Opcode::Sub)
      }
    })
  }

  fn exec_mul(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_mul_fast(Opcode::Mul, inst.data())
      } else {
        this.exec_mul_slow(Opcode::Mul)
      }
    })
  }

  fn exec_div(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_div_fast(Opcode::Div, inst.data())
      } else {
        this.exec_div_slow(Opcode::Div)
      }
    })
  }

  fn exec_rem(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_rem_fast(Opcode::Rem, inst.data())
      } else {
        this.exec_rem_slow(Opcode::Rem)
      }
    })
  }

  fn exec_negate(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.unary_op(Opcode::Negate, |v| -v))
  }

  fn exec_not(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.unary_op(Opcode::Not, |v| Ok(!v)))
  }

  fn exec_index(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.binary_op(Opcode::Index, |_, _| Err(UsageError::InvalidBinary)))
  }

  fn exec_index_assign(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_pop();
      let index = this.stack_pop();
      let indexable = this.stack_pop();
      (indexable.vtable().assign_index)(MutPtr::new(this), indexable, index, value.clone())?;
      this.stack_push(value);
      Ok(())
    })
  }

  fn exec_or(&mut self, offset: usize) {
    self.exec_logical(offset, |v| v.truthy())
  }

  fn exec_and(&mut self, offset: usize) {
    self.exec_logical(offset, |v| v.falsy());
  }

  fn exec_swap(&mut self, (addr_a, addr_b): (ShortAddr, ShortAddr)) {
    let st_sz = self.stack_size() - 1;
    self.stack.swap(st_sz - addr_a.0, st_sz - addr_b.0);
  }

  fn exec_swap_pop(&mut self) {
    let idx = self.stack_size() - 2;
    self.stack.swap_remove(idx);
  }

  fn exec_is(&mut self) -> ExecResult {
    let right = self.stack_pop();
    let left = self.stack_pop();

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

  fn exec_quack(&mut self) -> ExecResult {
    let value = self.stack_pop();
    Err(self.error(UsageError::Quack(value)))
  }

  fn exec_unknown(&self, inst: Instruction) -> ExecResult {
    Err(self.error(UsageError::InvalidInstruction(inst)))
  }
}

#[no_mangle]
#[allow(unused_variables)]
pub extern "C" fn exec_disasm(vm: &Vm, inst: Instruction) {
  #[cfg(feature = "runtime-disassembly")]
  {
    vm.exec_disasm(inst);
  }
}

#[no_mangle]
pub extern "C" fn exec_pop(vm: &mut Vm) {
  vm.exec_pop();
}

#[no_mangle]
pub extern "C" fn exec_pop_n(vm: &mut Vm, inst: Instruction) {
  vm.exec_pop_n(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_const(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_const(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_store(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_store(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_load(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_load(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_nil(vm: &mut Vm) {
  vm.exec_nil()
}

#[no_mangle]
pub extern "C" fn exec_true(vm: &mut Vm) {
  vm.exec_true();
}

#[no_mangle]
pub extern "C" fn exec_false(vm: &mut Vm) {
  vm.exec_false();
}

#[no_mangle]
pub extern "C" fn exec_add(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_add(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_sub(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_sub(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_mul(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_mul(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_div(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_div(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_rem(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_rem(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_not_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_not_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_greater(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_greater(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_greater_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_greater_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_less(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_less(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_less_equal(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_less_equal(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_index(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_index();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_index_assign(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_index_assign();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_negate(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_negate();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_not(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_not();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_or(vm: &mut Vm, inst: Instruction) {
  vm.exec_or(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_and(vm: &mut Vm, inst: Instruction) {
  vm.exec_and(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_initialize_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_initialize_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_assign_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_assign_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_lookup_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_lookup_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_peek_member(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_peek_member(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_initialize_constructor(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_initialize_constructor();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_initialize_method(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_initialize_method(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_vec(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_vec(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_sized_vec(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_sized_vec(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_dyn_vec(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_create_dyn_vec();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_closure(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_create_closure();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_struct(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_struct(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_create_class(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_class(inst.data());
  vm.last_error.is_ok()
}

/// Create a module and make it the current
#[no_mangle]
pub extern "C" fn exec_create_module(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_create_module(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_check(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_check();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_println(vm: &mut Vm) {
  vm.exec_println();
}

#[no_mangle]
pub extern "C" fn exec_jump(vm: &mut Vm, inst: Instruction) {
  vm.exec_jump(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_jump_if_false(vm: &mut Vm, inst: Instruction) {
  vm.exec_jump_if_false(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_loop(vm: &mut Vm, inst: Instruction) {
  vm.exec_loop(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_call(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_call(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_req(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_req();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_export(vm: &mut Vm) {
  vm.exec_export();
}

#[no_mangle]
pub extern "C" fn exec_define_global(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_define_global(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_define_scope(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_define_scope(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_resolve(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_resolve(inst.data());
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_enter_block(vm: &mut Vm) {
  vm.exec_enter_block();
}

#[no_mangle]
pub extern "C" fn exec_pop_scope(vm: &mut Vm) {
  vm.exec_pop_scope();
}

#[no_mangle]
pub extern "C" fn exec_swap(vm: &mut Vm, inst: Instruction) {
  vm.exec_swap(inst.data());
}

#[no_mangle]
pub extern "C" fn exec_swap_pop(vm: &mut Vm) {
  vm.exec_swap_pop();
}

#[no_mangle]
pub extern "C" fn exec_is(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_is();
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_quack(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_quack();
  vm.last_error.is_ok()
}

#[cold]
#[no_mangle]
pub extern "C" fn exec_unknown(vm: &mut Vm, inst: Instruction) -> bool {
  vm.last_error = vm.exec_unknown(inst);
  vm.last_error.is_ok()
}

#[no_mangle]
pub extern "C" fn exec_dbg(vm: &mut Vm) -> bool {
  vm.last_error = vm.exec_dbg();
  vm.last_error.is_ok()
}
