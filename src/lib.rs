#[cfg(feature = "jtbl")]
mod bindings;
#[cfg(feature = "jtbl")]
mod c;

pub(crate) mod code;
pub(crate) mod dbg;
pub mod error;
pub(crate) mod exec;
mod memory;
pub mod stdlib;
mod util;
pub(crate) mod value;

pub mod prelude {
  pub use super::error::*;
  pub use super::exec::prelude::*;
  pub use super::memory::*;
  pub use super::stdlib;
  pub use super::value::prelude::*;
  pub use super::{MakeValueFrom, Vm};
  pub use macros::*;
  pub use ptr::SmartPtr;
}

pub mod macro_requirements {
  pub use crate::prelude::{
    methods, native, Args, Fields, MakeValueFrom, MaybeFrom, ModuleBuilder, Operators, TryUnwrapArg, UsageError, UsageResult,
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
use ahash::RandomState;
use clap::Parser;
use code::CompileOpts;
use dlopen2::wrapper::Container;
use dlopen2::wrapper::WrapperApi;
use memory::Gc;
use prelude::module_value::ModuleType;
use ptr::{MutPtr, SmartPtr};
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::{BTreeMap, HashMap, HashSet},
  fs, mem,
  path::{Path, PathBuf},
};
use value::{NativeBinaryOp, NativeUnaryOp, VTable};

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

  pub gc: SmartPtr<Gc>,

  cache: Cache,
  pub(crate) modules: ModuleStack,

  opt: bool,
  args: Vec<String>,

  filemap: FileMap,

  // usize for what frame to pop on, string for file path
  opened_files: Vec<FileInfo>,
  opened_native_libs: BTreeMap<PathBuf, Container<NativeApi>>,

  #[cfg(feature = "jtbl")]
  last_error: ExecResult,
}

/* core functionality */

impl Vm {
  pub fn new(gc: SmartPtr<Gc>, opt: bool, args: impl Into<Vec<String>>) -> Self {
    Self {
      stack_frame: Default::default(),
      stack_frames: Default::default(),
      stack: Stack::with_capacity(INITIAL_STACK_SIZE),
      gc,
      cache: Default::default(),
      modules: Default::default(),
      opt,
      args: args.into(),
      filemap: Default::default(),
      opened_files: Default::default(),
      opened_native_libs: Default::default(),

      #[cfg(feature = "jtbl")]
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

  pub fn eval(&mut self, source: impl AsRef<str>) -> Result<Value, Error> {
    let opts = CompileOpts { optimize: self.opt };
    let ctx = code::compile_string(&mut self.cache, source, opts)?;
    let module = current_module!(self).clone();
    self.modules.push(ModuleEntry::String(module));
    self.new_frame(ctx, 0);
    self.execute(RunMode::String)
  }

  pub(crate) fn execute(&mut self, exec_type: RunMode) -> ExecResult<Value> {
    #[cfg(feature = "jtbl")]
    {
      unsafe {
        bindings::duck_type_execute(
          self as *mut Vm as *mut std::ffi::c_void,
          self.ctx().instructions.as_ptr() as *const u64,
          self.stack_frame.ip_ptr(),
        )
      };

      let mut result = Ok(());
      mem::swap(&mut result, &mut self.last_error);
      result?;
    }

    #[cfg(not(feature = "jtbl"))]
    {
      'fetch_cycle: loop {
        let inst = self.stack_frame.ctx.fetch(self.stack_frame.ip());
        self.exec_disasm(inst);
        match inst.opcode() {
          Opcode::Pop => self.exec_pop(),
          Opcode::PopN => self.exec_pop_n(inst.data()),
          Opcode::Const => self.exec_const(inst.data()),
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
          Opcode::CreateVec => self.exec_create_vec(inst.data()),
          Opcode::CreateSizedVec => self.exec_create_sized_vec(inst.data()),
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
          Opcode::EnableModule => self.exec_enable_module()?,
          Opcode::PopScope => self.exec_pop_scope(),
          Opcode::Swap => self.exec_swap(inst.data()),
          Opcode::SwapPop => self.exec_swap_pop(),
          Opcode::Is => self.exec_is()?,
          Opcode::Quack => self.exec_quack()?,
          Opcode::Unknown => self.exec_unknown(inst)?,
          Opcode::Breakpoint => self.exec_dbg()?,
        }

        self.stack_frame.ip_inc(1);
      }
    }

    match exec_type {
      RunMode::File => {
        let info = self.opened_files.pop().expect("file must be popped when leaving a file");
        if let Some(export) = self.stack_frame.export {
          self.cache.add_lib(info.id, export);
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

  pub fn check_gc(&mut self) {
    self.check_gc_inc();
    self.gc.poll_deep(
      &self.stack,
      &self.modules,
      &mut self.cache,
      &self.stack_frame,
      &self.stack_frames,
    );
  }

  pub fn check_gc_inc(&mut self) {
    self.gc.poll_inc(
      &self.stack,
      &self.modules,
      &mut self.cache,
      &self.stack_frame,
      &self.stack_frames,
    );
  }

  pub fn force_gc(&mut self) -> usize {
    self.gc.deep_clean(
      &self.stack,
      &self.modules,
      &mut self.cache,
      &self.stack_frame,
      &self.stack_frames,
    )
  }

  pub fn get_global(&self, name: &str) -> Option<Value> {
    self.cache.get_global_by_name(name)
  }

  pub fn set_global(&mut self, name: impl ToString, value: impl Into<Value>) {
    let name = name.to_string();
    let value = value.into();

    let id = self.cache.add_const(&name);
    self.cache.set_global(id, value);
  }

  #[cold]
  fn error(&self, e: UsageError) -> Error {
    let instruction = self.stack_frame.ctx.instructions[self.stack_frame.ip()];
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
      .reflect(inst, self.stack_frame.ip())
      .map(f)
      .unwrap_or_else(|| RuntimeError {
        msg: UsageError::IpOutOfBounds(self.stack_frame.ip()).to_string(),
        file: self.filemap.get(self.stack_frame.ctx.meta.file_id),
        line: 0,
        column: 0,
        nested: false,
      })
  }
}

// ops

impl Vm {
  #[allow(unused)]
  fn exec_disasm(&self, inst: Instruction) {
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
          offset: self.stack_frame.ip(),
        }
      );
    }
  }

  fn exec_pop(&mut self) {
    self.stack_pop();
  }

  fn exec_pop_n(&mut self, count: usize) {
    self.stack_pop_n(count);
  }

  fn exec_const(&mut self, index: LongAddr) {
    let c = self.cache.const_at(index).clone();
    let value = self.make_value_from(c);
    self.stack_push(value);
    self.check_gc_inc();
  }

  fn exec_store(&mut self, (storage, addr): (Storage, LongAddr)) -> ExecResult {
    match storage {
      Storage::Stack => self.exec_store_stack(addr),
      Storage::Local => self.exec_store_local(addr),
      Storage::Global => self.wrap_err_mut(|this| this.exec_store_global(addr))?,
    }
    self.check_gc_inc();
    Ok(())
  }

  fn exec_load(&mut self, (storage, addr): (Storage, LongAddr)) -> ExecResult {
    match storage {
      Storage::Stack => self.exec_load_stack(addr),
      Storage::Local => self.exec_load_local(addr),
      Storage::Global => self.wrap_err_mut(|this| this.exec_load_global(addr))?,
    }
    self.check_gc_inc();
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
        // TODO this is a hacky workaround the borrow checker not understanding
        // the cache will not be modified on member sets
        let name = ptr::ConstPtr::new(name);
        obj.set_member(this, Field::new(loc, name.as_str()), value)?;
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })
  }

  fn exec_assign_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_pop();
      let mut obj = this.stack_pop();

      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        let name = ptr::ConstPtr::new(name);
        obj.set_member(this, Field::new(loc, name.as_str()), value)?;
        this.stack_push(value);
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })
  }

  fn exec_lookup_member(&mut self, loc: usize) -> ExecResult {
    self.wrap_err_mut(|this| {
      let obj = this.stack_pop();

      let name = this.cache.const_at(loc);

      if let ConstantValue::String(name) = name {
        let name = ptr::ConstPtr::new(name);
        let member = obj.get_member(this, Field::new(loc, name.as_str()))?.unwrap_or_default();
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
        let name = ptr::ConstPtr::new(name);
        let member = value.get_member(this, Field::new(loc, name.as_str()))?.unwrap_or_default();
        this.stack_push(member);
        Ok(())
      } else {
        Err(UsageError::InvalidIdentifier(name.to_string()))
      }
    })
  }

  fn exec_initialize_constructor(&mut self) -> ExecResult {
    let value = self.stack_pop();
    let mut obj = self.stack_peek();
    let class = self.wrap_err_mut(|_| obj.cast_to_mut::<ClassValue>().ok_or(UsageError::MethodAssignment))?;
    class.set_constructor(value);
    self.check_gc();
    Ok(())
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

    self.check_gc();
    Ok(())
  }

  fn exec_create_vec(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    let list = self.make_value_from(list);
    self.stack_push(list);
    self.check_gc();
  }

  fn exec_create_sized_vec(&mut self, repeats: usize) {
    let item = self.stack_pop();
    let vec = vec![item; repeats];
    let vec = self.make_value_from(vec);
    self.stack_push(vec);
    self.check_gc();
  }

  fn exec_create_dyn_vec(&mut self) -> ExecResult {
    let repeats = self.stack_pop();
    let repeats = self.wrap_err_mut(|_| repeats.cast_to::<i32>().ok_or(UsageError::CoercionError(repeats, "i32")))?;
    let item = self.stack_pop();
    let vec = vec![item; repeats as usize];
    let vec = self.make_value_from(vec);
    self.stack_push(vec);
    self.check_gc();
    Ok(())
  }

  fn exec_create_closure(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| {
      let function = this.stack_pop();
      let captures = this.stack_pop();

      let captures = captures.cast_to::<VecValue>().ok_or(UsageError::CaptureType)?;
      let function = function.cast_to::<FunctionValue>().ok_or(UsageError::ClosureType)?;

      let closure = this.make_value_from(ClosureValue::new(captures, function.clone()));
      this.stack_push(closure);
      Ok(())
    })?;

    self.check_gc();
    Ok(())
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

      let struct_value = this.make_value_from(StructValue::new(members));

      this.stack_push(struct_value);

      Ok(())
    })?;

    self.check_gc();
    Ok(())
  }

  fn exec_create_class(&mut self, loc: usize) -> ExecResult {
    let creator = self.stack_pop();
    let name = self.cache.const_at(loc);

    if let ConstantValue::String(name) = name {
      let v = self.make_value_from(ClassValue::new(name, creator));
      self.stack_push(v);
    } else {
      Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
    }

    self.check_gc();
    Ok(())
  }

  fn exec_create_module(&mut self, loc: usize) -> ExecResult {
    let name = self.cache.const_at(loc);

    if let ConstantValue::String(name) = name {
      let leaf = current_module!(self);
      let module = ModuleValue::new_child(name, leaf.handle.value);
      let value = self.make_value_from(module);
      self.stack_push(value);
    } else {
      Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
    }

    self.check_gc();
    Ok(())
  }

  fn exec_check(&mut self) -> ExecResult {
    let b = self.stack_pop();
    let a = self.stack_peek();
    self.wrap_err_mut(|this| this.do_binary_op(a, b, |vt| vt.eq, |a, b| Ok(Value::from(a.equals(b)))))
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
      self.stack_frame.ip_inc(1);
    } else {
      self.jump(offset);
    }
  }

  fn exec_loop(&mut self, offset: usize) {
    self.jump_back(offset);
  }

  fn exec_call(&mut self, airity: usize) -> ExecResult {
    let callable = self.stack_load_rev(airity);
    self.wrap_err_mut(|this| this.call_value(callable, airity))?;
    self.check_gc();
    Ok(())
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
        let value = self.cache.get_lib(file_id).unwrap_or_else(|| {
          let lib: Container<NativeApi> =
            unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

          let value: Value = lib.duck_type_load_module(self).into();
          self.opened_native_libs.insert(found_file, lib);
          self.cache.add_lib(file_id, value);
          value
        });

        self.stack_push(value);
      }
      _ => {
        let source = fs::read_to_string(&found_file).map_err(Error::other_system_err)?;

        if let Some(value) = self.cache.get_lib(file_id) {
          self.stack_push(value);
        } else {
          self.filemap.add(file_id, &found_file);

          let opts = CompileOpts { optimize: self.opt };
          let new_ctx =
            code::compile_file(&mut self.cache, file_id, source, opts).map_err(|e| e.with_filename(&self.filemap))?;
          let gmod = self.generate_stdlib(format!("<file export {}>", found_file.display()));

          self.new_frame(new_ctx, 0);
          self.modules.push(ModuleEntry::File(gmod));
          self.opened_files.push(FileInfo::new(found_file, file_id));
          let output = self.execute(RunMode::File)?;
          self.stack_push(output);
        }
      }
    }

    self.check_gc();
    Ok(())
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
        let value = this.stack_peek();
        if this.cache.set_global(loc, value) {
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
        let value = this.stack_peek();
        let module = current_module_mut!(this);
        if module.define(name.clone(), value) {
          this.cache.add_to_mod(module.value(), loc, value);
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

    let hit = self.cache.resolve_mod(obj, ident);

    match hit {
      Some(value) => {
        cache_hit!();
        self.stack_push(value);
      }
      None => {
        let name = self.cache.const_at(ident);

        if let ConstantValue::String(name) = name {
          let value = self.wrap_err(|_| obj.resolve(name))?;
          self.cache.add_to_mod(obj, ident, value);
          self.stack_push(value);
        } else {
          Err(self.error(UsageError::InvalidIdentifier(name.to_string())))?;
        }
      }
    }
    Ok(())
  }

  fn exec_enable_module(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_peek();
      let handle = this
        .maybe_make_usertype_handle::<ModuleValue>(value)
        .ok_or(UsageError::InvalidModule(value))?;
      this.modules.push(ModuleEntry::Mod(handle));
      Ok(())
    })
  }

  fn exec_pop_scope(&mut self) {
    self.pop_scope();
  }

  fn exec_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_equal_fast(inst.data())
      } else {
        this.exec_equal_slow()
      }
    })
  }

  fn exec_not_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_not_equal_fast(inst.data())
      } else {
        this.exec_not_equal_slow()
      }
    })
  }

  fn exec_greater(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_greater_fast(inst.data())
      } else {
        this.exec_greater_slow()
      }
    })
  }

  fn exec_greater_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_greater_equal_fast(inst.data())
      } else {
        this.exec_greater_equal_slow()
      }
    })
  }

  fn exec_less(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_less_fast(inst.data())
      } else {
        this.exec_less_slow()
      }
    })
  }

  fn exec_less_equal(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_less_equal_fast(inst.data())
      } else {
        this.exec_less_equal_slow()
      }
    })
  }

  fn exec_add(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_add_fast(inst.data())
      } else {
        this.exec_add_slow()
      }
    })
  }

  fn exec_sub(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_sub_fast(inst.data())
      } else {
        this.exec_sub_slow()
      }
    })
  }

  fn exec_mul(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_mul_fast(inst.data())
      } else {
        this.exec_mul_slow()
      }
    })
  }

  fn exec_div(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_div_fast(inst.data())
      } else {
        this.exec_div_slow()
      }
    })
  }

  fn exec_rem(&mut self, inst: Instruction) -> ExecResult {
    self.wrap_err_mut(|this| {
      if inst.has_data() {
        this.exec_rem_fast(inst.data())
      } else {
        this.exec_rem_slow()
      }
    })
  }

  fn exec_negate(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.unary_op(|vt| vt.neg, |v| -v))
  }

  fn exec_not(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.unary_op(|vt| vt.not, |v| Ok(!v)))
  }

  fn exec_index(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| this.binary_op(|vt| vt.index, |_, _| Err(UsageError::InvalidBinary)))
  }

  fn exec_index_assign(&mut self) -> ExecResult {
    self.wrap_err_mut(|this| {
      let value = this.stack_pop();
      let index = this.stack_pop();
      let indexable = this.stack_pop();
      (indexable.vtable().assign_index)(MutPtr::new(this), indexable, index, value)?;
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

/* op delegates */

impl Vm {
  fn unary_op<F1, F2>(&mut self, f1: F1, f2: F2) -> Result<(), UsageError>
  where
    F1: FnOnce(&VTable) -> NativeUnaryOp,
    F2: FnOnce(Value) -> Result<Value, UsageError>,
  {
    let value = self.stack_pop();

    if value.is_ptr() {
      let vtable = value.vtable();
      let unary_op = f1(vtable);
      let value = unary_op(MutPtr::new(self), value)?;
      self.stack_push(value);
      Ok(())
    } else {
      self.stack_push(f2(value)?);
      Ok(())
    }
  }

  fn binary_op<F1, F2>(&mut self, f1: F1, f2: F2) -> Result<(), UsageError>
  where
    F1: FnOnce(&VTable) -> NativeBinaryOp,
    F2: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    let bv = self.stack_pop();
    let av = self.stack_pop();
    self.do_binary_op(av, bv, f1, f2)
  }

  fn do_binary_op<F1, F2>(&mut self, av: Value, bv: Value, f1: F1, f2: F2) -> Result<(), UsageError>
  where
    F1: FnOnce(&VTable) -> NativeBinaryOp,
    F2: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    let value = self.invoke_binary_op(av, bv, f1, f2)?;
    self.stack_push(value);
    Ok(())
  }

  fn invoke_binary_op<F1, F2>(&mut self, av: Value, bv: Value, f1: F1, f2: F2) -> Result<Value, UsageError>
  where
    F1: FnOnce(&VTable) -> NativeBinaryOp,
    F2: FnOnce(Value, Value) -> Result<Value, UsageError>,
  {
    if av.is_ptr() {
      let vtable = av.vtable();
      let bin_op = f1(vtable);
      bin_op(MutPtr::new(self), av, bv)
    } else {
      f2(av, bv)
    }
  }

  fn bool_op<F1, F2>(&mut self, f1: F1, f2: F2) -> OpResult
  where
    F1: FnOnce(&VTable) -> NativeBinaryOp,
    F2: FnOnce(Value, Value) -> bool,
  {
    self.binary_op(f1, |a, b| Ok(Value::from(f2(a, b))))
  }

  fn do_bool<F1, F2>(&mut self, av: Value, bv: Value, f1: F1, f2: F2) -> OpResult
  where
    F1: FnOnce(&VTable) -> NativeBinaryOp,
    F2: FnOnce(Value, Value) -> bool,
  {
    self.do_binary_op(av, bv, f1, |a, b| Ok(Value::from(f2(a, b))))
  }

  fn call_value(&mut self, mut callable: Value, airity: usize) -> Result<(), UsageError> {
    let value = callable.call(self, airity)?;
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

      if !this.cache.set_global(loc, value) {
        Ok(())
      } else {
        Err(UsageError::UndefinedVar(name))
      }
    })
  }

  fn exec_add_slow(&mut self) -> OpResult {
    self.binary_op(|vt| vt.add, |a, b| a + b)
  }

  fn exec_add_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(av, bv, |vt| vt.add, |a, b| a + b)
  }

  fn exec_sub_slow(&mut self) -> OpResult {
    self.binary_op(|vt| vt.sub, |a, b| a - b)
  }

  fn exec_sub_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(av, bv, |vt| vt.sub, |a, b| a - b)
  }

  fn exec_mul_slow(&mut self) -> OpResult {
    self.binary_op(|vt| vt.mul, |a, b| a * b)
  }

  fn exec_mul_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(av, bv, |vt| vt.mul, |a, b| a * b)
  }

  fn exec_div_slow(&mut self) -> OpResult {
    self.binary_op(|vt| vt.div, |a, b| a / b)
  }

  fn exec_div_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(av, bv, |vt| vt.div, |a, b| a / b)
  }

  fn exec_rem_slow(&mut self) -> OpResult {
    self.binary_op(|vt| vt.rem, |a, b| a % b)
  }

  fn exec_rem_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_binary_op(av, bv, |vt| vt.rem, |a, b| a % b)
  }

  fn exec_equal_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.eq, |a, b| a.equals(b))
  }

  fn exec_equal_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.eq, |a, b| a.equals(b))
  }

  fn exec_not_equal_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.neq, |a, b| a.not_equals(b))
  }

  fn exec_not_equal_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.neq, |a, b| a.not_equals(b))
  }

  fn exec_greater_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.greater, |a, b| a.greater_than(b))
  }

  fn exec_greater_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.greater, |a, b| a.greater_than(b))
  }

  fn exec_greater_equal_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.geq, |a, b| a.greater_equal(b))
  }

  fn exec_greater_equal_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.geq, |a, b| a.greater_equal(b))
  }

  fn exec_less_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.less, |a, b| a.less_than(b))
  }

  fn exec_less_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.less, |a, b| a.less_than(b))
  }

  fn exec_less_equal_slow(&mut self) -> OpResult {
    self.bool_op(|vt| vt.leq, |a, b| a.less_equal(b))
  }

  fn exec_less_equal_fast(&mut self, (st_a, addr_a, st_b, addr_b): (Storage, ShortAddr, Storage, ShortAddr)) -> OpResult {
    let av = self.load_from_storage(st_a, addr_a)?;
    let bv = self.load_from_storage(st_b, addr_b)?;
    self.do_bool(av, bv, |vt| vt.leq, |a, b| a.less_equal(b))
  }

  fn jump(&mut self, offset: usize) {
    self.stack_frame.ip_inc(offset);
  }

  fn jump_back(&mut self, offset: usize) {
    self.stack_frame.ip_dec(offset);
  }

  /// when f evaluates to true, short circuit
  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) {
    let value = self.stack_peek();
    if f(value) {
      self.jump(offset);
    } else {
      self.stack_pop();
      self.stack_frame.ip_inc(1);
    }
  }

  fn pop_scope(&mut self) {
    self.modules.pop();
  }
}

/* Utility Functions */

impl Vm {
  pub fn generate_stdlib(&mut self, global_mod_name: impl ToString) -> UsertypeHandle<ModuleValue> {
    let args = self.args.clone();
    ModuleBuilder::initialize(self, ModuleType::new_global(global_mod_name), |vm, mut lib| {
      let libval = lib.value();
      lib.env.extend(stdlib::make_stdlib(vm, libval, args));
    })
  }

  pub fn maybe_make_usertype_handle<T: Usertype>(&mut self, value: Value) -> Option<UsertypeHandle<T>> {
    value.is::<T>().then(|| UsertypeHandle::new(self.make_handle(value)))
  }

  pub fn make_usertype_handle_from<T: Usertype>(&mut self, item: T) -> UsertypeHandle<T> {
    let value = self.gc.allocate_untracked(item);
    let handle = self.cache.make_handle(value);
    UsertypeHandle::new(handle)
  }

  fn make_handle(&mut self, value: Value) -> ValueHandle {
    self.gc.forget(value);
    self.cache.make_handle(value)
  }

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

  /// Tries to find the cached value at the given location and if not performs a slow lookup
  ///
  /// Since primitives can be globals, caching global values accessed via modules is not possible
  /// with how things are currently implemented
  ///
  /// In short, primitive globals cached in the current module won't have their value updated
  /// if modified outside of that scope
  fn value_of_ident(&self, ident_loc: impl Into<usize>) -> OpResult<Value> {
    let ident = ident_loc.into();
    let module = current_module!(self);
    let hit = self.cache.find_var(module, ident);
    match hit {
      Some(hit) => {
        cache_hit!();
        Ok(hit)
      }
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

  pub fn stack_display(&self) {
    println!("{}", self.stack);
    println!(
      "               | ip: {ip} sp: {sp}",
      ip = self.stack_frame.ip(),
      sp = self.stack_frame.sp
    );
  }
}

pub trait MakeValueFrom<T> {
  fn make_value_from(&mut self, item: T) -> Value;
}

impl MakeValueFrom<Value> for Vm {
  fn make_value_from(&mut self, item: Value) -> Value {
    item
  }
}

impl MakeValueFrom<&Value> for Vm {
  fn make_value_from(&mut self, item: &Value) -> Value {
    *item
  }
}

impl MakeValueFrom<()> for Vm {
  fn make_value_from(&mut self, item: ()) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<i32> for Vm {
  fn make_value_from(&mut self, item: i32) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<&i32> for Vm {
  fn make_value_from(&mut self, item: &i32) -> Value {
    self.make_value_from(*item)
  }
}

impl MakeValueFrom<f64> for Vm {
  fn make_value_from(&mut self, item: f64) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<&f64> for Vm {
  fn make_value_from(&mut self, item: &f64) -> Value {
    self.make_value_from(*item)
  }
}

impl MakeValueFrom<bool> for Vm {
  fn make_value_from(&mut self, item: bool) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<&bool> for Vm {
  fn make_value_from(&mut self, item: &bool) -> Value {
    self.make_value_from(*item)
  }
}

impl MakeValueFrom<char> for Vm {
  fn make_value_from(&mut self, item: char) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<&char> for Vm {
  fn make_value_from(&mut self, item: &char) -> Value {
    self.make_value_from(*item)
  }
}

impl MakeValueFrom<NativeFn> for Vm {
  fn make_value_from(&mut self, item: NativeFn) -> Value {
    Value::from(item)
  }
}

impl MakeValueFrom<ConstantValue> for Vm {
  fn make_value_from(&mut self, item: ConstantValue) -> Value {
    match item {
      ConstantValue::Integer(v) => self.make_value_from(v),
      ConstantValue::Float(v) => self.make_value_from(v),
      ConstantValue::String(v) => self.make_value_from(v),
      ConstantValue::StaticString(v) => self.make_value_from(v),
      ConstantValue::Fn(v) => {
        let env = current_module!(self).into();
        let env = self.make_value_from(ModuleValue::new_scope(env));
        self.make_value_from(FunctionValue::from_constant(v, env))
      }
    }
  }
}

impl MakeValueFrom<&str> for Vm {
  fn make_value_from(&mut self, item: &str) -> Value {
    self.gc.allocate::<StringValue>(item.into())
  }
}

impl MakeValueFrom<String> for Vm {
  fn make_value_from(&mut self, item: String) -> Value {
    self.gc.allocate::<StringValue>(item.into())
  }
}

impl MakeValueFrom<&String> for Vm {
  fn make_value_from(&mut self, item: &String) -> Value {
    self.gc.allocate::<StringValue>(item.clone().into())
  }
}

impl MakeValueFrom<&[Value]> for Vm {
  fn make_value_from(&mut self, item: &[Value]) -> Value {
    self.gc.allocate(VecValue::new_from_slice(item))
  }
}

impl MakeValueFrom<Vec<Value>> for Vm {
  fn make_value_from(&mut self, item: Vec<Value>) -> Value {
    self.gc.allocate(VecValue::new_from_vec(item))
  }
}

impl<T> MakeValueFrom<T> for Vm
where
  T: Usertype,
{
  fn make_value_from(&mut self, item: T) -> Value {
    self.gc.allocate::<T>(item)
  }
}

impl<T> MakeValueFrom<Vec<T>> for Vm
where
  T: Usertype,
{
  fn make_value_from(&mut self, item: Vec<T>) -> Value {
    let list = item.into_iter().map(|v| self.gc.allocate(v)).collect();
    self.gc.allocate(VecValue::new_from_vec(list))
  }
}
