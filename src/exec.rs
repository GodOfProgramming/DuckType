mod env;
pub mod memory;

use crate::{
  code::{Compiler, ConstantValue, OpCodeReflection},
  dbg::{Cli, RuntimeErrors},
  prelude::*,
  util::{FileIdType, FileMetadata, PlatformMetadata},
  UnwrapAnd,
};
use clap::Parser;
use dlopen2::wrapper::{Container, WrapperApi};
use memory::{Allocation, Gc};
use ptr::SmartPtr;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
  collections::BTreeMap,
  fmt::{self, Debug, Formatter},
  fs,
  path::{Path, PathBuf},
  rc::Rc,
  time::{Duration, Instant},
};

#[derive(WrapperApi)]
struct NativeApi {
  simple_script_load_module: fn(vm: &mut Vm) -> ValueResult,
}

pub mod prelude {
  pub use super::env::prelude::*;
  pub use super::memory::*;
  pub use super::{Opcode, Vm};
}

type ExecResult<T = ()> = Result<T, RuntimeErrors>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
  /** No operation instruction */
  NoOp,
  /** Looks up a constant value at the specified location. Location is specified by the tuple */
  Const(usize),
  /** Pushes a nil value on to the stack */
  Nil,
  /** Pushes a true value on to the stack */
  True,
  /** Pushes a false value on to the stack */
  False,
  /** Pops a value off the stack */
  Pop,
  /** Pops N values off the stack. N is specified by tuple */
  PopN(usize),
  /** Looks up a local variable. The index in the stack is specified by the modifying bits */
  LookupLocal(usize),
  /** Assigns a value to the local variable indexed by the tuple. The value comes off the top of the stack */
  AssignLocal(usize),
  /** Assigns to a global, defining it if it already doesn't exist. The name is stored in the enum. The value comes off the top of the stack */
  ForceAssignGlobal(usize),
  /** Defines a new global variable. The name is stored in the enum. The value comes off the top of the stack */
  DefineGlobal(usize),
  /** Looks up a global variable. The name is stored in the enum */
  LookupGlobal(usize),
  /** Assigns a value to the global variable. The Name is stored in the enum. The value comes off the top of the stack */
  AssignGlobal(usize),
  /** Defines a member on an object type. The first item popped off the stack is the value. The object is next which is left on for further assignments. The member name is specified by the modifying bits */
  AssignMember(usize),
  /** Initializes a member of an object, keeping the object on the stack for further assignments */
  InitializeMember(usize),
  /** Initializes a method on a class, keeping the class on the stack for further assignments */
  InitializeMethod(usize),
  /** Initializes the constructor on a class, keeping the class on the stack for further assignments */
  InitializeConstructor,
  /** Uses the constant pointed to by the modifying bits to lookup a value on the next item on the stack */
  LookupMember(usize),
  /** Uses the constant pointed to by the modifying bits to peek at a value on the next item on the stack */
  PeekMember(usize),
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
  /** Pops a value off the stack, and compars it with the peeked value, pushing the new value on */
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
  Or(usize),
  /** Peeks at the stack, if the top value is false short circuits to the instruction pointed to by the tuple */
  And(usize),
  /** Pops a value off the stack, inverts its truthy value, then pushes that back on */
  Not,
  /** Pops a value off the stack, inverts its numerical value, then pushes that back on */
  Negate,
  /** Pops a value off the stack and prints it to the screen */
  Print,
  /** Jumps to a code location indicated by the tuple */
  Jump(usize),
  /** Jumps to a code location indicated by the tuple */
  JumpIfFalse(usize),
  /** Jumps the instruction pointer backwards N instructions. N specified by the tuple */
  Loop(usize),
  /** Calls the value on the stack. Number of arguments is specified by the modifying bits */
  Invoke(usize),
  /** Exits from a function, returning nil on the previous frame */
  Ret,
  /** Exits from a function, returning the last value on the stack to the previous frame */
  RetValue,
  /** Returns the first item on the stack which is self */
  RetSelf,
  /** Require an external file. The file name is the top of the stack. Must be a string or convertible to */
  Req,
  /** Create a list of values and push it on the stack. Items come off the top of the stack and the number is specified by the modifying bits */
  CreateList(usize),
  /** Create a closure. The first item on the stack is the function itself, the second is the capture list  */
  CreateClosure,
  /** Create a new struct */
  CreateStruct,
  /** Create a new class */
  CreateClass,
  /** Create a new module */
  CreateModule,
  /** Halt the VM when this instruction is reached and enter repl mode */
  Breakpoint,
  /** Mark the current value as exported */
  Export,
  /** Defines the identifier on the variable */
  Define(usize),
  /** Resolve the specified identifier */
  Resolve(usize),
  /** Push a new env */
  EnterBlock,
  /** Pop an env */
  PopScope,
}

#[cfg(test)]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_nanos(100);

#[cfg(not(test))]
const DEFAULT_GC_FREQUENCY: Duration = Duration::from_millis(16);

#[derive(Default)]
pub(crate) struct EnvStack {
  envs: Vec<EnvEntry>,
}

impl EnvStack {
  fn new(env: UsertypeHandle<ModuleValue>) -> Self {
    Self {
      envs: vec![EnvEntry::File(env)],
    }
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
    }
  }

  fn last_mut(&mut self) -> &mut UsertypeHandle<ModuleValue> {
    match self.envs.last_mut().expect("last_mut: the env stack should never be empty") {
      EnvEntry::Fn(e) => e,
      EnvEntry::Mod(e) => e,
      EnvEntry::File(e) => e,
      EnvEntry::Block(e) => e,
    }
  }
}

pub(crate) enum EnvEntry {
  Fn(UsertypeHandle<ModuleValue>),
  Mod(UsertypeHandle<ModuleValue>),
  File(UsertypeHandle<ModuleValue>),
  Block(UsertypeHandle<ModuleValue>),
}

impl Debug for EnvEntry {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Fn(_) => f.debug_tuple("Fn").finish(),
      Self::Mod(_) => f.debug_tuple("Mod").finish(),
      Self::File(_) => f.debug_tuple("File").finish(),
      Self::Block(_) => f.debug_tuple("Block").finish(),
    }
  }
}

pub(crate) enum ExecType {
  File,
  Fn,
}

pub struct Vm {
  // Don't get rid of current_frame in favor of the vec of stack frames
  // current_frame will be needed for repl so locals at the 0 depth scope
  // stay alive
  pub(crate) current_frame: StackFrame,
  pub(crate) stack_frames: Vec<StackFrame>,
  pub gc: SmartPtr<Gc>,

  pub(crate) envs: EnvStack,

  args: Vec<String>,
  libs: Library,

  lib_cache: BTreeMap<FileIdType, Value>,

  // usize for what frame to pop on, string for file path
  opened_files: Vec<FileInfo>,
  opened_native_libs: BTreeMap<PathBuf, Container<NativeApi>>,

  next_gc: Instant,
}

impl Vm {
  pub fn new(gc: SmartPtr<Gc>, args: impl Into<Vec<String>>, libs: Library) -> Self {
    Self {
      current_frame: Default::default(),
      stack_frames: Default::default(),
      gc,
      envs: Default::default(),
      args: args.into(),
      libs,
      lib_cache: Default::default(),
      opened_files: Default::default(),
      opened_native_libs: Default::default(),
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

  pub fn load(&self, file: impl Into<PathBuf>, code: &str) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    Compiler::compile(file.into(), code)
  }

  pub fn run(
    &mut self,
    file: impl Into<PathBuf>,
    ctx: SmartPtr<Context>,
    env: UsertypeHandle<ModuleValue>,
  ) -> Result<Value, RuntimeErrors> {
    #[cfg(feature = "disassemble")]
    {
      ctx.disassemble();

      #[cfg(feature = "quit-after-disassembled")]
      {
        std::process::exit(0);
      }
    }

    let file = file.into();
    let id = PlatformMetadata::id_of(&file).unwrap_or(0);

    self.opened_files = vec![FileInfo::new(file, id, 0)];
    self.reinitialize(ctx, env);
    self.execute(ExecType::File)
  }

  fn reinitialize(&mut self, ctx: SmartPtr<Context>, env: UsertypeHandle<ModuleValue>) {
    self.current_frame = StackFrame::new(ctx);
    self.stack_frames = Default::default();
    self.envs = EnvStack::new(env);
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
          _ => Err(self.error(format!("invalid unary operation")))?,
        };

        let callable = v.get_member(&mut self.gc, key).map_err(|e| self.error(e))?;
        self.call_value(callable, [])
      } else {
        self.stack_push(f(v).map_err(|e| self.error(e))?);
        Ok(())
      }
    } else {
      Err(self.error("cannot operate on empty stack"))
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
            _ => Err(self.error(format!("invalid binary operation")))?,
          };

          let callable = av.get_member(&mut self.gc, key).map_err(|e| self.error(e))?;
          self.call_value(callable, [bv])
        } else {
          self.stack_push(f(av, bv).map_err(|e| self.error(e))?);
          Ok(())
        }
      } else {
        Err(self.error("cannot operate on empty stack"))
      }
    } else {
      Err(self.error("cannot operate on empty stack"))
    }
  }

  fn global_op<F>(&mut self, index: usize, f: F) -> ExecResult
  where
    F: FnOnce(&mut Self, String) -> ExecResult,
  {
    match self.current_frame.ctx.global_const_at(index) {
      Some(ConstantValue::String(name)) => f(self, name.clone()),
      Some(name) => Err(self.error(format!("global variable ident is not an identifier: {}", name)))?,
      None => Err(self.error(format!("global variable ident does not exist at index {}", index)))?,
    }
  }

  #[cold]
  fn error<M: ToString>(&self, msg: M) -> RuntimeErrors {
    let opcode = self.current_frame.ctx.instructions[self.current_frame.ip].clone();
    RuntimeErrors::single(self.error_at(|opcode_ref| RuntimeError::from_ref(msg, opcode, opcode_ref)))
  }

  pub(crate) fn execute(&mut self, exec_type: ExecType) -> Result<Value, RuntimeErrors> {
    self.next_gc = Instant::now() + DEFAULT_GC_FREQUENCY;

    #[cfg(feature = "runtime-disassembly")]
    {
      println!("<< {} >>", self.current_frame.ctx.id);
    }

    let mut export = None;

    'ctx: while let Some(opcode) = self.current_frame.ctx.next(self.current_frame.ip) {
      let now = Instant::now();
      if now > self.next_gc {
        self.run_gc();
      }

      #[cfg(feature = "runtime-disassembly")]
      {
        self.stack_display();
        self.current_frame.ctx.display_instruction(&opcode, self.current_frame.ip);
      }

      match opcode {
        Opcode::NoOp => self.exec_noop()?,
        Opcode::Const(index) => self.exec_const(index)?,
        Opcode::Nil => self.exec_nil(),
        Opcode::True => self.exec_true(),
        Opcode::False => self.exec_false(),
        Opcode::Pop => self.exec_pop(),
        Opcode::PopN(count) => self.exec_pop_n(count),
        Opcode::ForceAssignGlobal(index) => self.exec_force_assign_global(index)?,
        Opcode::DefineGlobal(index) => self.exec_define_global(index)?,
        Opcode::LookupGlobal(index) => self.exec_lookup_global(index)?,
        Opcode::AssignGlobal(index) => self.exec_assign_global(index)?,
        Opcode::LookupLocal(index) => self.exec_lookup_local(index)?,
        Opcode::AssignLocal(index) => self.exec_assign_local(index)?,
        Opcode::InitializeMember(index) => self.exec_initialize_member(index)?,
        Opcode::InitializeMethod(index) => self.exec_initialize_method(index)?,
        Opcode::InitializeConstructor => self.exec_initialize_constructor()?,
        Opcode::AssignMember(index) => self.exec_assign_member(index)?,
        Opcode::LookupMember(index) => self.exec_lookup_member(index)?,
        Opcode::PeekMember(index) => self.exec_peek_member(index)?,
        Opcode::Check => self.exec_check()?,
        Opcode::Print => self.exec_print()?,
        Opcode::Jump(count) => {
          self.jump(count);
          continue 'ctx;
        }
        Opcode::JumpIfFalse(count) => {
          if self.exec_jump_if_false(count)? {
            continue 'ctx;
          }
        }
        Opcode::Loop(count) => {
          self.loop_back(count);
          continue 'ctx;
        }
        Opcode::Invoke(airity) => {
          self.current_frame.ip += 1;
          self.exec_call(airity)?;
          continue 'ctx;
        }
        Opcode::Ret => {
          export = Some(Value::nil);
          break 'ctx;
        }
        Opcode::RetValue => {
          export = Some(self.stack_pop().expect("value must be on stack to return"));
          break 'ctx;
        }
        Opcode::RetSelf => {
          export = Some(self.stack_index_0().expect("value must be on the stack"));
          break 'ctx;
        }
        Opcode::Req => {
          self.current_frame.ip += 1;
          self.exec_req()?;
          continue 'ctx;
        }
        Opcode::CreateList(num_items) => self.exec_create_list(num_items),
        Opcode::CreateClosure => self.exec_create_closure()?,
        Opcode::CreateStruct => self.exec_create_struct(),
        Opcode::CreateClass => self.exec_create_class(),
        Opcode::CreateModule => self.exec_create_module(),
        Opcode::Breakpoint => {
          self.ssdb().ok();
        }
        Opcode::Export => {
          if let Some(value) = self.stack_pop() {
            export = Some(value);
          } else {
            self.error("no value on stack to export");
          }
        }
        Opcode::Define(ident) => self.exec_define(ident)?,
        Opcode::Resolve(ident) => self.exec_scope_resolution(ident)?,
        Opcode::EnterBlock => self.push_scope(),
        Opcode::PopScope => self.pop_scope(),
        Opcode::Equal => self.exec_equal(&opcode)?,
        Opcode::NotEqual => self.exec_not_equal(&opcode)?,
        Opcode::Greater => self.exec_greater(&opcode)?,
        Opcode::GreaterEqual => self.exec_greater_equal(&opcode)?,
        Opcode::Less => self.exec_less(&opcode)?,
        Opcode::LessEqual => self.exec_less_equal(&opcode)?,
        Opcode::Add => self.exec_add(&opcode)?,
        Opcode::Sub => self.exec_sub(&opcode)?,
        Opcode::Mul => self.exec_mul(&opcode)?,
        Opcode::Div => self.exec_div(&opcode)?,
        Opcode::Rem => self.exec_rem(&opcode)?,
        Opcode::Or(count) => {
          if self.exec_or(count)? {
            continue 'ctx;
          }
        }
        Opcode::And(count) => {
          if self.exec_and(count)? {
            continue 'ctx;
          }
        }
        Opcode::Not => self.exec_not(&opcode)?,
        Opcode::Negate => self.exec_negate(&opcode)?,
      }

      self.current_frame.ip += 1;
    }

    #[cfg(feature = "runtime-disassembly")]
    {
      println!("<< END >>");
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
    }

    if let Some(stack_frame) = self.stack_frames.pop() {
      // the vm is returning from a function call or req
      self.current_frame = stack_frame;
    }

    Ok(export.unwrap_or_default())
  }

  /* Operations */

  fn exec_noop(&self) -> ExecResult {
    Err(self.error("executed noop opcode, should not happen"))
  }

  fn exec_const(&mut self, index: usize) -> ExecResult {
    if let Some(c) = self.current_frame.ctx.const_at(index) {
      let env = self.current_env().into();
      let value = Value::from_constant(&mut self.gc, env, c);
      self.stack_push(value);
    } else {
      Err(self.error("could not lookup constant"))?;
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

  fn exec_lookup_local(&mut self, location: usize) -> ExecResult {
    if let Some(local) = self.stack_index(location) {
      self.stack_push(local);
      Ok(())
    } else {
      Err(self.error(format!("could not index stack at pos {}", location)))
    }
  }

  fn exec_assign_local(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      self.stack_assign(location, value);
      Ok(())
    } else {
      Err(self.error(format!("could not replace stack value at pos {}", location)))
    }
  }

  fn exec_lookup_global(&mut self, location: usize) -> ExecResult {
    self.global_op(location, |this, name| {
      if let Some(global) = this.current_env().lookup(name) {
        this.stack_push(global);
        Ok(())
      } else {
        Err(this.error("use of undefined variable"))
      }
    })
  }

  fn exec_force_assign_global(&mut self, location: usize) -> ExecResult {
    self.global_op(location, |this, name| {
      // used with declarations only, so pop because it can't be chained
      if let Some(v) = this.stack_pop() {
        this.current_env_mut().define(name, v);
        Ok(())
      } else {
        Err(this.error("can not define global using empty stack"))
      }
    })
  }

  fn exec_define_global(&mut self, location: usize) -> ExecResult {
    self.global_op(location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if this.current_env_mut().define(name.clone(), v) {
          Ok(())
        } else {
          let level = this.current_env().search_for(0, &name);
          Err(this.error(format!("tried redefining a global variable at {level:?}: {name}")))
        }
      } else {
        Err(this.error("can not define global using empty stack"))
      }
    })
  }

  fn exec_assign_global(&mut self, location: usize) -> ExecResult {
    self.global_op(location, |this, name| {
      if let Some(v) = this.stack_peek() {
        if this.current_env_mut().assign(name, v) {
          Ok(())
        } else {
          Err(this.error("tried to assign to nonexistent global"))
        }
      } else {
        Err(this.error("can not assign to global using empty stack"))
      }
    })
  }

  fn exec_initialize_member(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            obj.set_member(&mut self.gc, name, value).map_err(|e| self.error(e))
          } else {
            Err(self.error("invalid name for member"))
          }
        } else {
          Err(self.error("no identifier found at index"))
        }
      } else {
        Err(self.error("no value on stack to initialize a member to"))
      }
    } else {
      Err(self.error("no value on stack to initialize to member"))
    }
  }

  fn exec_initialize_method(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            if let Some(class) = obj.as_class_mut() {
              if let Some(f) = value.as_fn() {
                class.set_method(name, f.clone());
                Ok(())
              } else {
                Err(self.error("methods can only be functions"))
              }
            } else {
              Err(self.error("can only create methods on classes"))
            }
          } else {
            Err(self.error("invalid name for member"))
          }
        } else {
          Err(self.error("no identifier found at index"))
        }
      } else {
        Err(self.error("no value on stack to assign a method to"))
      }
    } else {
      Err(self.error("no value on stack to assign to method"))
    }
  }

  fn exec_initialize_constructor(&mut self) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(class) = obj.as_class_mut() {
          class.set_constructor(value);
          Ok(())
        } else {
          Err(self.error("can only set constructors on classes"))
        }
      } else {
        Err(self.error("no value on stack to assign a member to"))
      }
    } else {
      Err(self.error("no value on stack to assign to constructor"))
    }
  }

  fn exec_assign_member(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_pop() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            obj.set_member(&mut self.gc, name, value.clone()).map_err(|e| self.error(e))?;
            self.stack_push(value);
            Ok(())
          } else {
            Err(self.error("constant at index is not an identifier"))
          }
        } else {
          Err(self.error("no ident found at index"))
        }
      } else {
        Err(self.error("no object to assign to"))
      }
    } else {
      Err(self.error("no value on stack to assign to member"))
    }
  }

  fn exec_lookup_member(&mut self, location: usize) -> ExecResult {
    if let Some(obj) = self.stack_pop() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let ConstantValue::String(name) = name {
          let value = obj.get_member(&mut self.gc, name).map_err(|e| self.error(e))?;
          self.stack_push(value);
          Ok(())
        } else {
          Err(self.error("member identifier is not a string"))
        }
      } else {
        Err(self.error("no identifier at index"))
      }
    } else {
      Err(self.error("no value on stack to perform lookup on"))
    }
  }

  fn exec_peek_member(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_peek() {
      if let Some(name) = self.current_frame.ctx.const_at(location) {
        if let ConstantValue::String(name) = name {
          let member = value.get_member(&mut self.gc, name).map_err(|e| self.error(e))?;
          self.stack_push(member);
          Ok(())
        } else {
          Err(self.error(format!("invalid lookup for member access: {}", value)))
        }
      } else {
        Err(self.error(format!("no name for member access: {}", value)))
      }
    } else {
      Err(self.error("no object to lookup on"))
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

  fn exec_check(&mut self) -> ExecResult {
    match self.stack_pop() {
      Some(a) => match self.stack_peek() {
        Some(b) => {
          self.stack_push(Value::from(a == b));
          Ok(())
        }
        None => Err(self.error("stack peek failed")),
      },
      None => Err(self.error("stack pop failed")),
    }
  }

  fn exec_add(&mut self, opcode: &Opcode) -> ExecResult {
    self.binary_op(opcode, |a, b| a + b)
  }

  fn exec_sub(&mut self, opcode: &Opcode) -> ExecResult {
    self.binary_op(opcode, |a, b| a - b)
  }

  fn exec_mul(&mut self, opcode: &Opcode) -> ExecResult {
    self.binary_op(opcode, |a, b| a * b)
  }

  fn exec_div(&mut self, opcode: &Opcode) -> ExecResult {
    self.binary_op(opcode, |a, b| a / b)
  }

  fn exec_rem(&mut self, opcode: &Opcode) -> ExecResult {
    self.binary_op(opcode, |a, b| a % b)
  }

  /// when f evaluates to true, short circuit

  fn exec_logical<F: FnOnce(Value) -> bool>(&mut self, offset: usize, f: F) -> ExecResult<bool> {
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
      None => Err(self.error("no item on the stack to peek")),
    }
  }

  fn exec_or(&mut self, offset: usize) -> ExecResult<bool> {
    self.exec_logical(offset, |v| v.truthy())
  }

  fn exec_and(&mut self, offset: usize) -> ExecResult<bool> {
    self.exec_logical(offset, |v| v.falsy())
  }

  fn exec_not(&mut self, opcode: &Opcode) -> ExecResult {
    self.unary_op(opcode, |v| Ok(!v))
  }

  fn exec_negate(&mut self, opcode: &Opcode) -> ExecResult {
    self.unary_op(opcode, |v| -v)
  }

  fn exec_print(&mut self) -> ExecResult {
    if let Some(v) = self.stack_pop() {
      println!("{}", v);
      Ok(())
    } else {
      Err(self.error("no value to print"))
    }
  }

  fn exec_jump_if_false(&mut self, offset: usize) -> ExecResult<bool> {
    match self.stack_pop() {
      Some(v) => {
        if !v.truthy() {
          self.jump(offset);
          Ok(true)
        } else {
          Ok(false)
        }
      }
      None => Err(self.error("no item on the stack to pop")),
    }
  }

  fn exec_call(&mut self, airity: usize) -> ExecResult {
    let args = self.stack_drain_from(airity);
    if let Some(callable) = self.stack_pop() {
      self.call_value(callable, args)
    } else {
      Err(self.error("cannot operate on empty stack"))
    }
  }

  fn exec_req(&mut self) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      let mut attempts = Vec::with_capacity(10);

      let file_str = value.to_string();
      let this_file = self.opened_files.last().map(|f| f.path.clone());

      let required_file = PathBuf::from(file_str.as_str());

      let mut found_file = None;

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

      // find relative first, skip if None, None will be during repl so go to cwd
      if let Some(this_dir) = this_file.and_then(|this_file| this_file.parent().map(|p| p.to_path_buf())) {
        found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
      }

      // then try to find from cwd
      if found_file.is_none() {
        let this_dir = std::env::current_dir().map_err(|e| self.error(e))?;
        found_file = try_to_find_file(&this_dir, &required_file, &mut attempts);
      }

      // if still not found, try searching library paths
      if found_file.is_none() {
        if let Some(library_mod) = self.current_env().lookup(module_value::LIB_GLOBAL) {
          if let Some(library_mod) = library_mod.as_struct() {
            if let Ok(Some(list)) = library_mod
              .get_field(&mut self.gc, module_value::PATHS_MEMBER)
              .map(|l| l.map(|l| l.as_array()))
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

      if let Some(found_file) = found_file {
        let id = PlatformMetadata::id_of(&found_file)
          .map_err(|e| self.error(format!("failed to get file info of {}: {}", found_file.display(), e)))?;

        match found_file.extension().and_then(|s| s.to_str()) {
          Some(dlopen2::utils::PLATFORM_FILE_EXTENSION) => {
            let value = if let Some(value) = self.lib_cache.get(&id) {
              value.clone()
            } else {
              let lib: Container<NativeApi> =
                unsafe { Container::load(&found_file).expect("somehow wasn't able to load found file") };

              let value = lib.simple_script_load_module(self).map_err(|e| self.error(e))?;
              self.opened_native_libs.insert(found_file, lib);
              self.lib_cache.insert(id, value.clone());
              value
            };

            self.stack_push(value);

            Ok(())
          }
          _ => match fs::read_to_string(&found_file) {
            Ok(data) => {
              if let Some(value) = self.lib_cache.get(&id) {
                self.stack_push(value.clone());
              } else {
                let new_ctx = Compiler::compile(found_file.clone(), &data)?;
                let gmod = ModuleBuilder::initialize(&mut self.gc, None, |gc, mut lib| {
                  lib.env = stdlib::load_libs(gc, lib.handle.value.clone(), &self.args, &self.libs.clone());
                });

                #[cfg(feature = "disassemble")]
                {
                  println!("!!!!! ENTERING {} !!!!!", found_file.display());
                  new_ctx.disassemble();
                  println!("!!!!! LEAVING  {} !!!!!", found_file.display());
                }

                self.new_frame(new_ctx);
                self.envs.push(EnvEntry::File(gmod));
                self.opened_files.push(FileInfo::new(found_file, id, self.stack_frames.len()));
                let output = self.execute(ExecType::File)?;
                self.stack_push(output);
              }

              Ok(())
            }
            Err(e) => Err(self.error(format!("unable to read file '{}': {}", file_str, e,))),
          },
        }
      } else {
        Err(self.error(format!("unable to find file, tried: {:#?}", attempts)))
      }
    } else {
      Err(self.error("no item on stack to require (logic error)"))
    }
  }

  fn exec_create_list(&mut self, num_items: usize) {
    let list = self.stack_drain_from(num_items);
    let list = self.gc.allocate(list);
    self.stack_push(list);
  }

  fn exec_create_closure(&mut self) -> ExecResult {
    match self.stack_pop() {
      Some(function) => match self.stack_pop() {
        Some(captures) => {
          if let Some(f) = function.as_fn() {
            if let Some(captures) = captures.as_array() {
              let closure = self.gc.allocate(ClosureValue::new(captures, f.clone()));
              self.stack_push(closure);
              Ok(())
            } else {
              Err(self.error("capture list must be a struct"))
            }
          } else {
            Err(self.error("closure must be a function"))
          }
        }
        None => Err(self.error("no item on the stack to pop for closure captures")),
      },
      None => Err(self.error("no item on the stack to pop for closure function")),
    }
  }

  fn exec_create_struct(&mut self) {
    let v = self.gc.allocate(StructValue::default());
    self.stack_push(v);
  }

  fn exec_create_class(&mut self) {
    let v = self.gc.allocate(ClassValue::default());
    self.stack_push(v);
  }

  /// Create a module and make it the current env
  fn exec_create_module(&mut self) {
    let leaf = self.current_env();
    let module = ModuleValue::new_child(leaf.handle.value.clone());
    let uhandle = Gc::allocate_handle(&mut self.gc, module);
    self.envs.push(EnvEntry::Mod(uhandle.clone()));
    self.stack_push(uhandle.handle.value.clone());
  }

  fn exec_scope_resolution(&mut self, ident: usize) -> ExecResult {
    if let Some(obj) = self.stack_pop() {
      if let Some(name) = self.current_frame.ctx.const_at(ident) {
        if let ConstantValue::String(name) = name {
          let value = obj.resolve(name).map_err(|e| self.error(e))?;
          self.stack_push(value);
          Ok(())
        } else {
          Err(self.error("member identifier is not a string"))
        }
      } else {
        Err(self.error("no identifier at index"))
      }
    } else {
      Err(self.error("no value on stack to resolve"))
    }
  }

  fn push_scope(&mut self) {
    let mut gc = self.gc.clone();
    let leaf = self.current_env();
    let handle = Gc::allocate_handle(&mut gc, ModuleValue::new_child(leaf.handle.value.clone()));
    self.envs.push(EnvEntry::Block(handle));
  }

  fn pop_scope(&mut self) {
    self.envs.pop();
  }

  fn exec_define(&mut self, location: usize) -> ExecResult {
    if let Some(value) = self.stack_pop() {
      if let Some(mut obj) = self.stack_peek() {
        if let Some(name) = self.current_frame.ctx.const_at(location) {
          if let ConstantValue::String(name) = name {
            obj.define(name, value).map_err(|e| self.error(e))?;
            Ok(())
          } else {
            Err(self.error("invalid name for member"))
          }
        } else {
          Err(self.error("no identifier found at index"))
        }
      } else {
        Err(self.error("no value on stack to define a member to"))
      }
    } else {
      Err(self.error("no value on stack to define a member on"))
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

  fn call_value(&mut self, mut callable: Value, args: impl Into<Vec<Value>>) -> ExecResult {
    callable.call(self, Args::new(args)).map_err(|e| self.error(e))?;

    #[cfg(feature = "runtime-disassembly")]
    println!("<< entering {} >>", self.current_frame.ctx.id);

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

  pub fn run_gc(&mut self) {
    let now = Instant::now();
    self.next_gc = now + DEFAULT_GC_FREQUENCY;
    self
      .gc
      .clean(&self.current_frame, &self.stack_frames, self.lib_cache.values());
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

#[derive(Default)]
pub struct StackFrame {
  pub ip: usize,
  pub ctx: SmartPtr<Context>,
  pub stack: Vec<Value>,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ip: Default::default(),
      ctx,
      stack: Default::default(),
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
}

pub(crate) struct FileInfo {
  path: PathBuf,
  id: FileIdType,
  frame: usize,
}

impl FileInfo {
  fn new(path: PathBuf, id: FileIdType, frame: usize) -> Self {
    Self { path, id, frame }
  }
}
