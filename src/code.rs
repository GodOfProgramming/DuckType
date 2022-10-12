use super::*;
use crate::dbg::RuntimeError;
use ast::Ast;
use gen::BytecodeGenerator;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  convert::TryFrom,
  env,
  error::Error,
  fmt::{self, Debug, Display, Formatter},
  path::PathBuf,
  str,
};

pub mod ast;
pub mod gen;
pub mod lex;
pub mod opt;

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
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
  Mod,
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
  /** Calls the instruction on the stack. Number of arguments is specified by the modifying bits */
  Call(usize),
  /** Exits from a function, returning nil on the previous frame */
  Ret,
  /** Exits from a function, returning the last value on the stack to the previous frame */
  RetValue,
  /** Require an external file. The file name is the top of the stack. Must be a string or convertible to */
  Req,
  /** Index into the indexable. The first argument off the stack is the index, the second is the indexable */
  Index,
  /** Create a list of values and push it on the stack. Items come off the top of the stack and the number is specified by the modifying bits */
  CreateList(usize),
  /** Create a closure. The first item on the stack is the function itself, the second is the capture list  */
  CreateClosure,
  /** Yield at the current location */
  Yield,
}

#[derive(Default)]
pub struct Compiler;

impl Compiler {
  pub fn compile(file: &str, source: &str) -> Result<SmartPtr<Context>, Vec<RuntimeError>> {
    let mut scanner = Scanner::new(file, source);

    let (tokens, meta) = scanner.scan().map_err(|errs| Self::reformat_errors(source, errs))?;

    let (ast, errors) = Ast::from(tokens, meta);

    if !errors.is_empty() {
      #[cfg(feature = "visit-ast")]
      {
        ast.dump(file);
      }

      return Err(Self::reformat_errors(source, errors));
    }

    let optimizer = Optimizer::<1>::new(ast);

    let ast = optimizer.optimize();

    let file = SmartPtr::new(String::from(file));
    let source_ptr = SmartPtr::new(String::from(source));

    let reflection = Reflection::new(file, source_ptr.clone());
    let ctx = SmartPtr::new(Context::new(reflection));

    let generator = BytecodeGenerator::new(ctx);

    generator.generate(ast).map_err(|errs| Self::reformat_errors(source, errs))
  }

  fn reformat_errors(source: &str, errs: Vec<RuntimeError>) -> Vec<RuntimeError> {
    errs
      .into_iter()
      .map(|mut e| {
        if let Some(src) = source.lines().nth(e.line - 1) {
          e.format_with_src_line(String::from(src));
        }
        e
      })
      .collect()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
  pub file: SmartPtr<String>,
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OpCodeInfo {
  pub line: usize,
  pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OpCodeReflection {
  pub file: String,
  pub source_line: String,
  pub line: usize,
  pub column: usize,
}

#[derive(Debug)]
enum ContextName {
  Main,
  Lambda,
  Closure,
  Method,
  Function(String),
}

#[derive(Debug)]
pub struct Context {
  name: ContextName,

  pub id: usize, // the function id within the local file

  global: SmartPtr<Context>,

  instructions: Vec<OpCode>,

  consts: Vec<Value>,
  // map of string to const vec location to save memory
  strings: BTreeMap<String, usize>,

  pub meta: Reflection,
}

impl Context {
  fn new(reflection: Reflection) -> Self {
    Self {
      name: ContextName::Main,
      id: Default::default(),
      global: Default::default(),
      instructions: Default::default(),
      consts: Default::default(),
      strings: Default::default(),
      meta: reflection,
    }
  }

  fn new_child(ctx: SmartPtr<Context>, reflection: Reflection, id: usize, name: ContextName) -> Self {
    let global = if ctx.global.valid() { ctx.global.clone() } else { ctx.clone() };

    Self {
      name,
      id,
      global,
      consts: Default::default(),
      strings: Default::default(),
      instructions: Default::default(),
      meta: reflection,
    }
  }

  pub fn global_ctx(&self) -> &Context {
    if self.global.valid() {
      &self.global
    } else {
      self
    }
  }

  pub fn global_ctx_mut(&mut self) -> &mut Context {
    if self.global.valid() {
      &mut self.global
    } else {
      self
    }
  }

  pub fn next(&self, index: usize) -> Option<OpCode> {
    self.instructions.get(index).cloned()
  }

  pub fn const_at(&self, index: usize) -> Option<&Value> {
    self.consts.get(index)
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<Value> {
    &self.consts
  }

  pub fn global_const_at(&self, index: usize) -> Option<&Value> {
    self.global_ctx().consts.get(index)
  }

  fn write(&mut self, op: OpCode, line: usize, column: usize) {
    #[cfg(test)]
    {
      println!("emitting {:?}", op);
    }
    self.instructions.push(op);
    self.meta.add(line, column);
  }

  fn write_const(&mut self, c: Value, line: usize, column: usize) {
    let c = self.add_const(c);
    self.write(OpCode::Const(c), line, column);
  }

  fn add_const(&mut self, c: Value) -> usize {
    let string = if let Ok(string) = c.as_str() {
      if let Some(index) = self.strings.get(string.as_str()) {
        return *index;
      }
      Some(string.deref().clone())
    } else {
      None
    };

    let index = self.consts.len();
    self.consts.push(c);

    if let Some(string) = string {
      self.strings.insert(string, index);
    }

    index
  }

  pub fn num_instructions(&self) -> usize {
    self.instructions.len()
  }

  fn replace_instruction(&mut self, index: usize, op: OpCode) -> bool {
    if let Some(inst) = self.instructions.get_mut(index) {
      *inst = op;
      true
    } else {
      false
    }
  }

  /* Debugging Functions */

  pub fn name(&self) -> &str {
    match &self.name {
      ContextName::Main => "MAIN",
      ContextName::Closure => "closure",
      ContextName::Lambda => "lambda",
      ContextName::Method => "method",
      ContextName::Function(name) => name.as_str(),
    }
  }

  #[cfg(debug_assertions)]
  pub fn disassemble(&self) {
    self.display_opcodes();

    for value in self.consts() {
      if let Ok(f) = value.as_fn() {
        f.context().disassemble();
      } else if let Ok(c) = value.as_class() {
        if let Some(i) = &c.initializer {
          if let Ok(f) = i.as_fn() {
            f.context().disassemble();
          } else if let Ok(m) = i.as_method() {
            m.context().disassemble();
          }
        }
        for (_name, method) in &c.methods {
          if let Ok(f) = method.as_fn() {
            f.context().disassemble();
          }
        }

        for static_method in c.static_members.values() {
          if let Ok(f) = static_method.as_fn() {
            f.context().disassemble();
          }
        }
      }
    }
  }

  pub fn display_str(&self) -> String {
    if self.id == 0 {
      String::from("MAIN")
    } else {
      format!("function {} {}", self.id, self.name())
    }
  }

  pub fn display_opcodes(&self) {
    println!("<< {} >>", self.display_str());

    for (i, op) in self.instructions.iter().enumerate() {
      self.display_instruction(op, i);
    }

    println!("<< END >>");
  }

  pub fn display_instruction(&self, op: &OpCode, offset: usize) {
    print!("{} ", Self::address_of(offset));
    if let Some(curr) = self.meta.get(offset) {
      if offset > 0 {
        if let Some(prev) = self.meta.get(offset - 1) {
          if curr.line == prev.line {
            print!("   | ");
          } else {
            print!("{:#04} ", curr.line);
          }
        } else {
          print!("?????");
        }
      } else {
        print!("{:#04} ", curr.line);
      }
    } else {
      print!("?????");
    }

    match op {
      OpCode::Const(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("Const"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      OpCode::PopN(count) => println!("{} {}", Self::opcode_column("PopN"), Self::value_column(*count)),
      OpCode::LookupGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("LookupGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      OpCode::ForceAssignGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("ForceAssignGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name)
      ),
      OpCode::DefineGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("DefineGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      OpCode::AssignGlobal(name) => println!(
        "{} {} {}",
        Self::opcode_column("AssignGlobal"),
        Self::value_column(*name),
        self.global_const_at_column(*name),
      ),
      OpCode::LookupLocal(index) => {
        println!("{} {}", Self::opcode_column("LookupLocal"), Self::value_column(*index))
      }
      OpCode::AssignLocal(index) => {
        println!("{} {}", Self::opcode_column("AssignLocal"), Self::value_column(*index))
      }
      OpCode::AssignMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("AssignMember"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      OpCode::LookupMember(index) => {
        println!(
          "{} {} {}",
          Self::opcode_column("LookupMember"),
          Self::value_column(*index),
          self.const_at_column(*index)
        );
      }
      OpCode::Jump(count) => println!("{} {: >14}", Self::opcode_column("Jump"), Self::address_of(offset + count)),
      OpCode::JumpIfFalse(count) => {
        println!(
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + count)
        )
      }
      OpCode::Loop(count) => println!("{} {: >14}", Self::opcode_column("Loop"), Self::address_of(offset - count)),
      OpCode::Or(count) => println!("{} {: >14}", Self::opcode_column("Or"), Self::address_of(offset + count)),
      OpCode::And(count) => println!("{} {: >14}", Self::opcode_column("And"), Self::address_of(offset + count)),
      OpCode::Call(count) => println!("{} {}", Self::opcode_column("Call"), Self::value_column(*count)),
      OpCode::CreateList(count) => println!("{} {}", Self::opcode_column("CreateList"), Self::value_column(*count)),
      x => println!("{}", Self::opcode_column(format!("{:?}", x))),
    }
  }

  fn opcode_column<O: ToString>(opcode: O) -> String {
    format!("{:<20}", opcode.to_string())
  }

  fn value_column(value: usize) -> String {
    format!("{: >4}", value)
  }

  fn global_const_at_column(&self, index: usize) -> String {
    format!("{: >4?}", self.global_const_at(index).unwrap_or(&mut Value::from("????")))
  }

  fn const_at_column(&self, index: usize) -> String {
    format!("{: >4?}", self.const_at(index).unwrap_or(&Value::from("????")))
  }

  pub fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}

#[derive(Debug, Default)]
pub struct StackFrame {
  pub ip: usize,
  pub ctx: SmartPtr<Context>,
  pub stack: Vec<Value>,
  pub last_lookup: Value,
}

impl StackFrame {
  pub fn new(ctx: SmartPtr<Context>) -> Self {
    Self {
      ip: Default::default(),
      ctx,
      stack: Default::default(),
      last_lookup: Default::default(),
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

#[derive(Debug)]
pub struct Yield {
  pub current_frame: StackFrame,
  pub stack_frames: Vec<StackFrame>,
  pub opened_files: Vec<(usize, PathBuf)>,
}

impl Yield {
  pub fn new(current_frame: StackFrame, stack_frames: Vec<StackFrame>, opened_files: Vec<(usize, PathBuf)>) -> Self {
    Self {
      current_frame,
      stack_frames,
      opened_files,
    }
  }
}

impl Display for Yield {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.current_frame.ctx.name())
  }
}

#[derive(Default)]
pub struct Env {
  vars: BTreeMap<String, Value>,

  with_lib_support: bool,

  library: Library,
  args: Vec<String>,
}

impl Env {
  pub fn with_library_support(args: &[String], library: Library) -> Self {
    let mut env = Env::default();

    env.vars = stdlib::load_libs(args, &library);

    let mut lib_paths = Vec::default();

    if let Ok(paths) = env::var("SIMPLE_LIBRARY_PATHS") {
      lib_paths.extend(paths.split_terminator(';').map(Value::from));
    }

    let mut module = StructValue::default();
    let lib_paths = Value::from(lib_paths);

    module.set("path", lib_paths);

    env.assign("$LIBRARY", Value::from(module));

    env.args.extend(args.iter().cloned());
    env.library = library;
    env.with_lib_support = true;

    env
  }

  pub fn define<T: ToString>(&mut self, name: T, value: Value) -> bool {
    self.vars.insert(name.to_string(), value).is_none()
  }

  pub fn is_defined(&self, name: &str) -> bool {
    self.vars.contains_key(name)
  }

  pub fn assign<T: ToString>(&mut self, name: T, value: Value) -> bool {
    self.vars.insert(name.to_string(), value).is_some()
  }

  pub fn lookup<T: ToString>(&self, name: T) -> Option<Value> {
    self.vars.get(&name.to_string()).cloned()
  }
}

#[derive(Debug)]
pub struct Reflection {
  pub file: SmartPtr<String>,
  pub source: SmartPtr<String>,
  pub opcode_info: Vec<OpCodeInfo>,
}

impl Reflection {
  fn new(file: SmartPtr<String>, source: SmartPtr<String>) -> Self {
    Reflection {
      file,
      source,
      opcode_info: Vec::default(),
    }
  }

  fn add(&mut self, line: usize, column: usize) {
    self.opcode_info.push(OpCodeInfo { line, column });
  }

  pub fn get(&self, offset: usize) -> Option<OpCodeReflection> {
    if let Some(info) = self.opcode_info.get(offset).cloned() {
      self.source.lines().nth(info.line - 1).map(|src| OpCodeReflection {
        file: self.file.access().clone(),
        source_line: String::from(src),
        line: info.line,
        column: info.column,
      })
    } else {
      None
    }
  }
}
