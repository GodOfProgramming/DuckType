pub mod ast;
pub mod gen;
pub mod lex;
pub mod opt;

use crate::{
  types::{Error, Struct, Value, ValueOpResult},
  ExecutionThread, New,
};
use ast::Ast;
use gen::BytecodeGenerator;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
  env,
  fmt::{Debug, Display, Formatter},
  str,
};

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
  /** Uses the constant pointed to by the modifying bits to lookup a value on the next item on the stack */
  LookupMember(usize),
  /** Assigns an initializer function to the class which is the next item on the stack */
  AssignInitializer,
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
  /** Exits from a function */
  Ret,
  /** Require an external file. The file name is the top of the stack. Must be a string or convertible to */
  Req,
  /** Index into the indexable. The first argument off the stack is the index, the second is the indexable */
  Index,
  /** Create a list of values and push it on the stack. Items come off the top of the stack and the number is specified by the modifying bits */
  CreateList(usize),
  /** Create a closure. The first item on the stack is the function itself, the second is the capture list  */
  CreateClosure,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,
  Comma,
  Dot,
  Colon,
  Semicolon,

  // One or two character tokens.
  Bang,
  BangEqual,
  Equal,
  EqualEqual,
  Greater,
  GreaterEqual,
  Less,
  LessEqual,
  Arrow,
  Plus,
  PlusEqual,
  Minus,
  MinusEqual,
  Asterisk,
  AsteriskEqual,
  Slash,
  SlashEqual,
  Percent,
  PercentEqual,

  // Literals.
  Identifier(String),
  String(String),
  Number(f64),

  // Keywords.
  And,
  Break,
  Class,
  Cont,
  Else,
  False,
  For,
  Fn,
  If,
  Let,
  Loop,
  Match,
  New,
  Nil,
  Or,
  Print,
  Req,
  Ret,
  True,
  While,
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Token::Identifier(i) => write!(f, "Identifier ({})", i),
      Token::String(s) => write!(f, "String ({})", s),
      Token::Number(n) => write!(f, "Number ({})", n),
      _ => write!(f, "{:?}", self),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceLocation {
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
      self
        .source
        .lines()
        .nth(info.line - 1)
        .map(|src| OpCodeReflection {
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

enum ContextName {
  Main,
  Lambda,
  Closure,
  Method,
  Function(String),
}

pub struct Context {
  name: ContextName,

  pub id: usize,          // the function id within the local file
  pub file_id: usize,     // the id of the file it was loaded from
  pub instance_id: usize, // the instance of the file it was loaded from

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
      file_id: Default::default(),
      instance_id: Default::default(),
      global: Default::default(),
      instructions: Default::default(),
      consts: Default::default(),
      strings: Default::default(),
      meta: reflection,
    }
  }

  fn new_child(
    ctx: SmartPtr<Context>,
    reflection: Reflection,
    id: usize,
    name: ContextName,
  ) -> Self {
    let global = if ctx.global.valid() {
      ctx.global.clone()
    } else {
      ctx.clone()
    };

    Self {
      name,
      id,
      file_id: Default::default(),
      instance_id: Default::default(),
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
    let string = if let Value::String(string) = &c {
      if let Some(index) = self.strings.get(string) {
        return *index;
      }
      Some(string.clone())
    } else {
      None
    };

    self.consts.push(c);
    let index = self.consts.len() - 1;

    if let Some(string) = string {
      self.strings.insert(string, index);
    }

    index
  }

  fn num_instructions(&self) -> usize {
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
      if let Value::Function(f) = value {
        f.context().disassemble()
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
      OpCode::PopN(count) => println!(
        "{} {}",
        Self::opcode_column("PopN"),
        Self::value_column(*count)
      ),
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
        println!(
          "{} {}",
          Self::opcode_column("LookupLocal"),
          Self::value_column(*index)
        )
      }
      OpCode::AssignLocal(index) => {
        println!(
          "{} {}",
          Self::opcode_column("AssignLocal"),
          Self::value_column(*index)
        )
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
      OpCode::Jump(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Jump"),
        Self::address_of(offset + count)
      ),
      OpCode::JumpIfFalse(count) => {
        println!(
          "{} {: >14}",
          Self::opcode_column("JumpIfFalse"),
          Self::address_of(offset + count)
        )
      }
      OpCode::Loop(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Loop"),
        Self::address_of(offset - count)
      ),
      OpCode::Or(count) => println!(
        "{} {: >14}",
        Self::opcode_column("Or"),
        Self::address_of(offset + count)
      ),
      OpCode::And(count) => println!(
        "{} {: >14}",
        Self::opcode_column("And"),
        Self::address_of(offset + count)
      ),
      OpCode::Call(count) => println!(
        "{} {}",
        Self::opcode_column("Call"),
        Self::value_column(*count)
      ),
      OpCode::CreateList(count) => println!(
        "{} {}",
        Self::opcode_column("CreateList"),
        Self::value_column(*count)
      ),
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
    format!(
      "{: >4?}",
      self.global_const_at(index).unwrap_or(&Value::new("????"))
    )
  }

  fn const_at_column(&self, index: usize) -> String {
    format!(
      "{: >4?}",
      self.const_at(index).unwrap_or(&Value::new("????"))
    )
  }

  fn address_of(offset: usize) -> String {
    format!("{:#010X} ", offset)
  }
}

#[derive(Default)]
pub struct Env {
  vars: BTreeMap<String, Value>,
}

impl Env {
  pub fn with_library_path() -> Self {
    let mut env = Env::default();

    let mut lib_paths = Vec::default();

    if let Ok(paths) = env::var("SIMPLE_LIBRARY_PATHS") {
      lib_paths.extend(paths.split_terminator(';').map(Value::new));
    }

    let mut module = Struct::default();
    let lib_paths = Value::new(lib_paths);

    module.set("path", lib_paths);

    env.assign("$LIBRARY", Value::new(module));

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

  pub fn create_native<
    K: ToString,
    F: FnMut(&mut ExecutionThread, &mut Env, Vec<Value>) -> ValueOpResult + 'static,
  >(
    &mut self,
    name: K,
    native: F,
  ) -> bool {
    self.assign(name.to_string(), Value::new((name.to_string(), native)))
  }
}

#[derive(Default)]
pub struct Compiler;

impl Compiler {
  pub fn compile(file: &str, source: &str) -> Result<SmartPtr<Context>, Vec<Error>> {
    let mut scanner = Scanner::new(file, source);

    let (tokens, meta) = scanner
      .scan()
      .map_err(|errs| Self::reformat_errors(source, errs))?;

    let ast = Ast::from(tokens, meta).map_err(|errs| Self::reformat_errors(source, errs))?;

    let optimizer = Optimizer::<1>::new(ast);

    let ast = optimizer.optimize();

    let file = SmartPtr::new(String::from(file));
    let source_ptr = SmartPtr::new(String::from(source));

    let reflection = Reflection::new(file, source_ptr.clone());
    let ctx = SmartPtr::new(Context::new(reflection));

    let generator = BytecodeGenerator::new(ctx);

    generator
      .generate(ast)
      .map_err(|errs| Self::reformat_errors(source, errs))
  }

  fn reformat_errors(source: &str, errs: Vec<Error>) -> Vec<Error> {
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
