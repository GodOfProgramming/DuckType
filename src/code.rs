pub mod ast;
pub mod gen;
pub mod lex;
pub mod opt;

use crate::{
  types::{Error, Value, ValueOpResult},
  New,
};
use ast::Ast;
use gen::BytecodeGenerator;
use lex::Scanner;
use opt::Optimizer;
use ptr::SmartPtr;
use std::{
  collections::BTreeMap,
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
  /** Pushes the stack pointer onto the stack */
  PushSp,
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

  pub fn const_at(&self, index: usize) -> Option<Value> {
    self.consts.get(index).cloned()
  }

  #[cfg(debug_assertions)]
  pub fn consts(&self) -> &Vec<Value> {
    &self.consts
  }

  pub fn global_const_at(&self, index: usize) -> Option<Value> {
    self.global_ctx().consts.get(index).cloned()
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
    self.write(OpCode::Const(self.consts.len()), line, column);
    self.consts.push(c);
  }

  fn add_const(&mut self, c: Value) -> usize {
    if let Value::String(string) = &c {
      if let Some(index) = self.strings.get(string) {
        return *index;
      }
    }
    self.consts.push(c);
    self.consts.len() - 1
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

  pub fn display_opcodes(&self) {
    println!(
      "<< {} >>",
      if self.id == 0 {
        String::from("MAIN")
      } else {
        format!("function {} {}", self.id, self.name())
      }
    );
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
        print!("{:<16} {:4} ", "Const", index);
        let c = self.const_at(*index);
        match c {
          Some(v) => println!("{}", v),
          None => println!("INVALID INDEX"),
        }
      }
      OpCode::PopN(count) => println!("{:<16} {:4}", "PopN", count),
      OpCode::LookupLocal(index) => println!("{:<16} {:4}", "LookupLocal", index),
      OpCode::AssignLocal(index) => println!("{:<16} {:4}", "AssignLocal", index),
      OpCode::LookupGlobal(name) => println!(
        "{:<16} {:4} {:?}",
        "LookupGlobal",
        name,
        if let Some(name) = self.const_at(*name) {
          name
        } else {
          Value::new("????")
        }
      ),
      OpCode::DefineGlobal(name) => println!(
        "{:<16} {:4} {:?}",
        "DefineGlobal",
        name,
        if let Some(name) = self.const_at(*name) {
          name
        } else {
          Value::new("????")
        }
      ),
      OpCode::AssignGlobal(name) => println!(
        "{:<16} {:4} {:?}",
        "AssignGlobal",
        name,
        if let Some(name) = self.const_at(*name) {
          name
        } else {
          Value::new("????")
        }
      ),
      OpCode::AssignMember(index) => {
        print!("{:<16} {:4} ", "AssignMember", index);
        let c = self.const_at(*index);
        match c {
          Some(v) => println!("{}", v),
          None => println!("INVALID INDEX"),
        }
      }
      OpCode::LookupMember(index) => {
        print!("{:<16} {:4} ", "LookupMember", index);
        let c = self.const_at(*index);
        match c {
          Some(v) => println!("{}", v),
          None => println!("INVALID INDEX"),
        }
      }
      OpCode::Jump(count) => println!("{:<19} {}", "Jump", Self::address_of(offset + count)),
      OpCode::JumpIfFalse(count) => {
        println!("{:<19} {}", "JumpIfFalse", Self::address_of(offset + count))
      }
      OpCode::Loop(count) => println!("{:<19} {}", "Loop", Self::address_of(offset - count)),
      OpCode::Or(count) => println!("{:<19} {}", "Or", Self::address_of(offset + count)),
      OpCode::And(count) => println!("{:<19} {}", "And", Self::address_of(offset + count)),
      OpCode::Call(count) => println!("{:<16} {:4}", "Call", count),
      OpCode::CreateList(count) => println!("{:<16} {:4}", "CreateList", count),
      x => println!("{:<16?}", x),
    }
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
  pub fn define<T: ToString>(&mut self, name: T, value: Value) -> bool {
    self.vars.insert(name.to_string(), value).is_none()
  }

  pub fn assign<T: ToString>(&mut self, name: T, value: Value) -> bool {
    self.vars.insert(name.to_string(), value).is_some()
  }

  pub fn lookup<T: ToString>(&self, name: T) -> Option<Value> {
    self.vars.get(&name.to_string()).cloned()
  }

  pub fn create_native<K: ToString, F: FnMut(&mut Env, Vec<Value>) -> ValueOpResult + 'static>(
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
