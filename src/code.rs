use crate::{
  types::{Env, Value},
  Error,
};
use std::{
  f64::consts::PI,
  fmt::{self, Debug, Display},
  str,
};

#[derive(Debug, Clone)]
pub enum OpCode {
  /**
   * No operation instruction
   */
  NoOp,
  /**
   *  Looks up a constant value at the specified location. Location is specified by the tuple
   */
  Const(usize),
  /**
   * Pushes a nil value on to the stack
   */
  Nil,
  /**
   * Pushes a true value on to the stack
   */
  True,
  /**
   * Pushes a false value on to the stack
   */
  False,
  /**
   * Pops a value off the stack
   */
  Pop,
  /**
   * Pops N values off the stack. N is specified by tuple
   */
  PopN(usize),
  /**
   * Looks up a local variable. The index in the stack is specified by the modifying bits
   */
  LookupLocal(usize),
  /**
   * Assigns a value to the local variable indexed by the tuple. The value comes off the top of the stack
   */
  AssignLocal(usize),
  /**
   * Looks up a global variable. The name is stored in the enum
   */
  LookupGlobal(String),
  /**
   * Defines a new global variable. The name is stored in the enum. The value comes off the top of the stack
   */
  DefineGlobal(String),
  /**
   * Assigns a value to the global variable. The Name is stored in the enum. The value comes off the top of the stack
   */
  AssignGlobal(String),
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Equal,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  NotEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Greater,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  GreaterEqual,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  Less,
  /**
   * Pops two values off the stack, compares, then pushes the result back on
   */
  LessEqual,
  /**
   * Pops a value off the stack, and compars it with the peeked value, pushing the new value on
   */
  Check,
  /**
   * Pops two values off the stack, calculates the sum, then pushes the result back on
   */
  Add,
  /**
   * Pops two values off the stack, calculates the difference, then pushes the result back on
   */
  Sub,
  /**
   * Pops two values off the stack, calculates the product, then pushes the result back on
   */
  Mul,
  /**
   * Pops two values off the stack, calculates the quotient, then pushes the result back on
   */
  Div,
  /**
   * Pops two values off the stack, calculates the remainder, then pushes the result back on
   */
  Mod,
  /**
   * Pops a value off the stack, inverts its truthy value, then pushes that back on
   */
  Not,
  /**
   * Pops a value off the stack, inverts its numerical value, then pushes that back on
   */
  Negate,
  /**
   * Pops a value off the stack and prints it to the screen
   */
  Print,
  /**
   * Swaps the top two values on the stack.
   */
  Swap,
  /**
   * Jumps to a code location indicated by the tuple
   */
  Jump(usize),
  /**
   * Jumps to a code location indicated by the tuple
   */
  JumpIfFalse(usize),
  /**
   * Jumps the instruction pointer backwards N instructions. N specified by the tuple
   */
  Loop(usize),
  /**
   * Peeks at the stack, if the top value is true short circuits to the instruction pointed to by the tuple
   */
  Or(usize),
  /**
   * Peeks at the stack, if the top value is false short circuits to the instruction pointed to by the tuple
   */
  And(usize),
  /**
   * Pushes the stack pointer onto the stack
   */
  PushSp,
  /** Calls the instruction on the stack. Number of arguments is specified by the modifying bits */
  Call,
  /** Exits from a function */
  Return,
}

type Instructions = Vec<OpCode>;

#[derive(Debug, PartialEq)]
pub enum Token {
  Invalid,

  // Single-character tokens.
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  Comma,
  Dot,
  Semicolon,
  Plus,
  Minus,
  Asterisk,
  Slash,
  Modulus,

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
  End,
  False,
  For,
  Fn,
  If,
  Let,
  Load,
  Loop,
  Match,
  Nil,
  Or,
  Print,
  Ret,
  True,
  While,
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
    match self {
      Token::Identifier(i) => write!(f, "Identifier ({})", i),
      Token::String(s) => write!(f, "String ({})", s),
      Token::Number(n) => write!(f, "Number ({})", n),
      _ => write!(f, "{:?}", self),
    }
  }
}

pub struct CodeMeta;

impl CodeMeta {
  fn line_at(&self, offset: usize) -> usize {
    todo!();
  }
}

pub struct Context {
  instructions: Instructions,
  ip: usize,

  env: Env,
  stack: Vec<Value>,
  consts: Vec<Value>,

  meta: CodeMeta,
}

impl Context {
  pub fn new(instructions: Instructions, meta: CodeMeta) -> Self {
    Self {
      instructions,
      ip: 0,
      env: Env::new(),
      stack: Vec::new(),
      consts: Vec::new(),
      meta,
    }
  }

  pub fn done(&self) -> bool {
    self.ip >= self.instructions.len()
  }

  pub fn next(&self) -> OpCode {
    self.instructions[self.ip].clone()
  }

  pub fn advance(&mut self) {
    self.ip += 1;
  }

  pub fn stack_push(&mut self, value: Value) {
    self.stack.push(value);
  }

  pub fn stack_pop(&mut self) -> Option<Value> {
    self.stack.pop()
  }

  pub fn stack_pop_n(&mut self, count: usize) {
    self.stack.truncate(self.stack.len().saturating_sub(count));
  }

  pub fn stack_index(&self, index: usize) -> Option<Value> {
    self.stack.get(index).cloned()
  }

  pub fn stack_peek(&self) -> Option<Value> {
    self.stack.last().cloned()
  }

  pub fn stack_assign(&mut self, index: usize, value: Value) {
    self.stack[index] = value;
  }

  pub fn const_at(&self, index: usize) -> Option<Value> {
    self.consts.get(index).cloned()
  }

  pub fn lookup_global(&self, name: &String) -> Option<Value> {
    self.env.lookup(name)
  }

  pub fn define_global(&mut self, name: String, value: Value) -> bool {
    self.env.define(name, value)
  }

  pub fn assign_global(&mut self, name: String, value: Value) -> bool {
    self.env.assign(name, value)
  }

  pub fn jump(&mut self, count: usize) {
    self.ip = self.ip.saturating_add(count);
  }

  pub fn loop_back(&mut self, count: usize) {
    self.ip = self.ip.saturating_sub(count);
  }

  pub fn display_opcodes(&self) {
    println!("<< MAIN >>");
    for (i, op) in self.instructions.iter().enumerate() {
      self.display_instruction(op, i);
    }
    println!("<< END >>");
  }

  pub fn display_instruction(&self, op: &OpCode, offset: usize) {
    print!("0x{:#04X} ", offset);
    if offset > 0 && self.meta.line_at(offset) == self.meta.line_at(offset - 1) {
      print!("   | ");
    } else {
      print!("{:#04} ", self.meta.line_at(offset));
    }

    match op {
      OpCode::Const(index) => {
        print!("{:<16?} {:4} ", op, index);
        let c = self.const_at(*index);
        match c {
          Some(v) => println!("'{}'", v),
          None => println!("INVALID INDEX"),
        }
      }
      OpCode::PopN(count) => println!("{:<16?} {:4}", op, count),
      OpCode::LookupLocal(index) => println!("{:<16?} {:4}", op, index),
      OpCode::AssignLocal(index) => println!("{:<16?} {:4}", op, index),
      OpCode::LookupGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::DefineGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::AssignGlobal(name) => println!("{:<16?} '{}'", op, name),
      OpCode::Jump(count) => println!("{:<16?} {:4}", op, count),
      OpCode::JumpIfFalse(count) => println!("{:<16?} {:4}", op, count),
      OpCode::Loop(count) => println!("{:<16?} {:4}", op, count),
      OpCode::Or(count) => println!("{:<16?} {:4}", op, count),
      OpCode::And(count) => println!("{:<16?} {:4}", op, count),
      x => println!("{:?}", x),
    }
  }
}

struct Scanner<'src> {
  file: &'src str,
  raw_src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
  errors: Option<Vec<Error>>,
}

impl<'src> Scanner<'src> {
  fn new(file: &'src str, source: &'src str) -> Self {
    Scanner {
      file,
      raw_src: source.as_bytes(),
      start_pos: 0,
      pos: 0,
      line: 0,
      column: 0,
      errors: None,
    }
  }

  fn scan(&mut self) -> Result<Vec<Token>, Vec<Error>> {
    let mut tokens = Vec::new();

    loop {
      self.skip_whitespace();
      let mut should_advance = true;
      if let Some(c) = self.peek() {
        self.start_pos = self.pos;

        if cfg!(test) {
          println!(
            "pos = {}, line = {}, col = {}",
            self.pos, self.line, self.column,
          );
        }

        let token = match c {
          '(' => Token::LeftParen,
          ')' => Token::RightParen,
          '{' => Token::LeftBrace,
          '}' => Token::RightBrace,
          ',' => Token::Comma,
          '.' => Token::Dot,
          ';' => Token::Semicolon,
          '+' => Token::Plus,
          '-' => Token::Minus,
          '*' => Token::Asterisk,
          '/' => Token::Slash,
          '%' => Token::Modulus,
          '!' => {
            if self.advance_if_match('=') {
              Token::BangEqual
            } else {
              Token::Bang
            }
          }
          '=' => {
            if self.advance_if_match('=') {
              Token::EqualEqual
            } else if self.advance_if_match('>') {
              Token::Arrow
            } else {
              Token::Equal
            }
          }
          '<' => {
            if self.advance_if_match('=') {
              Token::LessEqual
            } else {
              Token::Less
            }
          }
          '>' => {
            if self.advance_if_match('=') {
              Token::GreaterEqual
            } else {
              Token::Greater
            }
          }
          '"' => self.make_string(),
          c if Self::is_digit(c) => {
            should_advance = false;
            self.make_number()
          }
          c if Self::is_alpha(c) => {
            should_advance = false;
            self.make_ident()
          }
          c => {
            self.error(format!("invalid character: '{}'", c));
            Token::Invalid
          }
        };

        if cfg!(test) {
          println!("made token {:?}", token);
        }

        tokens.push(token);

        if should_advance {
          self.advance();
        }
      } else {
        break;
      }
    }

    if let Some(errs) = self.errors.take() {
      Err(errs)
    } else {
      Ok(tokens)
    }
  }

  fn error(&mut self, msg: String) {
    if self.errors.is_none() {
      self.errors = Some(Vec::new());
    }

    if let Some(errs) = &mut self.errors {
      errs.push(Error {
        msg,
        file: String::from(self.file),
        line: self.line + 1,
        column: self.column + 1,
      });
    }
  }

  fn make_number(&mut self) -> Token {
    while let Some(c) = self.peek() {
      if Self::is_digit(c) {
        self.advance();
      } else {
        break;
      }
    }

    if let Some(c1) = self.peek() {
      if c1 == '.' {
        if let Some(c2) = self.peek_n(1) {
          if Self::is_digit(c2) {
            self.advance(); // advance past the '.'
            self.advance(); // advance past the first digit
            while let Some(c) = self.peek() {
              if Self::is_digit(c) {
                self.advance();
              } else {
                break;
              }
            }
          }
        }
      }
    }

    let lexeme = String::from_utf8_lossy(&self.raw_src[self.start_pos..self.pos]);

    match lexeme.parse() {
      Ok(n) => Token::Number(n),
      Err(e) => {
        self.error(format!("{} ('{}')", e, lexeme));
        Token::Invalid
      }
    }
  }

  fn make_string(&mut self) -> Token {
    self.advance(); // skip the first "
    let mut error_detected = false;
    while let Some(c) = self.peek() {
      match c {
        '"' => break,
        '\n' => {
          self.error(String::from("multiline strings are unsupported"));
          error_detected = true;
          self.advance();
          self.line += 1;
          self.column = 0;
        }
        _ => self.advance(),
      }
    }

    if error_detected {
      return Token::Invalid;
    }

    if self.at_end() {
      self.error(String::from("unterminated string"));
      return Token::Invalid;
    }

    match str::from_utf8(&self.raw_src[self.start_pos + 1..self.pos]) {
      Ok(string) => Token::String(String::from(string)),
      Err(e) => {
        self.error(format!("{}", e));
        Token::Invalid
      }
    }
  }

  fn make_ident(&mut self) -> Token {
    while let Some(c) = self.peek() {
      if Self::is_alphanumeric(c) {
        self.advance();
      } else {
        break;
      }
    }

    self.check_keywords()
  }

  fn create_ident(&mut self) -> Token {
    match str::from_utf8(&self.raw_src[self.start_pos..self.pos]) {
      Ok(string) => Token::Identifier(String::from(string)),
      Err(e) => {
        self.error(format!("{}", e));
        Token::Invalid
      }
    }
  }

  fn check_keywords(&mut self) -> Token {
    let do_at_depth =
      |this: &mut Self, depth, f: &dyn Fn(&mut Self, usize, char) -> Token| -> Token {
        if let Some(c) = this.index_n(this.start_pos + depth) {
          f(this, depth + 1, c)
        } else {
          this.create_ident()
        }
      };

    do_at_depth(self, 0, &|this, d, c0| match c0 {
      'a' => this.check_keyword(d, "nd", Token::And),
      'b' => this.check_keyword(d, "reak", Token::Break),
      'c' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'l' => this.check_keyword(d, "ass", Token::Class),
        'o' => this.check_keyword(d, "nt", Token::Cont),
        _ => this.create_ident(),
      }),
      'e' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'l' => this.check_keyword(d, "se", Token::Else),
        'n' => this.check_keyword(d, "d", Token::End),
        _ => this.create_ident(),
      }),
      'f' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'a' => this.check_keyword(d, "lse", Token::False),
        'n' => this.check_keyword(d, "", Token::Fn),
        'o' => this.check_keyword(d, "r", Token::For),
        _ => this.create_ident(),
      }),
      'i' => this.check_keyword(d, "f", Token::If),
      'l' => do_at_depth(this, d, &|this, d, c1| match c1 {
        'e' => this.check_keyword(d, "t", Token::Let),
        'o' => do_at_depth(this, d, &|this, d, c2| match c2 {
          'a' => this.check_keyword(d, "d", Token::Load),
          'o' => this.check_keyword(d, "p", Token::Loop),
          _ => this.create_ident(),
        }),
        _ => this.create_ident(),
      }),
      'm' => this.check_keyword(d, "atch", Token::Match),
      'n' => this.check_keyword(d, "il", Token::Nil),
      'o' => this.check_keyword(d, "r", Token::Or),
      'p' => this.check_keyword(d, "rint", Token::Print),
      'r' => this.check_keyword(d, "et", Token::Ret),
      't' => this.check_keyword(d, "rue", Token::True),
      'w' => this.check_keyword(d, "hile", Token::While),
      _ => this.create_ident(),
    })
  }

  fn check_keyword(&mut self, start: usize, rest: &str, checkee: Token) -> Token {
    let bytes = rest.as_bytes();
    let begin = self.start_pos + start;
    if self.pos - self.start_pos == start + rest.len()
      && &self.raw_src[begin..begin + rest.len()] == bytes
    {
      checkee
    } else {
      self.create_ident()
    }
  }

  fn skip_whitespace(&mut self) {
    while let Some(c) = self.peek() {
      match c {
        '#' => {
          while !self.at_end() {
            if let Some(c) = self.peek() {
              if c == '\n' {
                break;
              } else {
                self.advance();
              }
            } else {
              break;
            }
          }
        }
        '\n' => {
          self.advance();
          self.line += 1;
          self.column = 0;
        }
        c if c == ' ' || c == '\r' || c == '\t' => {
          self.advance();
        }
        _ => break,
      }
    }
  }

  fn at_end(&self) -> bool {
    self.pos >= self.raw_src.len()
  }

  fn peek(&self) -> Option<char> {
    self.raw_src.get(self.pos).map(|c| *c as char)
  }

  fn peek_n(&self, n: usize) -> Option<char> {
    self
      .raw_src
      .get(self.pos.saturating_add(n))
      .map(|c| *c as char)
  }

  fn index_n(&self, n: usize) -> Option<char> {
    self.raw_src.get(n).map(|c| *c as char)
  }

  fn advance(&mut self) {
    self.pos += 1;
    self.column += 1;
  }

  fn advance_if_match(&mut self, expected: char) -> bool {
    match self.peek_n(1) {
      Some(c) => {
        if c == expected {
          self.advance();
          true
        } else {
          false
        }
      }
      None => false,
    }
  }

  fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
  }

  fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '@'
  }

  fn is_alphanumeric(c: char) -> bool {
    Self::is_alpha(c) || Self::is_digit(c)
  }
}

pub struct Compiler;

impl Compiler {
  pub fn compile(&self, file: &str, source: &str) -> Result<Vec<Token>, Vec<Error>> {
    let mut scanner = Scanner::new(file, source);

    scanner
      .scan()
      .map_err(|errs| self.reformat_errors(source, errs))
  }

  fn reformat_errors(&self, source: &str, errs: Vec<Error>) -> Vec<Error> {
    errs
      .into_iter()
      .map(|mut e| {
        let mut line = source.lines().skip(e.line - 1);
        if let Some(line) = line.next() {
          let bottom = format!("{}^", " ".repeat(e.column - 1));
          e.msg = format!("{}\n{}\n{}", e.msg, line, bottom);
        }
        e
      })
      .collect()
  }
}

#[cfg(test)]
mod test;
