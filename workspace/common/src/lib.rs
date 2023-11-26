pub mod errors;
pub mod util;

pub use errors::Error;

use std::{
  fmt::{self, Display, Formatter},
  str::Utf8Error,
};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
  pub line: usize,
  pub column: usize,
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
  Semicolon,
  At,
  Pipe,

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
  BackArrow,
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
  Colon,
  ColonColon,

  // Literals.
  Identifier(String),
  String(String),
  Number(NumberToken),

  // Keywords.
  And,
  As,
  Break,
  Class,
  Cont,
  Else,
  Export,
  False,
  For,
  Fn,
  If,
  Is,
  Let,
  Loop,
  Match,
  Mod,
  New,
  Nil,
  Or,
  Println,
  Quack,
  Req,
  Ret,
  Struct,
  True,
  Use,
  While,

  // Special
  Breakpoint,
}

impl Token {
  /// Gets the char repr of a single character token
  ///
  /// Returns None if not a single char token
  pub fn chr(&self) -> Option<char> {
    match self {
      Self::RightParen => Some('('),
      Self::Pipe => Some('|'),
      _ => None,
    }
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Identifier(i) => write!(f, "Identifier ({})", i),
      Self::String(s) => write!(f, "String ({})", s),
      Self::Number(n) => write!(
        f,
        "Number ({})",
        match n {
          NumberToken::I32(i) => i.to_string(),
          NumberToken::F64(f) => f.to_string(),
        }
      ),
      _ => write!(f, "{:?}", self),
    }
  }
}

impl From<f64> for Token {
  fn from(v: f64) -> Self {
    Self::Number(NumberToken::F64(v))
  }
}

impl From<i32> for Token {
  fn from(v: i32) -> Self {
    Self::Number(NumberToken::I32(v))
  }
}

impl TryFrom<&[u8]> for Token {
  type Error = Utf8Error;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Ok(match std::str::from_utf8(bytes)? {
      "and" => Self::And,
      "as" => Self::As,
      "break" => Self::Break,
      "class" => Self::Class,
      "cont" => Self::Cont,
      "else" => Self::Else,
      "export" => Self::Export,
      "false" => Self::False,
      "fn" => Self::Fn,
      "for" => Self::For,
      "if" => Self::If,
      "is" => Self::Is,
      "let" => Self::Let,
      "loop" => Self::Loop,
      "match" => Self::Match,
      "mod" => Self::Mod,
      "new" => Self::New,
      "nil" => Self::Nil,
      "or" => Self::Or,
      "println" => Self::Println,
      "quack" => Self::Quack,
      "req" => Self::Req,
      "ret" => Self::Ret,
      "struct" => Self::Struct,
      "true" => Self::True,
      "use" => Self::Use,
      "while" => Self::While,
      "__breakpoint__" => Self::Breakpoint,
      word => Self::Identifier(String::from(word)),
    })
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberToken {
  I32(i32),
  F64(f64),
}

macro_rules! opstr {
  ($op:ident) => {
    concat!("__", stringify!($op), "__")
  };
}

pub mod ops {
  pub const NOT: &str = opstr!(not);
  pub const NEG: &str = opstr!(neg);

  pub const ADD: &str = opstr!(add);
  pub const SUB: &str = opstr!(sub);
  pub const MUL: &str = opstr!(mul);
  pub const DIV: &str = opstr!(div);
  pub const REM: &str = opstr!(rem);

  pub const EQUALITY: &str = opstr!(eq);
  pub const NOT_EQUAL: &str = opstr!(neq);
  pub const LESS: &str = opstr!(less);
  pub const LESS_EQUAL: &str = opstr!(leq);
  pub const GREATER: &str = opstr!(greater);
  pub const GREATER_EQUAL: &str = opstr!(geq);

  pub const INDEX: &str = opstr!(index);
  pub const INDEX_ASSIGN: &str = opstr!(idxeq);

  pub const CALL: &str = opstr!(call);
}
