use crate::{
  error::CompiletimeError,
  util::{self, FileIdType},
};

use super::*;
use std::{ops::RangeInclusive, str};

#[cfg(test)]
mod test;

macro_rules! collect_digits {
  ($this:ident, $f:path, $digits:ident) => {
    while let Some(c) = $this.peek() {
      if $f(c) {
        $digits.push(c);
        $this.advance();
      } else if c == '_' {
        $this.advance()
      } else {
        break;
      }
    }
  };
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumberToken {
  I32(i32),
  F64(f64),
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
  Let,
  Loop,
  Match,
  Mod,
  New,
  Nil,
  Or,
  Println,
  Req,
  Ret,
  Struct,
  True,
  Use,
  While,

  // Special
  Breakpoint,
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
  type Error = Box<dyn std::error::Error>;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Ok(match str::from_utf8(bytes)? {
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
      "let" => Self::Let,
      "loop" => Self::Loop,
      "match" => Self::Match,
      "mod" => Self::Mod,
      "new" => Self::New,
      "nil" => Self::Nil,
      "or" => Self::Or,
      "println" => Self::Println,
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

pub struct Scanner<'src> {
  file_id: Option<FileIdType>,
  src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
  errors: CompiletimeErrors,
}

impl<'src> Scanner<'src> {
  pub fn new(file_id: Option<FileIdType>, source: &'src str) -> Self {
    Scanner {
      file_id,
      src: source.as_bytes(),
      start_pos: 0,
      pos: 0,
      line: 0,
      column: 0,
      errors: Default::default(),
    }
  }

  pub fn into_tokens(mut self) -> Result<(Vec<Token>, Vec<SourceLocation>), CompiletimeErrors> {
    let mut tokens = Vec::new();
    let mut meta = Vec::new();

    loop {
      self.skip_whitespace();
      let mut should_advance = true;
      if let Some(c) = self.peek() {
        self.start_pos = self.pos;

        let line = self.line;
        let column = self.column;

        if cfg!(test) {
          println!("pos = {}, line = {}, col = {}", self.pos, self.line, self.column,);
        }

        let token = match c {
          '(' => Token::LeftParen,
          ')' => Token::RightParen,
          '{' => Token::LeftBrace,
          '}' => Token::RightBrace,
          '[' => Token::LeftBracket,
          ']' => Token::RightBracket,
          ',' => Token::Comma,
          '.' => Token::Dot,
          ':' => {
            if self.advance_if_next(':') {
              Token::ColonColon
            } else {
              Token::Colon
            }
          }
          ';' => Token::Semicolon,
          '@' => Token::At,
          '|' => Token::Pipe,
          '+' => {
            if self.advance_if_next('=') {
              Token::PlusEqual
            } else {
              Token::Plus
            }
          }
          '-' => {
            if self.advance_if_next('=') {
              Token::MinusEqual
            } else {
              match self.peek_n(1) {
                Some(d) if Self::is_digit(d) => {
                  if let Some(tok) = self.make_number(true) {
                    should_advance = false;
                    tok
                  } else {
                    continue;
                  }
                }
                Some(_) => Token::Minus,
                None => continue,
              }
            }
          }
          '*' => {
            if self.advance_if_next('=') {
              Token::AsteriskEqual
            } else {
              Token::Asterisk
            }
          }
          '/' => {
            if self.advance_if_next('=') {
              Token::SlashEqual
            } else {
              Token::Slash
            }
          }
          '%' => {
            if self.advance_if_next('=') {
              Token::PercentEqual
            } else {
              Token::Percent
            }
          }
          '!' => {
            if self.advance_if_next('=') {
              Token::BangEqual
            } else {
              Token::Bang
            }
          }
          '=' => {
            if self.advance_if_next('=') {
              Token::EqualEqual
            } else if self.advance_if_next('>') {
              Token::Arrow
            } else {
              Token::Equal
            }
          }
          '<' => {
            if self.advance_if_next('=') {
              Token::LessEqual
            } else if self.advance_if_next('-') {
              Token::BackArrow
            } else {
              Token::Less
            }
          }
          '>' => {
            if self.advance_if_next('=') {
              Token::GreaterEqual
            } else {
              Token::Greater
            }
          }
          '"' => {
            if let Some(tok) = self.make_string::<'"'>() {
              tok
            } else {
              continue;
            }
          }
          '\'' => {
            if let Some(tok) = self.make_string::<'\''>() {
              tok
            } else {
              continue;
            }
          }
          c if Self::is_digit(c) => {
            if let Some(tok) = self.make_number(false) {
              should_advance = false;
              tok
            } else {
              // sanity check, should never fail
              self.advance();
              continue;
            }
          }
          c if Self::is_alpha(c) => {
            if let Some(tok) = self.make_word() {
              should_advance = false;
              tok
            } else {
              // sanity check, should never fail
              self.advance();
              continue;
            }
          }
          c => {
            self.error(format!("invalid character: '{}'", c));
            self.advance();
            continue;
          }
        };

        if cfg!(test) {
          println!("{}: {:?}", tokens.len(), token);
        }

        tokens.push(token);
        meta.push(SourceLocation {
          line: line + 1,
          column: column + 1,
        });

        if should_advance {
          self.advance();
        }
      } else {
        break;
      }
    }

    if self.errors.is_empty() {
      Ok((tokens, meta))
    } else {
      Err(self.errors)
    }
  }

  fn error(&mut self, msg: impl ToString) {
    self.errors.add(CompiletimeError {
      msg: msg.to_string(),
      file_display: self.file_id.map(FileDisplay::Id),
      line: self.line + 1,
      column: self.column + 1,
    });
  }

  /// supports the following numbers
  /// 1, 01, 0x1, 1.0, 1.0e1, 1.0e-1
  fn make_number(&mut self, negate: bool) -> Option<Token> {
    match self.peek()? {
      '0' if !matches!(self.peek_n(1), Some('.')) => {
        self.advance();
        match self.peek()? {
          'x' | 'X' => {
            self.advance();
            self.make_hex(negate)
          }
          d if Self::is_digit(d) => self.make_octal(negate),
          _ => Some(Token::Number(NumberToken::I32(0))),
        }
      }
      _ => self.make_decimal(negate),
    }
  }

  fn make_hex(&mut self, negate: bool) -> Option<Token> {
    let mut digits = Vec::new();

    collect_digits!(self, Self::is_hex, digits);

    let lexeme = digits.iter().collect::<String>();

    match i32::from_str_radix(&lexeme, 16) {
      Ok(mut int) => {
        if negate {
          int = -int;
        }

        Some(Token::Number(NumberToken::I32(int)))
      }
      Err(e) => {
        self.error(e);
        None
      }
    }
  }

  fn make_octal(&mut self, negate: bool) -> Option<Token> {
    let mut digits = Vec::new();

    collect_digits!(self, Self::is_oct, digits);

    let lexeme = digits.iter().collect::<String>();
    match i32::from_str_radix(&lexeme, 8) {
      Ok(mut int) => {
        if negate {
          int = -int;
        }

        Some(Token::Number(NumberToken::I32(int)))
      }
      Err(e) => {
        self.error(e);
        None
      }
    }
  }

  fn make_decimal(&mut self, negate: bool) -> Option<Token> {
    let mut is_float = false;
    let mut digits = Vec::new();

    collect_digits!(self, Self::is_digit, digits);

    if let Some(c1) = self.peek() {
      if c1 == '.' {
        digits.push(c1);
        if let Some(c2) = self.peek_n(1) {
          if Self::is_digit(c2) {
            digits.push(c2);
            is_float = true;
            self.advance(); // advance past the '.'
            self.advance(); // advance past the first digit

            collect_digits!(self, Self::is_digit, digits);
          }
        }
      }
    }

    let lexeme = digits.iter().collect::<String>();

    if is_float {
      let mut number = lexeme.parse::<f64>().unwrap();

      if let Some(c) = self.peek() {
        if c == 'e' || c == 'E' {
          const EVAL: f64 = 10.0;

          self.advance();

          let negate = self.advance_if_match('-');

          let mut exp = Vec::new();
          collect_digits!(self, Self::is_digit, exp);
          let lexeme = exp.iter().collect::<String>();

          match lexeme.parse::<i32>() {
            Ok(mut exp) => {
              if negate {
                exp = -exp;
              }

              number *= EVAL.powf(exp as f64);
            }
            Err(e) => {
              self.error(e);
              return None;
            }
          }
        }
      }

      if negate {
        number = -number;
      }

      Some(Token::Number(NumberToken::F64(number)))
    } else {
      match lexeme.parse::<i32>() {
        Ok(mut int) => {
          if let Some(c) = self.peek() {
            if c == 'e' || c == 'E' {
              const EVAL: f64 = 10.0;

              let mut fint = int as f64;

              self.advance();

              let negate = self.advance_if_match('-');

              let mut exp = Vec::new();
              collect_digits!(self, Self::is_digit, exp);
              let lexeme = exp.iter().collect::<String>();

              match lexeme.parse::<i32>() {
                Ok(mut exp) => {
                  if negate {
                    exp = -exp;
                  }

                  fint *= EVAL.powf(exp as f64);

                  if negate {
                    fint = -fint;
                  }

                  return Some(Token::Number(NumberToken::F64(fint)));
                }
                Err(e) => {
                  self.error(e);
                  return None;
                }
              }
            }
          }

          if negate {
            int = -int;
          }

          Some(Token::Number(NumberToken::I32(int)))
        }
        Err(e) => {
          self.error(e);
          None
        }
      }
    }
  }

  fn make_string<const C: char>(&mut self) -> Option<Token> {
    self.advance(); // skip the first "
    let mut error_detected = false;
    while let Some(c) = self.peek() {
      match c {
        '\n' => {
          self.error(String::from("multiline strings are unsupported"));
          error_detected = true;
          self.advance();
          self.line += 1;
          self.column = 0;
        }
        c if c == C => break,
        c => {
          if c == '\\' {
            // backslash must always be followed by something
            self.advance();
          }
          self.advance()
        }
      }
    }

    if error_detected {
      return None;
    }

    if self.at_end() {
      self.error(String::from("unterminated string"));
      return None;
    }

    match str::from_utf8(&self.src[self.start_pos + 1..self.pos]) {
      Ok(string) => {
        if C == '"' {
          match util::strproc::escape(string) {
            Ok(string) => Some(Token::String(string)),
            Err(e) => {
              self.error(format!("{}", e));
              None
            }
          }
        } else {
          Some(Token::String(string.to_string()))
        }
      }
      Err(e) => {
        self.error(format!("{}", e));
        None
      }
    }
  }

  fn current_word(&self) -> &[u8] {
    &self.src[self.start_pos..self.pos]
  }

  fn make_word(&mut self) -> Option<Token> {
    while let Some(c) = self.peek() {
      if Self::is_alphanumeric(c) {
        self.advance();
      } else {
        break;
      }
    }

    match Token::try_from(self.current_word()) {
      Ok(t) => Some(t),
      Err(e) => {
        self.error(e.to_string());
        None
      }
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
    self.pos >= self.src.len()
  }

  fn peek(&self) -> Option<char> {
    self.src.get(self.pos).map(|c| *c as char)
  }

  fn peek_n(&self, n: usize) -> Option<char> {
    self.src.get(self.pos.saturating_add(n)).map(|c| *c as char)
  }

  fn advance(&mut self) {
    self.pos += 1;
    self.column += 1;
  }

  fn advance_if_match(&mut self, expected: char) -> bool {
    match self.peek() {
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

  fn advance_if_next(&mut self, expected_next: char) -> bool {
    match self.peek_n(1) {
      Some(c) => {
        if c == expected_next {
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
    const ZERO_TO_NINE: RangeInclusive<char> = '0'..='9';
    ZERO_TO_NINE.contains(&c)
  }

  fn is_hex(c: char) -> bool {
    const HEX_LOCASE: RangeInclusive<char> = 'a'..='f';
    const HEX_UPCASE: RangeInclusive<char> = 'A'..='F';
    Self::is_digit(c) || HEX_LOCASE.contains(&c) || HEX_UPCASE.contains(&c)
  }

  fn is_oct(c: char) -> bool {
    const OCT: RangeInclusive<char> = '0'..='7';
    OCT.contains(&c)
  }

  fn is_alpha(c: char) -> bool {
    const A_Z_LOCASE: RangeInclusive<char> = 'a'..='z';
    const A_Z_UPCASE: RangeInclusive<char> = 'A'..='Z';
    A_Z_LOCASE.contains(&c) || A_Z_UPCASE.contains(&c) || c == '_' || c == '$'
  }

  fn is_alphanumeric(c: char) -> bool {
    Self::is_alpha(c) || Self::is_digit(c)
  }
}
