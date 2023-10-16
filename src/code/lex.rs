use super::*;
use std::{ops::RangeInclusive, str};

#[cfg(test)]
mod test;

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
  Colon,
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

  // Literals.
  Identifier(String),
  String(String),
  Number(NumberToken),

  // Keywords.
  And,
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
  Print,
  Req,
  Ret,
  Struct,
  True,
  Use,
  While,
  Yield,

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
  type Error = Box<dyn Error>;

  fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
    Ok(match str::from_utf8(bytes)? {
      "and" => Self::And,
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
      "print" => Self::Print,
      "req" => Self::Req,
      "ret" => Self::Ret,
      "struct" => Self::Struct,
      "true" => Self::True,
      "use" => Self::Use,
      "while" => Self::While,
      "yield" => Self::Yield,
      "__breakpoint__" => Self::Breakpoint,
      word => Self::Identifier(String::from(word)),
    })
  }
}

pub struct Scanner<'src> {
  file: Rc<PathBuf>,
  src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
  errors: Option<Vec<RuntimeError>>,
}

impl<'src> Scanner<'src> {
  pub fn new(file: Rc<PathBuf>, source: &'src str) -> Self {
    Scanner {
      file,
      src: source.as_bytes(),
      start_pos: 0,
      pos: 0,
      line: 0,
      column: 0,
      errors: None,
    }
  }

  pub fn scan(&mut self) -> Result<(Vec<Token>, Vec<SourceLocation>), Vec<RuntimeError>> {
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
          ':' => Token::Colon,
          ';' => Token::Semicolon,
          '@' => Token::At,
          '|' => Token::Pipe,
          '+' => {
            if self.advance_if_match('=') {
              Token::PlusEqual
            } else {
              Token::Plus
            }
          }
          '-' => {
            if self.advance_if_match('=') {
              Token::MinusEqual
            } else if self.advance_if_match('>') {
              Token::Arrow
            } else {
              Token::Minus
            }
          }
          '*' => {
            if self.advance_if_match('=') {
              Token::AsteriskEqual
            } else {
              Token::Asterisk
            }
          }
          '/' => {
            if self.advance_if_match('=') {
              Token::SlashEqual
            } else {
              Token::Slash
            }
          }
          '%' => {
            if self.advance_if_match('=') {
              Token::PercentEqual
            } else {
              Token::Percent
            }
          }
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
            } else {
              Token::Equal
            }
          }
          '<' => {
            if self.advance_if_match('=') {
              Token::LessEqual
            } else if self.advance_if_match('-') {
              Token::BackArrow
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
          '"' => {
            if let Some(tok) = self.make_string() {
              tok
            } else {
              continue;
            }
          }
          c if Self::is_digit(c) => {
            if let Some(tok) = self.make_number() {
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
          file: Rc::clone(&self.file),
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

    if let Some(errs) = self.errors.take() {
      Err(errs)
    } else {
      Ok((tokens, meta))
    }
  }

  fn error(&mut self, msg: String) {
    if self.errors.is_none() {
      self.errors = Some(Vec::new());
    }

    if let Some(errs) = &mut self.errors {
      errs.push(RuntimeError {
        msg,
        file: Rc::clone(&self.file),
        line: self.line + 1,
        column: self.column + 1,
      });
    }
  }

  fn make_number(&mut self) -> Option<Token> {
    let mut is_float = false;

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
            is_float = true;
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

    let lexeme = String::from_utf8_lossy(&self.src[self.start_pos..self.pos]);

    if is_float {
      match lexeme.parse() {
        Ok(n) => Some(Token::Number(NumberToken::F64(n))),
        Err(e) => {
          self.error(format!("{} ('{}')", e, lexeme));
          None
        }
      }
    } else {
      match lexeme.parse() {
        Ok(n) => Some(Token::Number(NumberToken::I32(n))),
        Err(e) => {
          self.error(format!("{} ('{}')", e, lexeme));
          None
        }
      }
    }
  }

  fn make_string(&mut self) -> Option<Token> {
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
      return None;
    }

    if self.at_end() {
      self.error(String::from("unterminated string"));
      return None;
    }

    match str::from_utf8(&self.src[self.start_pos + 1..self.pos]) {
      Ok(string) => Some(Token::String(String::from(string))),
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
    const ZERO_TO_NINE: RangeInclusive<char> = '0'..='9';
    ZERO_TO_NINE.contains(&c)
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
