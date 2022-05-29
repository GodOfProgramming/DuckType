use super::*;
use crate::types::Error;
use std::str;

#[cfg(test)]
mod test;

pub struct Scanner<'src> {
  file: &'src str,
  raw_src: &'src [u8],
  start_pos: usize,
  pos: usize,
  line: usize,
  column: usize,
  errors: Option<Vec<Error>>,
}

impl<'src> Scanner<'src> {
  pub fn new(file: &'src str, source: &'src str) -> Self {
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

  pub fn scan(&mut self) -> Result<(Vec<Token>, Vec<TokenMeta>), Vec<Error>> {
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
          println!("{}: {:?}", tokens.len(), token);
        }

        tokens.push(token);
        meta.push(TokenMeta {
          file: self.file,
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
    ('0'..='9').contains(&c)
  }

  fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' || c == '@'
  }

  fn is_alphanumeric(c: char) -> bool {
    Self::is_alpha(c) || Self::is_digit(c)
  }
}
