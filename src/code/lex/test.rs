use super::*;
use std::f64::consts::PI;
use tfix::{fixture, TestFixture};

const ALL_TOKENS: &str = include_str!("all_tokens.ss");

const CONSECUTIVE_TOKENS: &str = include_str!("consecutive_tokens.ss");

const SCANNING_ERRORS: &str = include_str!("scanning_errors.ss");

const TOKEN_META_TEST: &str = include_str!("token_meta_test.ss");

#[derive(Default)]
struct ScannerTest {
  tokens: String,
}

impl ScannerTest {
  fn test_scan<F: FnOnce(Vec<Token>, Vec<SourceLocation>)>(&self, f: F) {
    let mut scanner = Scanner::new("test", &self.tokens);
    match scanner.scan() {
      Ok((actual, meta)) => f(actual, meta),
      Err(errors) => {
        for error in errors {
          println!("{}", error);
        }
        panic!("failed to scan");
      }
    }
  }

  fn test_failure<F: FnOnce(Vec<RuntimeError>)>(&self, f: F) {
    let mut scanner = Scanner::new("test", &self.tokens);
    match scanner.scan() {
      Ok((_actual, _meta)) => panic!("should not have succeeded"),
      Err(errors) => f(errors),
    }
  }

  fn mk_meta(&self, line: usize, column: usize) -> SourceLocation {
    SourceLocation { line, column }
  }
}

impl TestFixture for ScannerTest {
  fn set_up() -> Self {
    Self::default()
  }
}

#[fixture(ScannerTest)]
mod tests {
  use super::*;

  #[test]
  fn scanner_scans(test: &mut ScannerTest) {
    let expected = vec![
      Token::LeftParen,
      Token::RightParen,
      Token::LeftBrace,
      Token::RightBrace,
      Token::Comma,
      Token::Dot,
      Token::Semicolon,
      Token::Plus,
      Token::Minus,
      Token::Asterisk,
      Token::Slash,
      Token::Percent,
      Token::Bang,
      Token::BangEqual,
      Token::Equal,
      Token::EqualEqual,
      Token::Greater,
      Token::GreaterEqual,
      Token::Less,
      Token::LessEqual,
      Token::Arrow,
      Token::BackArrow,
      Token::Identifier(String::from("foobar")),
      Token::String(String::from("some string")),
      Token::Number(PI),
      Token::And,
      Token::Break,
      Token::Class,
      Token::Cont,
      Token::Else,
      Token::False,
      Token::For,
      Token::Fn,
      Token::If,
      Token::Let,
      Token::Loop,
      Token::Match,
      Token::Nil,
      Token::Or,
      Token::Print,
      Token::Req,
      Token::Ret,
      Token::True,
      Token::While,
    ];

    test.tokens = ALL_TOKENS.to_string();

    test.test_scan(|actual, _| {
      assert_eq!(actual.len(), expected.len());

      for (a, e) in actual.iter().zip(expected.iter()) {
        assert_eq!(a, e);
      }
    });
  }

  #[test]
  fn scanner_scans_consecutively(t: &mut ScannerTest) {
    let expected = vec![
      Token::LeftParen,
      Token::RightParen,
      Token::LeftBrace,
      Token::RightBrace,
      Token::Comma,
      Token::Dot,
      Token::Semicolon,
      Token::Plus,
      Token::Minus,
      Token::Asterisk,
      Token::Slash,
      Token::Percent,
      Token::Identifier(String::from("bang")),
      Token::Bang,
      Token::Identifier(String::from("bang_equal")),
      Token::BangEqual,
      Token::Identifier(String::from("equal")),
      Token::Equal,
      Token::Identifier(String::from("equal_equal")),
      Token::EqualEqual,
      Token::Identifier(String::from("greater")),
      Token::Greater,
      Token::Identifier(String::from("greater_equal")),
      Token::GreaterEqual,
      Token::Identifier(String::from("less")),
      Token::Less,
      Token::Identifier(String::from("less_equal")),
      Token::LessEqual,
      Token::Identifier(String::from("arrow")),
      Token::Arrow,
      Token::Number(1.0),
      Token::Bang,
      Token::Number(1.0),
      Token::BangEqual,
      Token::Number(12.0),
      Token::Equal,
      Token::Number(123.0),
      Token::EqualEqual,
      Token::Number(1234.0),
      Token::Greater,
      Token::Number(12345.0),
      Token::GreaterEqual,
      Token::Number(123456.0),
      Token::Less,
      Token::Number(1234567.0),
      Token::LessEqual,
      Token::Number(12345678.0),
      Token::Arrow,
      Token::String(String::from("bang")),
      Token::Bang,
      Token::String(String::from("bang_equal")),
      Token::BangEqual,
      Token::String(String::from("equal")),
      Token::Equal,
      Token::String(String::from("equal_equal")),
      Token::EqualEqual,
      Token::String(String::from("greater")),
      Token::Greater,
      Token::String(String::from("greater_equal")),
      Token::GreaterEqual,
      Token::String(String::from("less")),
      Token::Less,
      Token::String(String::from("less_equal")),
      Token::LessEqual,
      Token::String(String::from("arrow")),
      Token::Arrow,
    ];

    t.tokens = CONSECUTIVE_TOKENS.to_string();

    t.test_scan(|actual, _| {
      assert_eq!(
        actual.len(),
        expected.len(),
        "actual len = {}, expected len = {}\nactual tokens = {:#?}\nexpected tokens = {:#?}",
        actual.len(),
        expected.len(),
        actual,
        expected
      );

      for (a, e) in actual.iter().zip(expected.iter()) {
        assert_eq!(a, e, "actual = {:?}, expected = {:?}", a, e);
      }
    });
  }

  #[test]
  fn scans_empty_string(t: &mut ScannerTest) {
    let expected = vec![Token::String(String::from(""))];

    t.tokens = r#""""#.to_string();

    t.test_scan(|actual, _| {
      assert_eq!(actual.len(), expected.len());

      for (a, e) in actual.iter().zip(expected.iter()) {
        assert_eq!(a, e);
      }
    });
  }

  #[test]
  fn returns_errors_at_right_location_when_detected(t: &mut ScannerTest) {
    let expected = vec![
      RuntimeError {
        msg: String::from("invalid character: '^'"),
        file: String::from("test"),
        line: 6,
        column: 8,
      },
      RuntimeError {
        msg: String::from("invalid character: '?'"),
        file: String::from("test"),
        line: 7,
        column: 9,
      },
      RuntimeError {
        msg: String::from("invalid character: '`'"),
        file: String::from("test"),
        line: 7,
        column: 10,
      },
    ];

    t.tokens = SCANNING_ERRORS.to_string();

    t.test_failure(|actual| {
      assert_eq!(
        actual.len(),
        expected.len(),
        "actual len = {}, expected len = {}",
        actual.len(),
        expected.len(),
      );

      for (a, e) in actual.iter().zip(expected.iter()) {
        assert_eq!(a, e, "actual = {:?}, expected = {:?}", a, e);
      }
    });
  }

  #[test]
  fn produces_the_correct_meta_information(t: &mut ScannerTest) {
    let expected_tokens = vec![
      Token::Fn,
      Token::Identifier(String::from("foo")),
      Token::LeftParen,
      Token::RightParen,
      Token::LeftBrace,
      Token::Print,
      Token::Number(1.0),
      Token::Semicolon,
      Token::RightBrace,
      Token::Identifier(String::from("foo")),
      Token::LeftParen,
      Token::RightParen,
      Token::Semicolon,
    ];
    let expected_meta = vec![
      t.mk_meta(1, 1),
      t.mk_meta(1, 4),
      t.mk_meta(1, 7),
      t.mk_meta(1, 8),
      t.mk_meta(1, 10),
      t.mk_meta(2, 3),
      t.mk_meta(2, 9),
      t.mk_meta(2, 10),
      t.mk_meta(3, 1),
      t.mk_meta(5, 1),
      t.mk_meta(5, 4),
      t.mk_meta(5, 5),
      t.mk_meta(5, 6),
    ];

    t.tokens = TOKEN_META_TEST.to_string();

    t.test_scan(|actual_tokens, actual_meta| {
      assert_eq!(actual_tokens.len(), expected_tokens.len());
      assert_eq!(actual_meta.len(), expected_meta.len());

      for (a, e) in actual_tokens.iter().zip(expected_tokens.iter()) {
        assert_eq!(a, e);
      }

      for (i, (a, e)) in actual_meta.iter().zip(expected_meta.iter()).enumerate() {
        assert_eq!(a, e, "Token::{}", expected_tokens[i]);
      }
    });
  }
}
