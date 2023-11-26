use crate::{util::FileIdType, Opcode, Token};
use std::{
  fmt::{self, Display, Formatter},
  num::ParseIntError,
  str::Utf8Error,
};

pub enum Error {
  Compiler(CompilerError),
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Error::Compiler(compiler_error) => write!(f, "{compiler_error}"),
    }
  }
}

pub enum CompilerError {
  Lexical(Vec<LexicalError>),
  AstGeneration(Vec<AstGenerationError>),
  BytecodeGeneration(Vec<BytecodeGenerationError>),
}

impl Display for CompilerError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      CompilerError::Lexical(errors) => {
        for error in errors.iter() {
          writeln!(f, "{error}")?;
        }
      }
      CompilerError::AstGeneration(errors) => {
        for error in errors.iter() {
          writeln!(f, "{error}")?;
        }
      }
      CompilerError::BytecodeGeneration(errors) => {
        for error in errors.iter() {
          writeln!(f, "{error}")?;
        }
      }
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompileError<M>
where
  M: Display + fmt::Debug,
{
  pub msg: M,
  pub file: Option<FileIdType>,
  pub line: usize,
  pub column: usize,
}

impl<M> CompileError<M>
where
  M: Display + fmt::Debug,
{
  pub fn new(msg: M, file: Option<FileIdType>, line: usize, column: usize) -> Self {
    Self { msg, file, line, column }
  }
}

impl<M> Display for CompileError<M>
where
  M: Display + fmt::Debug,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} ({}, {}): {}", self.file, self.line, self.column, self.msg)
  }
}

pub type LexicalError = CompileError<LexicalErrorMsg>;
pub type AstGenerationError = CompileError<AstGenerationErrorMsg>;
pub type BytecodeGenerationError = CompileError<BytecodeGenerationErrorMsg>;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum LexicalErrorMsg {
  #[error("Invalid character '{0}'")]
  InvalidCharacter(char),

  #[error("{0}")]
  ParseError(ParseIntError),

  #[error("Multiline strings are unsupported")]
  MultilineString,

  #[error("Unterminated string detected")]
  UnterminatedString,

  #[error("Invalid UTF-8 detected: {0}")]
  InvalidUtf8(Utf8Error),
}

#[derive(Debug, thiserror::Error)]
pub enum AstGenerationErrorMsg {
  #[error("Expected {0}")]
  FailedConsumption(Token),

  #[error("Expected an expression")]
  MissingExpression,

  #[error("Unexpected token {0}")]
  UnexpectedToken(Token),

  #[error("Found 'self' in an invalid context")]
  SelfNotAllowed,

  #[error("Unclosed scope detected")]
  UnclosedScope,

  #[error("Duplicate identifier detected")]
  DuplicateIdentifier,

  #[error("Duplicate class function detected")]
  DuplicateClassFunction,

  #[error("'self' must be the first parameter")]
  InvalidSelfPosition,

  #[error("Invalid identifier {0}")]
  InvalidIdentifier(Token),

  #[error("Invalid fn identifier {0}")]
  InvalidFnIdentifier(Token),

  #[error("Invalid parameter {0}")]
  InvalidParameter(Token),

  #[error("Invalid token detected in body: {0}")]
  InvalidToken(Token),

  #[error("'self' must be declared in class body")]
  ClassReprUndefined,

  #[error("Closure captures can only be identifiers")]
  InvalidCapture,

  #[error("Invalid expression")]
  InvalidExpression,

  #[error("Expected identifier")]
  MissingIdentifier,

  #[error("'self' is already defined in class")]
  ClassReprRedefinition,

  #[error("Class method must contain 'self'")]
  SelfParameterUndefined,

  #[error("Invalid number of parameters in overload, found {0} but expected {1}")]
  OperatorOverloadInvalidNumberOfParameters(usize, usize),

  #[error("'break' can only be used within loops")]
  InvalidBreakStatement,

  #[error("'cont' can only be used within loops")]
  InvalidContStatement,

  #[error("'ret' can only be used within functions")]
  InvalidRetStatement,

  #[error("Reached max precedence")]
  MaxPrecedence,

  #[error("'self' cannot be assigned to")]
  ImmutableSelf,

  #[error("Invalid LValue")]
  InvalidLValue,

  #[error("Multiple exports aren't allowed")]
  MultipleExports,

  /* Below this are errors triggered by broken ast generation logic */
  #[error("Could not find location of token at index {0}")]
  MissingTokenLocation(usize),

  #[error("Unable to find suitable location of token in backtrace. Original Err: {0}")]
  NoLocationForError(Box<Self>),

  #[error("Unexpected end of file")]
  UnexpectedEof,
}

#[derive(Debug, thiserror::Error)]
pub enum BytecodeGenerationErrorMsg {
  #[error("Exports can only be made at the surface scope")]
  InvalidExport,

  #[error("Returns can only be made within functions")]
  InvalidRet,

  /* Below this are errors triggered by broken bytecode generation logic */
  #[error("Instruction generation failure for opcode {0}")]
  InstructionGeneration(Opcode),

  #[error("Class functions should only be statics or methods")]
  InvalidClassFunction,

  #[error("'new' should be a static class function, not method")]
  MethodAsInitializer,

  #[error("Undeclared local variable '{0}'")]
  UndeclaredLocal(String),

  #[error("sanity check, should never happen")]
  SanityCheck,
}
