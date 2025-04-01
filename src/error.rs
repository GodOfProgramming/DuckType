use crate::{Opcode, code::lex::Token, util::FileIdType};
use crate::{
  code::FileMap,
  prelude::{Instruction, Storage},
  value::Value,
};
use std::{
  fmt::{self, Debug, Display, Formatter},
  io,
  path::PathBuf,
};
use std::{num::ParseIntError, str::Utf8Error};

pub type UsageResult<T = Value> = Result<T, UsageError>;

pub enum Error {
  Single(FormattedError),
  Multiple(Vec<FormattedError>),
  Plain(String),
}

impl Error {
  pub(crate) fn single(file: impl ToString, line: usize, column: usize, msg: impl ToString, source: &str) -> Self {
    Self::Single(FormattedError::from_parts(
      file,
      line,
      column,
      msg,
      Self::source_line(source, line),
      Self::indicator(column),
    ))
  }

  pub(crate) fn from_compiler_error(error: CompilerError, file_map: &FileMap, source: &str) -> Self {
    match error {
      CompilerError::Lexical(errors) => Self::Multiple(Self::from_compile_errors(errors, file_map, source)),
      CompilerError::AstGeneration(errors) => Self::Multiple(Self::from_compile_errors(errors, file_map, source)),
      CompilerError::BytecodeGeneration(errors) => Self::Multiple(Self::from_compile_errors(errors, file_map, source)),
    }
  }

  fn from_compile_errors<M>(errors: Vec<CompileError<M>>, file_map: &FileMap, source: &str) -> Vec<FormattedError>
  where
    M: Display + fmt::Debug,
  {
    errors
      .into_iter()
      .map(|error| {
        FormattedError::from_parts(
          file_map.get(error.file).display(),
          error.line,
          error.column,
          error.msg,
          Self::source_line(source, error.line),
          Self::indicator(error.column),
        )
      })
      .collect()
  }

  fn source_line(source: &str, line: usize) -> String {
    source.lines().nth(line - 1).map(|line| line.to_string()).unwrap_or_default()
  }

  fn indicator(column: usize) -> String {
    format!("{}^", " ".repeat(column - 1),)
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Error::Single(error) => write!(f, "{error}"),
      Error::Multiple(errors) => {
        write!(f, "{}", itertools::join(errors, "\n"))
      }
      Error::Plain(error) => write!(f, "{error}"),
    }
  }
}

impl Debug for Error {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    <Self as Display>::fmt(self, f)
  }
}

pub struct FormattedError {
  file: String,
  line: usize,
  column: usize,
  msg: String,
  line_text: String,
  indicator: String,
}

impl FormattedError {
  fn from_parts(
    file: impl ToString,
    line: usize,
    column: usize,
    msg: impl ToString,
    line_text: impl ToString,
    indicator: impl ToString,
  ) -> Self {
    Self {
      file: file.to_string(),
      line,
      column,
      msg: msg.to_string(),
      line_text: line_text.to_string(),
      indicator: indicator.to_string(),
    }
  }
}

impl Display for FormattedError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(
      f,
      "{} ({}, {}): {}\n{}\n{}",
      self.file, self.line, self.column, self.msg, self.line_text, self.indicator
    )
  }
}

impl From<io::Error> for Error {
  fn from(error: io::Error) -> Self {
    Self::Plain(error.to_string())
  }
}

#[derive(Debug, thiserror::Error)]
pub enum UsageError {
  /// fn name
  #[error("MissingSelf: missing self in call to {0}")]
  MissingSelf(&'static str),

  /// fn name, argument index
  #[error("InvalidArgument: wrong type passed to {0} in argument position {1}")]
  InvalidArgument(&'static str, usize),

  /// fn name, type, actual/this
  #[error("BadCast: wrong cast in {0} (casting from {2} to {1})")]
  BadCast(&'static str, &'static str, Value),

  /// given, expected
  #[error("ArgumentError: wrong number of arguments (given {0} expected {1})")]
  ArgumentError(usize, usize),

  /// tried, needed
  #[error("CoercionError: cannot coerce {0} to {1}")]
  CoercionError(Value, &'static str),

  /// name
  #[error("Undefined variable {0}")]
  UndefinedVar(String),

  /// op, lhs, rhs
  #[error("Tried to perform '{0}' with {1} and {2}")]
  InvalidOperation(char, Value, Value),

  #[error("{0} is not implemented")]
  Unimplemented(&'static str),

  /// method, value
  #[error("{0} not implemented for {1}")]
  UnimplementedError(&'static str, Value),

  #[error("{0} is undefined for {0}")]
  UndefinedMethod(&'static str, String),

  /// index, value
  #[error("Index {0} out of bounds in {1}")]
  InvalidIndex(i32, Value),

  /// member name
  #[error("Tried assigning a value to unimplemented member {0}")]
  InvalidAssignment(String),

  /// value
  #[error("Tried to lookup a member on a primitive '{0}'")]
  InvalidLookup(Value),

  /// ident
  #[error("Cannot modify immutable object '{0}'")]
  Immutable(String),

  /// ident
  #[error("Tried to access undefined member '{0}'")]
  UndefinedMember(String),

  // idx
  #[error("Tried to access undefined member id '{0}'")]
  UndefinedMemberId(usize),

  /// actual, expected
  #[error("{0} is not a {1}")]
  TypeError(String, String),

  /// ident
  #[error("use of undefined variable {0}")]
  NameError(String),

  /// Native function produced an error
  /// fn name
  #[error("error in native function: {0}")]
  NativeApi(String),

  /// Default return value for usertype initializers
  #[error("Undefined initializer reached")]
  UndefinedInitializer,

  #[error("unable to find file, tried: {0:#?}")]
  BadReq(Vec<PathBuf>),

  #[error("unexpected nil value")]
  UnexpectedNil,

  #[error("Expected field name but found none")]
  EmptyField,

  #[error("quack {0}")]
  Quack(Value),

  /* Below can only be reached from bad bytecode generation */
  #[error("Empty stack")]
  EmptyStack,

  #[error("Invalid unary operation")]
  InvalidUnary,

  #[error("Invalid binary operation")]
  InvalidBinary,

  #[error("Invalid identifier {0}")]
  InvalidIdentifier(String),

  #[error("Constant not found at index {0}")]
  InvalidConst(usize),

  #[error("Stack entry not found at index {0}")]
  InvalidStackIndex(usize),

  #[error("could not fetch info for instruction {0:04X}")]
  IpOutOfBounds(usize),

  #[error("tried redefining a global variable at {level:?}: {name}")]
  Redefine { level: Option<usize>, name: String },

  #[error("methods can only be defined on classes")]
  MethodAssignment,

  #[error("methods can only functions")]
  MethodType,

  #[error("closures can only functions")]
  ClosureType,

  #[error("capture list must be a vec")]
  CaptureType,

  #[error("Expected a module, found {0}")]
  InvalidModule(Value),

  #[error("Invalid instruction: {0}")]
  InvalidInstruction(Instruction),

  #[error("Used storage type {0:?} in an invalid context")]
  InvalidStorageOperation(Storage),

  #[error("The constant id supplied was not translatable to an identifier")]
  InvalidConstantIdentifier,

  #[error("{0}")]
  Preformatted(Error),

  #[error("Infallible")]
  Infallible,
}

impl UsageError {
  pub fn native(e: impl ToString) -> Self {
    Self::NativeApi(e.to_string())
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

  #[error("'next' can only be used within loops")]
  InvalidNextStatement,

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

  #[error("Parameter cannot be named like a global")]
  GlobalParameter,

  #[error("Variable already declared earlier")]
  DuplicateDeclaration,

  /* Below this are errors triggered by broken bytecode generation logic */
  #[error("Instruction generation failure for opcode {0}")]
  InstructionGeneration(Opcode),

  #[error("Invalid variable definition")]
  InvalidVariableDefinition,

  #[error("Class functions should only be statics or methods")]
  InvalidClassFunction,

  #[error("'new' should be a static class function, not method")]
  MethodAsInitializer,

  #[error("Undeclared local variable '{0}'")]
  UndeclaredLocal(String),

  #[error("sanity check, should never happen")]
  SanityCheck,
}
