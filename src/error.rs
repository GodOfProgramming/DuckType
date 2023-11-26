use crate::{code::FileMap, value::Value};
use common::{errors::CompilerError, Instruction, Storage};
use std::{
  fmt::{self, Debug, Display, Formatter},
  io,
  path::PathBuf,
};

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

  pub(crate) fn from_common(error: common::Error, file_map: &FileMap, source: &str) -> Self {
    match error {
      common::Error::Compiler(compiler) => match compiler {
        CompilerError::Lexical(errors) => Self::Multiple(Self::many_common(errors, file_map, source)),
        CompilerError::AstGeneration(errors) => Self::Multiple(Self::many_common(errors, file_map, source)),
        CompilerError::BytecodeGeneration(errors) => Self::Multiple(Self::many_common(errors, file_map, source)),
      },
    }
  }

  fn many_common<M>(errors: Vec<common::errors::CompileError<M>>, file_map: &FileMap, source: &str) -> Vec<FormattedError>
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
      indicator: line_text.to_string(),
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

  #[error("Infallible")]
  Infallible,
}

impl UsageError {
  pub fn native(e: impl ToString) -> Self {
    Self::NativeApi(e.to_string())
  }
}
