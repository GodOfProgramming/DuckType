use crate::prelude::*;
use std::{
  error::Error,
  fmt::{Debug, Display, Formatter, Result as FmtResult},
  path::PathBuf,
  rc::Rc,
};

#[allow(unused)]
#[cfg(debug_assertions)]
macro_rules! here {
  () => {
    crate::dbg::_here(file!(), line!());
  };
}

use clap::{ArgAction, Parser, Subcommand};
#[allow(unused)]
#[cfg(debug_assertions)]
pub(crate) use here;

use crate::code::{OpCodeReflection, Opcode};

pub mod prelude {
  #[allow(unused)]
  #[cfg(debug_assertions)]
  pub(crate) use super::here;
  pub use super::RuntimeError;
}

pub fn _here(file: &str, line: u32) {
  use std::io::{stdout, Write};
  println!("{}:{}", file, line);
  stdout().flush().unwrap();
}

#[derive(PartialEq, Eq)]
pub struct RuntimeError {
  pub msg: String,
  pub file: Rc<PathBuf>,
  pub line: usize,
  pub column: usize,
}

impl RuntimeError {
  pub fn from_ref<M: ToString>(msg: M, opcode: &Opcode, opcode_ref: OpCodeReflection) -> Self {
    let mut err = Self {
      msg: msg.to_string(),
      file: opcode_ref.file.clone(),
      line: opcode_ref.line,
      column: opcode_ref.column,
    };
    err.format_with_src_line(opcode_ref.source_line);
    err.msg = format!("{}\nOffending OpCode: {:?}", err.msg, opcode);
    err
  }

  pub fn format_with_src_line(&mut self, src: &str) {
    self.msg = format!("{}\n{}\n{}^", self.msg, src, " ".repeat(self.column - 1));
  }
}

impl Debug for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(f, "{} ({}, {}): {}", self.file.display(), self.line, self.column, self.msg)
  }
}

impl Display for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    writeln!(f, "{} ({}, {}): {}", self.file.display(), self.line, self.column, self.msg)
  }
}

#[derive(Parser)]
pub struct Cli {
  #[command(subcommand)]
  command: Command,
}

impl Cli {
  pub fn exec(self, vm: &mut Vm, env: &mut Env) -> Result<CommandOutput, Box<dyn Error>> {
    self.command.exec(vm, env)
  }
}

pub struct CommandOutput {
  pub response: Option<String>,
  pub quit: bool,
}

impl CommandOutput {
  fn new(response: Option<String>, quit: bool) -> Self {
    Self { response, quit }
  }
}

#[derive(Subcommand)]
pub enum Command {
  Quit,
  Env {
    #[command(subcommand)]
    command: EnvCmd,
  },
  Stack {
    #[command(subcommand)]
    command: StackCmd,
  },
}

impl Command {
  pub fn exec(self, vm: &mut Vm, env: &mut Env) -> Result<CommandOutput, Box<dyn Error>> {
    match self {
      Command::Quit => Ok(CommandOutput::new(None, true)),
      Command::Env { command } => command.exec(vm, env),
      Command::Stack { command } => command.exec(vm, env),
    }
  }
}

#[derive(Subcommand)]
pub enum EnvCmd {
  Get {
    #[arg()]
    name: String,
  },
  Set {
    #[arg()]
    name: String,

    #[command(subcommand)]
    value: ValueCommand,
  },
}

impl EnvCmd {
  fn exec(self, _vm: &mut Vm, env: &mut Env) -> Result<CommandOutput, Box<dyn Error>> {
    let output = match self {
      EnvCmd::Get { name } => Some(if let Some(value) = env.lookup(&name) {
        format!("{:?}", value)
      } else {
        format!("no item in the env with the name '{}'", name)
      }),
      EnvCmd::Set { name, value } => {
        env.assign(
          name,
          match value {
            ValueCommand::Bool { value } => value.into(),
            ValueCommand::I32 { value } => value.into(),
            ValueCommand::F64 { value } => value.into(),
            ValueCommand::Char { value } => value.into(),
            ValueCommand::String { value } => value.into(),
          },
        );
        None
      }
    };
    Ok(CommandOutput::new(output, false))
  }
}

#[derive(Subcommand)]
pub enum ValueCommand {
  Bool {
    #[arg(action = ArgAction::Set)]
    value: bool,
  },
  I32 {
    #[arg()]
    value: i32,
  },
  F64 {
    #[arg()]
    value: f64,
  },
  Char {
    #[arg()]
    value: char,
  },
  String {
    #[arg()]
    value: String,
  },
}

#[derive(Subcommand)]
pub enum StackCmd {
  Display,
  Index {
    #[arg()]
    index: usize,
  },
}

impl StackCmd {
  fn exec(&self, vm: &mut Vm, _env: &mut Env) -> Result<CommandOutput, Box<dyn Error>> {
    let output = match self {
      StackCmd::Display => {
        vm.stack_display();
        None
      }
      StackCmd::Index { index } => Some(if let Some(value) = vm.stack_index(*index) {
        format!("{}", value)
      } else {
        format!("invalid stack index {}", index)
      }),
    };
    Ok(CommandOutput::new(output, false))
  }
}
