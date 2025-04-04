use crate::{prelude::*, MakeValueFrom};
use clap::{ArgAction, Parser, Subcommand};
use std::error::Error;

#[derive(Parser)]
pub struct Cli {
  #[command(subcommand)]
  command: Command,
}

impl Cli {
  pub fn exec(self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    self.command.exec(vm)
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
  Continue,
  Exit,
  Env {
    #[command(subcommand)]
    command: EnvCmd,
  },
  Stack {
    #[command(subcommand)]
    command: StackCmd,
  },
  Gc {
    #[command(subcommand)]
    command: GcCmd,
  },
}

impl Command {
  pub fn exec(self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    match self {
      Command::Continue => Ok(CommandOutput::new(None, true)),
      Command::Exit => Err("exit entered")?,
      Command::Env { command } => command.exec(vm),
      Command::Stack { command } => command.exec(vm),
      Command::Gc { command } => command.exec(vm),
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
  Count,
}

impl EnvCmd {
  fn exec(self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    let output = match self {
      EnvCmd::Get { name } => Some(match vm.modules.last().lookup(&name) { Some(value) => {
        format!("{:?}", value)
      } _ => {
        format!("no item in the env with the name '{}'", name)
      }}),
      EnvCmd::Set { name, value } => {
        let value = match value {
          ValueCommand::Bool { value } => value.into(),
          ValueCommand::I32 { value } => value.into(),
          ValueCommand::F64 { value } => value.into(),
          ValueCommand::Char { value } => value.into(),
          ValueCommand::String { value } => vm.make_value_from(value),
        };

        vm.modules.last_mut().define(name, value);
        None
      }
      EnvCmd::Count => Some(format!("{}", vm.modules.len())),
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
  fn exec(&self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    let output = match self {
      StackCmd::Display => {
        vm.stack_display();
        None
      }
      StackCmd::Index { index } => Some(if let Some(value) = vm.stack.get(*index) {
        format!("{}", value)
      } else {
        format!("invalid stack index {}", index)
      }),
    };
    Ok(CommandOutput::new(output, false))
  }
}

#[derive(Subcommand)]
pub enum GcCmd {
  Count,
  Clean,
}

impl GcCmd {
  fn exec(&self, _vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    todo!()
  }
}

#[allow(unused)]
#[cfg(debug_assertions)]
pub(crate) mod macros {
  macro_rules! here {
    () => {
      crate::dbg::macros::_here(file!(), line!());
    };
    ($($arg:tt)*) => {
      crate::dbg::macros::_here_msg(file!(), line!(), format!($($arg)*));
    }
  }

  pub(crate) use here;

  pub fn _here(file: &str, line: u32) {
    use std::io::{stdout, Write};
    println!("{}:{}", file, line);
    stdout().flush().unwrap();
  }

  pub fn _here_msg(file: &str, line: u32, msg: impl ToString) {
    use std::io::{stdout, Write};
    println!("{}:{} => {}", file, line, msg.to_string());
    stdout().flush().unwrap();
  }
}
