use crate::prelude::*;
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
  pub fn exec(self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    match self {
      Command::Quit => Ok(CommandOutput::new(None, true)),
      Command::Env { command } => command.exec(vm),
      Command::Stack { command } => command.exec(vm),
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
  fn exec(self, vm: &mut Vm) -> Result<CommandOutput, Box<dyn Error>> {
    let output = match self {
      EnvCmd::Get { name } => Some(if let Some(value) = vm.current_env().lookup(&name) {
        format!("{:?}", value)
      } else {
        format!("no item in the env with the name '{}'", name)
      }),
      EnvCmd::Set { name, value } => {
        let value = match value {
          ValueCommand::Bool { value } => value.into(),
          ValueCommand::I32 { value } => value.into(),
          ValueCommand::F64 { value } => value.into(),
          ValueCommand::Char { value } => value.into(),
          ValueCommand::String { value } => vm.gc.allocate(value),
        };

        vm.current_env_mut().define(name, value);
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
  DisplayAll,
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
      StackCmd::DisplayAll => {
        vm.stack_display();
        for stack in &vm.stack_frames {
          println!("{}", stack);
        }
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

#[allow(unused)]
#[cfg(debug_assertions)]
pub(crate) mod macros {
  macro_rules! here {
    () => {
      crate::dbg::macros::_here(file!(), line!());
    };
  }

  pub(crate) use here;

  pub fn _here(file: &str, line: u32) {
    use std::io::{stdout, Write};
    println!("{}:{}", file, line);
    stdout().flush().unwrap();
  }
}
