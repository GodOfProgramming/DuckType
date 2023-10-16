use clap::{Parser, Subcommand};
use simple_script::prelude::*;
use std::{fs, path::PathBuf, process};
use uuid::Uuid;

#[derive(Debug, Parser)]
struct Args {
  #[command(subcommand)]
  command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
  Uuid,
  Run {
    #[arg()]
    file: Option<PathBuf>,
    #[clap(last = true)]
    runargs: Vec<String>,
  },
}

fn main() {
  let args = Args::parse();

  match args.command {
    Command::Uuid => println!("{}", Uuid::new_v4().to_string()),
    Command::Run { file, runargs } => {
      let mut exit_code = 0;

      let vm = Vm::new();
      let env = Env::initialize(&runargs, Library::All);

      if let Some(file) = file {
        if !run_file(vm, file, env) {
          exit_code = 1;
        }
      }

      process::exit(exit_code);
    }
  }
}

fn run_file(mut vm: Vm, file: PathBuf, mut env: Env) -> bool {
  if file.exists() {
    match fs::read_to_string(&file) {
      Ok(contents) => match vm.load(file.clone(), &contents) {
        Ok(ctx) => {
          let mut yield_result = None;

          loop {
            if let Some(y) = yield_result.take() {
              match vm.resume(y, &mut env) {
                Ok(result) => match result {
                  Return::Value(v) => {
                    println!("=> {}", v);
                    break;
                  }
                  Return::Yield(y) => yield_result = Some(y),
                },
                Err(errors) => {
                  for err in errors {
                    println!("{} ({}, {}): {}", err.file.display(), err.line, err.column, err.msg);
                  }
                  return false;
                }
              }
            } else {
              match vm.run(file.clone(), ctx.clone(), &mut env) {
                Ok(result) => match result {
                  Return::Value(v) => {
                    println!("=> {}", v);
                    break;
                  }
                  Return::Yield(y) => yield_result = Some(y),
                },
                Err(errors) => {
                  for err in errors {
                    println!("{} ({}, {}): {}", err.file.display(), err.line, err.column, err.msg);
                  }
                  return false;
                }
              }
            }
          }
        }
        Err(errs) => {
          println!("errors detected when compiling! ({})", errs.len());
          for err in errs {
            println!("{} ({}, {}): {}", err.file.display(), err.line, err.column, err.msg);
          }
          return false;
        }
      },
      Err(err) => {
        println!("error detected reading file {}: {}", file.display(), err);
        return false;
      }
    }
  } else {
    println!("error: could not find source file '{}'", file.display());
    return false;
  }

  true
}
