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
    Command::Uuid => println!("{}", Uuid::new_v4()),
    Command::Run { file, runargs } => {
      let mut exit_code = 0;

      let mut gc = SmartPtr::new(Gc::default());

      let gmod = ModuleBuilder::initialize(&mut gc, None, |gc, mut lib| {
        lib.env = stdlib::load_libs(gc, lib.handle.value.clone(), &runargs, &Library::All);
      });

      let vm = Vm::new(gc, runargs.clone(), Library::All);

      if let Some(file) = file {
        if !run_file(vm, file, gmod) {
          exit_code = 1;
        }
      }

      process::exit(exit_code);
    }
  }
}

fn run_file(mut vm: Vm, file: PathBuf, env: UsertypeHandle<ModuleValue>) -> bool {
  if !file.exists() {
    println!("error: could not find source file '{}'", file.display());
    return false;
  }

  match fs::read_to_string(&file) {
    Ok(contents) => match vm.load(file.clone(), &contents) {
      Ok(ctx) => match vm.run(file.clone(), ctx.clone(), env.clone()) {
        Ok(result) => {
          println!("=> {result}");
          true
        }
        Err(errors) => {
          println!("{errors}");
          false
        }
      },
      Err(errs) => {
        println!("errors detected when compiling! ({})", errs.len());
        for err in errs {
          println!("{} ({}, {}): {}", err.file.display(), err.line, err.column, err.msg);
        }
        false
      }
    },
    Err(err) => {
      println!("error detected reading file {}: {}", file.display(), err);
      false
    }
  }
}
