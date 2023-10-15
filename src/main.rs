use simple_script::prelude::*;
use std::{env, error::Error, fs, path::Path, process};

fn main() -> Result<(), Box<dyn Error>> {
  let mut exit_code = 0;

  let mut args = env::args().into_iter().skip(1);
  let file = args.next();
  let args = args.collect::<Vec<String>>();

  let mut vm = Vm::new();
  let mut env = Env::initialize(&args, Library::All);

  if let Some(file) = file {
    if !run_file(vm, file, env) {
      exit_code = 1;
    }
    process::exit(exit_code);
  } else {
    vm.repl(&mut env)
  }
}

fn run_file<T: ToString>(mut vm: Vm, file: T, mut env: Env) -> bool {
  let file = file.to_string();
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => match vm.load(&file, &contents) {
        Ok(ctx) => {
          let mut yield_result = None;
          loop {
            if let Some(y) = yield_result.take() {
              match vm.resume(y, &mut env) {
                Ok(result) => match result {
                  Return::Value(v) => {
                    println!("{}", v);
                    break;
                  }
                  Return::Yield(y) => yield_result = Some(y),
                },
                Err(errors) => {
                  for err in errors {
                    println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
                  }
                  return false;
                }
              }
            } else {
              match vm.run(&file, ctx.clone(), &mut env) {
                Ok(result) => match result {
                  Return::Value(v) => {
                    println!("{}", v);
                    break;
                  }
                  Return::Yield(y) => yield_result = Some(y),
                },
                Err(errors) => {
                  for err in errors {
                    println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
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
            println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
          }
          return false;
        }
      },
      Err(err) => {
        println!("error detected reading file {}: {}", file, err);
        return false;
      }
    }
  } else {
    println!("error: could not find source file '{}'", p.display());
    return false;
  }

  true
}
