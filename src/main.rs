use simple_script::{ArrayValue, ClassValue, Env, Library, RunResult, StringValue, Vm};
use std::{env, fs, path::Path, process};

fn main() {
  let mut exit_code = 0;

  let mut args = env::args().into_iter().skip(1);
  let file = args.next();

  let vm = Vm::new();

  if let Some(file) = file {
    if !run_file(vm, file, &args.collect::<Vec<String>>()) {
      exit_code = 1;
    }
  } else {
    run_cli(vm)
  }

  process::exit(exit_code);
}

fn run_file<T: ToString>(mut vm: Vm, file: T, args: &[String]) -> bool {
  let file = file.to_string();
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => match vm.load(&file, &contents) {
        Ok(ctx) => {
          let mut env = Env::initialize(args, Library::All);

          let mut yield_result = None;

          loop {
            if let Some(y) = yield_result.take() {
              match vm.resume(y, &mut env) {
                Ok(result) => match result {
                  RunResult::Value(v) => {
                    println!("{}", v);
                    break;
                  }
                  RunResult::Yield(y) => yield_result = Some(y),
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
                  RunResult::Value(v) => {
                    println!("{}", v);
                    break;
                  }
                  RunResult::Yield(y) => yield_result = Some(y),
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

fn run_cli(_runner: Vm) {
  let _quit = false;
}
