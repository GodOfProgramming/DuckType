use simple_script::{Env, Library, Vm};
use std::{env, fs, path::Path, process};

fn main() {
  let mut exit_code = 0;

  let mut args = env::args().collect::<Vec<String>>().into_iter().skip(1);
  let file = args.next();
  eprintln!();

  let vm = Vm::new_with_libs(
    &args.collect::<Vec<String>>(),
    &[
      Library::Env,
      Library::Time,
      Library::String,
      Library::Console,
      Library::Ptr,
    ],
  );

  if let Some(file) = file {
    if !run_file(vm, file) {
      exit_code = 1;
    }
  } else {
    run_cli(vm)
  }

  process::exit(exit_code);
}

fn run_file(mut vm: Vm, file: String) -> bool {
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => match vm.load(file, &contents) {
        Ok(ctx) => {
          let mut env = Env::default();

          match vm.run(ctx, &mut env) {
            Ok(v) => println!("{}", v),
            Err(errors) => {
              for err in errors {
                println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
              }
              return false;
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
