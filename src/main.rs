use simple_script::{Value, Vm, Vpu};
use std::{env, fs, path::Path, process};

const DISASSEMBLE_FLAG: &str = "--disassemble";
const RUNTIME_DISASSEMBLE_FLAG: &str = "--runtime-disassembly";

fn main() {
  let mut exit_code = 0;
  let mut args: Vec<String> = env::args().collect();

  let show_disassembly = args.contains(&String::from(DISASSEMBLE_FLAG));
  let runtime_disassembly = args.contains(&String::from(RUNTIME_DISASSEMBLE_FLAG));

  if show_disassembly {
    args.retain(|arg| arg != DISASSEMBLE_FLAG);
  }

  if runtime_disassembly {
    args.retain(|arg| arg != RUNTIME_DISASSEMBLE_FLAG);
  }

  let vpu = Vpu::new(show_disassembly, runtime_disassembly);
  let runner = Vm::new(vpu);

  if let Some(file) = args.get(1).cloned() {
    if !run_file(runner, file) {
      exit_code = 1;
    }
  } else {
    run_cli(runner)
  }

  process::exit(exit_code);
}

fn run_file(runner: Vm<Vpu>, file: String) -> bool {
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => match runner.load(file, &contents) {
        Ok(mut ctx) => {
          ctx.assign_global(
            String::from("test"),
            Value::NativeFunction(|_args| {
              println!("test");
              Ok(Value::Nil)
            }),
          );
          match runner.run(&mut ctx) {
            Ok(v) => println!("{}", v),
            Err(err) => {
              println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
              return false;
            }
          }
        }
        Err(errs) => {
          println!("Errors detected when compiling! ({})", errs.len());
          for err in errs {
            println!("{} ({}, {}): {}", err.file, err.line, err.column, err.msg);
          }
          return false;
        }
      },
      Err(err) => {
        println!("{}", err);
        return false;
      }
    }
  } else {
    println!("error:could not find source file '{}'", p.display());
    return false;
  }

  true
}

fn run_cli(_runner: Vm<Vpu>) {
  let _quit = false;
}
