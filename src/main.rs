use simple_script::{Context, New, Value, Vm, Vpu};
use std::{env, fs, path::Path, process};

const DISASSEMBLE_FLAG: &str = "--disassemble";
const QUITE_AFTER_FLAG: &str = "--quit-after";
const RUNTIME_DISASSEMBLE_FLAG: &str = "--runtime-disassembly";

fn main() {
  let mut exit_code = 0;
  let mut args: Vec<String> = env::args().collect();

  let show_disassembly = args.contains(&String::from(DISASSEMBLE_FLAG));
  let quit_after = args.contains(&String::from(QUITE_AFTER_FLAG));
  let runtime_disassembly = args.contains(&String::from(RUNTIME_DISASSEMBLE_FLAG));

  if show_disassembly {
    args.retain(|arg| arg != DISASSEMBLE_FLAG);
  }

  if runtime_disassembly {
    args.retain(|arg| arg != RUNTIME_DISASSEMBLE_FLAG);
  }

  if quit_after {
    args.retain(|arg| arg != QUITE_AFTER_FLAG);
  }

  let vpu = Vpu::new(runtime_disassembly);
  let runner = Vm::new(vpu);

  if let Some(file) = args.get(1).cloned() {
    if !run_file(runner, file, show_disassembly, quit_after) {
      exit_code = 1;
    }
  } else {
    run_cli(runner)
  }

  process::exit(exit_code);
}

fn run_file(
  vm: Vm<Vpu>,
  file: String,
  show_disassembly: bool,
  quit_after_disassembled: bool,
) -> bool {
  let p = Path::new(&file);
  if p.exists() {
    match fs::read_to_string(p) {
      Ok(contents) => match vm.load(file, &contents) {
        Ok(mut ctx) => {
          ctx.create_native(String::from("clock"), |_args: Vec<Value>| {
            use std::time::{SystemTime, UNIX_EPOCH};
            let now = SystemTime::now();
            let since = now.duration_since(UNIX_EPOCH).expect("time went backwards");
            Ok(Value::new(since.as_nanos()))
          });

          ctx.create_native(String::from("clock_diff"), |args: Vec<Value>| {
            if let Some(Value::U128(before)) = args.get(0) {
              if let Some(Value::U128(after)) = args.get(1) {
                let diff = std::time::Duration::from_nanos((after - before) as u64);
                return Ok(Value::new(diff.as_secs_f64()));
              }
            }
            Err(String::from(
              "clock_diff called with wrong number of arguments or invalid types",
            ))
          });

          #[cfg(debug_assertions)]
          if show_disassembly {
            disassemble(&ctx);

            if quit_after_disassembled {
              std::process::exit(0);
            }
          }

          match vm.run(ctx) {
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

#[cfg(debug_assertions)]
fn disassemble(ctx: &Context) {
  ctx.display_opcodes();

  for value in ctx.consts() {
    if let Value::Function(f) = value {
      disassemble(f.context());
    }
  }
}
