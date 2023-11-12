use clap::{Parser, Subcommand};
use ducktype::prelude::*;
use std::{io::Read, path::PathBuf, time::Duration};
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
    files: Vec<PathBuf>,

    #[clap(last = true)]
    runargs: Vec<String>,
  },
  Pipe {
    #[clap(last = true)]
    runargs: Vec<String>,
  },
}

fn main() -> Result<(), Error> {
  #[cfg(feature = "profile")]
  let guard = {
    pprof::ProfilerGuardBuilder::default()
      .frequency(1000)
      .blocklist(&["libc", "libgcc", "libdl", "libm", "libpthread", "linux-vdso"])
      .build()
      .unwrap()
  };

  let args = Args::parse();

  match args.command {
    Command::Uuid => println!("{}", Uuid::new_v4()),
    Command::Run { files, runargs } => {
      let mut gc = SmartPtr::new(Gc::new(Duration::from_millis(0)));

      let mut vm = Vm::new(gc.clone(), runargs.clone());

      for file in files {
        println!("running {}", file.display());
        let gmod = ModuleBuilder::initialize(&mut gc, "*main*", None, |gc, mut lib| {
          let libval = lib.handle.value.clone();
          lib.env.extend(stdlib::enable_std(gc, libval, &runargs));
        });

        let value = vm.run_file(file.clone(), gmod)?;
        println!("=> {value}");
      }

      #[cfg(feature = "profile")]
      if let Ok(report) = guard.report().build() {
        use std::fs::File;
        let file = File::create("target/flamegraph.svg").unwrap();
        report.flamegraph(file).unwrap();
      }
    }
    Command::Pipe { runargs } => {
      let mut gc = SmartPtr::new(Gc::new(Duration::from_millis(16)));

      let gmod = ModuleBuilder::initialize(&mut gc, "*main*", None, |gc, mut lib| {
        let libval = lib.handle.value.clone();
        lib.env.extend(stdlib::enable_std(gc, libval, &runargs));
      });

      let mut vm = Vm::new(gc, runargs.clone());
      let mut input = String::new();
      std::io::stdin().read_to_string(&mut input).map_err(SystemError::IoError)?;
      let value = vm.run_string(input, gmod)?;
      println!("=> {value}");
    }
  }
  Ok(())
}
