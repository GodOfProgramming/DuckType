use clap::{Parser, Subcommand};
use ss::prelude::*;
use std::{
  fs::{self},
  io::Read,
  path::PathBuf,
};
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
    Command::Run { file, runargs } => {
      let mut gc = SmartPtr::new(Gc::default());

      let gmod = ModuleBuilder::initialize(&mut gc, "*main*", None, |gc, mut lib| {
        lib.env = stdlib::enable_std(gc, lib.handle.value.clone(), &runargs);
      });

      let mut vm = Vm::new(gc, runargs.clone());

      if let Some(file) = file {
        let value = vm.run_file(file.clone(), gmod)?;
        println!("=> {value}");
      }

      #[cfg(feature = "profile")]
      if let Ok(report) = guard.report().build() {
        use fs::File;
        let file = File::create("target/flamegraph.svg").unwrap();
        report.flamegraph(file).unwrap();
      }
    }
    Command::Pipe { runargs } => {
      let mut gc = SmartPtr::new(Gc::default());

      let gmod = ModuleBuilder::initialize(&mut gc, "*main*", None, |gc, mut lib| {
        lib.env = stdlib::enable_std(gc, lib.handle.value.clone(), &runargs);
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
