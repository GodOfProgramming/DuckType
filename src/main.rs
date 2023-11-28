use clap::{Parser, Subcommand};
use ducktype::prelude::*;
use std::{io::Read, path::PathBuf};
use uuid::Uuid;

#[derive(Debug, Parser)]
struct Args {
  #[arg(short, long)]
  optimize: bool,

  #[arg(short, long, default_value_t = 1)]
  gc_mb: usize,

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
  let args = Args::parse();

  match args.command {
    Command::Uuid => println!("{}", Uuid::new_v4()),
    Command::Run { files, runargs } => {
      let gc = SmartPtr::new(Gc::new(Memory::Mb(args.gc_mb)));

      let mut vm = Vm::new(gc.clone(), args.optimize, runargs.clone());

      #[cfg(feature = "profile")]
      let guard = {
        pprof::ProfilerGuardBuilder::default()
          .frequency(1000)
          .blocklist(&["libc", "libgcc", "libdl", "libm", "libpthread", "linux-vdso"])
          .build()
          .unwrap()
      };

      for file in files {
        let gmod = vm.generate_stdlib("*main*");
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
      let gc = SmartPtr::new(Gc::new(Memory::Mb(args.gc_mb)));
      let mut vm = Vm::new(gc, args.optimize, runargs.clone());
      let gmod = vm.generate_stdlib("*main*");
      let mut input = String::new();
      std::io::stdin().read_to_string(&mut input).map_err(Error::from)?;
      let value = vm.run_string(input, gmod)?;
      println!("=> {value}");
    }
  }
  Ok(())
}
