use clap::{Parser, Subcommand};
use ducktype::prelude::*;
use std::{io::Read, path::PathBuf};
use uuid::Uuid;

#[derive(Debug, Parser)]
struct Args {
  #[arg(short, long, help = "Run optimizations at the cost of slightly longer compilation times")]
  optimize: bool,

  #[arg(
    short,
    long,
    default_value_t = 1,
    help = "Specify the initial garbage collector limit in megabytes"
  )]
  gc_mb: usize,

  #[command(subcommand)]
  command: Command,

  #[clap(last = true, help = "Arguments to run the program with. Accessible via std::env::ARGV")]
  runargs: Vec<String>,
}

#[derive(Debug, Subcommand)]
enum Command {
  #[command(about = "Generate a UUID for creating a native type")]
  Uuid,
  #[command(about = "Run a file")]
  Run {
    #[arg(help = "The file to run")]
    file: PathBuf,
  },
  #[command(about = "Run a string from stdin")]
  Pipe,
}

fn main() -> Result<(), Error> {
  let args = Args::parse();

  match args.command {
    Command::Uuid => println!("{}", Uuid::new_v4()),
    Command::Run { file } => {
      let gc = Gc::new(Memory::Mb(args.gc_mb));
      let mut vm = Vm::new(gc, args.optimize, args.runargs);

      #[cfg(feature = "profile")]
      let guard = {
        pprof::ProfilerGuardBuilder::default()
          .frequency(1000)
          .blocklist(&["libc", "libgcc", "libdl", "libm", "libpthread", "linux-vdso"])
          .build()
          .unwrap()
      };

      let gmod = vm.generate_stdlib("*main*");
      let value = vm.run_file(file.clone(), gmod)?;
      println!("=> {value}");

      #[cfg(feature = "profile")]
      if let Ok(report) = guard.report().build() {
        use std::fs::File;
        let file = File::create("target/flamegraph.svg").unwrap();
        report.flamegraph(file).unwrap();
      }
    }
    Command::Pipe => {
      let gc = Gc::new(Memory::Mb(args.gc_mb));
      let mut vm = Vm::new(gc, args.optimize, args.runargs);
      let gmod = vm.generate_stdlib("*main*");
      let mut input = String::new();
      std::io::stdin().read_to_string(&mut input).map_err(Error::from)?;
      let value = vm.run_string(input, gmod)?;
      println!("=> {value}");
    }
  }
  Ok(())
}
