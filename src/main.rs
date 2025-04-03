use anyhow::Error;
use clap::{Parser, Subcommand};
use ducktype::prelude::*;
use std::io::Read;
use uuid::Uuid;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
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

  /// The file
  file: Option<String>,

  #[clap(last = true, help = "Arguments to run the program with. Accessible via std::env::ARGV")]
  runargs: Vec<String>,

  #[command(subcommand)]
  command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
  #[command(about = "Generate a UUID for creating a native type")]
  Uuid,
  #[command(about = "Run a string from stdin")]
  Pipe,
}

fn main() -> Result<(), Error> {
  let args = Args::parse();

  #[cfg(feature = "profiling")]
  let _guards = ducktype::perf::enable_profiling();

  match (args.file, args.command) {
    (Some(file), None) => {
      let gc = Gc::new(Memory::Mb(args.gc_mb));
      let mut vm = Vm::new(gc, args.optimize, args.runargs);
      let gmod = vm.generate_stdlib("*main*");
      vm.queue_file(file.clone(), gmod)?;
      let value = vm.execute()?;
      println!("=> {value}");
    }
    (None, Some(command)) => match command {
      Command::Uuid => println!("{}", Uuid::new_v4()),
      Command::Pipe => {
        let gc = Gc::new(Memory::Mb(args.gc_mb));
        let mut vm = Vm::new(gc, args.optimize, args.runargs);
        let gmod = vm.generate_stdlib("*main*");
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).map_err(Error::from)?;
        vm.queue_string(input, gmod)?;
        let value = vm.execute()?;
        println!("=> {value}");
      }
    },
    _ => {
      anyhow::bail!("Invalid argument combination");
    }
  }

  Ok(())
}
