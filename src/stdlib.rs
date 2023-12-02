mod libconsole;
mod libio;
mod libps;
mod libstr;
mod libtime;

use crate::{
  prelude::*,
  value::{prelude::module_value::ModuleType, NATIVE_FN_TAG},
};
use std::env;

pub(crate) mod names {
  pub const STD: &str = "std";

  pub const TYPES: &str = "types";

  pub const DEBUG: &str = "debug";

  pub const REFLECT: &str = "reflect";

  pub const ENV: &str = "env";
  pub(crate) mod env {
    pub const ARGV: &str = "ARGV";
    pub const PATHS_ENV_VAR: &str = "SS_LIBRARY_PATHS";
    pub const PATHS: &str = "paths";
    pub const PATH_SEPARATOR: char = ';';
  }

  pub const TIME: &str = "time";

  pub const STR: &str = "str";

  pub const CONSOLE: &str = "console";

  pub const PS: &str = "ps";

  pub const MATH: &str = "math";

  pub const IO: &str = "io";

  pub const VM: &str = "vm";
  pub(crate) mod vm {
    pub const GC: &str = "gc";
  }
}

pub fn make_stdlib(vm: &mut Vm, gmod: Value, args: impl Into<Vec<String>>) -> (String, Value) {
  let stdlib = ModuleBuilder::initialize(vm, ModuleType::new_child(names::STD, gmod), |vm, mut lib| {
    let lib = &mut lib;

    defmod(vm, lib, names::TYPES, |gc, mut lib| {
      lib.define("i32", Value::new::<i32>(Default::default()));
      lib.define("f64", Value::new::<f64>(Default::default()));
      lib.define("char", Value::new::<char>(Default::default()));
      lib.define("bool", Value::new::<bool>(Default::default()));
      lib.define("native", Value { bits: NATIVE_FN_TAG });
      lib.define("str", gc.make_value_from(IdValue::new(StringValue::ID)));
    });

    lib.define(names::DEBUG, Value::new::<NativeFn>(debug));

    defmod(vm, lib, names::REFLECT, |_, mut lib| {
      lib.define("disasm", Value::new::<NativeFn>(disasm));
    });

    defmod(vm, lib, names::ENV, |vm, mut lib| {
      let args = args
        .into()
        .iter()
        .map(|arg| vm.make_value_from(arg.clone()))
        .collect::<Vec<Value>>();
      let args = vm.make_value_from(args);
      lib.define(names::env::ARGV, args);

      let mut lib_paths = Vec::default();

      if let Ok(paths) = env::var(names::env::PATHS_ENV_VAR) {
        lib_paths.extend(
          paths
            .split_terminator(names::env::PATH_SEPARATOR)
            .map(|v| vm.make_value_from(v)),
        );
      }

      lib.define(names::env::PATHS, vm.make_value_from(lib_paths));
    });

    defmod(vm, lib, names::TIME, |vm, mut lib| {
      defmod(vm, &mut lib, "mono", libtime::mono);
    });

    defmod(vm, lib, names::STR, libstr::string);

    defmod(vm, lib, names::CONSOLE, libconsole::console);

    defmod(vm, lib, names::PS, libps::ps);

    defmod(vm, lib, names::MATH, |_, mut lib| {
      lib.define("rand_i32", Value::new::<NativeFn>(math_rand_i32));
      lib.define("abs", Value::new::<NativeFn>(math_abs));
    });

    let libval = lib.value();
    lib.define(names::IO, libio::duck_type_autogen_create_module(vm, libval));

    defmod(vm, lib, names::VM, |gc, mut lib| {
      lib.define("eval", Value::new::<NativeFn>(vm_eval));
      defmod(gc, &mut lib, names::vm::GC, |_, mut lib| {
        lib.define("total_cycles", Value::new::<NativeFn>(total_cycles));
        lib.define("print_stats", Value::new::<NativeFn>(print_stats));
      });
    });
  });

  (names::STD.into(), stdlib.into())
}

fn defmod<F>(vm: &mut Vm, lib: &mut UsertypeHandle<ModuleValue>, name: &str, init: F)
where
  F: FnOnce(&mut Vm, UsertypeHandle<ModuleValue>),
{
  let libval = lib.value();
  lib.define(name, ModuleBuilder::initialize(vm, ModuleType::new_child(name, libval), init));
}

#[native]
fn debug(value: Value) -> UsageResult<String> {
  Ok(format!("{:?}", value))
}

#[native(with_vm)]
fn disasm(vm: &mut Vm, value: Value) -> UsageResult<String> {
  if let Some(f) = value.cast_to::<FunctionValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
  } else if let Some(f) = value.cast_to::<ClosureValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
  } else if let Some(f) = value.cast_to::<MethodValue>() {
    Ok(f.context().disassemble(&vm.stack, &vm.cache))
  } else {
    Ok(String::new())
  }
}

#[native]
fn math_abs(arg: (Option<i32>, Option<f64>)) -> UsageResult {
  match arg {
    (Some(i), None) => Ok(Value::from(i.abs())),
    (None, Some(f)) => Ok(Value::from(f.abs())),
    _ => Err(UsageError::Infallible)?,
  }
}

#[native]
fn math_rand_i32() -> UsageResult<i32> {
  let val = rand::random();
  Ok(val)
}

#[native(with_vm)]
fn total_cycles(vm: &mut Vm) -> UsageResult<i32> {
  Ok(vm.gc.stats.total_deep_cleans as i32)
}

#[native(with_vm)]
fn print_stats(vm: &mut Vm) -> UsageResult<()> {
  let inc_ratio = vm.gc.stats.total_increments as f64 / vm.gc.stats.total_incremental_cleans as f64;
  let inc_per_deep_ratio = vm.gc.stats.total_deep_cleans as f64 / vm.gc.stats.total_incremental_cleans as f64;
  println!(
    "{}",
    itertools::join(
      [
        "------ Gc Stats ------",
        &format!("No. allocations -------- {}", vm.gc.allocations.len()),
        &format!("No. handles ------------ {}", vm.cache.native_handles.len()),
        &format!("No. deep cleans -------- {}", vm.gc.stats.total_deep_cleans),
        &format!("No. inc cleans --------- {}", vm.gc.stats.total_incremental_cleans),
        &format!("No. increments --------- {}", vm.gc.stats.total_increments),
        &format!("Increment ratio -------- {}", inc_ratio),
        &format!("Deep/Inc ratio --------- {}", inc_per_deep_ratio),
        &format!("Memory in use ---------- {}", vm.gc.allocated_memory),
        &format!("Limit till next cycle -- {}", vm.gc.limit),
      ],
      "\n"
    )
  );
  Ok(())
}

#[native(with_vm)]
fn gc_set_mode_standard(vm: &mut Vm) -> UsageResult<()> {
  vm.gc.set_mode(GcMode::Deep);
  Ok(())
}

#[native(with_vm)]
fn gc_set_mode_incremental(vm: &mut Vm) -> UsageResult<()> {
  vm.gc.set_mode(GcMode::Deep);
  Ok(())
}

#[native(with_vm)]
fn gc_set_mode_deep(vm: &mut Vm) -> UsageResult<()> {
  vm.gc.set_mode(GcMode::Deep);
  Ok(())
}

#[native(with_vm)]
fn vm_eval(vm: &mut Vm, source: &StringValue) -> UsageResult {
  vm.eval(source).map_err(UsageError::Preformatted)
}
