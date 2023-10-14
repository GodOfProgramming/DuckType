use crate::prelude::*;
use std::time::Instant;

pub struct LibTime;

impl LibTime {
  pub fn load() -> Value {
    LockedModule::initialize(|lib| {
      let mono = LockedModule::initialize(|mono| {
        mono
          .set(
            "now",
            Value::native(|_vm, _env, _args: Args| Ok(Value::from(TimestampValue::new()))),
          )
          .ok();

        mono
          .set(
            "elapsed",
            Value::native(|_vm, _env, args: Args| {
              if let Some(before) = args.list.get(0) {
                if before.is::<TimestampValue>() {
                  let now = Instant::now();
                  if let Some(ts) = before.cast_to::<TimestampValue>() {
                    let since = now.duration_since(**ts);
                    return Ok(Value::from(since.as_secs_f64()));
                  }
                }
              }
              Err(ValueError::Todo(String::from(
                "elapsed called with wrong number of arguments or invalid types",
              )))
            }),
          )
          .ok();
      });

      lib.set("Monotonic", Value::from(mono)).ok();
    })
    .into()
  }
}
