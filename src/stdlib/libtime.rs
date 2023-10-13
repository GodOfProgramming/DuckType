use crate::prelude::*;

pub struct LibTime;

impl LibTime {
  pub fn load() -> Value {
    use std::time::Instant;

    let mut lib = StructValue::default();

    // monotonic
    {
      let mut mono = StructValue::default();

      mono.set(
        "now",
        Value::native(|_vm, _env, _args: Args| Ok(Value::from(TimestampValue::new()))),
      );

      mono.set(
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
      );

      lib.set("Monotonic", Value::from(mono));
    }

    lib.into()
  }
}
