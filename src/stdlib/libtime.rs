use crate::{Args, ComplexValue, StructValue, TimestampValue, Value};

pub struct LibTime;

impl LibTime {
  pub fn load() -> Value {
    use std::time::Instant;

    let mut lib = StructValue::default();

    // monotonic
    {
      let mut mono = StructValue::default();

      mono
        .set(
          "now",
          Value::new_native_fn(|_thread, _env, _args: Args| Value::from(TimestampValue::new())),
        )
        .ok();

      mono
        .set(
          "elapsed",
          Value::new_native_fn(|_thread, _env, args: Args| {
            if let Some(before) = args.list.get(0) {
              if before.is::<TimestampValue>() {
                let now = Instant::now();
                if let Ok(ts) = before.cast_to::<TimestampValue>() {
                  let since = now.duration_since(**ts.clone());
                  return Value::from(since.as_secs_f64());
                }
              }
            }
            Value::new_err(String::from("elapsed called with wrong number of arguments or invalid types"))
          }),
        )
        .ok();

      lib.set("Monotonic", Value::from(mono)).ok();
    }

    lib.into()
  }
}
