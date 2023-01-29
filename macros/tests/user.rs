#[test]
fn user_tests() {
  let t = trybuild::TestCases::new();
  t.pass("tests/pass/*.rs")
}

#[cfg(test)]
mod tests {
  use macros::Class;
  use std::error::Error;

  trait Class {
    fn get(&self, field: &str) -> Option<Value>;
    fn set(&mut self, field: &str, value: Value) -> Result<(), Box<dyn Error>>;
  }

  trait ClassBody {
    fn lookup(name: &str) -> Option<Value>;
  }

  #[derive(Debug)]
  enum Value {
    I32(i32),
    F32(f32),
    Nil,
  }

  impl From<&i32> for Value {
    fn from(value: &i32) -> Self {
      Self::I32(*value)
    }
  }

  impl From<&f32> for Value {
    fn from(value: &f32) -> Self {
      Self::F32(*value)
    }
  }

  impl TryFrom<Value> for i32 {
    type Error = &'static str;
    fn try_from(value: Value) -> Result<Self, Self::Error> {
      if let Value::I32(i) = value {
        Ok(i)
      } else {
        Err("")
      }
    }
  }

  impl TryFrom<Value> for f32 {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
      if let Value::F32(f) = value {
        Ok(f)
      } else {
        Err("")
      }
    }
  }

  impl Default for Value {
    fn default() -> Self {
      Self::Nil
    }
  }

  #[derive(Default, Class)]
  struct Foo {
    #[field]
    foo: i32,

    #[field]
    foo2: f32,
  }

  #[macros::class_body]
  impl Foo {
    #[method]
    fn foo(&mut self) -> i32 {
      1
    }
  }

  #[test]
  fn try_compile() {
    Foo::default();
  }
}
