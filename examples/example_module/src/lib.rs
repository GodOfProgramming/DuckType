use ducktype::macro_requirements::*;
use ducktype::prelude::*;

#[native]
mod example_module {
  #[native]
  fn test_function(item: &StringValue) -> UsageResult<()> {
    println!("{}", item);
    Ok(())
  }

  #[native]
  fn test_clear(item: &mut StringValue) -> UsageResult<String> {
    let old = item.clone();
    *item = StringValue::default();
    Ok(old)
  }

  #[derive(Default, Debug, Usertype, Fields)]
  #[uuid("random")]
  struct Foo {
    #[field]
    value: i32,
  }

  #[methods]
  impl Foo {
    fn __new__(item: i32) -> UsageResult<Foo> {
      Ok(Foo { value: item })
    }
  }

  impl Operators for Foo {
    fn __str__(&self) -> String {
      format!("{self:?}")
    }
  }
}
