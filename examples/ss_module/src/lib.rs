use simple_script::macro_requirements::*;
use simple_script::prelude::StringValue;

#[native]
mod example_module {
  fn test_function(item: &StringValue) -> ValueResult<()> {
    println!("{}", item);
    Ok(())
  }

  fn test_clear(item: &mut StringValue) -> ValueResult {
    let old = item.clone();
    *item = StringValue::default();
    Ok(old.into())
  }

  #[derive(Default, Usertype, Fields)]
  #[uuid("random")]
  struct Foo {
    #[field]
    value: i32,
  }

  #[methods]
  impl Foo {
    fn __new__() -> ValueResult<Foo> {
      Ok(Foo::default())
    }
  }
}
