use simple_script::macro_requirements::*;
use simple_script::prelude::*;

#[native]
mod example_module {
  #[native]
  fn test_function(item: &StringValue) -> ValueResult<()> {
    println!("{}", item);
    Ok(())
  }

  #[native]
  fn test_clear(item: &mut StringValue) -> ValueResult<String> {
    let old = item.clone();
    *item = StringValue::default();
    Ok(old)
  }

  #[derive(Default, Usertype, Fields)]
  #[uuid("random")]
  struct Foo {
    #[field]
    value: i32,
  }

  #[methods]
  impl Foo {
    fn __new__(item: i32) -> ValueResult<Foo> {
      Ok(Foo { value: item })
    }

    fn __str__(&self) -> String {
      format!("Foo {{ value: {} }}", self.value)
    }
  }
}
