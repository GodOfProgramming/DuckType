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
}
