macro_rules! here {
  () => {
    crate::dbg::_here(line!());
  };
}

pub(crate) use here;

pub fn _here(line: u32) {
  use std::io::{stdout, Write};
  println!("HERE {}", line);
  stdout().flush().unwrap();
}
