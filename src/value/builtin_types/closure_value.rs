use crate::prelude::*;
use ptr::SmartPtr;

#[derive(Clone, Usertype, Fields)]
#[uuid("e924d375-93f0-4ce9-a2f3-5a4cf612492e")]
pub struct ClosureValue {
  #[trace]
  captures: Vec<Value>,
  #[trace]
  function: FunctionValue,
}

impl ClosureValue {
  pub fn new(captures: &[Value], function: FunctionValue) -> Self {
    Self {
      captures: captures.into(),
      function,
    }
  }

  pub fn context_ptr(&self) -> &SmartPtr<Context> {
    self.function.context_ptr()
  }

  pub fn context(&self) -> &Context {
    self.function.context()
  }
}

#[methods]
impl ClosureValue {
  fn __ivk__(&mut self, vm: &mut Vm, _this_fn: Value, args: Args) -> ValueResult<()> {
    println!("closure val exp {} act {}", self.function.airity, args.list.len());
    self.function.check_args(&args)?;

    let mut captures_with_args = Vec::with_capacity(self.captures.len() + args.list.len());
    captures_with_args.extend(self.captures.clone());
    captures_with_args.extend(args.list);

    self.function.invoke(vm, Args::new(captures_with_args))
  }

  fn __str__(&self) -> String {
    format!("closure {}", self.function.__str__())
  }

  fn __dbg__(&self) -> String {
    format!("<{}>", self.__str__())
  }
}
