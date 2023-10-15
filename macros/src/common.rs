use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn try_cast_arg_fn_tokens() -> TokenStream {
  quote! {
    pub fn try_arg_cast<T>(this: Value, fn_name: &'static str, pos: usize) -> ValueResult<T>
    where
      T: MaybeFrom<Value>,
    {
      T::maybe_from(this).ok_or(ValueError::InvalidArgument(fn_name, pos))
    }
  }
}
