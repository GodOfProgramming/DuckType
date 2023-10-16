use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::token::Comma;

pub(crate) fn try_cast_arg_fn_tokens() -> TokenStream {
  quote! {
    fn try_arg_cast<T>(this: Value, fn_name: &'static str, pos: usize) -> ValueResult<T>
    where
      T: MaybeFrom<Value>,
    {
      T::maybe_from(this).ok_or(ValueError::InvalidArgument(fn_name, pos))
    }
  }
}

pub(crate) fn make_arg_list(nargs: usize, name: impl ToTokens) -> TokenStream {
  let mut args = TokenStream::default();
  args.append_separated(
    (0..nargs).map(|i| {
      quote! {
        try_arg_cast(args
        .next()
        .cloned()
        .unwrap(), #name, #i)?
      }
    }),
    Comma::default(),
  );
  args
}
