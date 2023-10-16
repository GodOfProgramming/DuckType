use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::token::Comma;

pub(crate) fn make_arg_list(nargs: usize, name: impl ToTokens) -> TokenStream {
  let mut args = TokenStream::default();
  args.append_separated(
    (0..nargs).map(|i| {
      quote! {
        args.next_arg().try_unwrap_arg(#name, #i)?
      }
    }),
    Comma::default(),
  );
  args
}
