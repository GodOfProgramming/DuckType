use std::fmt::Display;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::token::Comma;

macro_rules! count_args {
  ($fn:ident) => {
    $fn.sig.inputs.iter().filter(|input| matches!(input, FnArg::Typed(_))).count()
  };
}

pub(crate) use count_args;

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

pub(crate) fn make_named_arg_list<const N: usize>(name: impl ToTokens, arg_names: [&'static str; N]) -> TokenStream {
  let mut args = TokenStream::default();
  args.append_separated(
    arg_names.iter().enumerate().map(|(i, n)| {
      let arg_name = Ident::new(n, Span::call_site());
      quote! {
        #arg_name.maybe_into().ok_or(UsageError::InvalidArgument(#name, #i))?
      }
    }),
    Comma::default(),
  );
  args
}

pub(crate) fn error<T, U>(item: T, msg: U) -> TokenStream
where
  T: ToTokens,
  U: Display,
{
  syn::Error::new_spanned(item, msg).into_compile_error()
}
