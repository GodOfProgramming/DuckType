mod common;
mod native_types;
mod user_types;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::ToTokens;
use syn::{
  parenthesized,
  parse::{Parse, ParseStream},
  parse_macro_input,
  token::Paren,
  Item, ItemImpl, ItemStruct,
};

struct UuidAttr {
  #[allow(unused)]
  paren_token: Paren,
  arg: Literal,
}

impl Parse for UuidAttr {
  fn parse(input: ParseStream) -> syn::Result<Self> {
    let content;
    let paren_token = parenthesized!(content in input);
    let arg = content.parse()?;
    Ok(Self { paren_token, arg })
  }
}

#[proc_macro_derive(Usertype, attributes(uuid))]
pub fn derive_usertype(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

  let uuid_attr = match struct_def.attrs.iter().find(|attr| attr.path.is_ident("uuid")) {
    Some(uuid_attr) => {
      let tokens = TokenStream::from(uuid_attr.tokens.clone());
      let uuid_value = parse_macro_input!(tokens as UuidAttr);
      Some(uuid_value.arg)
    }
    None => None,
  };

  user_types::derive_usertype(struct_def, uuid_attr).into()
}

#[proc_macro_derive(Class, attributes(field))]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);
  user_types::derive_class(struct_def).into()
}

#[proc_macro_attribute]
pub fn methods(_args: TokenStream, input: TokenStream) -> TokenStream {
  let struct_impl = parse_macro_input!(input as ItemImpl);
  user_types::derive_methods(struct_impl).into()
}

#[proc_macro_attribute]
pub fn native(_args: TokenStream, input: TokenStream) -> TokenStream {
  let item: Item = parse_macro_input!(input as Item);
  match item {
    Item::Fn(item) => native_types::native_fn(&item),
    Item::Mod(item) => native_types::native_mod(item),
    thing => {
      syn::Error::new_spanned(thing, "cannot impl method for fn signature not taking self reference").into_compile_error()
    }
  }
  .into()
}

fn message(location: impl ToTokens, msg: impl Into<String>) -> proc_macro2::TokenStream {
  return syn::Error::new_spanned(location, msg.into()).to_compile_error();
}
