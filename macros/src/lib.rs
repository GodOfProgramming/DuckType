mod common;
mod native_types;
mod user_types;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use syn::{
  parenthesized,
  parse::{Parse, ParseStream},
  parse_macro_input, Ident, Item, ItemImpl, ItemStruct,
};

struct StrAttr {
  string: Literal,
}

impl Parse for StrAttr {
  fn parse(input: ParseStream) -> syn::Result<Self> {
    let content;
    parenthesized!(content in input);
    Ok(Self {
      string: content.parse()?,
    })
  }
}

#[proc_macro_derive(Usertype, attributes(uuid, trace))]
pub fn derive_usertype(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

  let uuid_attr = match struct_def.attrs.iter().find(|attr| attr.path.is_ident("uuid")) {
    Some(uuid_attr) => {
      let tokens = TokenStream::from(uuid_attr.tokens.clone());
      let uuid_value = parse_macro_input!(tokens as StrAttr);
      Some(uuid_value.string)
    }
    None => None,
  };

  let traceables = struct_def
    .fields
    .iter()
    .filter_map(|f| {
      f.attrs
        .iter()
        .find(|attr| attr.path.is_ident("trace"))
        .and_then(|_| f.ident.clone())
    })
    .collect();

  user_types::derive_usertype(struct_def, uuid_attr, traceables).into()
}

#[proc_macro_derive(Fields, attributes(field))]
pub fn derive_fields(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);
  user_types::derive_fields(struct_def).into()
}

#[proc_macro_derive(Renameable, attributes(rename))]
pub fn allow_rename(_input: TokenStream) -> TokenStream {
  TokenStream::default()
}

#[proc_macro_attribute]
pub fn methods(_args: TokenStream, input: TokenStream) -> TokenStream {
  let struct_impl = parse_macro_input!(input as ItemImpl);
  user_types::derive_methods(struct_impl).into()
}

#[proc_macro_attribute]
pub fn native(args: TokenStream, input: TokenStream) -> TokenStream {
  let item: Item = parse_macro_input!(input as Item);
  match item {
    Item::Fn(item) => {
      let have_args = !args.is_empty();
      let with_vm = if have_args {
        let args: Ident = parse_macro_input!(args as Ident);
        args == "with_vm"
      } else {
        false
      };
      native_types::native_fn(&item, with_vm)
    }
    Item::Mod(item) => {
      let have_args = !args.is_empty();
      let no_entry = if have_args {
        let args: Ident = parse_macro_input!(args as Ident);
        args == "no_entry"
      } else {
        false
      };
      native_types::native_mod(item, no_entry)
    }
    thing => {
      syn::Error::new_spanned(thing, "cannot impl method for fn signature not taking self reference").into_compile_error()
    }
  }
  .into()
}
