mod bindings;
mod common;
mod native_types;
mod user_types;

use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use syn::{
  parenthesized,
  parse::{Parse, ParseStream},
  parse_macro_input, Ident, Item, ItemEnum, ItemFn, ItemImpl, ItemStruct, Path,
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

#[proc_macro_derive(NoMethods)]
pub fn derive_no_methods(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);
  let me = struct_def.ident;
  quote! {
    impl UsertypeMethods for #me {}
  }
  .into()
}

#[proc_macro_derive(NoOperators)]
pub fn derive_no_operators(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);
  let me = struct_def.ident;
  let me_str = me.to_string();
  let me_lit = Literal::string(&me_str);
  quote! {
    impl Operators for #me {
      fn __str__(&self) -> String {
        String::from(#me_lit)
      }

      fn __dbg__(&self) -> String {
        String::from(#me_lit)
      }
    }
  }
  .into()
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
    thing => syn::Error::new_spanned(thing, "cannot derive native type").into_compile_error(),
  }
  .into()
}

#[proc_macro_attribute]
pub fn binary(args: TokenStream, input: TokenStream) -> TokenStream {
  let item: ItemFn = parse_macro_input!(input as ItemFn);

  let have_args = !args.is_empty();
  let with_vm = if have_args {
    let args: Ident = parse_macro_input!(args as Ident);
    args == "with_vm"
  } else {
    false
  };

  native_types::native_binary(&item, with_vm).into()
}

#[proc_macro_attribute]
pub fn ternary(args: TokenStream, input: TokenStream) -> TokenStream {
  let item: ItemFn = parse_macro_input!(input as ItemFn);

  let have_args = !args.is_empty();
  let with_vm = if have_args {
    let args: Ident = parse_macro_input!(args as Ident);
    args == "with_vm"
  } else {
    false
  };

  native_types::native_ternary(&item, with_vm).into()
}

#[proc_macro_attribute]
pub fn opcode_bindings(args: TokenStream, input: TokenStream) -> TokenStream {
  let item_enum: ItemEnum = parse_macro_input!(input as ItemEnum);
  let binding: Path = parse_macro_input!(args as Path);

  bindings::bind_opcodes(item_enum, binding).into()
}
