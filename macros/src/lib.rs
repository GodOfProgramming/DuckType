use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemImpl, ItemStruct, PathArguments, Type};

#[proc_macro_derive(Class)]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def: ItemStruct = match syn::parse(input.clone()) {
    Ok(item) => item,
    Err(err) => return token_stream_with_error(input, err),
  };

  let mut _getters = Vec::<()>::new();
  let mut _setters = Vec::<()>::new();

  for field in &struct_def.fields {
    if let Some(seg) = field
      .attrs
      .iter()
      .find_map(|attr| attr.path.segments.iter().find(|seg| seg.ident == "field"))
    {
      /* struct Foo {
       *   #[field]
       *   foo: String,
       * }
       */
      if let PathArguments::Parenthesized(args) = &seg.arguments {
        for arg_input in &args.inputs {
          if let Type::Verbatim(_arg) = arg_input {
            // match syn::parse(arg.into()) {
            //   Ok(_) => todo!(),
            //   Err(err) => return token_stream_with_error(input, syn::Error::new_spanned(arg, "not a valid field")),
            // }
          }
        }
      }
    }
  }

  let name = struct_def.ident;
  quote! {
    impl Class for #name {

    }
  }
  .into()
}

#[proc_macro_attribute]
pub fn class_body(_args: TokenStream, input: TokenStream) -> TokenStream {
  let struct_impl: ItemImpl = match syn::parse(input.clone()) {
    Ok(item) => item,
    Err(err) => return token_stream_with_error(input, err),
  };

  let me = struct_impl.self_ty;

  quote! {
    impl ClassBody for #me {
      fn body
    }
  }
  .into()
}

fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
  tokens.extend(TokenStream::from(error.into_compile_error()));
  tokens
}
