use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use syn::{parse_macro_input, Fields, ItemImpl, ItemStruct};

#[proc_macro_derive(Class, attributes(field))]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

  let mut getters = Vec::new();
  let mut setters = Vec::new();

  if let Fields::Named(fields) = &struct_def.fields {
    /* struct Foo {
     *   #[field]
     *   foo: String,
     * }
     */
    for field in &fields.named {
      if field.attrs.iter().find(|attr| attr.path.is_ident("field")).is_some() {
        let ident = field.ident.clone().unwrap();
        getters.push(ident.clone());
        setters.push(ident);
      }
    }
  } else {
    return TokenStream::from(syn::Error::new_spanned(struct_def.fields, "not a valid class field").to_compile_error());
  }

  let name = struct_def.ident;

  let getter_strs = getters
    .iter()
    .map(|ident| Literal::string(&ident.to_string()))
    .collect::<Vec<Literal>>();
  quote! {
    impl Class for #name {
      fn get(&self, field: &str) -> Option<Value> {
        match field {
          #(#getter_strs => Some(Value::from(&self.#getters)),)*
          _ => None,
        }
      }

      fn set(&mut self, field: &str, value: Value) -> Result<(), Box<dyn std::error::Error>> {
        match field {
          #(#getter_strs => self.#getters = value.try_into()?,)*
          _ => (),
        }
        Ok(())
      }
    }
  }
  .into()
}

#[proc_macro_attribute]
pub fn class_body(_args: TokenStream, input: TokenStream) -> TokenStream {
  let struct_impl = parse_macro_input!(input as ItemImpl);

  let me = struct_impl.self_ty;

  quote! {
    impl ClassBody for #me {
    }
  }
  .into()
}

#[allow(unused)]
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
  tokens.extend(TokenStream::from(error.into_compile_error()));
  tokens
}
