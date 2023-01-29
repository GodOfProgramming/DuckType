use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::quote;
use syn::{parse_macro_input, Fields, FnArg, ImplItem, ItemImpl, ItemStruct};

#[proc_macro_derive(Class, attributes(field))]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

  let mut idents = Vec::new();

  if let Fields::Named(fields) = &struct_def.fields {
    /* struct Foo {
     *   #[field]
     *   foo: String,
     * }
     */
    for field in &fields.named {
      if field.attrs.iter().find(|attr| attr.path.is_ident("field")).is_some() {
        idents.push(field.ident.clone().unwrap());
      }
    }
  } else {
    return TokenStream::from(syn::Error::new_spanned(struct_def.fields, "not a valid class field").to_compile_error());
  }

  let name = struct_def.ident;

  let ident_strs = idents
    .iter()
    .map(|ident| Literal::string(&ident.to_string()))
    .collect::<Vec<Literal>>();
  quote! {
    impl Class for #name {
      fn get(&self, field: &str) -> Option<Value> {
        match field {
          #(#ident_strs => Some(Value::from(&self.#idents)),)*
          _ => None,
        }
      }

      fn set(&mut self, field: &str, value: Value) -> Result<(), Box<dyn std::error::Error>> {
        match field {
          #(#ident_strs => self.#idents = value.try_into()?,)*
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

  let me = struct_impl.self_ty.clone();

  let mut methods = Vec::new();
  let mut statics = Vec::new();

  for item in &struct_impl.items {
    /* impl Foo {
     *   #[method]
     *   fn do_foo(&mut self, arg1: i32) -> f32 {
     *     1.0
     *   }
     *
     *   #[static_method]
     *   fn do_bar(arg1: f32) -> i32 {
     *     1
     *   }
     * }
     */

    if let ImplItem::Method(method) = item {
      if let Some(FnArg::Receiver(_this)) = method.sig.inputs.iter().next() {
        methods.push(method.sig.ident.clone());
      } else {
        statics.push(method.sig.ident.clone());
      }
    }
  }

  let method_strs = methods
    .iter()
    .map(|m| Literal::string(&m.to_string()))
    .collect::<Vec<Literal>>();

  let static_strs = statics
    .iter()
    .map(|s| Literal::string(&s.to_string()))
    .collect::<Vec<Literal>>();

  quote! {
    #struct_impl

    impl ClassBody for #me {
      fn lookup(name: &str) -> Option<Value> {
        match name {
          #(#method_strs => Some(Value::from(#me::#methods)),)*
          #(#static_strs => Some(Value::from(#me::#statics)),)*
          _ => None,
        }
      }
    }
  }
  .into()
}

#[allow(unused)]
fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
  tokens.extend(TokenStream::from(error.into_compile_error()));
  tokens
}
