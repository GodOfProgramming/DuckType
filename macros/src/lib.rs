use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal};
use quote::{quote, TokenStreamExt};
use syn::{parse_macro_input, Fields, FnArg, ImplItem, ItemImpl, ItemStruct, Pat, PatType, Receiver, ReturnType, Token};

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
  let me_str = quote! { stringify!(#me) };

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

    struct Method {
      name: Ident,
      receiver: Receiver,
      nargs: usize,
      ret: ReturnType,
    }

    struct Static {
      name: Ident,
      nargs: usize,
      ret: ReturnType,
    }

    if let ImplItem::Method(method) = item {
      let name = method.sig.ident.clone();
      let nargs = method
        .sig
        .inputs
        .iter()
        .filter(|input| matches!(input, FnArg::Typed(_)))
        .count();
      let ret = method.sig.output.clone();

      if let Some(FnArg::Receiver(this)) = method.sig.inputs.iter().next() {
        methods.push(Method {
          name,
          receiver: this.clone(),
          nargs,
          ret,
        });
      } else {
        statics.push(Static { name, nargs, ret });
      }
    }
  }

  let method_strs = methods
    .iter()
    .map(|m| Literal::string(&m.name.to_string()))
    .collect::<Vec<Literal>>();

  let static_strs = statics
    .iter()
    .map(|s| Literal::string(&s.name.to_string()))
    .collect::<Vec<Literal>>();

  let mut lambda_bodies = Vec::new();
  for method in methods {
    if method.receiver.reference.is_some() {
      let nargs = method.nargs;
      let name = method.name;
      let name_str = Literal::string(&name.to_string());

      let mut args = quote! {};
      args.append_separated(
        (0..nargs).map(|i| {
          quote! {
            args
            .next()
            .unwrap()
            .try_into()
            .map_err(|e| ValueError::WrongType(#name_str, #i, e))?
          }
        }),
        quote! {,},
      );
      if method.receiver.mutability.is_some() {
        lambda_bodies.push(quote! {
          Value::native(|_, _, args| {
            if args.list.len() != #nargs {
              if let Some(mut this) = args.this {
                if let Some(this) = this.cast_to_mut::<#me>() {
                  let mut args = args.list.into_iter();
                  #me::#name(this, #args).map(Value::from)
                } else {
                  Err(ValueError::BadCast(#name_str, #me_str, this))
                }
              } else {
                Err(ValueError::MissingSelf(#name_str))
              }
            } else {
              Err(ValueError::ArgumentError(args.list.len(), #nargs))
            }
          })
        });
      } else {
      }
    } else {
      return TokenStream::from(
        syn::Error::new_spanned(method.name, "cannot impl method for fn signature not taking self reference")
          .into_compile_error(),
      );
    }
  }

  quote! {
    #struct_impl

    impl ClassBody for #me {
      fn lookup(name: &str) -> Option<Value> {
        match name {
           #(#method_strs => Some(#lambda_bodies ),)*
           // #(#static_strs => Some(Value::from(#me::#statics)),)*
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
