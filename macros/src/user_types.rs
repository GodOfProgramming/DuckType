use crate::common;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, TokenStreamExt};
use std::env;
use syn::{token::Comma, Fields, FnArg, ImplItem, ItemImpl, ItemStruct, Receiver};

pub(crate) fn derive_usertype(struct_def: ItemStruct, uuid_value: Option<Literal>) -> TokenStream {
  let name = struct_def.ident;

  let uuid_value = match uuid_value {
    Some(uuid_value) => {
      if uuid_value.to_string() == "\"random\"" {
        Literal::string(&uuid::Uuid::new_v4().to_string())
      } else {
        uuid_value
      }
    }
    None => {
      let envvar = format!("{}_UUID", name);
      match env::var(&envvar) {
        Ok(uuid_value) => Literal::string(&uuid_value),
        Err(e) => {
          return TokenStream::from(
            syn::Error::new_spanned(name, format!("Error looking up env var {}: {}", envvar, e)).to_compile_error(),
          )
        }
      }
    }
  };

  quote! {
    impl Usertype for #name {
      const ID: uuid::Uuid = uuid::uuid!(#uuid_value);
    }
  }
}

pub(crate) fn derive_class(struct_def: ItemStruct) -> TokenStream {
  let name = struct_def.ident;
  let mut idents = Vec::new();

  if let Fields::Named(fields) = &struct_def.fields {
    for field in &fields.named {
      if field.attrs.iter().find(|attr| attr.path.is_ident("field")).is_some() {
        idents.push(field.ident.clone().unwrap());
      }
    }
  } else {
    return TokenStream::from(syn::Error::new_spanned(struct_def.fields, "not a valid class field").to_compile_error());
  }

  let ident_strs = idents
    .iter()
    .map(|ident| Literal::string(&ident.to_string()))
    .collect::<Vec<Literal>>();
  quote! {
    #[automatically_derived]
    impl UsertypeFields for #name {
      fn get_field(&self, field: &str) -> Option<Value> {
        match field {
          #(#ident_strs => Some(Value::from(&self.#idents)),)*
          _ => None,
        }
      }

      fn set_field(&mut self, field: &str, value: Value) -> ValueResult<()> {
        match field {
          #(#ident_strs => self.#idents = value.try_into()?,)*
          _ => Err(ValueError::InvalidAssignment(field.to_string()))?,
        }
        Ok(())
      }
    }
  }
}

pub(crate) fn derive_methods(struct_impl: ItemImpl) -> TokenStream {
  let me = struct_impl.self_ty.clone();
  let me_str = quote! { stringify!(#me) };

  let mut methods = Vec::new();
  let mut statics = Vec::new();

  let mut display_fn = None;
  let mut debug_fn = None;
  let mut lock_fn = None;

  for item in &struct_impl.items {
    struct Method {
      name: Ident,
      receiver: Receiver,
      nargs: usize,
    }

    struct Static {
      name: Ident,
      nargs: usize,
    }

    if let ImplItem::Method(method) = item {
      let name = method.sig.ident.clone();
      let name_str = name.to_string();
      match name_str.as_str() {
        "__str__" => display_fn = Some(method),
        "__dbg__" => debug_fn = Some(method),
        "__lock__" => lock_fn = Some(method),
        _ => {
          let nargs = method
            .sig
            .inputs
            .iter()
            .filter(|input| matches!(input, FnArg::Typed(_)))
            .count();

          if let Some(FnArg::Receiver(this)) = method.sig.inputs.iter().next() {
            methods.push(Method {
              name,
              receiver: this.clone(),
              nargs,
            });
          } else {
            statics.push(Static { name, nargs });
          }
        }
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

  let mut method_lambda_bodies = Vec::new();
  for method in methods {
    if method.receiver.reference.is_some() {
      let nargs = method.nargs;
      let name = method.name;
      let name_str = Literal::string(&name.to_string());

      let mut args = TokenStream::default();
      args.append_separated(
        (0..nargs).map(|i| {
          quote! {
            try_arg_cast(args
            .next()
            .unwrap(), #name_str, #i)?
          }
        }),
        Comma::default(),
      );
      if method.receiver.mutability.is_some() {
        method_lambda_bodies.push(quote! {
          Value::new_native_fn_method(this.clone(), |_, _, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(mut this) = args.list.pop() {
                if let Some(this) = this.cast_to_mut::<#me>() {
                  let mut args = args.list.into_iter();
                  Ok(Value::from(#me::#name(this, #args)?))
                } else {
                  Err(ValueError::BadCast(#name_str, #me_str, this))
                }
              } else {
                Err(ValueError::MissingSelf(#name_str))
              }
            } else {
              Err(ValueError::ArgumentError(args.list.len(), #nargs + 1))
            }
          })
        });
      } else {
        method_lambda_bodies.push(quote! {
          Value::new_native_fn_method(this.clone(), |_, _, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(this) = args.list.pop() {
                if let Some(this) = this.cast_to::<#me>() {
                  let mut args = args.list.into_iter();
                  Ok(Value::from(#me::#name(this, #args)?))
                } else {
                  Err(ValueError::BadCast(#name_str, #me_str, this))
                }
              } else {
                Err(ValueError::MissingSelf(#name_str))
              }
            } else {
              Err(ValueError::ArgumentError(args.list.len(), #nargs + 1))
            }
          })
        });
      }
    } else {
      return TokenStream::from(
        syn::Error::new_spanned(method.name, "cannot impl method for fn signature not taking self reference")
          .into_compile_error(),
      );
    }
  }

  let mut static_lambda_bodies = Vec::new();
  for static_method in statics {
    let nargs = static_method.nargs;
    let name = static_method.name;
    let name_str = Literal::string(&name.to_string());

    let mut args = TokenStream::new();
    args.append_separated(
      (0..nargs).map(|i| {
        quote! {
          try_arg_cast(args
          .next()
          .unwrap(), #name_str, #i)?
        }
      }),
      Comma::default(),
    );

    static_lambda_bodies.push(quote! {
      Value::native(|_, _, args| {
        if args.list.len() == #nargs {
          let mut args = args.list.into_iter();
          Ok(Value::from(#me::#name(#args)?))
        } else {
          Err(ValueError::ArgumentError(args.list.len(), #nargs))
        }
      })
    });
  }

  let lock_impl = if let Some(lock_fn) = lock_fn {
    quote! {
      impl LockableValue for #me {
        #lock_fn
      }
    }
  } else {
    quote! {
      impl LockableValue for #me {}
    }
  };

  let debug_impl = if let Some(debug_fn) = debug_fn {
    quote! {
      impl DebugValue for #me {
        #debug_fn
      }
    }
  } else if display_fn.is_some() {
    quote! {
      impl DebugValue for #me {
        fn __dbg__(&self) -> String {
          self.__str__()
        }
      }
    }
  } else {
    quote! {
      impl DebugValue for #me {
        fn __dbg__(&self) -> String {
          #me_str.to_string()
        }
      }
    }
  };

  let display_impl = if let Some(display_fn) = display_fn {
    quote! {
      impl DisplayValue for #me {
        #display_fn
      }
    }
  } else {
    quote! {
      impl DisplayValue for #me {
        fn __str__(&self) -> String {
          #me_str.to_string()
        }
      }
    }
  };

  let try_cast_arg = common::try_cast_arg_fn_tokens();

  quote! {
    #struct_impl

    #[automatically_derived]
    impl UsertypeMethods for #me {
      fn get_method(&self, this: &Value, field: &str) -> Option<Value> {

        #try_cast_arg

        match field {
           #(#method_strs => Some(#method_lambda_bodies),)*
           #(#static_strs => Some(#static_lambda_bodies),)*
          _ => None,
        }
      }
    }

    #display_impl

    #debug_impl

    #lock_impl
  }
}
