use crate::common;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::quote;
use std::env;
use syn::{Fields, FnArg, ImplItem, ItemImpl, ItemStruct, Receiver};

pub(crate) fn derive_usertype(struct_def: ItemStruct, uuid_value: Option<Literal>, traceables: Vec<Ident>) -> TokenStream {
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

  let traceable_impl = if !traceables.is_empty() {
    quote! {
      impl TraceableValue for #name {
        fn trace(&self, marks: &mut Marker) {
          #(self.#traceables.trace(marks);)*
        }
      }
    }
  } else {
    quote! {
      impl TraceableValue for #name {
        fn trace(&self, _marks: &mut Marker) {
          // do nothing
        }
      }
    }
  };

  quote! {
    impl Usertype for #name {
      const ID: uuid::Uuid = uuid::uuid!(#uuid_value);
    }

    #traceable_impl
  }
}

pub(crate) fn derive_fields(struct_def: ItemStruct) -> TokenStream {
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
      fn get_field(&self, gc: &mut Gc, field: &str) -> ValueResult<Option<Value>> {
        match field {
          #(#ident_strs => Ok(Some(gc.allocate(&self.#idents))),)*
          _ => Ok(None),
        }
      }

      fn set_field(&mut self, gc: &mut Gc, field: &str, value: Value) -> ValueResult<()> {
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

  let mut constructor = None;
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
        "__new__" => constructor = Some(method),
        "__str__" => display_fn = Some(method),
        "__dbg__" => debug_fn = Some(method),
        "__lock__" => lock_fn = Some(method),
        _ => {
          let nargs = common::count_args!(method);

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
      let args = common::make_arg_list(nargs, &name_str);
      if method.receiver.mutability.is_some() {
        method_lambda_bodies.push(quote! {
          Value::new_native_fn_method(gc, this.clone(), |vm, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(mut this) = args.list.pop() {
                if let Some(this) = this.cast_to_mut::<#me>() {
                  let mut args = args.into_iter();
                  let output = #me::#name(this, #args)?;
                  Ok(vm.gc.allocate(output))
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
          Value::new_native_fn_method(gc, this.clone(), |vm, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(this) = args.list.pop() {
                if let Some(this) = this.cast_to::<#me>() {
                  let mut args = args.into_iter();
                  let output = #me::#name(this, #args)?;
                  Ok(vm.gc.allocate(output))
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
    let args = common::make_arg_list(nargs, name_str);

    static_lambda_bodies.push(quote! {
      Value::native(|vm, args| {
        if args.list.len() == #nargs {
          let mut args = args.into_iter();
          let output = #me::#name(#args)?;
          Ok(vm.gc.allocate(output))
        } else {
          Err(ValueError::ArgumentError(args.list.len(), #nargs))
        }
      })
    });
  }

  let constructor_impl = constructor
    .map(|constructor| {
      let nargs = common::count_args!(constructor);
      let name = &constructor.sig.ident;
      let name_str = Literal::string(&name.to_string());
      let args = common::make_arg_list(nargs, name_str);
      quote! {
        fn #name(vm: &mut Vm, mut args: Args) -> ValueResult {
          #constructor

          if args.list.len() == #nargs {
            let mut args = args.into_iter();
            let output = #name(#args)?;
            Ok(vm.gc.allocate(output))
          } else {
            Err(ValueError::ArgumentError(args.list.len(), #nargs))
          }
        }
      }
    })
    .unwrap_or_default();

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

  quote! {
    #struct_impl

    #[automatically_derived]
    impl UsertypeMethods for #me {
      #constructor_impl

      fn get_method(&self, gc: &mut Gc, this: &Value, field: &str) -> ValueResult<Option<Value>> {
        match field {
           #(#method_strs => Ok(Some(#method_lambda_bodies)),)*
           #(#static_strs => Ok(Some(#static_lambda_bodies)),)*
          _ => Ok(None),
        }
      }
    }

    #display_impl

    #debug_impl

    #lock_impl
  }
}
