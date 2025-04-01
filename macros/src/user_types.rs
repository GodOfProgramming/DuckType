use crate::common;
use proc_macro2::{Ident, Literal, Span, TokenStream};
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
          return common::error(name, format!("Error looking up env var {}: {}", envvar, e));
        }
      }
    }
  };

  let traceable_impl = if !traceables.is_empty() {
    quote! {
      impl TraceableValue for #name {
        fn deep_trace(&self, tracer: &mut Tracer) {
          #(self.#traceables.deep_trace(tracer);)*
        }

        fn incremental_trace(&self, tracer: &mut Tracer) {
          #(self.#traceables.incremental_trace(tracer);)*
        }
      }
    }
  } else {
    quote! {
      impl TraceableValue for #name {
        fn deep_trace(&self, _: &mut Tracer) {
          // do nothing
        }

        fn incremental_trace(&self, _: &mut Tracer) {
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

  match &struct_def.fields {
    Fields::Named(fields) => {
      for field in &fields.named {
        if field.attrs.iter().any(|attr| attr.path.is_ident("field")) {
          idents.push(field.ident.clone().unwrap());
        }
      }
    }
    _ => {
      return common::error(struct_def.fields, "not a valid class field");
    }
  }

  let ident_strs = idents
    .iter()
    .map(|ident| Literal::string(&ident.to_string()))
    .collect::<Vec<Literal>>();
  quote! {
    #[automatically_derived]
    impl UsertypeFields for #name {
      fn get_field(&self, vm: &mut Vm, field: Field) -> UsageResult<Option<Value>> {
        let name = match field {
          Field::Id(_) => return Err(UsageError::EmptyField),
          Field::Named(name) => name,
          Field::NamedId(_, name) => name,
        };

        match name {
          #(#ident_strs => Ok(Some(vm.make_value_from(&self.#idents))),)*
          _ => Ok(None),
        }
      }

      fn set_field(&mut self, vm: &mut Vm, field: Field, value: Value) -> UsageResult<()> {
        let name = match field {
          Field::Id(_) => return Err(UsageError::EmptyField),
          Field::Named(name) => name,
          Field::NamedId(_, name) => name,
        };

        match name {
          #(#ident_strs => self.#idents = value.try_into()?,)*
          _ => Err(UsageError::UndefinedMember(name.to_string()))?,
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
        _ => {
          let nargs = common::count_args!(method);

          match method.sig.inputs.iter().next() {
            Some(FnArg::Receiver(this)) => {
              methods.push(Method {
                name,
                receiver: this.clone(),
                nargs,
              });
            }
            _ => {
              statics.push(Static { name, nargs });
            }
          }
        }
      }
    }
  }

  let vm_ident = Ident::new("vm", Span::call_site());

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
          Value::native_method(#vm_ident, this, |#vm_ident, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(mut this) = args.list.pop() {
                if let Some(this) = this.cast_to_mut::<#me>() {
                  let mut args = args.into_arg_iter();
                  let output = #me::#name(this, #args)?;
                  Ok(vm.make_value_from(output))
                } else {
                  Err(UsageError::BadCast(#name_str, #me_str, this))
                }
              } else {
                Err(UsageError::MissingSelf(#name_str))
              }
            } else {
              Err(UsageError::ArgumentError(args.list.len(), #nargs + 1))
            }
          })
        });
      } else {
        method_lambda_bodies.push(quote! {
          Value::native_method(#vm_ident, this, |#vm_ident, mut args| {
            if args.list.len() == #nargs + 1 {
              if let Some(this) = args.list.pop() {
                if let Some(this) = this.cast_to::<#me>() {
                  let mut args = args.into_arg_iter();
                  let output = #me::#name(this, #args)?;
                  Ok(vm.make_value_from(output))
                } else {
                  Err(UsageError::BadCast(#name_str, #me_str, this))
                }
              } else {
                Err(UsageError::MissingSelf(#name_str))
              }
            } else {
              Err(UsageError::ArgumentError(args.list.len(), #nargs + 1))
            }
          })
        });
      }
    } else {
      return common::error(method.name, "cannot impl method for fn signature not taking self reference");
    }
  }

  let mut static_lambda_bodies = Vec::new();
  for static_method in statics {
    let nargs = static_method.nargs;
    let name = static_method.name;
    let name_str = Literal::string(&name.to_string());
    let args = common::make_arg_list(nargs, name_str);

    static_lambda_bodies.push(quote! {
      Value::new::<NativeFn>(|vm, args| {
        if args.list.len() == #nargs {
          let mut args = args.into_arg_iter();
          let output = #me::#name(#args)?;
          Ok(vm.make_value_from(output))
        } else {
          Err(UsageError::ArgumentError(args.list.len(), #nargs))
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
        fn #name(vm: &mut Vm, mut args: Args) -> UsageResult {
          #constructor

          if args.list.len() == #nargs {
            let mut args = args.into_arg_iter();
            let output = #name(#args)?;
            Ok(vm.make_value_from(output))
          } else {
            Err(UsageError::ArgumentError(args.list.len(), #nargs))
          }
        }
      }
    })
    .unwrap_or_default();

  quote! {
    #struct_impl

    #[automatically_derived]
    impl UsertypeMethods for #me {
      #constructor_impl

      fn get_method(&self, #vm_ident: &mut Vm, this: Value, field: Field) -> UsageResult<Option<Value>> {
        let name = match field {
          Field::Id(_) => return Err(UsageError::EmptyField),
          Field::Named(name) => name,
          Field::NamedId(_, name) => name,
        };

        match name {
          #(#method_strs => Ok(Some(#method_lambda_bodies)),)*
          #(#static_strs => Ok(Some(#static_lambda_bodies)),)*
          _ => Ok(None),
        }
      }
    }
  }
}
