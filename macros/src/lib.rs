use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, TokenStream as TokenStream2};
use quote::{quote, TokenStreamExt};
use syn::{parse_macro_input, token::Comma, Fields, FnArg, ImplItem, Item, ItemFn, ItemImpl, ItemMod, ItemStruct, Receiver};

#[proc_macro_derive(Usertype, attributes(__str__, __dbg__))]
pub fn derive_usertype(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

  let name = struct_def.ident;
  let name_str = name.to_string();
  let in_script_ident = Literal::string(&name_str);

  quote! {
    impl Usertype for #name {
      const ID: &'static str = #in_script_ident;
    }
  }
  .into()
}

#[proc_macro_derive(Class, attributes(field))]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);

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
    impl ClassFields for #name {
      fn get_member(&self, field: &str) -> Option<Value> {
        match field {
          #(#ident_strs => Some(Value::from(&self.#idents)),)*
          _ => None,
        }
      }

      fn set_member(&mut self, field: &str, value: Value) -> ValueResult<()> {
        match field {
          #(#ident_strs => self.#idents = value.try_into()?,)*
          _ => Err(ValueError::InvalidAssignment(field.to_string()))?,
        }
        Ok(())
      }
    }
  }
  .into()
}

#[proc_macro_attribute]
pub fn methods(_args: TokenStream, input: TokenStream) -> TokenStream {
  let struct_impl = parse_macro_input!(input as ItemImpl);

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

      let mut args = TokenStream2::default();
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

    let mut args = TokenStream2::new();
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

  let try_cast_arg = try_cast_arg_fn_tokens();

  quote! {
    #struct_impl

    #[automatically_derived]
    impl ClassMethods for #me {
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
  .into()
}

#[proc_macro_attribute]
pub fn native(_args: TokenStream, input: TokenStream) -> TokenStream {
  let item: Item = parse_macro_input!(input as Item);
  match item {
    Item::Fn(item) => native_fn(&item).into(),
    Item::Mod(item) => native_mod(item),
    thing => syn::Error::new_spanned(thing, "cannot impl method for fn signature not taking self reference")
      .into_compile_error()
      .into(),
  }
}

fn native_fn(item: &ItemFn) -> TokenStream2 {
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = item
    .sig
    .inputs
    .iter()
    .filter(|input| matches!(input, FnArg::Typed(_)))
    .count();

  let mut args = TokenStream2::new();
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

  let try_cast_arg = try_cast_arg_fn_tokens();

  quote! {
    fn #ident(_: &mut Vm, _: &mut Env, args: Args) -> ValueResult {
      #try_cast_arg

      #item

      if args.list.len() == #nargs {
        let mut args = args.list.into_iter();
        Ok(Value::from(#ident(#args)?))
      } else {
        Err(ValueError::ArgumentError(args.list.len(), #nargs + 1))
      }
    }
  }
}

fn native_mod(item: ItemMod) -> TokenStream {
  let mod_name = &item.ident;
  let mod_name_str = mod_name.to_string();
  let mod_name_lit = Literal::string(&mod_name_str);

  struct NativeFn {
    name: Ident,
    tokens: TokenStream2,
  }

  let mut functions = Vec::new();

  if let Some((_, module_content)) = &item.content {
    for item in module_content {
      match item {
        Item::Fn(item_fn) => {
          let name = item_fn.sig.ident.clone();
          let native_fn = native_fn(item_fn);
          functions.push(NativeFn { name, tokens: native_fn });
        }
        thing => {
          return syn::Error::new_spanned(thing, "cannot impl method for fn signature not taking self reference")
            .into_compile_error()
            .into()
        }
      }
    }
  } else {
    // have mod foo;
    // need mod foo {}
    return syn::Error::new_spanned(item, "mod impl must be implemented in the same file as it was declared")
      .into_compile_error()
      .into();
  }

  let mut fn_defs = TokenStream2::default();
  fn_defs.append_separated(
    functions.iter().map(|native_fn| {
      let native_fn_name = &native_fn.name;
      let native_fn_name_str = native_fn_name.to_string();
      let native_fn_name_lit = Literal::string(&native_fn_name_str);
      quote! {
        module.set(#native_fn_name_lit, Value::native(#native_fn_name)).ok();
      }
    }),
    Comma::default(),
  );

  let massaged_functions = functions.into_iter().map(|f| f.tokens).collect::<Vec<TokenStream2>>();

  quote! {
    mod #mod_name {
      use super::*;

      pub fn register_to(env: &mut Env) {
        env.define(#mod_name_lit, LockedModule::initialize(|module| {
           #fn_defs
        }));
      }

      #(#massaged_functions)*
    }
  }
  .into()
}

fn try_cast_arg_fn_tokens() -> TokenStream2 {
  quote! {
    pub fn try_arg_cast<T>(this: Value, fn_name: &'static str, pos: usize) -> ValueResult<T>
    where
      T: MaybeFrom<Value>,
    {
      T::maybe_from(this).ok_or_else(|| ValueError::InvalidArgument(fn_name, pos))
    }
  }
}
