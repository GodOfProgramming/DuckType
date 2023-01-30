use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal};
use quote::{quote, TokenStreamExt};
use syn::{
  parenthesized,
  parse::{Parse, ParseStream},
  parse_macro_input, token, Fields, FnArg, ImplItem, ItemImpl, ItemStruct, Receiver, Result as SynResult,
};

struct ModuleAttr {
  #[allow(unused)]
  paren_token: token::Paren,
  arg: Literal,
}

impl Parse for ModuleAttr {
  fn parse(input: ParseStream) -> SynResult<Self> {
    let content;
    let paren_token = parenthesized!(content in input);
    let arg = content.parse()?;
    Ok(Self { paren_token, arg })
  }
}

#[proc_macro_derive(Class, attributes(field, module))]
pub fn derive_class(input: TokenStream) -> TokenStream {
  let struct_def = parse_macro_input!(input as ItemStruct);
  let module_attr = struct_def.attrs.iter().find(|attr| attr.path.is_ident("module"));

  let name = struct_def.ident;
  let mut idents = Vec::new();

  let in_script_ident = match module_attr.map(|m| TokenStream::from(m.tokens.clone())) {
    Some(tokens) => parse_macro_input!(tokens as ModuleAttr).arg,
    None => Literal::string(&name.to_string()),
  };

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
    impl Class for #name {
      const MOD_NAME: &'static str = #in_script_ident;

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
        method_lambda_bodies.push(quote! {
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
        method_lambda_bodies.push(quote! {
          Value::native(|_, _, args| {
            if args.list.len() != #nargs {
              if let Some(this) = args.this {
                if let Some(this) = this.cast_to::<#me>() {
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

    static_lambda_bodies.push(quote! {
      Value::native(|_, _, args| {
        if args.list.len() != #nargs {
          let mut args = args.list.into_iter();
          #me::#name(#args).map(Value::from)
        } else {
          Err(ValueError::ArgumentError(args.list.len(), #nargs))
        }
      })
    });
  }

  quote! {
    #struct_impl

    impl ClassBody for #me {
      fn lookup(name: &str) -> Option<Value> {
        match name {
           #(#method_strs => Some(#method_lambda_bodies),)*
           #(#static_strs => Some(#static_lambda_bodies),)*
          _ => None,
        }
      }
    }
  }
  .into()
}
