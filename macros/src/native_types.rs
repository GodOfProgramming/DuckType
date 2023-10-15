use crate::{common, user_types};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::{token::Comma, FnArg, Item, ItemFn, ItemMod};

struct NativeFn {
  name: Ident,
  tokens: TokenStream,
}

pub(crate) fn native_fn(item: &ItemFn) -> TokenStream {
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = item
    .sig
    .inputs
    .iter()
    .filter(|input| matches!(input, FnArg::Typed(_)))
    .count();

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

  let try_cast_arg = common::try_cast_arg_fn_tokens();

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

struct NativeStruct {
  tokens: TokenStream,
}

struct NativeImpl {
  tokens: TokenStream,
}

struct ModuleDef {
  functions: Vec<NativeFn>,
  unchanged: Vec<Item>,
}

impl TryFrom<Vec<Item>> for ModuleDef {
  type Error = TokenStream;

  fn try_from(module_content: Vec<Item>) -> Result<Self, Self::Error> {
    let mut functions = Vec::new();
    let mut unchanged = Vec::new();
    for item in module_content {
      match item {
        Item::Fn(item_fn) => {
          let name = item_fn.sig.ident.clone();
          let native_fn = native_fn(&item_fn);
          functions.push(NativeFn { name, tokens: native_fn });
        }
        thing => unchanged.push(thing),
      }
    }

    Ok(Self { functions, unchanged })
  }
}

pub(crate) fn native_mod(item: ItemMod) -> TokenStream {
  let mod_name = &item.ident;
  let mod_name_str = mod_name.to_string();
  let mod_name_lit = Literal::string(&mod_name_str);

  let module_def = match item.content.map(|(_, content)| ModuleDef::try_from(content)).transpose() {
    Ok(Some(m)) => m,
    Ok(None) => {
      // have mod foo;
      // need mod foo {}
      return syn::Error::new_spanned(mod_name, "mod impl must be implemented in the same file as it was declared")
        .into_compile_error()
        .into();
    }
    Err(e) => return e,
  };

  let (fn_defs, functions) = collect_fn_defs(module_def.functions);
  let unchanged = module_def.unchanged;

  quote! {
    #[no_mangle]
    pub fn simple_script_load_module(vm: &mut Vm, env: &mut Env) -> ValueResult<()> {
      #mod_name::register_to(env);
      Ok(())
    }

    mod #mod_name {
      use super::*;

      pub fn register_to(env: &mut Env) {
        env.define(#mod_name_lit, LockedModule::initialize(|module| {
           #fn_defs
        }));
      }

      #(#functions)*

      #(#unchanged)*
    }
  }
}

fn collect_fn_defs(functions: Vec<NativeFn>) -> (TokenStream, Vec<TokenStream>) {
  let mut fn_defs = TokenStream::default();
  fn_defs.append_all(functions.iter().map(|native_fn| {
    let native_fn_name = &native_fn.name;
    let native_fn_name_str = native_fn_name.to_string();
    let native_fn_name_lit = Literal::string(&native_fn_name_str);
    quote! {
      module.set(#native_fn_name_lit, Value::native(#native_fn_name)).ok();
    }
  }));

  (fn_defs, functions.into_iter().map(|f| f.tokens).collect())
}
