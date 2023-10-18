use crate::common;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::{FnArg, Item, ItemFn, ItemMod, ItemStruct};

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
    .count()
    - 1;

  let args = common::make_arg_list(nargs, name_str);

  quote! {
    fn #ident(vm: &mut Vm, mut args: Args) -> ValueResult {
      #item

      if args.list.len() == #nargs {
        let mut args = args.into_iter();
        let output = #ident(vm, #args)?;
        let value = vm.gc.allocate(output);
        Ok(value)
      } else {
        Err(ValueError::ArgumentError(args.list.len(), #nargs + 1))
      }
    }
  }
}

struct NativeStruct {
  name: Ident,
  item: ItemStruct,
}

struct ModuleDef {
  functions: Vec<NativeFn>,
  structs: Vec<NativeStruct>,
  unchanged: Vec<Item>,
}

impl TryFrom<Vec<Item>> for ModuleDef {
  type Error = TokenStream;

  fn try_from(module_content: Vec<Item>) -> Result<Self, Self::Error> {
    let mut functions = Vec::new();
    let mut structs = Vec::new();
    let mut unchanged = Vec::new();
    for item in module_content {
      match item {
        Item::Fn(item_fn) => {
          let name = item_fn.sig.ident.clone();
          let native_fn = native_fn(&item_fn);
          functions.push(NativeFn { name, tokens: native_fn });
        }
        Item::Struct(item_struct) => {
          let name = item_struct.ident.clone();
          structs.push(NativeStruct { name, item: item_struct })
        }
        thing => unchanged.push(thing),
      }
    }

    Ok(Self {
      functions,
      structs,
      unchanged,
    })
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
  let (struct_defs, structs) = collect_struct_defs(module_def.structs);
  let unchanged = module_def.unchanged;

  quote! {
    #[no_mangle]
    pub fn simple_script_load_module(vm: &mut Vm) -> ValueResult<()> {
      #mod_name::register_to(vm.env());
      Ok(())
    }

    mod #mod_name {
      use super::*;

      pub fn register_to(env: &mut Env) {
        env.define(#mod_name_lit, LockedModule::initialize(|module| {
           #fn_defs
           #struct_defs
        }));
      }

      #(#functions)*

      #(#structs)*

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

fn collect_struct_defs(structs: Vec<NativeStruct>) -> (TokenStream, Vec<ItemStruct>) {
  let mut struct_defs = TokenStream::default();
  struct_defs.append_all(structs.iter().map(|native_struct| {
    let struct_name = &native_struct.name;
    let struct_name_str = struct_name.to_string();
    let struct_name_lit = Literal::string(&struct_name_str);
    quote! {
      module.set(#struct_name_lit, Value::native(<#struct_name as UsertypeMethods>::__new__)).ok();
    }
  }));

  (struct_defs, structs.into_iter().map(|f| f.item).collect())
}
