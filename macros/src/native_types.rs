use crate::common;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::{FnArg, Item, ItemFn, ItemMod, ItemStruct};

pub(crate) fn native_fn(item: &ItemFn, with_vm: bool) -> TokenStream {
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = common::count_args!(item);

  let args = common::make_arg_list(nargs - if with_vm { 1 } else { 0 }, name_str);

  let call_expr = if with_vm {
    quote! {
      #ident(vm, #args)
    }
  } else {
    quote! {
      #ident(#args)
    }
  };

  quote! {
    fn #ident(vm: &mut Vm, mut args: Args) -> ValueResult {
      #item

      if args.list.len() == #nargs {
        let mut args = args.into_arg_iter();
        let output = #call_expr?;
        let value = vm.gc.allocate(output);
        Ok(value)
      } else {
        Err(ValueError::ArgumentError(args.list.len(), #nargs + 1))
      }
    }
  }
}

struct FnDef {
  name: Ident,
  item: ItemFn,
}

struct StructDef {
  name: Ident,
  item: ItemStruct,
}

struct ModuleDef {
  functions: Vec<FnDef>,
  structs: Vec<StructDef>,
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
          let item_fn = item_fn;
          functions.push(FnDef { name, item: item_fn });
        }
        Item::Struct(item_struct) => {
          let name = item_struct.ident.clone();
          structs.push(StructDef { name, item: item_struct })
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

  let module_def = match item.content.map(|(_, content)| ModuleDef::try_from(content)).transpose() {
    Ok(Some(m)) => m,
    Ok(None) => {
      // have mod foo;
      // need mod foo {}
      return syn::Error::new_spanned(mod_name, "mod impl must be implemented in the same file as it was declared")
        .into_compile_error();
    }
    Err(e) => return e,
  };

  let (fn_defs, functions) = collect_fn_defs(module_def.functions);
  let (struct_defs, structs) = collect_struct_defs(module_def.structs);
  let unchanged = module_def.unchanged;

  quote! {
    #[no_mangle]
    pub fn simple_script_load_module(vm: &mut Vm) -> ValueResult {
      let module = #mod_name::simple_script_autogen_create_module(&mut vm.gc);
      Ok(module)
    }

    mod #mod_name {
      use super::*;

      pub fn simple_script_autogen_create_module(gc: &mut Gc) -> Value {
        let module = LockedModule::initialize(gc, |gc, module| {
          #fn_defs
          #struct_defs
        });

        gc.allocate(module)
      }

      #(#functions)*

      #(#structs)*

      #(#unchanged)*
    }
  }
}

fn collect_fn_defs(functions: Vec<FnDef>) -> (TokenStream, Vec<ItemFn>) {
  let mut fn_defs = TokenStream::default();
  fn_defs.append_all(functions.iter().map(|native_fn| {
    let native_fn_name = &native_fn.name;
    let native_fn_name_str = native_fn_name.to_string();
    let native_fn_name_lit = Literal::string(&native_fn_name_str);
    quote! {
      module.set(gc, #native_fn_name_lit, Value::native(#native_fn_name)).ok();
    }
  }));

  (fn_defs, functions.into_iter().map(|f| f.item).collect())
}

fn collect_struct_defs(structs: Vec<StructDef>) -> (TokenStream, Vec<ItemStruct>) {
  let mut struct_defs = TokenStream::default();
  struct_defs.append_all(structs.iter().map(|native_struct| {
    let struct_name = &native_struct.name;
    let struct_name_str = struct_name.to_string();
    let struct_name_lit = Literal::string(&struct_name_str);
    quote! {
      module.set(gc, #struct_name_lit, Value::native(<#struct_name as UsertypeMethods>::__new__)).ok();
    }
  }));

  (struct_defs, structs.into_iter().map(|f| f.item).collect())
}
