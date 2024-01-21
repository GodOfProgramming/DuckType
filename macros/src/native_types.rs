use crate::{common, StrAttr};
use proc_macro2::{Ident, Literal, Span, TokenStream};
use quote::{quote, TokenStreamExt};
use syn::{FnArg, Item, ItemFn, ItemMod, ItemStruct};

pub(crate) fn native_fn(item: &ItemFn, with_vm: bool) -> TokenStream {
  let vis = &item.vis;
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = common::count_args!(item) - if with_vm { 1 } else { 0 };

  let args = common::make_arg_list(nargs, name_str);

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
    #vis fn #ident(vm: &mut Vm, mut args: Args) -> UsageResult {
      #item

      if args.list.len() == #nargs {
        let mut args = args.into_arg_iter();
        let output = #call_expr?;
        let value = vm.make_value_from(output);
        Ok(value)
      } else {
        Err(UsageError::ArgumentError(args.list.len(), #nargs))
      }
    }
  }
}

pub(crate) fn native_binary(item: &ItemFn, with_vm: bool) -> TokenStream {
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = common::count_args!(item) - if with_vm { 1 } else { 0 };

  if nargs != 2 {
    return common::error(item, format!("expected 2 args, found {}", nargs));
  }

  let args = common::make_named_arg_list(name_str, ["left", "right"]);

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
    fn #ident(vm: &mut Vm, mut left: Value, mut right: Value) -> UsageResult<()> {
      #item

      let output = #call_expr?;
      let value = vm.make_value_from(output);
      vm.stack_push(value);
      Ok(())
    }
  }
}

pub(crate) fn native_ternary(item: &ItemFn, with_vm: bool) -> TokenStream {
  let ident = &item.sig.ident;
  let name_str = ident.to_string();
  let nargs = common::count_args!(item) - if with_vm { 1 } else { 0 };

  if nargs != 3 {
    return common::error(item, format!("expected 2 args, found {}", nargs));
  }

  let args = common::make_named_arg_list(name_str, ["left", "mid", "right"]);

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
    fn #ident(vm: &mut Vm, mut left: Value, mut mid: Value, mut right: Value) -> UsageResult<()> {
      #item

      let output = #call_expr?;
      let value = vm.make_value_from(output);
      vm.stack_push(value);
      Ok(())
    }
  }
}

struct FnDef {
  name: Ident,
  item: ItemFn,
}

struct StructDef {
  name: Ident,
  rename: Option<Literal>,
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

          let rename_attr = match item_struct.attrs.iter().find(|attr| attr.path.is_ident("rename")) {
            Some(rename_attr) => {
              let tokens = rename_attr.tokens.clone();
              match syn::parse::<StrAttr>(tokens.into()) {
                Ok(rename_value) => Some(rename_value.string),
                Err(e) => return Err(common::error(rename_attr, format!("rename value is not a string: {}", e))),
              }
            }
            None => None,
          };

          structs.push(StructDef {
            name,
            rename: rename_attr,
            item: item_struct,
          })
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

pub(crate) fn native_mod(item: ItemMod, no_entry: bool) -> TokenStream {
  let mod_name = &item.ident;

  let module_def = match item.content.map(|(_, content)| ModuleDef::try_from(content)).transpose() {
    Ok(Some(m)) => m,
    Ok(None) => {
      // have mod foo;
      // need mod foo {}
      return common::error(mod_name, "mod impl must be implemented in the same file as it was declared");
    }
    Err(e) => return e,
  };

  let collect_idents = ModDefIdentCollection {
    vm_ident: Ident::new("vm", Span::call_site()),
    module_ident: Ident::new("module", Span::call_site()),
  };

  let (fn_defs, functions) = collect_fn_defs(&collect_idents, module_def.functions);
  let (struct_defs, structs) = collect_struct_defs(&collect_idents, module_def.structs);
  let unchanged = module_def.unchanged;

  let ModDefIdentCollection { module_ident, vm_ident } = collect_idents;

  let entry_point = if no_entry {
    TokenStream::default()
  } else {
    quote! {
      #[no_mangle]
      pub fn duck_type_load_module(#vm_ident: &mut Vm) -> UsertypeHandle<ModuleValue> {
        let #module_ident = #vm_ident.current_module_value();
        #mod_name::duck_type_autogen_create_module(#vm_ident, #module_ident.into())
      }
    }
  };

  let name_lit = Literal::string(&mod_name.to_string());

  quote! {
    #entry_point

    mod #mod_name {
      use super::*;

      pub fn duck_type_autogen_create_module(#vm_ident: &mut Vm, #module_ident: Value) -> UsertypeHandle<ModuleValue> {
        ModuleBuilder::initialize(#vm_ident, ModuleType::new_child(#name_lit, #module_ident), |#vm_ident, mut #module_ident| {
          #fn_defs
          #struct_defs
        })
      }

      #(#functions)*

      #(#structs)*

      #(#unchanged)*
    }
  }
}

struct ModDefIdentCollection {
  vm_ident: Ident,
  module_ident: Ident,
}

fn collect_fn_defs(idents: &ModDefIdentCollection, functions: Vec<FnDef>) -> (TokenStream, Vec<ItemFn>) {
  let ModDefIdentCollection {
    vm_ident: _,
    module_ident,
  } = &idents;
  let mut fn_defs = TokenStream::default();
  fn_defs.append_all(functions.iter().map(|native_fn| {
    let native_fn_name = &native_fn.name;
    let native_fn_name_str = native_fn_name.to_string();
    let native_fn_name_lit = Literal::string(&native_fn_name_str);
    quote! {
      #module_ident.define(#native_fn_name_lit, Value::new::<NativeFn>(#native_fn_name));
    }
  }));

  (fn_defs, functions.into_iter().map(|f| f.item).collect())
}

fn collect_struct_defs(idents: &ModDefIdentCollection, structs: Vec<StructDef>) -> (TokenStream, Vec<ItemStruct>) {
  let ModDefIdentCollection {
    vm_ident: _,
    module_ident,
  } = &idents;
  let mut struct_defs = TokenStream::default();
  struct_defs.append_all(structs.iter().map(|native_struct| {
    let struct_name = &native_struct.name;
    let struct_name_str = struct_name.to_string();
    let struct_name_lit = &Literal::string(&struct_name_str);
    let struct_name_lit = native_struct.rename.as_ref().unwrap_or(struct_name_lit);
    quote! {
      #module_ident.define(#struct_name_lit, Value::new::<NativeFn>(<#struct_name as UsertypeMethods>::__new__));
    }
  }));

  (struct_defs, structs.into_iter().map(|f| f.item).collect())
}
