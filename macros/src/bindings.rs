use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{ItemEnum, Path};

pub(crate) fn bind_opcodes(item_enum: ItemEnum, path: Path) -> TokenStream {
  let mut redecls = Vec::new();

  for var in &item_enum.variants {
    let cpp_name = var.ident.clone();
    let cpp_name = format!("duck_type_Opcode_{}", cpp_name);
    let cpp_name = Ident::new(&cpp_name, Span::call_site());
    redecls.push(quote! {
      #var = #path::#cpp_name as isize,
    });
  }

  let vis = &item_enum.vis;
  let name = &item_enum.ident;

  quote! {
    #vis enum #name {
      #(#redecls)*
    }
  }
}
