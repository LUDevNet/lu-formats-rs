use heck::ToUpperCamelCase;
use kaitai_struct_types::EnumSpec;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};

pub(super) fn codegen_enum(name: &str, spec: &EnumSpec) -> TokenStream {
    let id = format_ident!("{}", name.to_upper_camel_case());
    let mut matches = Vec::<TokenStream>::new();
    let mut values = Vec::<TokenStream>::new();

    let mut integers: Vec<isize> = Vec::with_capacity(spec.0.len());
    for (key, value) in &spec.0 {
        let var_id = &value.id;
        let var_id = format_ident!("{}", var_id.0.to_upper_camel_case());
        let int_val: isize = key.parse().unwrap();
        integers.push(int_val);
        let val = Literal::isize_unsuffixed(int_val);
        let doc = format!("Value: `{}`", key);
        values.push(quote! {
            #[doc = #doc]
            #var_id = #val
        });
        matches.push(quote!(
            #val => Ok(Self::#var_id)
        ))
    }
    let min = *integers
        .iter()
        .min()
        .expect("enum has at least one element");
    let max = *integers
        .iter()
        .max()
        .expect("enum has at least one element");
    let consts = spec.0.iter().map(|(key, value)| {
        let id = &value.id;
        let id = format_ident!("_{}", id.0.to_uppercase());
        let val: u64 = key.parse().unwrap();
        let doc = format!("Value: `{}`", key);
        quote! {
            #[doc = #doc]
            pub const #id: u64 = #val;
        }
    });

    let try_from_u8 = (min >= isize::from(u8::MIN) && max <= isize::from(u8::MAX)).then(|| {
        quote!(
            impl ::std::convert::TryFrom<u8> for #id {
                type Error = ();

                fn try_from(v: u8) -> Result<Self, Self::Error> {
                    match v {
                        #(#matches,)*
                        _ => Err(())
                    }
                }
            }
        )
    });
    let try_from_u32 = (min >= (u32::MIN as isize) && max <= (u32::MAX as isize)).then(|| {
        quote!(
            impl ::std::convert::TryFrom<u32> for #id {
                type Error = ();

                fn try_from(v: u32) -> Result<Self, Self::Error> {
                    match v {
                        #(#matches,)*
                        _ => Err(())
                    }
                }
            }
        )
    });

    quote! {
        #[doc = ""]
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[cfg_attr(feature = "serde", derive(::serde::Serialize))]
        pub enum #id {
            #(#values),*
        }

        #try_from_u8
        #try_from_u32


        impl #id {
            #(#consts)*
        }
    }
}
