//! # Parser Code Generation
//!
//! This module is used to generate code for a parser based on [nom v7.x](https://docs.rs/nom/7)

use kaitai_struct_types::{EndianSpec, IntTypeRef, WellKnownTypeRef};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::r#type::Type;

/// Generate the parser expression for a well-known type
pub fn wk_parser(w: &WellKnownTypeRef, p_endian: &Ident) -> TokenStream {
    match w {
        WellKnownTypeRef::Unsigned(u) => match u {
            IntTypeRef::Int1 => quote!(::nom::number::complete::u8),
            IntTypeRef::Int2(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::u16(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_u16),
                EndianSpec::Big => quote!(::nom::number::complete::be_u16),
            },
            IntTypeRef::Int4(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::u32(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_u32),
                EndianSpec::Big => quote!(::nom::number::complete::be_u32),
            },
            IntTypeRef::Int8(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::u64(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_u64),
                EndianSpec::Big => quote!(::nom::number::complete::be_u64),
            },
        },
        WellKnownTypeRef::Signed(i) => match i {
            IntTypeRef::Int1 => quote!(::nom::number::complete::i8(#p_endian)),
            IntTypeRef::Int2(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::i16(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_i16),
                EndianSpec::Big => quote!(::nom::number::complete::be_i16),
            },
            IntTypeRef::Int4(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::i32(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_i32),
                EndianSpec::Big => quote!(::nom::number::complete::be_i32),
            },
            IntTypeRef::Int8(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::i64(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_i64),
                EndianSpec::Big => quote!(::nom::number::complete::be_i64),
            },
        },
        WellKnownTypeRef::F4(e) => match e {
            EndianSpec::Implicit => quote!(::nom::number::complete::f32(#p_endian)),
            EndianSpec::Little => quote!(::nom::number::complete::le_f32),
            EndianSpec::Big => quote!(::nom::number::complete::be_f32),
        },
        WellKnownTypeRef::F8(e) => match e {
            EndianSpec::Implicit => quote!(::nom::number::complete::f64(#p_endian)),
            EndianSpec::Little => quote!(::nom::number::complete::le_f64),
            EndianSpec::Big => quote!(::nom::number::complete::be_f64),
        },
        WellKnownTypeRef::Str => quote!(::nom::bytes::complete::take_until(
            ::std::slice::from_ref(&0)
        )), // FIXME?
        WellKnownTypeRef::StrZ => quote!(::nom::bytes::complete::take_until(
            ::std::slice::from_ref(&0)
        )),
    }
}

/// Generate the parser expression for a user type
pub fn user_type(named_ty: &Type, is_root_parser: bool) -> TokenStream {
    fn _root_field(i: Ident) -> TokenStream {
        quote!(_root.#i)
    }

    let p = &named_ty.parser_name;
    let path = if let Some(m) = &named_ty.source_mod {
        quote!(#m::#p)
    } else {
        quote!(#p)
    };
    let mut values: Vec<_> = named_ty
        .root_obligations
        .fields()
        .map(|f| format_ident!("{}", f))
        .map(match is_root_parser {
            true => Ident::into_token_stream,
            false => _root_field,
        })
        .collect();
    for generics in named_ty.field_generics.values() {
        if generics.external {
            values.push(quote!(|_| todo!()))
        }
    }

    if values.is_empty() {
        path
    } else {
        quote!(#path(#(#values),*))
    }
}
