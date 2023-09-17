use kaitai_struct_types::{EndianSpec, FloatTypeRef, IntTypeRef, WellKnownTypeRef};
use proc_macro2::{Ident, TokenStream};
use quote::quote;

pub fn sint_parser(width: usize, endian: EndianSpec, p_endian: &Ident) -> TokenStream {
    match (width, endian) {
        (1, _) => quote!(::nom::number::complete::i8(#p_endian)),
        (2, EndianSpec::Implicit) => quote!(::nom::number::complete::i16(#p_endian)),
        (2, EndianSpec::Little) => quote!(::nom::number::complete::le_i16),
        (2, EndianSpec::Big) => quote!(::nom::number::complete::be_i16),
        (4, EndianSpec::Implicit) => quote!(::nom::number::complete::i32(#p_endian)),
        (4, EndianSpec::Little) => quote!(::nom::number::complete::le_i32),
        (4, EndianSpec::Big) => quote!(::nom::number::complete::be_i32),
        (8, EndianSpec::Implicit) => quote!(::nom::number::complete::i64(#p_endian)),
        (8, EndianSpec::Little) => quote!(::nom::number::complete::le_i64),
        (8, EndianSpec::Big) => quote!(::nom::number::complete::be_i64),
        _ => panic!(),
    }
}

pub fn uint_parser(width: usize, endian: EndianSpec, p_endian: &Ident) -> TokenStream {
    match (width, endian) {
        (1, _) => quote!(::nom::number::complete::u8(#p_endian)),
        (2, EndianSpec::Implicit) => quote!(::nom::number::complete::u16(#p_endian)),
        (2, EndianSpec::Little) => quote!(::nom::number::complete::le_u16),
        (2, EndianSpec::Big) => quote!(::nom::number::complete::be_u16),
        (4, EndianSpec::Implicit) => quote!(::nom::number::complete::u32(#p_endian)),
        (4, EndianSpec::Little) => quote!(::nom::number::complete::le_u32),
        (4, EndianSpec::Big) => quote!(::nom::number::complete::be_u32),
        (8, EndianSpec::Implicit) => quote!(::nom::number::complete::u64(#p_endian)),
        (8, EndianSpec::Little) => quote!(::nom::number::complete::le_u64),
        (8, EndianSpec::Big) => quote!(::nom::number::complete::be_u64),
        _ => panic!(),
    }
}

pub fn float_parser(width: usize, endian: EndianSpec, p_endian: &Ident) -> TokenStream {
    match (width, endian) {
        (4, EndianSpec::Implicit) => quote!(::nom::number::complete::f32(#p_endian)),
        (4, EndianSpec::Little) => quote!(::nom::number::complete::le_f32),
        (4, EndianSpec::Big) => quote!(::nom::number::complete::be_f32),
        (8, EndianSpec::Implicit) => quote!(::nom::number::complete::f64(#p_endian)),
        (8, EndianSpec::Little) => quote!(::nom::number::complete::le_f64),
        (8, EndianSpec::Big) => quote!(::nom::number::complete::be_f64),
        _ => panic!(),
    }
}

/// Generate the parser expression for a well-known type
pub fn wk_parser(w: &WellKnownTypeRef, size: Option<&str>, p_endian: &Ident) -> TokenStream {
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
        WellKnownTypeRef::Float(f) => match f {
            FloatTypeRef::Float4(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::f32(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_f32),
                EndianSpec::Big => quote!(::nom::number::complete::be_f32),
            },
            FloatTypeRef::Float8(e) => match e {
                EndianSpec::Implicit => quote!(::nom::number::complete::f64(#p_endian)),
                EndianSpec::Little => quote!(::nom::number::complete::le_f64),
                EndianSpec::Big => quote!(::nom::number::complete::be_f64),
            },
        },
        WellKnownTypeRef::Str => {
            if let Some(size_expr) = size {
                let expr = super::expr::codegen_expr_str(size_expr);
                quote!(::nom::combinator::map(::nom::bytes::complete::take(#expr), ::std::borrow::Cow::Borrowed))
            } else {
                todo!("str without size")
                //quote!(::nom::bytes::complete::take(0x70D0 as usize))
            }
        } // FIXME?
        WellKnownTypeRef::StrZ => quote!(::nom::combinator::map(
            ::nom::bytes::complete::take_until(::std::slice::from_ref(&0)),
            ::std::borrow::Cow::Borrowed
        )),
    }
}
