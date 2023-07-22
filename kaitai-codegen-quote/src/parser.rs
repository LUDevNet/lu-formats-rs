//! # Parser Code Generation
//!
//! This module is used to generate code for a parser based on [nom v7.x](https://docs.rs/nom/7)

use kaitai_expr::{parse_expr, Expr, Op};
use kaitai_struct_types::{EndianSpec, IntTypeRef, WellKnownTypeRef};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::r#type::Type;

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
        WellKnownTypeRef::Str => {
            if let Some(size_expr) = size {
                let expr = codegen_expr_str(size_expr);
                quote!(::nom::bytes::complete::take(#expr))
            } else {
                quote!(::nom::bytes::complete::take(0x70D0 as usize))
            }
        } // FIXME?
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

fn codegen_expr(_expr: &Expr) -> TokenStream {
    match _expr {
        Expr::Input(i, fields) => {
            let mut ts = format_ident!("{}", i).into_token_stream();
            for field in fields {
                let f = format_ident!("{}", field);
                ts = quote!(#ts.#f);
            }
            ts
        }
        Expr::Number(u) => Literal::u64_unsuffixed(*u).into_token_stream(),
        Expr::BinOp { op, args } => {
            let lhs = codegen_expr(&args.0);
            let rhs = codegen_expr(&args.1);
            match op {
                Op::Mul => quote!((#lhs * #rhs)),
                Op::Div => quote!((#lhs / #rhs)),
                Op::Sub => quote!((#lhs - #rhs)),
                Op::Add => quote!((#lhs + #rhs)),
                Op::Dot => quote!((#lhs).#rhs),
                Op::GtEq => quote!((#lhs >= #rhs)),
                Op::Gt => quote!((#lhs > #rhs)),
                Op::LtEq => quote!((#lhs <= #rhs)),
                Op::Lt => quote!((#lhs < #rhs)),
                Op::Eq => quote!((#lhs == #rhs)),
                Op::And => quote!((#lhs && #rhs)),
                Op::Or => quote!((#lhs || #rhs)),
                Op::LParen | Op::RParen | Op::TernaryTrue | Op::TernaryFalse => quote!(0xBEEF),
            }
        }
    }
}

pub fn codegen_expr_str(expr: &str) -> TokenStream {
    let parsed_expr = parse_expr(expr).expect(expr);
    codegen_expr(&parsed_expr)
}
