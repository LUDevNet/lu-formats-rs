//! # Parser Code Generation
//!
//! This module is used to generate code for a parser based on [nom v7.x](https://docs.rs/nom/7)

use crate::{
    ctx::NamingContext,
    r#type::{uint_ty, CaseKind, Field, FieldGenerics, ResolvedType, Type},
};
use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, Contents, Endian, Repeat, TypeRef, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

mod expr;
mod obligations;
mod primitive;

/// Generate the parser expression for a user type
fn user_type(
    nc: &NamingContext,
    named_ty: &Type,
    is_root_parser: bool,
    in_parent: bool,
    p_endian: &Ident,
) -> TokenStream {
    let p = &named_ty.parser_name;
    let path = if let Some(m) = &named_ty.source_mod {
        quote!(#m::#p)
    } else {
        quote!(#p)
    };
    let mut values = Vec::new();
    obligations::root_obligation_values(named_ty, &mut values, is_root_parser);
    obligations::parent_obligation_values(named_ty, &mut values, in_parent);
    for generics in named_ty.field_generics.values() {
        if generics.external {
            let variants_parser = variant_parser_expr(nc, is_root_parser, generics, true, p_endian);
            values.push(variants_parser)
        }
    }

    if values.is_empty() {
        path
    } else {
        quote!(#path(#(#values),*))
    }
}

pub(super) fn codegen_parser_fn(
    self_ty: &Type,
    seq: &[Attribute],
    tc: &super::TyContext,
    nc: &NamingContext,
) -> TokenStream {
    let mut parser = vec![];
    let mut field_idents = Vec::<Ident>::with_capacity(seq.len());

    let p_endian = format_ident!("_endian");
    let p_input = format_ident!("_input");

    for (i, attr) in seq.iter().enumerate() {
        if attr.id.is_none() {
            continue;
        }

        let field = &self_ty.fields[i];
        field_idents.push(field.ident().clone());
        parser.push(codegen_attr_parse(
            field, self_ty, attr, &p_input, &p_endian, nc,
        ));
    }

    let input_lifetime = match self_ty.needs_lifetime {
        true => None,
        false => Some(quote!('a,)),
    };

    let mut parser_args = Vec::<TokenStream>::new();
    let _root = obligations::root_obligation_params(self_ty, nc, &mut parser_args);
    let _parent = obligations::parent_obligation_params(self_ty, &mut parser_args);

    let _input_ty = quote!(&'a [u8]);
    let _external_field_generics: Vec<_> = self_ty
        .field_generics
        .iter()
        .filter(|f| f.1.external)
        .map(|(_field, generics)| {
            let p = &generics.parser;
            let t = &generics.type_;
            quote!(mut #p: impl FnMut(&'a [u8]) -> ::nom::IResult<#_input_ty, #t>,)
        })
        .collect();

    let v_endian = match self_ty.endian {
        Endian::LittleEndian => quote!(::nom::number::Endianness::Little),
        Endian::BigEndian => quote!(::nom::number::Endianness::Big),
    };
    let parser_name = &self_ty.parser_name;
    let generics = &tc.generics[..];

    let id = &self_ty.ident;
    let q_body = if self_ty.is_var_len_str() {
        let s = &field_idents[1];
        quote!(#id(#s))
    } else if self_ty.is_newtype() {
        let s = &field_idents[0];
        quote!(#id(#s))
    } else {
        quote!(#id {
            #(#field_idents),*
        })
    };
    let q_parser_impl = quote!(
        let #p_endian = #v_endian;
        #(#parser)*
        Ok((#p_input, #q_body))
    );
    let q_parser_attr = quote!(
        #[cfg(feature = "nom")]
        #[allow(unused_parens)]
    );
    let gen_use = tc.gen_use();
    let q_result = quote!(::nom::IResult<#_input_ty, #id #gen_use>);
    if _root.is_some() || _parent.is_some() || !_external_field_generics.is_empty() {
        quote!(
            #q_parser_attr
            pub fn #parser_name<#input_lifetime #(#generics),*> (
                #(#parser_args),*
                #(#_external_field_generics)*
            ) -> impl FnMut(#_input_ty) -> #q_result {
                #_root
                #_parent
                move |#p_input: #_input_ty| {
                    #q_parser_impl
                }
            }
        )
    } else {
        quote!(
            #q_parser_attr
            pub fn #parser_name<#input_lifetime #(#generics),*> (#p_input: &'a [u8]) -> #q_result {
                #q_parser_impl
            }
        )
    }
}

fn codegen_type_ref_parse(
    type_ref_id: Option<&str>,
    nc: &NamingContext,
    ty: &TypeRef,
    size: Option<&str>,
    self_ty: &Type,
    resolved_ty: &ResolvedType,
    p_endian: &Ident,
) -> TokenStream {
    let in_parent = false;
    if let ResolvedType::Enum(e, w) = resolved_ty {
        let e = nc.get_enum(e).unwrap();
        let i_enum = &e.ident;
        let q_parser = primitive::wk_parser(w, size, p_endian);
        return quote!(::nom::combinator::map_res(#q_parser, #i_enum::try_from));
    }
    match ty {
        TypeRef::WellKnown(w) => primitive::wk_parser(w, size, p_endian),
        TypeRef::Named(n) => {
            let _named_ty = nc.resolve(n).unwrap();
            user_type(nc, _named_ty, self_ty.is_root, in_parent, p_endian)
        }
        TypeRef::Dynamic { switch_on, cases } => {
            match resolved_ty {
                ResolvedType::Dynamic(_, _) => {
                    let id = type_ref_id.unwrap();
                    let fg = self_ty.field_generics.get(id).expect(id);
                    if fg.external {
                        fg.parser.to_token_stream()
                    } else {
                        variant_parser_expr(nc, self_ty.is_root, fg, in_parent, p_endian)
                    }
                }
                ResolvedType::Magic => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::User(_) => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::Enum(_, _) => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::SInt { .. } => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::Float { .. } => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::Str { .. } => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::Bytes { .. } => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::UInt { width, .. } => {
                    let switch_expr = match switch_on {
                        AnyScalar::Null => todo!(),
                        AnyScalar::Bool(_) => todo!(),
                        AnyScalar::String(expr) => expr::codegen_expr_str(expr),
                        AnyScalar::UInt(_) => todo!(),
                    };
                    let ty = uint_ty(*width);
                    let mut q_cases = Vec::<TokenStream>::new();
                    for (k, v) in cases {
                        let case = match k {
                            AnyScalar::Bool(b) => quote!(#b),
                            AnyScalar::Null => todo!(),
                            AnyScalar::String(_) => todo!(),
                            AnyScalar::UInt(_) => todo!(),
                        };
                        let parser = match v {
                            TypeRef::WellKnown(w) => primitive::wk_parser(w, None, p_endian),
                            TypeRef::Named(_) => todo!(),
                            TypeRef::Dynamic { .. } => todo!(),
                        };
                        q_cases.push(
                            quote!(#case => Box::new(::nom::combinator::map(#parser, #ty::from))),
                        );
                    }
                    let exhaustive = true;
                    if !exhaustive {
                        q_cases.push(quote!( _ => Box::new(::nom::combinator::map(|_| todo!(), |x: u8| x as #ty))));
                    }

                    quote!({
                        let __parser: Box<dyn FnMut(&'a [u8]) -> ::nom::IResult<&'a[u8], #ty>> = match #switch_expr {
                            #(#q_cases,)*
                        };
                        __parser
                    })
                    /**/
                }
            }
        }
    }
}

fn codegen_attr_parse(
    field: &Field,
    self_ty: &Type,
    attr: &Attribute,
    p_input: &Ident,
    p_endian: &Ident,
    nc: &NamingContext,
) -> TokenStream {
    let f_ident = field.ident();
    let mut parser = if let Some(ty) = &attr.ty {
        codegen_type_ref_parse(
            attr.id.as_deref(),
            nc,
            ty,
            attr.size.as_deref(),
            self_ty,
            field.resolved_ty(),
            p_endian,
        )
    } else if let Some(contents) = &attr.contents {
        let tag = match contents {
            Contents::String(s) => quote!(#s),
            Contents::Bytes(b) => quote!(&[#(#b),*]),
        };
        // FIXME: different value?
        quote!(::nom::combinator::value((), ::nom::bytes::complete::tag(#tag)))
    } else if let Some(size_expr) = &attr.size {
        let size = expr::codegen_expr_str(size_expr);
        quote!(::nom::bytes::complete::take(#size))
    } else {
        todo!("{:?}", attr);
    };
    if let Some(_r) = &attr.repeat {
        parser = match _r {
            Repeat::Expr => {
                let expr = attr
                    .repeat_expr
                    .as_ref()
                    .expect("repeat == 'expr'")
                    .as_str();
                let expr = expr::codegen_expr_str(expr);
                quote!(::nom::multi::count(#parser, #expr as usize))
            }
            Repeat::Eos => todo!(),
            Repeat::Until => todo!(),
        }
    } else if let Some(_e) = &attr.if_expr {
        let expr = expr::codegen_expr_str(_e);
        parser = quote!(::nom::combinator::cond(#expr, #parser));
    }
    quote!(let (#p_input, #f_ident) = #parser(#p_input)?;)
}

fn variant_parser_expr(
    nc: &NamingContext,
    is_root_parser: bool,
    fg: &FieldGenerics,
    in_parent: bool,
    p_endian: &Ident,
) -> TokenStream {
    let match_on = expr::codegen_expr_str_with(&fg.switch_expr, in_parent);

    let de = quote!('a);
    let p_ty = fg.variant_type();
    let p_var_enum = &fg.var_enum;
    let case_other = &fg.var_enum_other;
    let p_input = format_ident!("_input");

    let p_cases: Vec<_> = fg
        .cases
        .iter()
        .map(|case| {
            let p_case_id = &case.ident;
            let case_parser = match &case.ty {
                ResolvedType::SInt { width, endian } => {
                    primitive::sint_parser(*width, *endian, p_endian)
                }
                ResolvedType::UInt { width, endian } => {
                    primitive::uint_parser(*width, *endian, p_endian)
                }
                ResolvedType::Float { width, endian } => {
                    primitive::float_parser(*width, *endian, p_endian)
                }
                ResolvedType::User(n) => {
                    let _named_ty = nc.resolve(n).unwrap();
                    user_type(nc, _named_ty, is_root_parser, in_parent, p_endian)
                }
                _ => todo!("{:?}", &case.ty),
            };
            let p_parser =
                quote!(Box::new(::nom::combinator::map(#case_parser, #p_var_enum::#p_case_id)));
            match &case.kind {
                CaseKind::Enum(e, var) => {
                    let p_enum = format_ident!("{}", e.to_upper_camel_case()); // FIXME: use naming context
                    let p_var = format_ident!("_{}", var.to_uppercase());
                    quote!(#p_enum::#p_var => #p_parser)
                }
                CaseKind::Bool(_) => todo!(),
                CaseKind::Number(_) => todo!(),
            }
        })
        .collect();

    quote!(
        // FIXME: ideally the match would be the outer expression and the parser the inner,
        // different closures generally have incompatible types.
        {
            let __parser: Box<dyn FnMut(&#de [u8]) -> nom::IResult<&#de [u8], #p_ty>> = match (#match_on) as u64 {
                #(#p_cases,)*
                _ => Box::new(|#p_input| {
                    Ok((#p_input, #p_var_enum::#case_other))
                })
            };
            __parser
        }
    )
}

pub(crate) fn serialize_with(attr: &Attribute) -> Option<TokenStream> {
    if let Some(TypeRef::WellKnown(WellKnownTypeRef::Str)) = &attr.ty {
        let encoding = attr.encoding.as_deref().unwrap();
        if encoding.eq_ignore_ascii_case("utf-16le") {
            Some(quote!(
                #[cfg_attr(feature = "serde", serde(serialize_with = "crate::_rt::serialize_utf16_le"))]
            ))
        } else if encoding.eq_ignore_ascii_case("utf-16be") {
            Some(quote!(
                #[cfg_attr(feature = "serde", serde(serialize_with = "crate::_rt::serialize_utf16_be"))]
            ))
        } else if encoding.eq_ignore_ascii_case("utf-8") {
            Some(quote!(
                #[cfg_attr(feature = "serde", serde(serialize_with = "crate::_rt::serialize_utf18"))]
            ))
        } else if encoding.eq_ignore_ascii_case("ascii") {
            Some(quote!(
                #[cfg_attr(feature = "serde", serde(serialize_with = "crate::_rt::serialize_ascii"))]
            ))
        } else {
            None
        }
    } else {
        None
    }
}
