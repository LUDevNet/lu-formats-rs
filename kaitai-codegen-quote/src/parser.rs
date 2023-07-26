//! # Parser Code Generation
//!
//! This module is used to generate code for a parser based on [nom v7.x](https://docs.rs/nom/7)

use heck::ToUpperCamelCase;
use kaitai_expr::{parse_expr, Expr, Op};
use kaitai_struct_types::{
    AnyScalar, Attribute, Contents, Endian, EndianSpec, IntTypeRef, Repeat, TypeRef,
    WellKnownTypeRef,
};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::{
    ctx::NamingContext,
    r#type::{ident_of, CaseKind, Field, FieldGenerics, ResolvedType, Type},
};

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
fn user_type(
    nc: &NamingContext,
    named_ty: &Type,
    is_root_parser: bool,
    in_parent: bool,
    p_endian: &Ident,
) -> TokenStream {
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
    for obligation in named_ty.parent_obligations.fields() {
        match obligation.as_str() {
            "_parent" => {
                for obligation in named_ty
                    .parent_obligations
                    .get(obligation)
                    .unwrap()
                    .fields()
                {
                    match obligation.as_str() {
                        "_parent" => todo!(),
                        _ => {
                            let i = ident_of(obligation);
                            values.push(if in_parent {
                                i.into_token_stream()
                            } else {
                                quote!(_parent.#i)
                            });
                        }
                    }
                }
            }
            _ => {
                values.push(ident_of(obligation).into_token_stream());
            }
        }
    }
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

struct ExprCodegen {
    in_parent: bool,
}

impl ExprCodegen {
    fn new() -> Self {
        Self { in_parent: false }
    }

    fn codegen_expr(&self, _expr: &Expr) -> TokenStream {
        match _expr {
            Expr::Path(_root, _components) => {
                let i_enum = format_ident!("{}", _root.to_upper_camel_case());
                let i_var = format_ident!("{}", _components[0].to_upper_camel_case());
                quote!((#i_enum::#i_var))
            }
            Expr::Input(i, fields) => {
                let mut field_iter = fields.iter();
                let mut ts = ident_of(match *i {
                    "_parent" if self.in_parent => field_iter.next().unwrap(),
                    _ => i,
                })
                .into_token_stream();
                for field in field_iter {
                    let f = ident_of(field);
                    ts = quote!(#ts.#f);
                }
                ts
            }
            Expr::If(args) => {
                let cond = self.codegen_expr(&args.0);
                let then_case = self.codegen_expr(&args.1);
                let else_case = self.codegen_expr(&args.2);
                quote!(match #cond { true => #then_case, false => #else_case })
            }
            Expr::Number(u) => Literal::u64_unsuffixed(*u).into_token_stream(),
            Expr::BinOp { op, args } => {
                let lhs = self.codegen_expr(&args.0);
                let rhs = self.codegen_expr(&args.1);
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
                    Op::NEq => quote!((#lhs != #rhs)),
                    Op::And => quote!((#lhs && #rhs)),
                    Op::Or => quote!((#lhs || #rhs)),
                    Op::Path | Op::LParen | Op::RParen | Op::TernaryTrue | Op::TernaryFalse => {
                        quote!(0xBEEF)
                    }
                }
            }
        }
    }
}

fn codegen_expr_str(expr: &str) -> TokenStream {
    ExprCodegen::new().codegen_expr(&parse_expr(expr).expect(expr))
}

fn codegen_expr_str_with(expr: &str, in_parent: bool) -> TokenStream {
    ExprCodegen { in_parent }.codegen_expr(&parse_expr(expr).expect(expr))
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

    let root_fields: Vec<_> = self_ty
        .root_obligations
        .fields()
        .map(|f| {
            let f_name = format_ident!("{}", f);
            quote!(#f_name: u32,)
        })
        .collect();
    let root_values: Vec<_> = self_ty
        .root_obligations
        .fields()
        .map(|f| format_ident!("{}", f))
        .collect();

    let mut parser_args = Vec::<TokenStream>::new();
    let _root = if !self_ty.root_obligations.is_empty() {
        Some(quote!(
            struct _Root {
                #(#root_fields)*
            }
            let _root = _Root {
                #(#root_values)*
            };
        ))
    } else {
        None
    };
    let _parent = (!self_ty.parent_obligations.is_empty()).then(|| {
        let mut parent_values = Vec::<TokenStream>::new();
        let mut parent_fields = Vec::<TokenStream>::new();
        let mut prev = Vec::<TokenStream>::new();
        for field in self_ty.parent_obligations.fields() {
            let field_ident = format_ident!("{}", field);
            let (field_ty, field_val) = if field == "_parent" {
                let pp_ident = format_ident!("_ParentParent");
                let mut pp_values = Vec::<TokenStream>::new();
                let mut pp_fields = Vec::<TokenStream>::new();
                let obligations = self_ty.parent_obligations.get("_parent").unwrap();
                for fields in obligations.fields() {
                    let pp_field_ty = quote!(u32);
                    let pp_field_id = format_ident!("{}", fields);
                    let pp_field = quote!(#pp_field_id: #pp_field_ty);
                    parser_args.push(pp_field.clone());
                    pp_fields.push(pp_field);
                    pp_values.push(pp_field_id.to_token_stream());
                }
                prev.push(quote!(struct #pp_ident {
                    #(#pp_fields),*
                }));
                (
                    quote!(#pp_ident),
                    quote!(#pp_ident {
                        #(#pp_values),*
                    }),
                )
            } else {
                (quote!(u32), field_ident.to_token_stream())
            };
            let field_decl = quote!(#field_ident: #field_ty);
            if field != "_parent" {
                parser_args.push(field_decl.clone());
            }
            parent_values.push(quote!(#field_ident: #field_val));
            parent_fields.push(field_decl);
        }
        assert_eq!(parent_fields.len(), parent_values.len());
        quote!(
            #(#prev)*
            struct _Parent {
                #(#parent_fields),*
            }
            let _parent = _Parent {
                #(#parent_values),*
            };
        )
    });

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
                #(#root_fields)*
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
        let q_parser = wk_parser(w, size, p_endian);
        return quote!(::nom::combinator::map_res(#q_parser, #i_enum::try_from));
    }
    match ty {
        TypeRef::WellKnown(w) => wk_parser(w, size, p_endian),
        TypeRef::Named(n) => {
            let _named_ty = nc.resolve(n).unwrap();
            user_type(nc, _named_ty, self_ty.is_root, in_parent, p_endian)
        }
        TypeRef::Dynamic { switch_on, cases } => {
            match resolved_ty {
                ResolvedType::Auto => {
                    let id = type_ref_id.unwrap();
                    let fg = self_ty.field_generics.get(id).expect(id);
                    if fg.external {
                        fg.parser.to_token_stream()
                    } else {
                        variant_parser_expr(nc, self_ty.is_root, fg, in_parent, p_endian)
                    }
                }
                ResolvedType::Enum(_, _) => panic!("Not a TypeRef::Dynamic"),
                ResolvedType::UInt { width } => {
                    let switch_expr = match switch_on {
                        AnyScalar::Null => todo!(),
                        AnyScalar::Bool(_) => todo!(),
                        AnyScalar::String(expr) => codegen_expr_str(expr),
                        AnyScalar::UInt(_) => todo!(),
                    };
                    let ty = match width {
                        // FIXME uses cases
                        1 => quote!(u8),
                        2 => quote!(u16),
                        4 => quote!(u32),
                        8 => quote!(u64),
                        _ => todo!(),
                    };
                    let mut q_cases = Vec::<TokenStream>::new();
                    for (k, v) in cases {
                        let case = match k {
                            AnyScalar::Bool(b) => quote!(#b),
                            AnyScalar::Null => todo!(),
                            AnyScalar::String(_) => todo!(),
                            AnyScalar::UInt(_) => todo!(),
                        };
                        let parser = match v {
                            TypeRef::WellKnown(w) => wk_parser(w, None, p_endian),
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
    } else {
        quote!(::nom::number::complete::le_u32)
    };
    if let Some(_r) = &attr.repeat {
        parser = match _r {
            Repeat::Expr => {
                let expr = attr
                    .repeat_expr
                    .as_ref()
                    .expect("repeat == 'expr'")
                    .as_str();
                let expr = codegen_expr_str(expr);
                quote!(::nom::multi::count(#parser, #expr as usize))
            }
            Repeat::Eos => todo!(),
            Repeat::Until => todo!(),
        }
    } else if let Some(_e) = &attr.if_expr {
        let expr = codegen_expr_str(_e);
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
    let match_on = codegen_expr_str_with(&fg.switch_expr, in_parent);

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
                TypeRef::WellKnown(w) => {
                    wk_parser(w, None, p_endian) // FIXME: size
                }
                TypeRef::Named(n) => {
                    let _named_ty = nc.resolve(n).unwrap();
                    user_type(nc, _named_ty, is_root_parser, in_parent, p_endian)
                }
                TypeRef::Dynamic { .. } => todo!(),
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
