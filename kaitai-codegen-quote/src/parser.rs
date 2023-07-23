//! # Parser Code Generation
//!
//! This module is used to generate code for a parser based on [nom v7.x](https://docs.rs/nom/7)

use kaitai_expr::{parse_expr, Expr, Op};
use kaitai_struct_types::{
    Attribute, Contents, Endian, EndianSpec, IntTypeRef, Repeat, TypeRef, WellKnownTypeRef,
};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::{
    ctx::NamingContext,
    r#type::{ident_of, Field, FieldGenerics, ResolvedType, Type},
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
            let variants_parser = variant_parser_expr(generics, true);
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
                    Op::And => quote!((#lhs && #rhs)),
                    Op::Or => quote!((#lhs || #rhs)),
                    Op::LParen | Op::RParen | Op::TernaryTrue | Op::TernaryFalse => quote!(0xBEEF),
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
            quote!(#p: impl Fn(&'a [u8]) -> ::nom::IResult<#_input_ty, #t>,)
        })
        .collect();

    let v_endian = match self_ty.endian {
        Endian::LittleEndian => quote!(::nom::number::Endianness::Little),
        Endian::BigEndian => quote!(::nom::number::Endianness::Big),
    };
    let parser_name = &self_ty.parser_name;
    let generics = &tc.generics[..];

    let id = &self_ty.ident;
    let q_parser_impl = quote!(
        let #p_endian = #v_endian;
        #(#parser)*
        Ok((#p_input, #id {
            #(#field_idents),*
        }))
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
            ) -> impl Fn(#_input_ty) -> #q_result {
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
        match ty {
            TypeRef::WellKnown(w) => wk_parser(w, attr.size.as_deref(), p_endian),
            TypeRef::Named(n) => {
                let _named_ty = nc.resolve(n).unwrap();
                user_type(_named_ty, self_ty.is_root)
            }
            TypeRef::Dynamic {
                switch_on: _,
                cases: _,
            } => match field.resolved_ty() {
                ResolvedType::Auto => {
                    let id = attr.id.as_deref().unwrap();
                    let fg = self_ty.field_generics.get(id).expect(id);
                    if fg.external {
                        fg.parser.to_token_stream()
                    } else {
                        variant_parser_expr(fg, false)
                    }
                }
                ResolvedType::UInt { width } => match width {
                    1 => quote!(::nom::number::complete::le_u8),
                    2 => quote!(::nom::number::complete::le_u16),
                    4 => quote!(::nom::number::complete::le_u32),
                    8 => quote!(::nom::number::complete::le_u64),
                    _ => todo!(),
                },
            },
        }
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

pub(super) fn variant_parser_expr(fg: &FieldGenerics, in_parent: bool) -> TokenStream {
    let match_on = codegen_expr_str_with(&fg.switch_expr, in_parent);

    let de = quote!('a);
    let p_ty = fg.variant_type();
    let p_enum = &fg.var_enum;
    let case_other = &fg.var_enum_other;
    let p_input = format_ident!("_input");
    quote!(
        // FIXME: ideally the match would be the outer expression and the parser the inner,
        // different closures generally have incompatible types.
        {
            let __parser: &dyn Fn(&#de [u8]) -> nom::IResult<&#de [u8], #p_ty> = &match #match_on {
                _ => |#p_input| {
                    Ok((#p_input, #p_enum::#case_other))
                }
            };
            __parser
        }
    )
}
