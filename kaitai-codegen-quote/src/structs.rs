use kaitai_struct_types::{Attribute, IntTypeRef, StringOrArray, TypeRef, WellKnownTypeRef};
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::BTreeSet, io};

use crate::{
    ctx::NamingContext,
    doc, parser,
    r#type::{self, Field, FieldGenerics, ResolvedType, Type},
};

fn quote_wk_typeref(wktr: WellKnownTypeRef) -> TokenStream {
    match wktr {
        WellKnownTypeRef::Unsigned(IntTypeRef::Int1) => quote!(u8),
        WellKnownTypeRef::Unsigned(IntTypeRef::Int2(_)) => quote!(u16),
        WellKnownTypeRef::Unsigned(IntTypeRef::Int4(_)) => quote!(u32),
        WellKnownTypeRef::Unsigned(IntTypeRef::Int8(_)) => quote!(u64),
        WellKnownTypeRef::Signed(IntTypeRef::Int1) => quote!(i8),
        WellKnownTypeRef::Signed(IntTypeRef::Int2(_)) => quote!(i16),
        WellKnownTypeRef::Signed(IntTypeRef::Int4(_)) => quote!(i32),
        WellKnownTypeRef::Signed(IntTypeRef::Int8(_)) => quote!(u64),
        WellKnownTypeRef::F4(_) => quote!(f32),
        WellKnownTypeRef::F8(_) => quote!(f64),
        // Note: we always use u8, independent of encoding because alignment isn't guaranteed.
        WellKnownTypeRef::Str => quote!(&'a [u8]),
        WellKnownTypeRef::StrZ => quote!(&'a [u8]),
    }
}

fn codegen_named_ty(nc: &NamingContext, n: &str) -> TokenStream {
    let ty = nc.resolve(n).unwrap();
    let mut q_ty = ty.token_stream();
    if let Some(src) = &ty.source_mod {
        q_ty = quote!(#src::#q_ty);
    }
    q_ty
}

pub(crate) struct TyContext {
    generics: Vec<TokenStream>,
    generics_use: Vec<TokenStream>,
    traits: Vec<TokenStream>,
}

impl TyContext {
    fn new() -> Self {
        Self {
            generics: vec![],
            generics_use: vec![],
            traits: vec![],
        }
    }

    pub(crate) fn get_generics(&self) -> &[TokenStream] {
        &self.generics
    }

    pub(crate) fn gen_use(&self) -> Option<TokenStream> {
        match &self.generics_use[..] {
            [] => None,
            generics_use => Some(quote!(<#(#generics_use),*>)),
        }
    }

    fn gen(&self) -> Option<TokenStream> {
        match &self.generics[..] {
            [] => None,
            generics => Some(quote!(<#(#generics),*>)),
        }
    }
}

fn codegen_type_ref(
    ty: &TypeRef,
    nc: &NamingContext,
    tc: &mut TyContext,
    // Name of the type this is used in
    enclosing_type: &str,
    // Field name or enum variant
    discriminant: &Ident,
    field_generics: Option<&FieldGenerics>,
) -> TokenStream {
    match ty {
        TypeRef::WellKnown(wktr) => quote_wk_typeref(*wktr),
        TypeRef::Named(n) => codegen_named_ty(nc, n),
        TypeRef::Dynamic {
            switch_on: _,
            cases,
        } => {
            let gen = field_generics.unwrap();
            let g = &gen.type_;
            let t = &gen.trait_;
            if gen.external {
                tc.generics.push(quote!(#g: #t));
                tc.generics_use.push(quote!(#g));
            }
            let trait_doc = format!("Marker trait for [`{enclosing_type}::{discriminant}`]");
            let var_enum_doc = format!("Raw variants for [`{}::{}`]", enclosing_type, discriminant);
            let mut enum_cases: Vec<TokenStream> = Vec::<TokenStream>::with_capacity(cases.len());
            let mut trait_impl_cases = Vec::<TokenStream>::with_capacity(cases.len());

            let mut inner_set = BTreeSet::<String>::new();

            let enclosing_type = gen.var_enum.to_string();
            for (i, case_type) in cases.values().enumerate() {
                let n = gen.cases[i].ident();
                let inner = codegen_type_ref(case_type, nc, tc, &enclosing_type, n, field_generics);
                enum_cases.push(quote! {
                    #n(#inner),
                });
                let need_lifetime = nc.need_lifetime(case_type);
                let l = match need_lifetime {
                    true => quote!(<'a>),
                    false => quote!(),
                };

                let inner_str = inner.to_string();
                if !inner_set.contains(&inner_str) {
                    inner_set.insert(inner_str);
                    trait_impl_cases.push(quote!(
                        impl #l #t for #inner {}
                    ));
                }
            }

            // variant enum generics
            let var_enum_gen = gen.var_enum_generics();

            let v_use = gen.variant_type();
            let case_other = &gen.var_enum_other;
            tc.traits.push(quote! {
                #[doc = #trait_doc]
                pub trait #t {
                    // TODO
                }

                #[doc = #var_enum_doc]
                #[derive(Debug, Clone, PartialEq)]
                #[cfg_attr(feature = "serde", derive(::serde::Serialize))]
                #[cfg_attr(feature = "serde", serde(untagged))] // FIXME: make conditional?
                pub enum #v_use {
                    #(#enum_cases)*
                    #case_other
                }

                impl #var_enum_gen #t for #v_use {}
                impl #t for () {} // used for empty generics

                #(#trait_impl_cases)*
            });
            if gen.external {
                quote!(#g)
            } else {
                quote!(#v_use)
            }
        }
    }
}

fn codegen_attr_ty(
    nc: &NamingContext,
    attr: &Attribute,
    self_ty: &Type,
    field: &Field,
    tc: &mut TyContext,
) -> io::Result<TokenStream> {
    let attr_id = field.ident();
    let orig_attr_id = field.id();
    let fg = self_ty.field_generics.get(orig_attr_id);
    let ty = match field.resolved_ty() {
        ResolvedType::Dynamic(_, _) => {
            if let Some(ty) = &attr.ty {
                let enclosing_type = self_ty.rust_struct_name.as_str();
                codegen_type_ref(ty, nc, tc, enclosing_type, attr_id, fg)
            } else {
                quote!(())
            }
        }
        ResolvedType::Magic => quote!(()),
        ResolvedType::User(n) => codegen_named_ty(nc, n),
        ResolvedType::Enum(e, _) => nc.get_enum(e).unwrap().ident.to_token_stream(),
        ResolvedType::UInt { width, .. } => r#type::uint_ty(*width),
        ResolvedType::SInt { width, .. } => r#type::sint_ty(*width),
        ResolvedType::Float { width, .. } => r#type::float_ty(*width),
        ResolvedType::Str { .. } | ResolvedType::Bytes { .. } => quote!(&'a [u8]),
    };
    let ty = if let Some(_rep) = &attr.repeat {
        quote!(Vec<#ty>)
    } else if let Some(_expr) = &attr.if_expr {
        quote!(Option<#ty>)
    } else {
        ty
    };
    Ok(ty)
}

fn codegen_attr(
    nc: &NamingContext,
    attr: &Attribute,
    self_ty: &Type,
    field: &Field,
    tc: &mut TyContext,
) -> io::Result<TokenStream> {
    let attr_doc = doc::doc_attr(attr);
    let serialize_with = parser::serialize_with(attr);

    let ty = codegen_attr_ty(nc, attr, self_ty, field, tc)?;
    let attr_id = field.ident();
    Ok(quote!(
        #attr_doc
        #serialize_with
        pub #attr_id: #ty
    ))
}

fn codegen_struct_body(
    seq: &[Attribute],
    self_ty: &Type,
    nc: &NamingContext,
    tc: &mut TyContext,
) -> Result<TokenStream, io::Error> {
    if self_ty.is_var_len_str() {
        return Ok(quote!((pub &'a [u8]);));
    } else if self_ty.is_newtype() {
        let attr = &seq[0];
        let field = &self_ty.fields[0];
        let ty = codegen_attr_ty(nc, attr, self_ty, field, tc).unwrap();
        return Ok(quote!((pub #ty);));
    }

    let mut attrs = vec![];
    for (i, attr) in seq.iter().enumerate() {
        if attr.id.is_none() {
            continue;
        }
        let field = &self_ty.fields[i];
        let attr = codegen_attr(nc, attr, self_ty, field, tc)?;
        attrs.push(attr);
    }
    Ok(quote!({
        #(#attrs),*
    }))
}

pub(super) fn codegen_struct(
    nc: &NamingContext,
    doc: Option<&str>,
    doc_ref: Option<&StringOrArray>,
    seq: &[Attribute],
    self_ty: &Type,
) -> io::Result<TokenStream> {
    let q_doc = doc::doc_struct(self_ty, nc, doc, doc_ref);
    let needs_lifetime = self_ty.needs_lifetime;

    let mut tc = TyContext::new();

    if needs_lifetime {
        tc.generics.push(quote!('a));
        tc.generics_use.push(quote!('a));
    }

    let q_body = codegen_struct_body(seq, self_ty, nc, &mut tc)?;
    let q_parser = parser::codegen_parser_fn(self_ty, seq, &tc, nc);

    let gen = tc.gen();
    let traits = &tc.traits[..];
    let id = &self_ty.ident;

    let string_from_impl = self_ty.is_var_len_str().then(|| {
        let encoding = seq[1].encoding.as_deref().unwrap().to_ascii_lowercase();
        let input = quote!(s.0);
        let q_impl = match encoding.as_str() {
            "utf-16le" => quote!(char::decode_utf16(#input.chunks(2).map(|s| u16::from_le_bytes([s[0], s[1]])))
            .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect::<String>()),
            "utf-16be" => quote!(char::decode_utf16(#input.chunks(2).map(|s| u16::from_be_bytes([s[0], s[1]])))
            .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect::<String>()),
            "ascii" | "utf-8" => quote!(String::from_utf8_lossy(#input).into_owned()),
            _ => todo!()
        };
        quote!(
            impl #gen From<#id #gen> for String {
                fn from(s: #id #gen) -> String {
                    #q_impl
                }
            }
        )
    });
    let serde_into = self_ty
        .is_var_len_str()
        .then(|| quote!(#[cfg_attr(feature = "serde", serde(into = "String"))]));

    let q = quote! {
        #q_doc
        #[cfg_attr(feature = "serde", derive(::serde::Serialize))]
        #serde_into
        #[derive(Debug, Clone, PartialEq)]
        pub struct #id #gen #q_body

        #q_parser
        #string_from_impl

        #(#traits)*
    };
    Ok(q)
}