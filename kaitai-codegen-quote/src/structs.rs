use kaitai_struct_types::{Attribute, StringOrArray};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::BTreeSet;

use crate::{
    ctx::NamingContext,
    doc, parser,
    r#type::{
        self, Case, Field, FieldGenerics, ResolvedType, ResolvedTypeCount, ResolvedTypeKind, Type,
    },
};

#[derive(Debug)]
pub enum Error {
    RequiredFieldGenerics,
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

fn codegen_cases(
    cases: &[Case],
    nc: &NamingContext,
    tc: &mut TyContext,
    // Name of the type this is used in
    enclosing_type: &str,
    // Field name or enum variant
    discriminant: &Ident,
    field_generics: Option<&FieldGenerics>,
) -> Result<TokenStream, Error> {
    let gen = field_generics.ok_or(Error::RequiredFieldGenerics)?;
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
    for (i, case_type) in cases.iter().enumerate() {
        let n = gen.cases[i].ident();
        let inner = codegen_resolved_ty(nc, &case_type.ty, &enclosing_type, tc, n, field_generics)?;
        enum_cases.push(quote! {
            #n(#inner),
        });
        let need_lifetime = nc.need_lifetime(&case_type.ty);
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
        #[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
        #[cfg_attr(feature = "serde", serde(untagged))] // FIXME: make conditional?
        pub enum #v_use {
            #(#enum_cases)*
            #case_other
        }

        impl #var_enum_gen #t for #v_use {}
        impl #t for () {} // used for empty generics

        #(#trait_impl_cases)*
    });
    Ok(if gen.external {
        quote!(#g)
    } else {
        quote!(#v_use)
    })
}

fn codegen_attr_ty(
    nc: &NamingContext,
    self_ty: &Type,
    field: &Field,
    tc: &mut TyContext,
) -> Result<TokenStream, Error> {
    let attr_id = field.ident();
    let orig_attr_id = field.id();
    let fg = self_ty.field_generics.get(orig_attr_id);
    let resolved = field.resolved_ty();
    let enclosing_type = self_ty.rust_struct_name.as_str();
    codegen_resolved_ty(nc, resolved, enclosing_type, tc, attr_id, fg)
}

fn codegen_resolved_ty(
    nc: &NamingContext,
    ty: &ResolvedType,
    enclosing_type: &str,
    tc: &mut TyContext,
    attr_id: &Ident,
    fg: Option<&FieldGenerics>,
) -> Result<TokenStream, Error> {
    let inner = codegen_resolved_ty_kind(&ty.kind, enclosing_type, nc, tc, attr_id, fg)?;
    Ok(match &ty.count {
        ResolvedTypeCount::Required => inner,
        ResolvedTypeCount::Optional => quote!(Option<#inner>),
        ResolvedTypeCount::Variable => quote!(Vec<#inner>),
        ResolvedTypeCount::Fixed(i) => quote!([#inner; #i]),
    })
}

fn codegen_resolved_ty_kind(
    ty_kind: &ResolvedTypeKind,
    enclosing_type: &str,
    nc: &NamingContext,
    tc: &mut TyContext,
    attr_id: &Ident,
    fg: Option<&FieldGenerics>,
) -> Result<TokenStream, Error> {
    match ty_kind {
        ResolvedTypeKind::Dynamic(_switch_on, cases) => {
            codegen_cases(cases, nc, tc, enclosing_type, attr_id, fg)
        }
        ResolvedTypeKind::Magic => Ok(quote!(())),
        ResolvedTypeKind::User(n) => Ok(codegen_named_ty(nc, n)),
        ResolvedTypeKind::Enum(e, _) => Ok(nc.get_enum(e).unwrap().ident.to_token_stream()),
        ResolvedTypeKind::UInt { width, .. } => Ok(r#type::uint_ty(*width)),
        ResolvedTypeKind::SInt { width, .. } => Ok(r#type::sint_ty(*width)),
        ResolvedTypeKind::Float { width, .. } => Ok(r#type::float_ty(*width)),
        ResolvedTypeKind::Str { .. } | ResolvedTypeKind::Bytes { .. } => {
            Ok(quote!(::std::borrow::Cow<'a, [u8]>))
        }
    }
}

fn codegen_attr(
    nc: &NamingContext,
    attr: &Attribute,
    self_ty: &Type,
    field: &Field,
    tc: &mut TyContext,
) -> Result<TokenStream, Error> {
    let attr_doc = doc::doc_attr(attr);
    let serialize_with = parser::serialize_with(attr);

    let ty = codegen_attr_ty(nc, self_ty, field, tc)?;
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
) -> Result<TokenStream, Error> {
    if self_ty.is_var_len_str() {
        return Ok(quote!((pub ::std::borrow::Cow<'a, [u8]>);));
    } else if self_ty.is_newtype() {
        let field = &self_ty.fields[0];
        let ty = codegen_attr_ty(nc, self_ty, field, tc).unwrap();
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

fn codegen_instance_fn(
    _nc: &NamingContext,
    _self_ty: &Type,
    instance: &Field,
    _tc: &mut TyContext,
) -> Result<TokenStream, Error> {
    let id = format_ident!("parse_{}", instance.ident());
    let (ty, parser) = match codegen_attr_ty(_nc, _self_ty, instance, _tc) {
        Ok(ty) => (ty, quote!(todo!())),
        Err(_) => (quote!(()), quote!(todo!())),
    };
    Ok(quote!(
        pub fn #id<'a>(&self, _input: &'a [u8]) -> ::nom::IResult<&'a [u8], #ty> {
            #parser
        }
    ))
}

pub(super) fn codegen_struct(
    nc: &NamingContext,
    doc: Option<&str>,
    doc_ref: Option<&StringOrArray>,
    seq: &[Attribute],
    self_ty: &Type,
) -> Result<TokenStream, Error> {
    let q_doc = doc::doc_struct(self_ty, nc, doc, doc_ref);
    let needs_lifetime = self_ty.needs_lifetime;

    let mut tc = TyContext::new();
    let mut q_impls: Vec<TokenStream> = Vec::new();

    if needs_lifetime {
        tc.generics.push(quote!('a));
        tc.generics_use.push(quote!('a));
    }

    let q_body = codegen_struct_body(seq, self_ty, nc, &mut tc)?;
    let q_parser = parser::codegen_parser_fn(self_ty, seq, &tc, nc);

    for instance in &self_ty.instances {
        let _fn = codegen_instance_fn(nc, self_ty, instance, &mut tc)?;
        q_impls.push(_fn)
    }

    let gen = tc.gen();
    let gen_use = tc.gen_use();
    let traits = &tc.traits[..];
    let id = &self_ty.ident;

    let string_from_impl = self_ty.is_var_len_str().then(|| {
        let encoding = seq[1].encoding.as_deref().unwrap().to_ascii_lowercase();
        let input = quote!(::std::convert::AsRef::as_ref(&s.0));
        let q_decode_impl = match encoding.as_str() {
            "utf-16le" => quote!(char::decode_utf16(#input.chunks(2).map(|s| u16::from_le_bytes([s[0], s[1]])))
            .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect::<String>()),
            "utf-16be" => quote!(char::decode_utf16(#input.chunks(2).map(|s| u16::from_be_bytes([s[0], s[1]])))
            .map(|c| c.unwrap_or(char::REPLACEMENT_CHARACTER))
            .collect::<String>()),
            "ascii" | "utf-8" => quote!(String::from_utf8_lossy(#input).into_owned()),
            _ => todo!()
        };
        let q_encode_impl = match encoding.as_str() {
            "utf-16le" => quote!(s.encode_utf16().flat_map(|v| v.to_le_bytes()).collect::<Vec<u8>>()),
            "utf-16be" => quote!(s.encode_utf16().flat_map(|v| v.to_be_bytes()).collect::<Vec<u8>>()),
            "ascii" | "utf-8" => quote!(s.into_bytes()),
            _ => todo!()
        };
        quote!(
            impl #gen From<#id #gen> for String {
                fn from(s: #id #gen) -> String {
                    #q_decode_impl
                }
            }

            impl #gen From<String> for #id #gen {
                fn from(s: String) -> #id #gen {
                    Self(::std::borrow::Cow::Owned(#q_encode_impl))
                }
            }
        )
    });
    let serde_into = self_ty
        .is_var_len_str()
        .then(|| quote!(#[cfg_attr(feature = "serde", serde(into = "String", from = "String"))]));

    let q = quote! {
        #q_doc
        #[cfg_attr(feature = "serde", derive(::serde::Serialize, ::serde::Deserialize))]
        #serde_into
        #[derive(Debug, Clone, PartialEq)]
        pub struct #id #gen #q_body

        impl #gen #id #gen_use {
            #(#q_impls)*
        }

        #q_parser
        #string_from_impl

        #(#traits)*
    };
    Ok(q)
}
