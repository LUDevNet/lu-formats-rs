use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as FmtWrite,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use ctx::NamingContext;
use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    Attribute, IntTypeRef, KsySchema, StringOrArray, TypeRef, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use r#type::{Field, FieldGenerics, ResolvedType, Type};

mod ctx;
mod parser;
mod rt;
mod r#type;

pub use rt::codegen_rt;

pub struct Module {
    pub id: Ident,
    pub import: TokenStream,
    pub out_path: PathBuf,
    pub types: BTreeMap<String, Type>,
}

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

struct TyContext {
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

    fn gen_use(&self) -> Option<TokenStream> {
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

/// Context for transpiling a module / schema file
pub struct Context<'a> {
    /// Identifier for the parent module, e.g. `quote!(crate)`
    pub parent: TokenStream,
    pub available_imports: &'a BTreeMap<String, Module>,
    pub schema: &'a KsySchema,
    pub out_dir: &'a Path,
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
        TypeRef::Named(n) => {
            let ty = nc.resolve(n).unwrap();
            let mut q_ty = ty.token_stream();
            if let Some(src) = &ty.source_mod {
                q_ty = quote!(#src::#q_ty);
            }
            q_ty
        }
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

fn doc_type_seq<S: AsRef<str>, I: IntoIterator<Item = S>>(
    nc: &NamingContext,
    key: &str,
    iterable: I,
) -> TokenStream {
    let mut text = format!("## {}\n", key);
    for name in iterable.into_iter() {
        let named = nc.resolve(name.as_ref()).unwrap();
        if let Some(m) = &named.source_mod {
            writeln!(text, "- [`{}::{}`]", m, named.ident)
        } else {
            writeln!(text, "- [`{}`]", named.ident)
        }
        .unwrap();
    }
    quote!(#[doc = #text])
}

fn doc_type_list(nc: &NamingContext, key: &str, list: &[String]) -> Option<TokenStream> {
    (!list.is_empty()).then(|| doc_type_seq(nc, key, list))
}

fn doc_type_set(nc: &NamingContext, key: &str, set: &BTreeSet<String>) -> Option<TokenStream> {
    (!set.is_empty()).then(|| doc_type_seq(nc, key, set))
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
        ResolvedType::Auto => {
            if let Some(ty) = &attr.ty {
                let enclosing_type = self_ty.rust_struct_name.as_str();
                codegen_type_ref(ty, nc, tc, enclosing_type, attr_id, fg)
            } else {
                quote!(())
            }
        }
        ResolvedType::UInt { width } => match *width {
            1 => quote!(u8),
            2 => quote!(u16),
            4 => quote!(u32),
            8 => quote!(u64),
            _ => panic!("width not supported"),
        },
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
    let attr_doc = attr.doc.as_deref().map(|d| quote!(#[doc = #d]));
    let attr_doc_ref = attr.doc_ref.as_ref();
    let attr_doc_refs = attr_doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);

    let if_doc = attr
        .if_expr
        .as_ref()
        .map(|i| format!("If: `{}`", i))
        .map(|s| {
            quote!(
                #[doc = #s]
            )
        });
    let repeat_doc = attr.repeat.as_ref().map(|r| {
        let rep_doc = format!("Repeat: `{:?}`", r);
        quote!(#[doc = #rep_doc])
    });

    let ty = codegen_attr_ty(nc, attr, self_ty, field, tc)?;

    let serialize_with = parser::serialize_with(attr);

    let attr_id = field.ident();
    Ok(quote!(
        #attr_doc
        #(#[doc = #attr_doc_refs])*
        #if_doc
        #repeat_doc
        #serialize_with
        pub #attr_id: #ty
    ))
}

impl Context<'_> {
    fn codegen_struct(
        &self,
        nc: &NamingContext,
        doc: Option<&str>,
        doc_ref: Option<&StringOrArray>,
        seq: &[Attribute],
        self_ty: &Type,
    ) -> io::Result<TokenStream> {
        let doc = doc.map(|d| quote!(#[doc = #d]));
        let doc_root_obligations = self_ty.root_obligations.doc("_root");
        let doc_parent_obligations = self_ty.parent_obligations.doc("_parent");
        let doc_parents = doc_type_list(nc, "parents", &self_ty.parents);
        let doc_maybe_parents = doc_type_list(nc, "maybe_parents", &self_ty.maybe_parents);
        let doc_depends_on = doc_type_set(nc, "depends_on", &self_ty.depends_on);
        let doc_may_depend_on = doc_type_set(nc, "may_depend_on", &self_ty.may_depend_on);
        let doc_refs = doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);
        let needs_lifetime = self_ty.needs_lifetime;

        let mut tc = TyContext::new();

        if needs_lifetime {
            tc.generics.push(quote!('a));
            tc.generics_use.push(quote!('a));
        }

        let q_body = {
            let mut attrs = vec![];
            for (i, attr) in seq.iter().enumerate() {
                if attr.id.is_none() {
                    continue;
                }
                let field = &self_ty.fields[i];
                let attr = codegen_attr(nc, attr, self_ty, field, &mut tc)?;
                attrs.push(attr);
            }
            quote!({
                #(#attrs),*
            })
        };

        let gen = tc.gen();
        let traits = &tc.traits[..];
        let q_parser = parser::codegen_parser_fn(self_ty, seq, &tc, nc);
        let id = &self_ty.ident;

        let q = quote! {
            #doc
            #(#[doc = #doc_refs])*
            #doc_root_obligations
            #doc_parent_obligations
            #doc_parents
            #doc_maybe_parents
            #doc_depends_on
            #doc_may_depend_on
            #[cfg_attr(feature = "serde", derive(::serde::Serialize))]
            #[derive(Debug, Clone, PartialEq)]
            pub struct #id #gen #q_body

            #q_parser

            #(#traits)*
        };
        Ok(q)
    }

    pub fn codegen(&self, file_id: Option<&str>) -> Result<Module, io::Error> {
        let schema = &self.schema;
        let out_dir = &self.out_dir;
        let p = &self.parent;
        let id = schema.meta.id.0.as_str();
        let sid = format_ident!("{}", id);
        let import = quote!(#p::#sid);

        let out_file = format!("{}.rs", file_id.unwrap_or(&schema.meta.id.0));
        let out_path = out_dir.join(out_file);

        let mut nc = NamingContext::new();
        let mut structs = vec![];

        let mut q_imports = vec![];
        for imp in &schema.meta.imports {
            let module = self.available_imports.get(imp).expect("Missing import");
            let mod_id = &module.import;
            q_imports.push(quote!(use #mod_id;));
            nc.import_module(module);
        }

        // First stage analysis
        let _root_ty = Type::new_root(schema);
        let root_endian = _root_ty.endian;
        nc.set_root(id, _root_ty);
        for (key, spec) in &schema.types {
            let st = Type::new(key, spec, root_endian);
            nc.add(key, st);
        }

        nc.process_dependencies();

        // Struct Codegen
        for (key, spec) in &schema.types {
            let doc = spec.doc.as_deref();
            let doc_ref = spec.doc_ref.as_ref();
            let struct_ty = nc.resolve(key).unwrap();
            let st = self.codegen_struct(&nc, doc, doc_ref, &spec.seq, struct_ty)?;
            structs.push(st);
        }

        let enums = schema.enums.iter().map(|(name, spec)| {
            let id = format_ident!("{}", name.to_upper_camel_case());
            let values = spec.0.iter().map(|(key, value)| {
                let id = &value.id;
                let id = format_ident!("{}", id.0.to_upper_camel_case());
                let val: isize = key.parse().unwrap();
                let doc = format!("Value: `{}`", key);
                quote! {
                    #[doc = #doc]
                    #id = #val
                }
            });
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

            quote! {
                #[doc = ""]
                pub enum #id {
                    #(#values),*
                }

                impl #id {
                    #(#consts)*
                }
            }
        });

        let mod_doc = schema.doc.as_ref().map(|d| {
            quote!(
                #![doc = #d]
            )
        });

        let root_ty = nc.get_root().unwrap();
        let file_struct = match schema.seq.is_empty() {
            true => None,
            false => Some(
                self.codegen_struct(
                    &nc,
                    schema.doc.as_deref(),
                    schema.doc_ref.as_ref(),
                    &schema.seq,
                    root_ty,
                )
                .unwrap(),
            ),
        };

        let file = quote! {
            #mod_doc

            #(#q_imports)*
            #file_struct
            #(#structs)*
            #(#enums)*
        };

        write_file(&out_path, file)?;

        Ok(Module {
            id: sid,
            out_path,
            types: nc.into_types(),
            import,
        })
    }
}

fn write_file(out_path: &Path, file: TokenStream) -> Result<(), io::Error> {
    let writer = std::fs::File::create(out_path)?;
    let mut writer = BufWriter::new(writer);
    write!(writer, "{}", file)?;
    Command::new("rustfmt").arg(out_path).spawn().unwrap();
    Ok(())
}
