use std::{
    collections::{BTreeMap, BTreeSet},
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use ctx::NamingContext;
use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, KsySchema, StringOrArray, TypeRef, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use r#type::{FieldGenerics, Type};

mod ctx;
mod r#type;

pub struct Module {
    pub id: Ident,
    pub import: TokenStream,
    pub out_path: PathBuf,
    pub types: BTreeMap<String, Type>,
}

fn quote_wk_typeref(wktr: WellKnownTypeRef) -> TokenStream {
    match wktr {
        WellKnownTypeRef::U1 => quote!(u8),
        WellKnownTypeRef::U2(_) => quote!(u16),
        WellKnownTypeRef::U4(_) => quote!(u32),
        WellKnownTypeRef::U8(_) => quote!(u64),
        WellKnownTypeRef::S1 => quote!(i8),
        WellKnownTypeRef::S2(_) => quote!(i16),
        WellKnownTypeRef::S4(_) => quote!(i32),
        WellKnownTypeRef::S8(_) => quote!(u64),
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
    discriminant: &str,
    field_generics: Option<&FieldGenerics>,
) -> TokenStream {
    match ty {
        TypeRef::WellKnown(wktr) => quote_wk_typeref(*wktr),
        TypeRef::Named(n) => {
            if let Some(ty) = nc.resolve(n) {
                let mut q_ty = ty.token_stream();
                if let Some(src) = &ty.source_mod {
                    q_ty = quote!(#src::#q_ty);
                }
                q_ty
            } else {
                quote!(())
            }
        }
        TypeRef::Dynamic {
            switch_on: _,
            cases,
        } => {
            if let Some(gen) = field_generics {
                let g = &gen.type_;
                let t = &gen.trait_;
                let v = &gen.var_enum;
                if gen.external {
                    tc.generics.push(quote!(#g: #t));
                    tc.generics_use.push(quote!(#g));
                }
                let trait_doc = format!("Marker trait for [`{enclosing_type}::{discriminant}`]");
                let var_enum_doc =
                    format!("Raw variants for [`{}::{}`]", enclosing_type, discriminant);
                let mut enum_cases: Vec<TokenStream> =
                    Vec::<TokenStream>::with_capacity(cases.len());
                let mut trait_impl_cases = Vec::<TokenStream>::with_capacity(cases.len());
                for (case_key, case_type) in cases.iter() {
                    let mut _enum_set = BTreeSet::<String>::new();
                    let name: String = match case_key {
                        AnyScalar::Null => todo!(),
                        AnyScalar::Bool(true) => "True".to_owned(),
                        AnyScalar::Bool(false) => "False".to_owned(),
                        AnyScalar::String(s) => if let Some((_enum, part)) = s.split_once("::") {
                            _enum_set.insert(_enum.to_owned());
                            part
                        } else {
                            s
                        }
                        .to_upper_camel_case(),
                        AnyScalar::UInt(i) => format!("N{}", i),
                    };
                    let n: Ident = format_ident!("{}", name);
                    let inner =
                        codegen_type_ref(case_type, nc, tc, &v.to_string(), &name, field_generics);
                    enum_cases.push(quote! {
                        #n(#inner),
                    });
                    let need_lifetime = nc.need_lifetime(case_type);
                    let l = match need_lifetime {
                        true => quote!(<'a>),
                        false => quote!(),
                    };

                    trait_impl_cases.push(quote!(
                        impl #l #t for #inner {}
                    ));
                }

                // variant enum generics
                let var_enum_gen = match gen.need_lifetime {
                    false => quote!(),
                    true => quote!(<'a>),
                };

                tc.traits.push(quote! {
                    #[doc = #trait_doc]
                    pub trait #t {
                        // TODO
                    }

                    #[doc = #var_enum_doc]
                    pub enum #v #var_enum_gen {
                        #(#enum_cases)*
                    }

                    // FIXME: remove
                    impl #t for () {}

                    impl #var_enum_gen #t for #v #var_enum_gen {}

                    #(#trait_impl_cases)*
                });
                if gen.external {
                    quote!(#g)
                } else {
                    quote!(#v #var_enum_gen)
                }
            } else {
                quote!(())
            }
        }
    }
}

impl Context<'_> {
    fn codegen_attr(
        &self,
        nc: &NamingContext,
        struct_name: &str,
        attr: &Attribute,
        self_ty: Option<&Type>,
        tc: &mut TyContext,
    ) -> io::Result<TokenStream> {
        let orig_attr_id = attr.id.as_ref().unwrap().as_str();
        let attr_id = match orig_attr_id {
            "type" => quote!(r#type),
            _ => format_ident!("{}", orig_attr_id).to_token_stream(),
        };
        let attr_doc = attr.doc.as_deref().unwrap_or("");
        let attr_doc_ref = attr.doc_ref.as_ref();
        let attr_doc_refs = attr_doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);

        let ty_doc = format!("Type: `{:?}`", attr.ty);
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
        let fg = self_ty.and_then(|ty| ty.field_generics.get(orig_attr_id));
        let ty = if let Some(ty) = &attr.ty {
            let ty = codegen_type_ref(ty, nc, tc, struct_name, orig_attr_id, fg);
            if let Some(_rep) = &attr.repeat {
                quote!(Vec<#ty>)
            } else if let Some(_expr) = &attr.if_expr {
                quote!(Option<#ty>)
            } else {
                ty
            }
        } else {
            quote!(())
        };

        Ok(quote!(
            #[doc = #attr_doc]
            #(#[doc = #attr_doc_refs])*
            #[doc = #ty_doc]
            #if_doc
            #repeat_doc
            pub #attr_id: #ty
        ))
    }

    fn codegen_struct(
        &self,
        nc: &NamingContext,
        name: &str,
        doc: Option<&str>,
        doc_ref: Option<&StringOrArray>,
        seq: &[Attribute],
    ) -> io::Result<TokenStream> {
        let self_ty = nc.resolve(name);
        let rust_struct_name = name.to_upper_camel_case();
        let id = format_ident!("{}", rust_struct_name);
        let doc = doc.unwrap_or("");
        let doc_refs = doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);
        let needs_lifetime = self_ty.map(|t| t.needs_lifetime).unwrap_or(true);

        let mut tc = TyContext::new();

        if needs_lifetime {
            tc.generics.push(quote!('a));
            tc.generics_use.push(quote!('a));
        }

        let mut attrs = vec![];

        for attr in seq {
            if attr.id.is_none() {
                continue;
            }
            attrs.push(self.codegen_attr(nc, &rust_struct_name, attr, self_ty, &mut tc)?);
        }

        let gen = match &tc.generics[..] {
            [] => None,
            generics => Some(quote!(<#(#generics),*>)),
        };

        let gen_use = match &tc.generics_use[..] {
            [] => None,
            generics_use => Some(quote!(<#(#generics_use),*>)),
        };

        let input_lifetime = match needs_lifetime {
            true => None,
            false => Some(quote!('a,)),
        };
        let file_parser_name = format_ident!("parse_file");
        let parser_name = self_ty.map(|t| &t.parser_name).unwrap_or(&file_parser_name);
        let generics = &tc.generics[..];
        let q_parser = quote!(
            pub fn #parser_name<#input_lifetime #(#generics),*> (_input: &'a [u8]) -> Result<(&'a [u8], #id #gen_use), ()> {
                todo!()
            }
        );

        let traits = &tc.traits[..];

        let q = quote! {
            #[doc = #doc]
            #(#[doc = #doc_refs])*
            #[derive(Debug, Clone, PartialEq)]
            pub struct #id #gen {
                #(#attrs),*
            }

            #q_parser

            #(#traits)*
        };
        Ok(q)
    }

    pub fn codegen(&self) -> Result<Module, io::Error> {
        let schema = &self.schema;
        let out_dir = &self.out_dir;
        let p = &self.parent;
        let sid = format_ident!("{}", &schema.meta.id.0);
        let import = quote!(#p::#sid);

        let out_file = format!("{}.rs", &schema.meta.id);
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
        for (key, spec) in &schema.types {
            let st = Type::new(key, spec);
            nc.add(key, st);
        }

        nc.process_dependencies();

        // Struct Codegen
        for (key, spec) in &schema.types {
            let doc = spec.doc.as_deref();
            let doc_ref = spec.doc_ref.as_ref();
            let st = self.codegen_struct(&nc, key, doc, doc_ref, &spec.seq)?;
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
            quote! {
                #[doc = ""]
                pub enum #id {
                    #(#values),*
                }
            }
        });

        let mod_doc = schema.doc.as_ref().map(|d| {
            quote!(
                #![doc = #d]
            )
        });

        let file_struct = match schema.seq.is_empty() {
            true => None,
            false => Some(
                self.codegen_struct(
                    &nc,
                    "file",
                    schema.doc.as_deref(),
                    schema.doc_ref.as_ref(),
                    &schema.seq,
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

        let writer = std::fs::File::create(&out_path)?;
        let mut writer = BufWriter::new(writer);
        write!(writer, "{}", file)?;

        Command::new("rustfmt").arg(&out_path).spawn().unwrap();

        Ok(Module {
            id: sid,
            out_path,
            types: nc.into_types(),
            import,
        })
    }
}
