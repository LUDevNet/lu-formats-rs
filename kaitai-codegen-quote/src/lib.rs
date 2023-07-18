use std::{
    collections::{BTreeMap, BTreeSet},
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use ctx::NamingContext;
use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, Contents, EndianSpec, KsySchema, StringOrArray, TypeRef, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use r#type::{Field, FieldGenerics, Type};

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
                    #[derive(Debug, Clone, PartialEq)]
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

fn codegen_attr_parse(
    field: &Field,
    attr: &Attribute,
    p_input: &Ident,
    p_endian: &Ident,
) -> TokenStream {
    let f_ident = field.ident();
    let mut parser = if let Some(ty) = &attr.ty {
        //let ty_str = format!("{:?}", ty).replace('{', "{{").replace('}', "}}");
        match ty {
            TypeRef::WellKnown(w) => match w {
                WellKnownTypeRef::U1 => quote!(::nom::number::complete::u8),
                WellKnownTypeRef::U2(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::u16(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_u16),
                    EndianSpec::Big => quote!(::nom::number::complete::be_u16),
                },
                WellKnownTypeRef::U4(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::u32(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_u32),
                    EndianSpec::Big => quote!(::nom::number::complete::be_u32),
                },
                WellKnownTypeRef::U8(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::u64(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_u64),
                    EndianSpec::Big => quote!(::nom::number::complete::be_u64),
                },
                WellKnownTypeRef::S1 => quote!(::nom::number::complete::i8(#p_endian)),
                WellKnownTypeRef::S2(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::i16(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_i16),
                    EndianSpec::Big => quote!(::nom::number::complete::be_i16),
                },
                WellKnownTypeRef::S4(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::i32(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_i32),
                    EndianSpec::Big => quote!(::nom::number::complete::be_i32),
                },
                WellKnownTypeRef::S8(e) => match e {
                    EndianSpec::Implicit => quote!(::nom::number::complete::i64(#p_endian)),
                    EndianSpec::Little => quote!(::nom::number::complete::le_i64),
                    EndianSpec::Big => quote!(::nom::number::complete::be_i64),
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
            },
            TypeRef::Named(n) => {
                if let Some((module, ty)) = n.split_once("::") {
                    let m = format_ident!("{}", module);
                    let p = format_ident!("parse_{}", ty);
                    quote!(super::super::#m::#p)
                } else {
                    format_ident!("parse_{}", n).into_token_stream()
                }
            }
            TypeRef::Dynamic {
                switch_on: _,
                cases: _,
            } => {
                quote!(::nom::number::complete::le_u32)
            }
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
        parser = quote!(::nom::multi::count(#parser, 1));
    } else if let Some(_e) = &attr.if_expr {
        parser = quote!(::nom::combinator::cond(false, #parser));
    }
    quote!(let (#p_input, #f_ident) = #parser(#p_input)?;)
}

impl Context<'_> {
    fn codegen_attr(
        &self,
        nc: &NamingContext,
        struct_name: &str,
        attr: &Attribute,
        self_ty: Option<&Type>,
        field: &Field,
        tc: &mut TyContext,
    ) -> io::Result<TokenStream> {
        let orig_attr_id = field.id();
        let attr_id = field.ident();
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
        fields: &[Field],
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
        let mut parser = vec![];
        let mut constructor = vec![];

        let p_input = format_ident!("_input");
        let p_endian = format_ident!("_endian");

        for (i, attr) in seq.iter().enumerate() {
            if attr.id.is_none() {
                continue;
            }
            let field = &fields[i];
            let f_ident = field.ident();
            constructor.push(quote!(#f_ident,));
            attrs.push(self.codegen_attr(nc, &rust_struct_name, attr, self_ty, field, &mut tc)?);
            parser.push(codegen_attr_parse(field, attr, &p_input, &p_endian));
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
            #[cfg(feature = "nom")]
            pub fn #parser_name<#input_lifetime #(#generics),*> (#p_input: &'a [u8]) -> ::nom::IResult<&'a [u8], #id #gen_use> {
                let #p_endian = ::nom::number::Endianness::Little;
                #(#parser)*
                Ok((#p_input, #id {
                    #(#constructor)*
                }))
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
            let struct_ty = nc.resolve(key).unwrap();
            let st = self.codegen_struct(&nc, key, doc, doc_ref, &spec.seq, &struct_ty.fields)?;
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

        let file_fields = schema
            .seq
            .iter()
            .map(|a| a.id.as_ref().unwrap().as_str())
            .map(Field::new)
            .collect::<Vec<_>>();
        let file_struct = match schema.seq.is_empty() {
            true => None,
            false => Some(
                self.codegen_struct(
                    &nc,
                    "file",
                    schema.doc.as_deref(),
                    schema.doc_ref.as_ref(),
                    &schema.seq,
                    &file_fields,
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
