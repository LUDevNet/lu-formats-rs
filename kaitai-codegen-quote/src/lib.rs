use std::{
    collections::{BTreeMap, BTreeSet},
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, KsySchema, StringOrArray, TypeRef, TypeSpec, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

#[derive(Debug, Clone)]
pub struct Type {
    pub parser_name: Ident,
    pub source_mod: Option<Ident>,
    pub ident: Ident,
    pub needs_lifetime: bool,
    pub field_generics: BTreeMap<String, Generics>,
    pub depends_on: BTreeSet<String>,
}

#[derive(Debug, Clone)]
pub struct Generics {
    type_: Ident,
    trait_: Ident,
}

pub struct Module {
    pub id: Ident,
    pub import: TokenStream,
    pub out_path: PathBuf,
    pub types: BTreeMap<String, Type>,
}

/// Context for transpiling a module / schema file
pub struct Context<'a> {
    /// Identifier for the parent module, e.g. `quote!(crate)`
    pub parent: TokenStream,
    pub available_imports: &'a BTreeMap<String, Module>,
    pub schema: &'a KsySchema,
    pub out_dir: &'a Path,
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

impl Context<'_> {
    fn codegen_struct(
        &self,
        types: &BTreeMap<String, Type>,
        name: &str,
        doc: Option<&str>,
        doc_ref: Option<&StringOrArray>,
        seq: &[Attribute],
    ) -> io::Result<TokenStream> {
        let self_ty = types.get(name);
        let rust_struct_name = name.to_upper_camel_case();
        let id = format_ident!("{}", rust_struct_name);
        let doc = doc.unwrap_or("");
        let doc_refs = doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);

        /*
        let needs_lifetime = seq.iter().any(|a| {
            matches!(
                &a.ty,
                TypeRef::WellKnown(WellKnownTypeRef::Str)
                    | TypeRef::WellKnown(WellKnownTypeRef::StrZ)
            )
        });
        */
        let needs_lifetime = self_ty.map(|t| t.needs_lifetime).unwrap_or(true);

        let mut generics = vec![];
        let mut generics_use = vec![];
        let mut traits = vec![];

        if needs_lifetime {
            generics.push(quote!('a));
            generics_use.push(quote!('a));
        }

        let mut attrs = vec![];

        for attr in seq {
            if attr.id.is_none() {
                continue;
            }

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
            let mut ty = match &attr.ty {
                TypeRef::WellKnown(wktr) => quote_wk_typeref(*wktr),
                TypeRef::Named(n) => {
                    if let Some(ty) = types.get(n) {
                        let ty_id = &ty.ident;
                        let mut q_ty = if ty.field_generics.is_empty() && !ty.needs_lifetime {
                            quote!(#ty_id)
                        } else {
                            let lifetime = match ty.needs_lifetime {
                                true => Some(quote!('a)),
                                false => None,
                            };
                            let f_gen = ty.field_generics.values().map(|_v| quote!(()));
                            let gen = lifetime.into_iter().chain(f_gen);
                            quote!(#ty_id<#(#gen),*>)
                        };
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
                    cases: _,
                } => {
                    if let Some(gen) = self_ty.and_then(|ty| ty.field_generics.get(orig_attr_id)) {
                        let g = &gen.type_;
                        let t = &gen.trait_;
                        generics.push(quote!(#g: #t));
                        generics_use.push(quote!(#g));
                        traits.push(quote! {
                            pub trait #t {
                                // TODO
                            }

                            // FIXME: remove
                            impl #t for () {}
                        });
                        quote!(#g)
                    } else {
                        quote!(())
                    }
                }
            };
            if let Some(_rep) = &attr.repeat {
                ty = quote!(Vec<#ty>);
            } else if let Some(_expr) = &attr.if_expr {
                ty = quote!(Option<#ty>);
            }

            attrs.push(quote!(
                #[doc = #attr_doc]
                #(#[doc = #attr_doc_refs])*
                #[doc = #ty_doc]
                #if_doc
                #repeat_doc
                pub #attr_id: #ty
            ));
        }

        let gen = match generics.is_empty() {
            true => None,
            false => Some(quote!(<#(#generics),*>)),
        };

        let gen_use = match generics_use.is_empty() {
            true => None,
            false => Some(quote!(<#(#generics_use),*>)),
        };

        let input_lifetime = match needs_lifetime {
            true => None,
            false => Some(quote!('a,)),
        };
        let file_parser_name = format_ident!("parse_file");
        let parser_name = self_ty.map(|t| &t.parser_name).unwrap_or(&file_parser_name);
        let q_parser = quote!(
            pub fn #parser_name<#input_lifetime #(#generics),*> (_input: &'a [u8]) -> Result<(&'a [u8], #id #gen_use), ()> {
                todo!()
            }
        );

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

    #[allow(clippy::collapsible_match)]
    fn struct_type(&self, key: &str, spec: &TypeSpec) -> Type {
        let rust_struct_name = key.to_upper_camel_case();
        let mut needs_lifetime = false;
        let mut field_generics = BTreeMap::new();
        let mut depends_on = BTreeSet::new();
        for a in &spec.seq {
            let orig_attr_id = a.id.as_deref().unwrap();
            match &a.ty {
                TypeRef::WellKnown(WellKnownTypeRef::Str | WellKnownTypeRef::StrZ) => {
                    needs_lifetime = true;
                }
                TypeRef::Named(n) => {
                    depends_on.insert(n.clone());
                }
                TypeRef::Dynamic {
                    switch_on,
                    cases: _,
                } => {
                    if let AnyScalar::String(s) = switch_on {
                        if s.starts_with("_parent.") {
                            // TODO: improve heuristic
                            let rust_generic_name = orig_attr_id.to_upper_camel_case();
                            let g = format_ident!("{}", rust_generic_name);
                            let t = format_ident!("{}{}", rust_struct_name, rust_generic_name);
                            field_generics.insert(
                                orig_attr_id.to_string(),
                                Generics {
                                    trait_: t,
                                    type_: g,
                                },
                            );
                        }
                    }
                }
                _ => {}
            }
        }
        Type {
            parser_name: format_ident!("parse_{}", key),
            source_mod: None,
            ident: format_ident!("{}", rust_struct_name),
            needs_lifetime,
            field_generics,
            depends_on,
        }
    }

    pub fn codegen(&self) -> Result<Module, io::Error> {
        let schema = &self.schema;
        let out_dir = &self.out_dir;
        let p = &self.parent;
        let sid = format_ident!("{}", &schema.meta.id.0);
        let import = quote!(#p::#sid);

        let out_file = format!("{}.rs", &schema.meta.id);
        let out_path = out_dir.join(out_file);

        let mut types = BTreeMap::new();
        let mut structs = vec![];

        let mut q_imports = vec![];
        for imp in &schema.meta.imports {
            let module = self.available_imports.get(imp).expect("Missing import");
            let mod_id = &module.import;
            q_imports.push(quote!(use #mod_id;));

            for (s, ty) in &module.types {
                types.insert(
                    format!("{}::{}", module.id, s),
                    Type {
                        source_mod: Some(module.id.clone()),
                        ..ty.clone()
                    },
                );
            }
        }

        // First stage analysis
        for (key, spec) in &schema.types {
            let st = self.struct_type(key, spec);
            types.insert(key.clone(), st);
        }

        // By type name, collect all structs that depend on it
        let mut dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (s, ty) in &types {
            if ty.source_mod.is_none() {
                for dep in &ty.depends_on {
                    dependencies.entry(dep.clone()).or_default().push(s.clone());
                }
            }
        }

        let mut lifetime_set = BTreeSet::<String>::new();
        let mut to_process: BTreeSet<String> = types
            .iter()
            .filter_map(|(k, v)| {
                if v.needs_lifetime {
                    Some(k.clone())
                } else {
                    None
                }
            })
            .collect();
        while !to_process.is_empty() {
            let mut new_to_process = BTreeSet::new();
            for e in &to_process {
                if let Some(deps) = dependencies.get(e) {
                    for dep in deps {
                        if !lifetime_set.contains(dep) && !to_process.contains(dep) {
                            new_to_process.insert(dep.clone());
                        }
                    }
                }
            }
            lifetime_set.append(&mut to_process);
            to_process.append(&mut new_to_process);
        }
        for s in lifetime_set {
            let ty = types.get_mut(&s).unwrap();
            ty.needs_lifetime = true;
        }

        // Struct Codegen
        for (key, spec) in &schema.types {
            let doc = spec.doc.as_deref();
            let doc_ref = spec.doc_ref.as_ref();
            let st = self.codegen_struct(&types, key, doc, doc_ref, &spec.seq)?;
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
                    &types,
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
            types,
            import,
        })
    }
}
