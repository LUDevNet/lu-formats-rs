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
    /// This represents a set of generics that depend on
    /// values in the parent.
    pub field_generics: BTreeMap<String, FieldGenerics>,
    pub depends_on: BTreeSet<String>,
}

impl Type {
    fn has_generics(&self) -> bool {
        self.field_generics.values().all(|fg| !fg.external) && !self.needs_lifetime
    }

    fn token_stream(&self) -> TokenStream {
        let ty_id = &self.ident;
        if self.has_generics() {
            quote!(#ty_id)
        } else {
            let lifetime = match self.needs_lifetime {
                true => Some(quote!('a)),
                false => None,
            };
            let f_gen = self
                .field_generics
                .values()
                .filter(|fg| fg.external)
                .map(FieldGenerics::variant_type); // FIXME: more intelligent?
            let gen = lifetime.into_iter().chain(f_gen);
            quote!(#ty_id<#(#gen),*>)
        }
    }
}

#[derive(Debug, Clone)]
pub struct FieldGenerics {
    /// The name of the generic field
    type_: Ident,
    /// The trait associated with that field
    trait_: Ident,
    /// Identifier for an enum that has all options
    var_enum: Ident,

    /// (relative) names of the types this field generic may depend on
    depends_on: BTreeSet<String>,
    need_lifetime: bool,

    external: bool,
}

impl FieldGenerics {
    fn variant_type(&self) -> TokenStream {
        let n: &Ident = &self.var_enum;
        match self.need_lifetime {
            true => quote!(#n<'a>),
            false => quote!(#n),
        }
    }
}

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

struct NamingContext {
    types: BTreeMap<String, Type>,
}

impl NamingContext {
    fn new() -> Self {
        Self {
            types: BTreeMap::new(),
        }
    }

    fn add(&mut self, key: &str, ty: Type) {
        self.types.insert(key.to_owned(), ty);
    }

    fn resolve(&self, key: &str) -> Option<&Type> {
        self.types.get(key)
    }

    fn need_lifetime(&self, type_ref: &TypeRef) -> bool {
        match type_ref {
            TypeRef::WellKnown(WellKnownTypeRef::Str) => true,
            TypeRef::WellKnown(WellKnownTypeRef::StrZ) => true,
            TypeRef::WellKnown(_) => false,
            TypeRef::Named(n) => self.resolve(n).is_some_and(|v| v.needs_lifetime),
            TypeRef::Dynamic {
                switch_on: _,
                cases: _,
            } => todo!(),
        }
    }

    fn import_module(&mut self, module: &Module) {
        for (s, ty) in &module.types {
            // FIXME: on demand?
            self.types.insert(
                format!("{}::{}", module.id, s),
                Type {
                    source_mod: Some(module.id.clone()),
                    ..ty.clone()
                },
            );
        }
    }

    fn process_dependencies(&mut self) {
        // By type name, collect all structs that depend on it
        let mut dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (s, ty) in &self.types {
            if ty.source_mod.is_none() {
                for dep in &ty.depends_on {
                    dependencies.entry(dep.clone()).or_default().push(s.clone());
                }
            }
        }

        let mut lifetime_set = BTreeSet::<String>::new();
        let mut to_process: BTreeSet<String> = self
            .types
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
        for s in &lifetime_set {
            let ty = self.types.get_mut(s).unwrap();
            ty.needs_lifetime = true;
        }

        for ty in self.types.values_mut() {
            for gen in &mut ty.field_generics.values_mut() {
                gen.need_lifetime = gen.depends_on.iter().any(|f| lifetime_set.contains(f));
            }
        }
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
        let mut ty = codegen_type_ref(&attr.ty, nc, tc, struct_name, orig_attr_id, fg);
        if let Some(_rep) = &attr.repeat {
            ty = quote!(Vec<#ty>);
        } else if let Some(_expr) = &attr.if_expr {
            ty = quote!(Option<#ty>);
        }

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
                TypeRef::WellKnown(_) => {}
                TypeRef::Named(n) => {
                    depends_on.insert(n.clone());
                }
                TypeRef::Dynamic { switch_on, cases } => {
                    if let AnyScalar::String(s) = switch_on {
                        let rust_generic_name = orig_attr_id.to_upper_camel_case();
                        let g = format_ident!("{}", rust_generic_name);
                        let t = format_ident!("I{}{}", rust_struct_name, rust_generic_name);
                        let var_enum =
                            format_ident!("{}{}Variants", rust_struct_name, rust_generic_name);
                        let mut depends_on = BTreeSet::new();
                        for case in cases.values() {
                            if let TypeRef::Named(n) = case {
                                depends_on.insert(n.clone());
                            }
                        }
                        let fg = FieldGenerics {
                            trait_: t,
                            type_: g,
                            var_enum,

                            depends_on,
                            need_lifetime: false,
                            external: s.starts_with("_parent."),
                        };

                        field_generics.insert(orig_attr_id.to_string(), fg);
                    }
                }
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
            let st = self.struct_type(key, spec);
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
            types: nc.types,
            import,
        })
    }
}
