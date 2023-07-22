use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as FmtWrite,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use ctx::NamingContext;
use heck::ToUpperCamelCase;
use kaitai_expr::{parse_expr, Expr, Op};
use kaitai_struct_types::{
    AnyScalar, Attribute, Contents, IntTypeRef, KsySchema, Repeat, StringOrArray, TypeRef,
    WellKnownTypeRef,
};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use r#type::{Field, FieldGenerics, ResolvedType, Type};

mod ctx;
mod parser;
mod r#type;

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
            let v = &gen.var_enum;
            if gen.external {
                tc.generics.push(quote!(#g: #t));
                tc.generics_use.push(quote!(#g));
            }
            let trait_doc = format!("Marker trait for [`{enclosing_type}::{discriminant}`]");
            let var_enum_doc = format!("Raw variants for [`{}::{}`]", enclosing_type, discriminant);
            let mut enum_cases: Vec<TokenStream> = Vec::<TokenStream>::with_capacity(cases.len());
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
        }
    }
}

fn codegen_expr(_expr: &Expr) -> TokenStream {
    match _expr {
        Expr::Input(i, fields) => match *i {
            "_parent" => quote!(0xBEEF),
            _ => {
                let mut ts = format_ident!("{}", i).into_token_stream();
                for field in fields {
                    let f = format_ident!("{}", field);
                    ts = quote!(#ts.#f);
                }
                ts
            }
        },
        Expr::Number(u) => Literal::u64_unsuffixed(*u).into_token_stream(),
        Expr::BinOp { op, args } => {
            let lhs = codegen_expr(&args.0);
            let rhs = codegen_expr(&args.1);
            match op {
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

fn codegen_expr_str(expr: &str) -> TokenStream {
    let parsed_expr = parse_expr(expr).expect(expr);
    codegen_expr(&parsed_expr)
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
            TypeRef::WellKnown(w) => parser::wk_parser(w, p_endian),
            TypeRef::Named(n) => {
                let _named_ty = nc.resolve(n).unwrap();
                parser::user_type(_named_ty, self_ty.is_root)
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
                        let t = &fg.var_enum;
                        quote!((|i: &'a[u8]| -> ::nom::IResult<&'a[u8], #t> { todo!() }))
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

impl Context<'_> {
    fn codegen_attr(
        &self,
        nc: &NamingContext,
        struct_name: &str,
        attr: &Attribute,
        self_ty: &Type,
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
        let fg = self_ty.field_generics.get(orig_attr_id);
        let ty = match field.resolved_ty() {
            ResolvedType::Auto => {
                if let Some(ty) = &attr.ty {
                    codegen_type_ref(ty, nc, tc, struct_name, orig_attr_id, fg)
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
        self_ty: &Type,
    ) -> io::Result<TokenStream> {
        let rust_struct_name = name.to_upper_camel_case();
        let id = format_ident!("{}", rust_struct_name);
        let doc = doc.unwrap_or("");
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

        let mut attrs = vec![];
        let mut parser = vec![];
        let mut constructor = vec![];

        let p_input = format_ident!("_input");
        let p_endian = format_ident!("_endian");

        for (i, attr) in seq.iter().enumerate() {
            if attr.id.is_none() {
                continue;
            }
            let field = &self_ty.fields[i];
            let f_ident = field.ident();
            constructor.push(quote!(#f_ident,));
            attrs.push(self.codegen_attr(nc, &rust_struct_name, attr, self_ty, field, &mut tc)?);
            parser.push(codegen_attr_parse(
                field, self_ty, attr, &p_input, &p_endian, nc,
            ));
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

        let parser_name = &self_ty.parser_name;
        let generics = &tc.generics[..];
        let q_parser_impl = quote!(
            let #p_endian = ::nom::number::Endianness::Little;
            #(#parser)*
            Ok((#p_input, #id {
                #(#constructor)*
            }))
        );
        let q_parser_attr = quote!(
            #[cfg(feature = "nom")]
            #[allow(unused_parens)]
        );
        let q_result = quote!(::nom::IResult<#_input_ty, #id #gen_use>);
        let q_parser = if _root.is_some() || !_external_field_generics.is_empty() {
            quote!(
                #q_parser_attr
                pub fn #parser_name<#input_lifetime #(#generics),*> (
                    #(#root_fields)*
                    #(#_external_field_generics)*
                ) -> impl FnMut(#_input_ty) -> #q_result {
                    #_root
                    move |#p_input: #_input_ty| {
                        #q_parser_impl
                    }
                }
            )
        } else {
            quote!(
                #q_parser_attr
                pub fn #parser_name<#input_lifetime #(#generics),*> (#p_input: &'a [u8]) -> #q_result {
                    #_root
                    #q_parser_impl
                }
            )
        };

        let traits = &tc.traits[..];

        let q = quote! {
            #[doc = #doc]
            #(#[doc = #doc_refs])*
            #doc_root_obligations
            #doc_parent_obligations
            #doc_parents
            #doc_maybe_parents
            #doc_depends_on
            #doc_may_depend_on
            #[derive(Debug, Clone, PartialEq)]
            pub struct #id #gen {
                #(#attrs),*
            }

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
        for (key, spec) in &schema.types {
            let st = Type::new(key, spec);
            nc.add(key, st);
        }

        nc.set_root(id, Type::new_root(schema));

        nc.process_dependencies();

        // Struct Codegen
        for (key, spec) in &schema.types {
            let doc = spec.doc.as_deref();
            let doc_ref = spec.doc_ref.as_ref();
            let struct_ty = nc.resolve(key).unwrap();
            let st = self.codegen_struct(&nc, key, doc, doc_ref, &spec.seq, struct_ty)?;
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

        let root_ty = nc.get_root().unwrap();
        let file_struct = match schema.seq.is_empty() {
            true => None,
            false => Some(
                self.codegen_struct(
                    &nc,
                    id,
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
