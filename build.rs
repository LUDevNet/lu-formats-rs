use std::{
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, KsySchema, StringOrArray, TypeRef, WellKnownTypeRef,
};
use quote::{format_ident, quote, ToTokens};

fn codegen_struct(
    name: &str,
    doc: Option<&str>,
    doc_ref: Option<&StringOrArray>,
    seq: &[Attribute],
) -> io::Result<impl ToTokens> {
    let rust_struct_name = name.to_upper_camel_case();
    let id = format_ident!("{}", rust_struct_name);
    let doc = doc.unwrap_or("");
    let doc_refs = doc_ref.map(StringOrArray::as_slice).unwrap_or(&[]);

    let needs_lifetime = seq.iter().any(|a| {
        matches!(
            &a.ty,
            TypeRef::WellKnown(WellKnownTypeRef::Str) | TypeRef::WellKnown(WellKnownTypeRef::StrZ)
        )
    });

    let mut generics = vec![];
    let mut traits = vec![];

    if needs_lifetime {
        generics.push(quote!('a));
    }

    let attrs = seq
        .iter()
        .filter(|a| a.id.is_some())
        .map(|attr| {
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
                TypeRef::WellKnown(wk) => match wk {
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
                },
                TypeRef::Named(_) => quote!(()),
                TypeRef::Dynamic {
                    switch_on,
                    cases: _,
                } => {
                    match switch_on {
                        AnyScalar::String(s) => {
                            if s.starts_with("_parent.") {
                                // TODO: improve heuristic
                                let rust_generic_name = orig_attr_id.to_upper_camel_case();
                                let g = format_ident!("{}", rust_generic_name);
                                let t = format_ident!("{}{}", rust_struct_name, rust_generic_name);
                                generics.push(quote!(#g: #t));
                                traits.push(quote! {
                                    pub trait #t {
                                        // TODO
                                    }
                                });
                                quote!(#g)
                            } else {
                                quote!(())
                            }
                        }
                        _ => quote!(()),
                    }
                }
            };
            if let Some(_expr) = &attr.if_expr {
                ty = quote!(Option<#ty>);
            }

            quote!(
                #[doc = #attr_doc]
                #(#[doc = #attr_doc_refs])*
                #[doc = #ty_doc]
                #if_doc
                #repeat_doc
                pub #attr_id: #ty
            )
        })
        .collect::<Vec<_>>(); // need to collect here to release the borrow on `generics`

    let id = match generics.is_empty() {
        true => id.to_token_stream(),
        false => quote!(#id<#(#generics),*>),
    };

    let q = quote! {
        #[doc = #doc]
        #(#[doc = #doc_refs])*
        #[derive(Debug, Clone, PartialEq)]
        pub struct #id {
            #(#attrs),*
        }

        #(#traits)*
    };
    Ok(q)
}

fn codegen(path: &Path, out_dir: &Path) -> Result<PathBuf, io::Error> {
    let file = std::fs::read_to_string(path)?;
    println!("cargo:rerun-if-changed={}", path.display());
    let schema: KsySchema = serde_yaml::from_str(&file).unwrap();
    println!("{:?}", schema);

    let out_file = format!("{}.rs", &schema.meta.id);
    let out_path = out_dir.join(out_file);

    let structs = schema.types.iter().map(|(key, spec)| {
        codegen_struct(key, spec.doc.as_deref(), spec.doc_ref.as_ref(), &spec.seq).unwrap()
    });

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
            codegen_struct(
                "File",
                schema.doc.as_deref(),
                schema.doc_ref.as_ref(),
                &schema.seq,
            )
            .unwrap(),
        ),
    };
    let file = quote! {
        #mod_doc
        #file_struct
        #(#structs)*
        #(#enums)*
    };

    let writer = std::fs::File::create(&out_path)?;
    let mut writer = BufWriter::new(writer);
    write!(writer, "{}", file)?;

    Command::new("rustfmt").arg(&out_path).spawn().unwrap();

    Ok(out_path)
}

fn main() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let build_rs = root.join("build.rs");
    let lu_formats = root.join("lu_formats");
    let lu_formats_files = lu_formats.join("files");
    let lu_formats_common = lu_formats.join("common");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    println!("cargo:rerun-if-changed={}", build_rs.display());

    let common = lu_formats_common.join("common.ksy");
    let common_path = codegen(&common, &out_dir).unwrap();
    let common_path = common_path.to_string_lossy();
    let common_id = format_ident!("common");

    let luz = lu_formats_files.join("luz.ksy");
    let luz_path = codegen(&luz, &out_dir).unwrap();
    let luz_path = luz_path.to_string_lossy();
    let luz_id = format_ident!("luz");

    let lib = quote!(
        #[path = #common_path]
        pub mod #common_id;

        #[doc = "# File Formats"]
        pub mod files {
            #[path = #luz_path]
            pub mod #luz_id;
        }
    );

    std::fs::write(out_dir.join("lib.rs"), lib.to_string()).unwrap();
}
