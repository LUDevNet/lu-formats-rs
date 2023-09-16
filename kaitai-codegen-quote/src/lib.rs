use std::{
    collections::BTreeMap,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::Command,
};

use ctx::NamingContext;
use kaitai_struct_types::KsySchema;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use r#type::{Enum, Type};

mod ctx;
mod doc;
mod enums;
mod parser;
mod rt;
mod structs;
mod r#type;

pub use rt::codegen_rt;

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

impl Context<'_> {
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

        for (key, spec) in &schema.enums {
            let r#enum = Enum::new(key, spec);
            nc.add_enum(key, r#enum);
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
            let st = structs::codegen_struct(&nc, doc, doc_ref, &spec.seq, struct_ty).unwrap();
            structs.push(st);
        }

        let enums = schema
            .enums
            .iter()
            .map(|(name, spec)| enums::codegen_enum(name, spec));

        let mod_doc = schema.doc.as_deref().map(|d| quote!(#![doc = #d]));

        if !schema.seq.is_empty() {
            let root_ty = nc.get_root().unwrap();
            let root_struct = structs::codegen_struct(
                &nc,
                schema.doc.as_deref(),
                schema.doc_ref.as_ref(),
                &schema.seq,
                root_ty,
            )
            .unwrap();
            structs.push(root_struct);
        };

        let file = quote! {
            #mod_doc

            #(#q_imports)*
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
