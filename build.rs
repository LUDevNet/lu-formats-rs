use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
};

use kaitai_codegen_quote::{Context, Module};
use kaitai_struct_types::KsySchema;
use quote::{format_ident, quote, ToTokens};

struct RunContext {
    out_dir: PathBuf,
    ksy_root: PathBuf,
    available_imports: BTreeMap<String, Module>,
}

impl RunContext {
    fn run(&mut self, dir: &str, file: &str) -> io::Result<(PathBuf, impl ToTokens)> {
        let path = self.ksy_root.join(dir).join(format!("{}.ksy", file));
        let ksy_yaml = std::fs::read_to_string(&path)?;
        println!("cargo:rerun-if-changed={}", path.display());
        let schema: KsySchema = serde_yaml::from_str(&ksy_yaml).unwrap();

        let dir_id = format_ident!("{}", dir);
        let parent = if dir == file {
            quote!(crate)
        } else {
            quote!(crate::#dir_id)
        };
        let context = Context {
            parent,
            available_imports: &self.available_imports,
            schema: &schema,
            out_dir: &self.out_dir,
        };
        let module = context.codegen().unwrap();
        let mod_path = module.out_path.clone();
        let ident = format_ident!("{}", file);
        self.available_imports
            .insert(format!("../{}/{}", dir, file), module);
        Ok((mod_path, ident))
    }
}

fn main() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let build_rs = root.join("build.rs");
    let lu_formats = root.join("lu_formats");

    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    println!("cargo:rerun-if-changed={}", build_rs.display());

    let mut ctx = RunContext {
        out_dir,
        ksy_root: lu_formats,
        available_imports: BTreeMap::new(),
    };
    let lib_path = ctx.out_dir.join("lib.rs");
    let (common_path, common_id) = ctx.run("common", "common").unwrap();
    let (luz_path, luz_id) = ctx.run("files", "luz").unwrap();

    let common_path = common_path.to_string_lossy();
    let luz_path = luz_path.to_string_lossy();

    let lib = quote!(
        #[path = #common_path]
        pub mod #common_id;

        #[doc = "# File Formats"]
        pub mod files {
            #[path = #luz_path]
            pub mod #luz_id;
        }
    );

    let mut out_file = File::create(lib_path).unwrap();
    write!(out_file, "{}", lib).unwrap();
}
