use std::{
    collections::BTreeMap,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
};

use abbau_codegen_quote::{Context, KsySchema, Module};
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
        let out_dir = self.out_dir.join(dir);
        std::fs::create_dir_all(&out_dir)?;
        let context = Context {
            parent,
            available_imports: &self.available_imports,
            schema: &schema,
            out_dir: &out_dir,
        };
        let file_id = if dir == file { Some("mod") } else { None };
        let module = context.codegen(file_id).unwrap();
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
    let rt_path = ctx.out_dir.join("_rt.rs");
    abbau_codegen_quote::codegen_rt(&rt_path).unwrap();
    let (_common_path, common_id) = ctx.run("common", "common").unwrap();
    let files_mod_path = ctx.out_dir.join("files").join("mod.rs");
    let (_fdb_path, fdb_id) = ctx.run("files", "fdb").unwrap();
    //let (_g_path, g_id) = ctx.run("files", "g").unwrap(); // needs bit parsers
    let (_kfm_path, kfm_id) = ctx.run("files", "kfm").unwrap();
    let (_luz_path, luz_id) = ctx.run("files", "luz").unwrap();
    let (_lvl_path, lvl_id) = ctx.run("files", "lvl").unwrap(); // needs instances
    let (_pki_path, pki_id) = ctx.run("files", "pki").unwrap();

    let files_mod = quote!(
        pub mod #fdb_id;
        //pub mod #g_id;
        pub mod #kfm_id;
        pub mod #luz_id;
        pub mod #lvl_id;
        pub mod #pki_id;
    );
    let mut files_mod_file = File::create(files_mod_path).unwrap();
    write!(files_mod_file, "{}", files_mod).unwrap();

    let lib = quote!(
        pub(crate) mod _rt;
        pub mod #common_id;

        #[doc = "# File Formats"]
        pub mod files;
    );

    let mut out_file = File::create(lib_path).unwrap();
    write!(out_file, "{}", lib).unwrap();
}
