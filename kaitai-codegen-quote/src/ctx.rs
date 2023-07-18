use std::collections::{BTreeMap, BTreeSet};

use kaitai_struct_types::{TypeRef, WellKnownTypeRef};

use crate::{r#type::Type, Module};

pub(super) struct NamingContext {
    types: BTreeMap<String, Type>,
}

impl NamingContext {
    pub fn into_types(self) -> BTreeMap<String, Type> {
        self.types
    }

    pub fn new() -> Self {
        Self {
            types: BTreeMap::new(),
        }
    }

    pub fn add(&mut self, key: &str, ty: Type) {
        self.types.insert(key.to_owned(), ty);
    }

    pub fn resolve(&self, key: &str) -> Option<&Type> {
        self.types.get(key)
    }

    pub fn need_lifetime(&self, type_ref: &TypeRef) -> bool {
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

    pub fn import_module(&mut self, module: &Module) {
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

    pub fn process_dependencies(&mut self) {
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
