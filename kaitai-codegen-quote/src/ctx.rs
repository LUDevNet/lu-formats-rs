use std::collections::{BTreeMap, BTreeSet};

use kaitai_struct_types::{TypeRef, WellKnownTypeRef};

use crate::{
    r#type::{ObligationTree, Type},
    Module,
};

pub(super) struct NamingContext {
    types: BTreeMap<String, Type>,
    root: Option<String>,
}

impl NamingContext {
    pub fn into_types(self) -> BTreeMap<String, Type> {
        self.types
    }

    pub fn new() -> Self {
        Self {
            types: BTreeMap::new(),
            root: None,
        }
    }

    pub fn add(&mut self, key: &str, ty: Type) {
        self.types.insert(key.to_owned(), ty);
    }

    pub fn set_root(&mut self, key: &str, ty: Type) {
        self.root = Some(key.to_owned());
        self.types.insert(key.to_owned(), ty);
    }

    pub fn resolve(&self, key: &str) -> Option<&Type> {
        self.types.get(key)
    }

    pub fn get_root(&self) -> Option<&Type> {
        self.resolve(self.root.as_ref()?)
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

    fn merge_root_obligations<'a>(
        &'a self,
        id: &'a str,
        t: &'a Type,
        in_stack: &mut BTreeSet<&'a str>,
        results: &mut BTreeMap<String, ObligationTree>,
    ) -> ObligationTree {
        let mut o = t.root_obligations.clone();
        for list in [&t.depends_on, &t.may_depend_on] {
            for dep in list {
                let t = self.resolve(dep).expect(dep);
                if t.source_mod.is_none() && !in_stack.contains(dep.as_str()) {
                    in_stack.insert(dep.as_str());
                    o.union(&self.merge_root_obligations(dep, t, in_stack, results));
                    in_stack.remove(dep.as_str());
                }
            }
        }
        results.entry(id.to_owned()).or_default().union(&o);
        o
    }

    /// Ensures that root obligations (parser paramters)
    /// are attached to all parents.
    fn propagate_root_obligations(&mut self) {
        let root_id = self.root.as_ref().unwrap().as_str();
        let root = self.get_root().unwrap();
        let mut in_stack = BTreeSet::new();
        let mut results = BTreeMap::new();
        let _ = self.merge_root_obligations(root_id, root, &mut in_stack, &mut results);

        // After analysis, overwrite all the types
        for (name, res) in results {
            if let Some(t) = self.types.get_mut(&name) {
                if t.source_mod.is_none() {
                    t.root_obligations = res;
                }
            }
        }
    }

    pub fn process_dependencies(&mut self) {
        self.propagate_root_obligations();
        // By type name, collect all structs that depend on it
        let mut dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        // By type name, dependencies via Field Generics
        let mut maybe_dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (s, ty) in &self.types {
            if ty.source_mod.is_none() {
                for dep in &ty.depends_on {
                    dependencies.entry(dep.clone()).or_default().push(s.clone());
                }
                for dep in &ty.may_depend_on {
                    maybe_dependencies
                        .entry(dep.clone())
                        .or_default()
                        .push(s.clone());
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

        // Finally set all the parent
        for (name, parents) in dependencies {
            if let Some(ty) = self.types.get_mut(&name) {
                ty.parents = parents;
            }
        }

        // ... and parents via field generics
        for (name, maybe_parents) in maybe_dependencies {
            if let Some(ty) = self.types.get_mut(&name) {
                ty.maybe_parents = maybe_parents;
            }
        }
    }
}
