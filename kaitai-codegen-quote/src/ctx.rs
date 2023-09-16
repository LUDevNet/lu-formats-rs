use std::collections::{BTreeMap, BTreeSet};

use crate::{
    r#type::{Enum, ObligationTree, ResolvedType, ResolvedTypeKind, Type, TypeDep},
    Module,
};

pub(super) struct NamingContext {
    types: BTreeMap<String, Type>,
    enums: BTreeMap<String, Enum>,
    root: Option<String>,
}

impl NamingContext {
    pub fn into_types(self) -> BTreeMap<String, Type> {
        self.types
    }

    pub fn new() -> Self {
        Self {
            types: BTreeMap::new(),
            enums: BTreeMap::new(),
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

    pub fn get_root_mut(&mut self) -> Option<&mut Type> {
        self.types.get_mut(self.root.as_ref()?)
    }

    pub fn need_lifetime(&self, ty: &ResolvedType) -> bool {
        match &ty.kind {
            ResolvedTypeKind::Bytes { .. } => true,
            ResolvedTypeKind::Str { .. } => true,
            ResolvedTypeKind::User(n) => self.resolve(n).is_some_and(|v| v.needs_lifetime),
            _ => false,
        }
    }

    pub fn import_module(&mut self, module: &Module) {
        for (s, ty) in &module.types {
            // FIXME: on demand?
            self.types.insert(format!("{}::{}", module.id, s), {
                let mut t = ty.clone();
                t.source_mod = Some(module.id.clone());
                t
            });
        }
    }

    fn merge_root_obligations<'a>(
        &'a self,
        path: &mut Vec<&'a TypeDep>,
        id: &'a str,
        t: &'a Type,
        in_stack: &mut BTreeSet<&'a str>,
        results: &mut BTreeMap<String, ObligationTree>,
    ) -> ObligationTree {
        // Start with the explicit root_obligation on the type
        let mut o = t.root_obligations.clone();
        // FIXME: push field id to `path` argument so we can check whether the current traversal
        // _is_ a parent of a nested root obligation e.g. `_root.fib_chunk.data.version`
        for (dep, info) in &t.depends_on {
            path.push(info);
            if let Some(r) = self.merge_root_obligation(dep, in_stack, path, results) {
                if info.should_merge_root_obligation() {
                    o.union(&r);
                }
            }
            path.pop();
        }
        if !t.is_root {
            o.mark_local(path);
            results.entry(id.to_owned()).or_default().union(&o);
        }
        o
    }

    fn merge_root_obligation<'a>(
        &'a self,
        dep: &'a str,
        in_stack: &mut BTreeSet<&'a str>,
        path: &mut Vec<&'a TypeDep>,
        results: &mut BTreeMap<String, ObligationTree>,
    ) -> Option<ObligationTree> {
        let t = self.resolve(dep).expect(dep);
        if t.source_mod.is_none() && !in_stack.contains(dep) {
            in_stack.insert(dep);
            let r = self.merge_root_obligations(path, dep, t, in_stack, results);
            in_stack.remove(dep);
            Some(r)
        } else {
            None
        }
    }

    /// Ensures that root obligations (parser paramters)
    /// are attached to all parents.
    fn propagate_root_obligations(&mut self) {
        let root_id = self.root.as_ref().unwrap().as_str();
        let root = self.get_root().unwrap();
        let mut path = vec![];
        let mut in_stack = BTreeSet::new();
        let mut results = BTreeMap::new();
        let _ = self.merge_root_obligations(&mut path, root_id, root, &mut in_stack, &mut results);

        // After analysis, overwrite all the types
        for (name, res) in results {
            if let Some(t) = self.types.get_mut(&name) {
                if t.source_mod.is_none() {
                    t.root_obligations = res;
                }
            }
        }
        self.get_root_mut()
            .unwrap()
            .root_obligations
            .mark_local(&[]);
    }

    pub fn process_dependencies(&mut self) {
        self.propagate_root_obligations();
        // By type name, collect all structs that depend on it
        let mut dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        // By type name, dependencies via Field Generics
        let mut maybe_dependencies: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (s, ty) in &self.types {
            if ty.source_mod.is_none() {
                for (dep, value) in &ty.depends_on {
                    if !value.fields.is_empty() {
                        &mut dependencies
                    } else {
                        &mut maybe_dependencies
                    }
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

    pub(crate) fn get_enum(&self, key: &str) -> Option<&Enum> {
        self.enums.get(key)
    }

    pub(crate) fn add_enum(&mut self, key: &str, value: Enum) {
        self.enums.insert(key.to_owned(), value);
    }
}
