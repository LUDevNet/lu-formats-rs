use std::collections::BTreeMap;

use proc_macro2::TokenStream;
use quote::quote;

use super::TypeDep;

/// Grouping of values outside the current struct/parser.
///
/// These are values that need to be known before parsing can begin.
///
/// Each type can have `_root` and `_parent` obligation trees.
///
/// - Root Obligations are passed on to parent parsers unchanged until they reach
///   the top level parser.
/// - Parent Obligations are passed on with one `_parent` removed until
///   the obligations refer to local fields.
#[derive(Default, Debug, Clone)]
pub struct ObligationTree {
    local: bool,
    inner: BTreeMap<String, ObligationTree>,
}

/*impl fmt::Debug for ObligationTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}*/

impl ObligationTree {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn doc(&self, key: &str) -> Option<TokenStream> {
        (!self.inner.is_empty()).then(|| {
            let text = format!("```ron\n{}: {:#?}\n```", key, &self.inner);
            quote!(#[doc = #text])
        })
    }

    pub fn union(&mut self, other: &ObligationTree) {
        self.local |= other.local;
        for (key, value) in &other.inner {
            self.inner.entry(key.clone()).or_default().union(value);
        }
    }

    pub fn mark_local(&mut self, path: &[&TypeDep]) {
        // self.local = true;
        if let Some((first, rest)) = path.split_first() {
            for field in &first.fields {
                if let Some(inner) = self.inner.get_mut(field) {
                    //inner.local = true;
                    inner.mark_local(rest);
                }
            }
        } else {
            self.local = true;
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn local_or_all_local(&self) -> bool {
        self.local || (!self.is_empty() && self.inner.values().all(Self::local_or_all_local))
    }

    pub fn all_local(&self) -> bool {
        self.inner.values().all(Self::is_local)
    }

    pub fn empty_or_all_local(&self) -> bool {
        self.is_empty() || self.local && self.all_local()
    }

    pub fn fields(&self) -> impl Iterator<Item = &String> {
        self.inner.keys()
    }

    pub fn entries(&self) -> impl Iterator<Item = (&String, &ObligationTree)> {
        self.inner.iter()
    }

    pub fn get(&self, field: &str) -> Option<&ObligationTree> {
        self.inner.get(field)
    }

    pub(super) fn add(&mut self, fields: &[&str]) {
        if let Some((first, rest)) = fields.split_first() {
            self.inner.entry(first.to_string()).or_default().add(rest);
        }
    }

    pub(crate) fn is_local(&self) -> bool {
        self.local
    }
}
