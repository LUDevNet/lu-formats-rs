use std::collections::BTreeSet;

use heck::ToUpperCamelCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use super::Case;

#[derive(Debug, Clone, PartialEq)]
pub struct FieldGenerics {
    /// The name of the generic field
    pub(crate) type_: Ident,
    /// The trait associated with that field
    pub(crate) trait_: Ident,
    pub(crate) parser: Ident,
    /// Identifier for an enum that has all options
    pub(crate) var_enum: Ident,
    pub(crate) var_enum_other: Ident,
    pub(crate) switch_expr: String,

    // NOTE: depends on stable ordering in kaitai-struct-types
    pub(crate) cases: Vec<Case>,

    /// (relative) names of the types this field generic may depend on
    pub(crate) depends_on: BTreeSet<String>,
    pub(crate) need_lifetime: bool,

    pub(crate) external: bool,
}

impl FieldGenerics {
    pub(super) fn new(
        orig_attr_id: &str,
        self_ident: &Ident,
        switch_expr: &str,
        fg_cases: Vec<Case>,
        fg_depends_on: BTreeSet<String>,
    ) -> FieldGenerics {
        let rust_generic_name = orig_attr_id.to_upper_camel_case();
        let g = format_ident!("{}", rust_generic_name);
        let t = format_ident!("I{}{}", self_ident, &g);
        let var_enum = format_ident!("{}{}Variants", self_ident, &g);
        FieldGenerics {
            trait_: t,
            type_: g,
            var_enum,
            var_enum_other: format_ident!("_Other"),
            parser: format_ident!("parse_{}", orig_attr_id),
            switch_expr: switch_expr.to_string(),
            cases: fg_cases,

            depends_on: fg_depends_on,
            need_lifetime: false,
            external: switch_expr.starts_with("_parent."),
        }
    }

    pub fn variant_type(&self) -> TokenStream {
        let n: &Ident = &self.var_enum;
        match self.need_lifetime {
            true => quote!(#n<'a>),
            false => quote!(#n),
        }
    }

    pub fn var_enum_generics(&self) -> Option<TokenStream> {
        match self.need_lifetime {
            false => None,
            true => Some(quote!(<'a>)),
        }
    }
}
