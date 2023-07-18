use std::collections::{BTreeMap, BTreeSet};

use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, IntTypeRef, KsySchema, TypeRef, TypeSpec, WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

#[derive(Debug, Clone)]
pub struct Type {
    pub parser_name: Ident,
    pub source_mod: Option<Ident>,
    pub ident: Ident,
    pub needs_lifetime: bool,
    /// This represents a set of generics that depend on
    /// values in the parent.
    pub field_generics: BTreeMap<String, FieldGenerics>,
    pub depends_on: BTreeSet<String>,

    pub fields: Vec<Field>,
}

impl Type {
    fn new_named(key: &str, seq: &[Attribute]) -> Self {
        let rust_struct_name = key.to_upper_camel_case();
        let mut t = Self {
            parser_name: format_ident!("parse_{}", key),
            source_mod: None,
            ident: format_ident!("{}", rust_struct_name),
            needs_lifetime: false,
            field_generics: BTreeMap::new(),
            depends_on: BTreeSet::new(),
            fields: Vec::new(),
        };
        for a in seq {
            t.push_seq_elem(a);
        }
        t
    }

    pub fn new(key: &str, spec: &TypeSpec) -> Self {
        Self::new_named(key, &spec.seq)
    }

    pub fn new_root(spec: &KsySchema) -> Self {
        Self::new_named(&spec.meta.id.0, &spec.seq)
    }

    fn push_seq_elem(&mut self, a: &Attribute) {
        let orig_attr_id = a.id.as_deref().unwrap();
        let mut f = Field::new(orig_attr_id);

        if let Some(ty) = &a.ty {
            match ty {
                TypeRef::WellKnown(WellKnownTypeRef::Str | WellKnownTypeRef::StrZ) => {
                    self.needs_lifetime = true;
                }
                TypeRef::WellKnown(_) => {}
                TypeRef::Named(n) => {
                    self.depends_on.insert(n.clone());
                }
                TypeRef::Dynamic { switch_on, cases } => {
                    if let AnyScalar::String(s) = switch_on {
                        let rust_generic_name = orig_attr_id.to_upper_camel_case();
                        let g = format_ident!("{}", rust_generic_name);
                        let t = format_ident!("I{}{}", self.ident, &g);
                        let var_enum = format_ident!("{}{}Variants", self.ident, &g);
                        let mut depends_on = BTreeSet::new();
                        let mut unsigned: Option<IntTypeRef> = None;
                        let mut all_unsigned = true;
                        for case in cases.values() {
                            if let TypeRef::Named(n) = case {
                                depends_on.insert(n.clone());
                            }
                            if let TypeRef::WellKnown(WellKnownTypeRef::Unsigned(u)) = case {
                                unsigned = match unsigned {
                                    Some(i) if u > &i => Some(*u),
                                    Some(i) => Some(i),
                                    None => Some(*u),
                                };
                            } else {
                                all_unsigned = false;
                            }
                        }
                        if all_unsigned {
                            let width = unsigned.unwrap().bytes();
                            f.resolved_type = ResolvedType::UInt { width }
                        } else {
                            let fg = FieldGenerics {
                                trait_: t,
                                type_: g,
                                var_enum,
    
                                depends_on,
                                need_lifetime: false,
                                external: s.starts_with("_parent."),
                            };
    
                            self.field_generics.insert(orig_attr_id.to_string(), fg);
                        }
                    }
                }
            }
        } else {
            // TODO
        }
        self.fields.push(f);
    }

    fn has_generics(&self) -> bool {
        self.field_generics.values().all(|fg| !fg.external) && !self.needs_lifetime
    }

    pub fn token_stream(&self) -> TokenStream {
        let ty_id = &self.ident;
        if self.has_generics() {
            quote!(#ty_id)
        } else {
            let lifetime = match self.needs_lifetime {
                true => Some(quote!('a)),
                false => None,
            };
            let f_gen = self
                .field_generics
                .values()
                .filter(|fg| fg.external)
                .map(FieldGenerics::variant_type); // FIXME: more intelligent?
            let gen = lifetime.into_iter().chain(f_gen);
            quote!(#ty_id<#(#gen),*>)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedType {
    Auto,
    UInt { width: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ident: Ident,
    id: String,

    resolved_type: ResolvedType,
}

impl Field {
    pub(crate) fn new(orig_attr_id: &str) -> Self {
        let ident = match orig_attr_id {
            "type" => format_ident!("r#type"),
            _ => format_ident!("{}", orig_attr_id),
        };
        Self {
            ident,
            id: orig_attr_id.to_owned(),
            resolved_type: ResolvedType::Auto,
        }
    }

    /// ID for this field
    pub fn resolved_ty(&self) -> &ResolvedType {
        &self.resolved_type
    }
    
    /// ID for this field
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Rust identifier for this field
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldGenerics {
    /// The name of the generic field
    pub(crate) type_: Ident,
    /// The trait associated with that field
    pub(crate) trait_: Ident,
    /// Identifier for an enum that has all options
    pub(crate) var_enum: Ident,

    /// (relative) names of the types this field generic may depend on
    pub(crate) depends_on: BTreeSet<String>,
    pub(crate) need_lifetime: bool,

    pub(crate) external: bool,
}

impl FieldGenerics {
    pub fn variant_type(&self) -> TokenStream {
        let n: &Ident = &self.var_enum;
        match self.need_lifetime {
            true => quote!(#n<'a>),
            false => quote!(#n),
        }
    }
}
