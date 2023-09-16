use kaitai_struct_types::Attribute;
use proc_macro2::Ident;

use super::{ident_of, ResolvedType};

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ident: Ident,
    id: String,

    resolved_type: ResolvedType,
}

impl Field {
    pub(crate) fn new(orig_attr_id: &str, a: &Attribute) -> Self {
        let resolved_type = ResolvedType::of_attribute(a);
        Self {
            ident: ident_of(orig_attr_id),
            id: orig_attr_id.to_owned(),
            resolved_type,
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
