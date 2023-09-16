use std::collections::BTreeMap;

use serde::Deserialize;

use crate::{Attribute, EnumSpec, MetaSpec, ParamSpec, StringOrArray};

/// Specification for a type
#[derive(Deserialize, Debug)]
pub struct TypeSpec {
    /// Metadata
    #[serde(default)]
    pub meta: Option<MetaSpec>,
    /// Used to give a more detailed description of a user-defined type.
    #[serde(default)]
    pub doc: Option<String>,
    /// Reference to external documentation
    #[serde(default)]
    pub doc_ref: Option<StringOrArray>,
    /// Generic Parameters
    #[serde(default)]
    pub params: Vec<ParamSpec>,
    /// The sequence of attributes
    #[serde(default)]
    pub seq: Vec<Attribute>,
    /// Named, dynamic attributes
    #[serde(default)]
    pub instances: BTreeMap<String, Attribute>,
    /// A sequence of internal types
    #[serde(default)]
    pub types: BTreeMap<String, TypeSpec>,
    /// Enums defined by this schema
    #[serde(default)]
    pub enums: BTreeMap<String, EnumSpec>,
}
