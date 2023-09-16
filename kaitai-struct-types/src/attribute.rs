use serde::Deserialize;

use crate::{StringOrArray, TypeRef};

/// An attribute in a type schema
#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Attribute {
    /// Unique Identifier within this struct
    ///
    /// Attributes with the same name are merged
    #[serde(default)]
    pub id: Option<String>,
    /// Used to give a more detailed description of a user-defined type.
    #[serde(default)]
    pub doc: Option<String>,
    /// Reference to external documentation
    #[serde(default)]
    pub doc_ref: Option<StringOrArray>,

    /// The type of this attribute
    #[serde(rename = "type")]
    pub ty: Option<TypeRef>,

    /// The size of this attribute (if present, creates a substream)
    #[serde(default)]
    pub size: Option<String>,

    /// The size of this attribute (if present, creates a substream)
    #[serde(default, rename = "if")]
    pub if_expr: Option<String>,

    /// Whether this attribute is repeated
    #[serde(default)]
    pub repeat: Option<Repeat>,

    /// If [Repeat::Expr], the expression to repeat
    #[serde(default)]
    pub repeat_expr: Option<String>,

    /// If [Repeat::Expr], the expression to check for repetition end
    #[serde(default)]
    pub repeat_until: Option<String>,

    /// Fixed expected contents
    #[serde(default)]
    pub contents: Option<Contents>,

    /// String encoding
    #[serde(default)]
    pub encoding: Option<String>,

    /// Name of an enumeration
    #[serde(default)]
    pub r#enum: Option<String>,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
/// A string or an array
pub enum Contents {
    /// A String
    String(String),
    /// An array of strings
    Bytes(Vec<u8>),
}

/// Kind of repetition
#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Repeat {
    /// Based on an expression
    Expr,
    /// Repeat until the end of the stream
    Eos,
    /// Repeat until a token is reached
    Until,
}
