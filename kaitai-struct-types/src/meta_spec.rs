use serde::Deserialize;

use crate::{Identifier, StringOrArray};

#[derive(Default, Deserialize, Debug)]
/// Cross-References
pub struct XRef {}

/// Metadata for a type
#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct MetaSpec {
    /// The ID for this format
    pub id: Identifier,
    /// The name of the schema
    #[serde(default)]
    pub title: Option<String>,
    /// The application the schema comes from
    #[serde(default)]
    pub application: Option<StringOrArray>,
    /// The file extensions used for this schema
    #[serde(default)]
    pub file_extension: Option<StringOrArray>,
    /// Cross References
    #[serde(default)]
    pub xref: Option<XRef>,
    /// License SPDX Identifier
    #[serde(default)]
    pub license: Option<String>,
    /// Version of Kaitai Struct
    #[serde(default)]
    pub ks_version: Option<String>,
    /// Whether to enable debug in KSC
    #[serde(default)]
    pub ks_debug: bool,
    /// Whether to enable opaque types
    #[serde(default)]
    pub ks_opaque_types: bool,
    /// KSY files that this schema depends on
    #[serde(default)]
    pub imports: Vec<String>,
    /// The default encoding used for strings
    #[serde(default)]
    pub encoding: Option<String>,
    /// The default endianness
    #[serde(default)]
    pub endian: Option<Endian>,
}

#[derive(Deserialize, Debug, Copy, Clone)]
/// Endianness qualifier
pub enum Endian {
    /// Least Significant Byte first
    #[serde(rename = "le")]
    LittleEndian,
    /// Most significant Byte first, aka "Network Byte Order"
    #[serde(rename = "be")]
    BigEndian,
}
