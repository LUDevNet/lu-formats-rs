#![warn(missing_docs)]
//! # Base Types for Kaitai

use std::{collections::BTreeMap, fmt, str::FromStr};

use serde::Deserialize;

/// Definition of a type or file format
#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct KsySchema {
    /// Metadata
    pub meta: MetaSpec,
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

/// An enum specification
#[derive(Deserialize, Debug)]
pub struct EnumSpec(pub BTreeMap<String, EnumValueSpec>);

/// An enum value specification
#[derive(Debug)]
pub struct EnumValueSpec {
    /// Identifier
    pub id: Identifier,
}

impl<'de> serde::Deserialize<'de> for EnumValueSpec {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct __Visitor;

        impl<'de> serde::de::Visitor<'de> for __Visitor {
            type Value = EnumValueSpec;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "string or object")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec {
                    id: Identifier(v.to_string()),
                })
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec { id: Identifier(v) })
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(EnumValueSpec {
                    id: Identifier(match v {
                        true => "true".to_string(),
                        false => "false".to_string(),
                    }),
                })
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut id = None;
                while let Some(key) = map.next_key::<&str>()? {
                    match key {
                        "id" => {
                            id = Some(map.next_value()?);
                        }
                        // FIXME: doc, doc-ref
                        _ => {
                            map.next_value::<serde::de::IgnoredAny>()?;
                        }
                    }
                }
                Ok(EnumValueSpec { id: id.unwrap() })
            }
        }

        deserializer.deserialize_any(__Visitor)
    }
}

#[derive(Deserialize, Debug)]
/// Generic Paramter
pub struct ParamSpec {
    // TODO
}

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

    /// Fixed expected contents
    #[serde(default)]
    pub contents: Option<Contents>,

    /// String encoding
    #[serde(default)]
    pub encoding: Option<String>,
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
#[derive(Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum Repeat {
    /// Based on an expression
    Expr,
    /// Repeat until the end of the stream
    Eos,
    /// Repeat until a token is reached
    Until,
}

#[derive(Deserialize, Debug)]
/// An identifier
pub struct Identifier(pub String);

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
/// A string or an array
pub enum StringOrArray {
    /// A String
    String(String),
    /// An array of strings
    Array(Vec<String>),
}

impl StringOrArray {
    /// Turn this into a slice of string
    pub fn as_slice(&self) -> &[String] {
        match self {
            StringOrArray::String(s) => std::slice::from_ref(s),
            StringOrArray::Array(s) => s,
        }
    }
}

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

/// Any scalar YAML value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AnyScalar {
    /// null
    Null,
    /// Boolean
    Bool(bool),
    /// string
    String(String),
    /// integer
    UInt(u64),
}

struct AnyScalarVisitor;

impl<'de> serde::de::Visitor<'de> for AnyScalarVisitor {
    type Value = AnyScalar;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "any scalar")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(AnyScalar::String(v.to_string()))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(AnyScalar::String(v))
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(AnyScalar::Bool(v))
    }

    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(AnyScalar::UInt(v))
    }
}

impl<'de> Deserialize<'de> for AnyScalar {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(AnyScalarVisitor)
    }
}

/// Reference to a Type
#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    /// A well-known type
    WellKnown(WellKnownTypeRef),
    /// A named type
    Named(String),
    /// A type that depends on an expression
    Dynamic {
        /// Expression to switch on
        switch_on: AnyScalar,
        /// Types depending on the value of the expression
        cases: BTreeMap<AnyScalar, TypeRef>,
    },
}

struct TypeRefVisitor;

impl<'de> serde::de::Visitor<'de> for TypeRefVisitor {
    type Value = TypeRef;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string or {{ switch-on, cases }}")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        match WellKnownTypeRef::from_str(v) {
            Ok(w) => Ok(TypeRef::WellKnown(w)),
            Err(()) => Ok(TypeRef::Named(v.to_string())),
        }
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut switch_on = None;
        let mut cases = None;

        while let Some(key) = map.next_key::<&str>()? {
            match key {
                "switch-on" => {
                    switch_on = Some(map.next_value()?);
                }
                "cases" => {
                    cases = Some(map.next_value()?);
                }
                _ => return Err(serde::de::Error::custom("Invalid field")),
            }
        }

        Ok(TypeRef::Dynamic {
            switch_on: switch_on.unwrap(),
            cases: cases.unwrap(),
        })
    }
}

impl<'de> Deserialize<'de> for TypeRef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(TypeRefVisitor)
    }
}

/// Endian Specification
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EndianSpec {
    /// Unspecified
    Implicit,
    /// Always Little Endian
    Little,
    /// Always Big Endian
    Big,
}

/// Integer byte width specification
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntTypeRef {
    /// 8 bit integer
    Int1,
    /// 16 bit integer
    Int2(EndianSpec),
    /// 32 bit integer
    Int4(EndianSpec),
    /// 64 bit integer
    Int8(EndianSpec),
}

impl IntTypeRef {
    /// Get the number of bytes for values of this type
    pub fn bytes(&self) -> usize {
        match self {
            IntTypeRef::Int1 => 1,
            IntTypeRef::Int2(_) => 2,
            IntTypeRef::Int4(_) => 4,
            IntTypeRef::Int8(_) => 8,
        }
    }
}

/// Well known [TypeRef]s
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WellKnownTypeRef {
    /// unsigned integer
    Unsigned(IntTypeRef),
    /// signed integer
    Signed(IntTypeRef),
    /// 32 bit IEEE floating point number (single precision)
    F4(EndianSpec),
    /// 64 bit IEEE floating point number (double precision)
    F8(EndianSpec),
    /// String
    Str,
    /// Nul-Terminated String
    StrZ,
}

impl FromStr for WellKnownTypeRef {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "u1" => Self::Unsigned(IntTypeRef::Int1),
            "u2" => Self::Unsigned(IntTypeRef::Int2(EndianSpec::Implicit)),
            "u2le" => Self::Unsigned(IntTypeRef::Int2(EndianSpec::Little)),
            "u2be" => Self::Unsigned(IntTypeRef::Int2(EndianSpec::Big)),
            "u4" => Self::Unsigned(IntTypeRef::Int4(EndianSpec::Implicit)),
            "u4le" => Self::Unsigned(IntTypeRef::Int4(EndianSpec::Little)),
            "u4be" => Self::Unsigned(IntTypeRef::Int4(EndianSpec::Big)),
            "u8" => Self::Unsigned(IntTypeRef::Int8(EndianSpec::Implicit)),
            "u8le" => Self::Unsigned(IntTypeRef::Int8(EndianSpec::Little)),
            "u8be" => Self::Unsigned(IntTypeRef::Int8(EndianSpec::Big)),

            "s1" => Self::Signed(IntTypeRef::Int1),
            "s2" => Self::Signed(IntTypeRef::Int2(EndianSpec::Implicit)),
            "s2le" => Self::Signed(IntTypeRef::Int2(EndianSpec::Little)),
            "s2be" => Self::Signed(IntTypeRef::Int2(EndianSpec::Big)),
            "s4" => Self::Signed(IntTypeRef::Int4(EndianSpec::Implicit)),
            "s4le" => Self::Signed(IntTypeRef::Int4(EndianSpec::Little)),
            "s4be" => Self::Signed(IntTypeRef::Int4(EndianSpec::Big)),
            "s8" => Self::Signed(IntTypeRef::Int8(EndianSpec::Implicit)),
            "s8le" => Self::Signed(IntTypeRef::Int8(EndianSpec::Little)),
            "s8be" => Self::Signed(IntTypeRef::Int8(EndianSpec::Big)),

            "f4" => Self::F4(EndianSpec::Implicit),
            "f4le" => Self::F4(EndianSpec::Little),
            "f4be" => Self::F4(EndianSpec::Big),
            "f8" => Self::F8(EndianSpec::Implicit),
            "f8le" => Self::F8(EndianSpec::Little),
            "f8be" => Self::F8(EndianSpec::Big),

            "str" => Self::Str,
            "strz" => Self::StrZ,
            _ => return Err(()),
        })
    }
}
