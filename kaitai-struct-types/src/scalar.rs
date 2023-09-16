use serde::Deserialize;
use std::fmt;

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
