use std::{collections::BTreeMap, fmt, str::FromStr};

use serde::Deserialize;

use crate::AnyScalar;

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

    /// Get the endian spec for this int type ref
    pub fn endian(&self) -> EndianSpec {
        match self {
            IntTypeRef::Int1 => EndianSpec::Implicit,
            IntTypeRef::Int2(e) | IntTypeRef::Int4(e) | IntTypeRef::Int8(e) => *e,
        }
    }
}

/// Float byte width specification
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatTypeRef {
    /// 32 bit IEEE floating point number (single precision)
    Float4(EndianSpec),
    /// 64 bit IEEE floating point number (double precision)
    Float8(EndianSpec),
}

impl FloatTypeRef {
    /// Get the number of bytes
    pub fn bytes(&self) -> usize {
        match self {
            FloatTypeRef::Float4(_) => 4,
            FloatTypeRef::Float8(_) => 8,
        }
    }

    /// Get the endian spec for this float type ref
    pub fn endian(&self) -> EndianSpec {
        match self {
            FloatTypeRef::Float4(e) | FloatTypeRef::Float8(e) => *e,
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
    /// floating point number
    Float(FloatTypeRef),
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

            "f4" => Self::Float(FloatTypeRef::Float4(EndianSpec::Implicit)),
            "f4le" => Self::Float(FloatTypeRef::Float4(EndianSpec::Little)),
            "f4be" => Self::Float(FloatTypeRef::Float4(EndianSpec::Big)),
            "f8" => Self::Float(FloatTypeRef::Float8(EndianSpec::Implicit)),
            "f8le" => Self::Float(FloatTypeRef::Float8(EndianSpec::Little)),
            "f8be" => Self::Float(FloatTypeRef::Float8(EndianSpec::Big)),

            "str" => Self::Str,
            "strz" => Self::StrZ,
            _ => return Err(()),
        })
    }
}
