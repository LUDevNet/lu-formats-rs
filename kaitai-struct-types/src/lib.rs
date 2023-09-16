#![warn(missing_docs)]
//! # Base Types for Kaitai

mod attribute;
mod enum_spec;
mod ksy_schema;
mod meta_spec;
mod param_spec;
mod scalar;
mod type_ref;
mod type_spec;
pub use {
    attribute::{Attribute, Contents, Repeat},
    enum_spec::{EnumSpec, EnumValueSpec},
    ksy_schema::KsySchema,
    meta_spec::{Endian, MetaSpec, XRef},
    param_spec::ParamSpec,
    scalar::{AnyScalar, Identifier, StringOrArray},
    type_ref::{EndianSpec, FloatTypeRef, IntTypeRef, TypeRef, WellKnownTypeRef},
    type_spec::TypeSpec,
};
