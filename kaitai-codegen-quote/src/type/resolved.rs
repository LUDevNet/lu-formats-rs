use heck::ToUpperCamelCase;
use kaitai_struct_types::{
    AnyScalar, Attribute, EndianSpec, IntTypeRef, TypeRef, WellKnownTypeRef,
};

use super::{Case, CaseKind};

#[derive(Debug, Clone, PartialEq)]
pub struct ResolvedType {
    pub kind: ResolvedTypeKind,
    pub count: ResolvedTypeCount,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTypeCount {
    Required,
    Optional,
    Fixed(usize),
    Variable,
}

impl ResolvedType {
    pub(super) fn of_attribute(a: &Attribute) -> Self {
        Self {
            kind: ResolvedTypeKind::of_attribute(a),
            count: if a.repeat.is_some() {
                ResolvedTypeCount::Variable // TODO: fixed
            } else if a.if_expr.is_some() {
                ResolvedTypeCount::Optional
            } else {
                ResolvedTypeCount::Required
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedTypeKind {
    UInt {
        width: usize,
        endian: EndianSpec,
    },
    SInt {
        width: usize,
        endian: EndianSpec,
    },
    Float {
        width: usize,
        endian: EndianSpec,
    },
    Str {
        encoding: (),
        zero_terminator: bool,
    },
    Bytes {
        size_expr: String,
    },
    Enum(String, WellKnownTypeRef),
    /// A named user-type
    User(String),
    Dynamic(String, Vec<Case>),
    Magic,
}

impl ResolvedTypeKind {
    pub(super) fn needs_lifetime_a_priori(&self) -> bool {
        use ResolvedTypeKind::*;
        matches!(self, Str { .. } | Bytes { .. })
    }

    fn of_well_known(w: WellKnownTypeRef) -> Self {
        match w {
            WellKnownTypeRef::Unsigned(u) => ResolvedTypeKind::UInt {
                width: u.bytes(),
                endian: u.endian(),
            },
            WellKnownTypeRef::Signed(s) => ResolvedTypeKind::SInt {
                width: s.bytes(),
                endian: s.endian(),
            },
            WellKnownTypeRef::Float(f) => ResolvedTypeKind::Float {
                width: f.bytes(),
                endian: f.endian(),
            },
            WellKnownTypeRef::Str => ResolvedTypeKind::Str {
                encoding: (),
                zero_terminator: false,
            },
            WellKnownTypeRef::StrZ => ResolvedTypeKind::Str {
                encoding: (),
                zero_terminator: true,
            },
        }
    }

    fn of_type_ref(a: &TypeRef) -> Self {
        match a {
            TypeRef::WellKnown(w) => Self::of_well_known(*w),
            TypeRef::Named(n) => Self::User(n.to_owned()),
            TypeRef::Dynamic { .. } => todo!(),
        }
    }

    fn of_attribute(a: &Attribute) -> Self {
        if let Some(ty) = &a.ty {
            match ty {
                TypeRef::WellKnown(w) => {
                    if let Some(e) = &a.r#enum {
                        Self::Enum(e.to_string(), *w)
                    } else {
                        Self::of_well_known(*w)
                    }
                }
                TypeRef::Named(n) => Self::User(n.to_owned()),
                TypeRef::Dynamic { switch_on, cases } => {
                    if let Some(type_ref) = all_unsigned(cases.values()) {
                        Self::UInt {
                            endian: EndianSpec::Implicit, // FIXME
                            width: type_ref.bytes(),
                        }
                    } else if let AnyScalar::String(s) = switch_on {
                        Self::Dynamic(
                            s.to_owned(),
                            cases.iter().map(|(k, v)| var_case(k, v)).collect(),
                        )
                    } else {
                        todo!()
                    }
                }
            }
        } else if a.contents.is_some() {
            Self::Magic
        } else if let Some(size_expr) = a.size.as_deref() {
            Self::Bytes {
                size_expr: size_expr.to_owned(),
            }
        } else {
            todo!("{:?}", a)
        }
    }
}

fn all_unsigned<'a, I: Iterator<Item = &'a TypeRef>>(cases: I) -> Option<IntTypeRef> {
    let mut unsigned: Option<IntTypeRef> = None;
    let mut all_unsigned = true;
    for case in cases {
        if let TypeRef::WellKnown(WellKnownTypeRef::Unsigned(u)) = case {
            unsigned = match unsigned {
                Some(i) if u > &i => Some(*u),
                Some(i) => Some(i),
                None => Some(*u),
            };
        } else {
            all_unsigned = false;
            break;
        }
    }
    if all_unsigned {
        unsigned
    } else {
        None
    }
}

fn var_case(key: &AnyScalar, val: &TypeRef) -> Case {
    let (name, kind) = match key {
        AnyScalar::Null => todo!(),
        AnyScalar::Bool(true) => ("True".to_owned(), CaseKind::Bool(true)),
        AnyScalar::Bool(false) => ("False".to_owned(), CaseKind::Bool(false)),
        AnyScalar::String(s) => {
            let (_enum, part) = s.split_once("::").unwrap();
            (
                part.to_upper_camel_case(),
                CaseKind::Enum(_enum.to_string(), part.to_string()),
            )
        }
        AnyScalar::UInt(i) => (format!("N{}", i), CaseKind::Number(*i)),
    };
    let ty_kind = ResolvedTypeKind::of_type_ref(val);
    Case::new(
        &name,
        kind,
        ResolvedType {
            kind: ty_kind,
            count: ResolvedTypeCount::Required,
        },
    )
}
