use std::collections::{BTreeMap, BTreeSet};

use heck::ToUpperCamelCase;
use kaitai_expr::{parse_expr, Expr};
use kaitai_struct_types::{
    AnyScalar, Attribute, Endian, EndianSpec, IntTypeRef, KsySchema, TypeRef, TypeSpec,
    WellKnownTypeRef,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

mod obligations;

pub use obligations::ObligationTree;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Standard Kaitai type, generated as a rust `struct` with named fields
    Record,
    /// Variable Length String, generated as a newtype of `&[u8]`
    VarStr,
    /// Wrapper for a single sequence element, generated as a newtype
    Newtype,
}

impl TypeKind {
    /// Detect the [TypeKind] from type ID and attribute sequence
    fn detect(id: &str, seq: &[Attribute]) -> Self {
        match seq {
            [a] if a.size.is_none() && a.repeat.is_none() && a.id.as_deref() == Some(id) => {
                TypeKind::Newtype
            }
            [a, b]
                if a.id.as_deref() == Some("length")
                    && b.ty == Some(TypeRef::WellKnown(WellKnownTypeRef::Str)) =>
            {
                let encoding = b.encoding.as_deref().unwrap();
                let size_expr = if encoding.eq_ignore_ascii_case("utf-16le")
                    || encoding.eq_ignore_ascii_case("utf-16be")
                {
                    "length * 2"
                } else {
                    "length"
                };
                if b.size.as_deref() == Some(size_expr) {
                    Self::VarStr
                } else {
                    Self::Record
                }
            }
            _ => Self::Record,
        }
    }
}

impl Default for TypeKind {
    fn default() -> Self {
        Self::Record
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypeDep {
    pub(crate) fields: BTreeSet<String>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub is_root: bool,
    pub endian: Endian,

    pub rust_struct_name: String,
    pub orig_id: String,
    pub parser_name: Ident,
    pub source_mod: Option<Ident>,
    pub ident: Ident,
    pub needs_lifetime: bool,
    /// This represents a set of generics that depend on
    /// values in the parent.
    pub field_generics: BTreeMap<String, FieldGenerics>,
    pub depends_on: BTreeMap<String, TypeDep>,

    /// Map from user-type id to a set of uses
    pub may_depend_on: BTreeMap<String, TypeDep>,

    /// Whether this type encodes a variable length string
    kind: TypeKind,

    pub fields: Vec<Field>,
    pub instances: Vec<Field>,
    pub parents: Vec<String>,
    /// Structs that use this one, via field generics
    pub maybe_parents: Vec<String>,

    pub root_obligations: ObligationTree,
    pub parent_obligations: ObligationTree,
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
    Case::new(&name, kind, ResolvedType::of_type_ref(val))
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

impl Type {
    fn new_named(
        key: &str,
        seq: &[Attribute],
        instances: &BTreeMap<String, Attribute>,
        is_root: bool,
        endian: Endian,
    ) -> Self {
        let rust_struct_name = key.to_upper_camel_case();
        let mut t = Self {
            is_root,
            endian,
            parser_name: format_ident!("parse_{}", key),
            source_mod: None,
            orig_id: key.to_owned(),
            ident: format_ident!("{}", rust_struct_name),
            rust_struct_name,
            needs_lifetime: false,
            field_generics: BTreeMap::new(),

            kind: TypeKind::detect(key, seq),

            depends_on: BTreeMap::new(),
            may_depend_on: BTreeMap::new(),

            instances: Vec::new(),
            fields: Vec::new(),
            parents: Vec::new(),
            maybe_parents: Vec::new(),

            root_obligations: ObligationTree::new(),
            parent_obligations: ObligationTree::new(),
        };
        for a in seq {
            t.push_seq_elem(a);
        }
        for (k, v) in instances {
            t.push_instances_elem(k, v);
        }
        t
    }

    pub fn new(key: &str, spec: &TypeSpec, root_endian: Endian) -> Self {
        let endian = spec
            .meta
            .as_ref()
            .and_then(|m| m.endian)
            .unwrap_or(root_endian);
        Self::new_named(key, &spec.seq, &spec.instances, false, endian)
    }

    pub fn new_root(spec: &KsySchema) -> Self {
        let endian = spec.meta.endian.unwrap_or(Endian::LittleEndian);
        Self::new_named(&spec.meta.id.0, &spec.seq, &spec.instances, true, endian)
    }

    pub(crate) fn is_var_len_str(&self) -> bool {
        self.kind == TypeKind::VarStr
    }

    fn check_obligations(&mut self, expr: &Expr) {
        match expr {
            Expr::Path(_, _) => {}
            Expr::Input("_root", fields) => self.root_obligations.add(fields),
            Expr::Input("_parent", fields) => self.parent_obligations.add(fields),
            Expr::Input(_, _) | Expr::Number(_) => {}
            Expr::If(args) => {
                self.check_obligations(&args.0);
                self.check_obligations(&args.1);
                self.check_obligations(&args.2);
            }
            Expr::BinOp { op: _, args } => {
                self.check_obligations(&args.0);
                self.check_obligations(&args.1);
            }
        }
    }

    fn push_seq_elem(&mut self, a: &Attribute) {
        let orig_attr_id = a.id.as_deref().unwrap();
        let f = Field::new(orig_attr_id, ResolvedType::of_attribute(a));

        self.needs_lifetime = self.needs_lifetime || f.resolved_type.needs_lifetime_a_priori();
        if let ResolvedType::User(n) = &f.resolved_type {
            self.depends_on
                .entry(n.to_owned())
                .or_default()
                .fields
                .insert(orig_attr_id.to_owned());
        }

        if let ResolvedType::Dynamic(s, cases) = &f.resolved_type {
            let fg = self.new_field_generics(orig_attr_id, cases, s);
            self.field_generics.insert(orig_attr_id.to_string(), fg);
        }

        if let Some(expr) = &a.if_expr {
            let expr = parse_expr(expr).unwrap();
            self.check_obligations(&expr);
        }

        self.fields.push(f);
    }

    fn push_instances_elem(&mut self, key: &str, value: &Attribute) {
        let f = Field::new(key, ResolvedType::of_attribute(value));
        if let ResolvedType::User(n) = &f.resolved_type {
            self.may_depend_on
                .entry(n.to_owned())
                .or_default()
                .fields
                .insert(key.to_owned());
        } else if let ResolvedType::Dynamic(_s, cases) = &f.resolved_type {
            for case in cases {
                if let ResolvedType::User(n) = &case.ty {
                    self.may_depend_on
                        .entry(n.to_owned())
                        .or_default()
                        .fields
                        .insert(key.to_owned());
                }
            }
        }
        self.instances.push(f);
    }

    fn new_field_generics(
        &mut self,
        orig_attr_id: &str,
        cases: &[Case],
        switch_expr: &str,
    ) -> FieldGenerics {
        let mut fg_depends_on = BTreeSet::new();
        let mut _enum_set = BTreeSet::<String>::new();
        let mut fg_cases = Vec::<Case>::new();
        for case in cases {
            if let ResolvedType::User(n) = &case.ty {
                self.may_depend_on
                    .entry(n.to_owned())
                    .or_default()
                    .fields
                    .insert(orig_attr_id.to_owned());
                fg_depends_on.insert(n.clone());
            }
            fg_cases.push(case.clone())
        }
        FieldGenerics::new(
            orig_attr_id,
            &self.ident,
            switch_expr,
            fg_cases,
            fg_depends_on,
        )
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

    pub(crate) fn is_newtype(&self) -> bool {
        self.kind == TypeKind::Newtype
    }

    /// Find a field by its identifier
    pub(crate) fn find_field(&self, name: &str) -> Option<&Field> {
        self.fields
            .iter()
            .chain(self.instances.iter())
            .find(|&f| f.id() == name)
    }

    /// For a given `id` within this type, find the ([`ResolvedType`]) that represents type.
    pub(crate) fn type_of_field(&self, name: &str) -> Option<&ResolvedType> {
        self.find_field(name).map(Field::resolved_ty)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedType {
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

impl ResolvedType {
    fn needs_lifetime_a_priori(&self) -> bool {
        use ResolvedType::*;
        matches!(self, Str { .. } | Bytes { .. })
    }

    fn of_well_known(w: WellKnownTypeRef) -> Self {
        match w {
            WellKnownTypeRef::Unsigned(u) => ResolvedType::UInt {
                width: u.bytes(),
                endian: u.endian(),
            },
            WellKnownTypeRef::Signed(s) => ResolvedType::SInt {
                width: s.bytes(),
                endian: s.endian(),
            },
            WellKnownTypeRef::F4(endian) => ResolvedType::Float { width: 4, endian },
            WellKnownTypeRef::F8(endian) => ResolvedType::Float { width: 8, endian },
            WellKnownTypeRef::Str => ResolvedType::Str {
                encoding: (),
                zero_terminator: false,
            },
            WellKnownTypeRef::StrZ => ResolvedType::Str {
                encoding: (),
                zero_terminator: true,
            },
        }
    }

    fn of_type_ref(a: &TypeRef) -> ResolvedType {
        match a {
            TypeRef::WellKnown(w) => Self::of_well_known(*w),
            TypeRef::Named(n) => ResolvedType::User(n.to_owned()),
            TypeRef::Dynamic { .. } => todo!(),
        }
    }

    fn of_attribute(a: &Attribute) -> ResolvedType {
        if let Some(ty) = &a.ty {
            match ty {
                TypeRef::WellKnown(w) => {
                    if let Some(e) = &a.r#enum {
                        ResolvedType::Enum(e.to_string(), *w)
                    } else {
                        Self::of_well_known(*w)
                    }
                }
                TypeRef::Named(n) => ResolvedType::User(n.to_owned()),
                TypeRef::Dynamic { switch_on, cases } => {
                    if let Some(type_ref) = all_unsigned(cases.values()) {
                        ResolvedType::UInt {
                            endian: EndianSpec::Implicit, // FIXME
                            width: type_ref.bytes(),
                        }
                    } else if let AnyScalar::String(s) = switch_on {
                        ResolvedType::Dynamic(
                            s.to_owned(),
                            cases.iter().map(|(k, v)| var_case(k, v)).collect(),
                        )
                    } else {
                        todo!()
                    }
                }
            }
        } else if a.contents.is_some() {
            ResolvedType::Magic
        } else if let Some(size_expr) = a.size.as_deref() {
            ResolvedType::Bytes {
                size_expr: size_expr.to_owned(),
            }
        } else {
            todo!("{:?}", a)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ident: Ident,
    id: String,

    resolved_type: ResolvedType,
}

impl Field {
    pub(crate) fn new(orig_attr_id: &str, resolved_type: ResolvedType) -> Self {
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

pub(crate) fn ident_of(id: &str) -> Ident {
    match id {
        "type" => format_ident!("r#type"),
        _ => format_ident!("{}", id),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CaseKind {
    Enum(String, String),
    Bool(bool),
    Number(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
    pub ident: Ident,
    pub kind: CaseKind,
    pub ty: ResolvedType,
}

impl Case {
    pub fn new(name: &str, kind: CaseKind, ty: ResolvedType) -> Self {
        Self {
            ident: format_ident!("{}", name),
            kind,
            ty,
        }
    }

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
    pub(crate) parser: Ident,
    /// Identifier for an enum that has all options
    pub(crate) var_enum: Ident,
    pub(crate) var_enum_other: Ident,
    pub(crate) switch_expr: String,

    // NOTE: depends on stable ordering in kaitai-struct-types
    pub(crate) cases: Vec<Case>,

    /// (relative) names of the types this field generic may depend on
    pub(crate) depends_on: BTreeSet<String>,
    pub(crate) need_lifetime: bool,

    pub(crate) external: bool,
}

impl FieldGenerics {
    fn new(
        orig_attr_id: &str,
        self_ident: &Ident,
        switch_expr: &str,
        fg_cases: Vec<Case>,
        fg_depends_on: BTreeSet<String>,
    ) -> FieldGenerics {
        let rust_generic_name = orig_attr_id.to_upper_camel_case();
        let g = format_ident!("{}", rust_generic_name);
        let t = format_ident!("I{}{}", self_ident, &g);
        let var_enum = format_ident!("{}{}Variants", self_ident, &g);
        FieldGenerics {
            trait_: t,
            type_: g,
            var_enum,
            var_enum_other: format_ident!("_Other"),
            parser: format_ident!("parse_{}", orig_attr_id),
            switch_expr: switch_expr.to_string(),
            cases: fg_cases,

            depends_on: fg_depends_on,
            need_lifetime: false,
            external: switch_expr.starts_with("_parent."),
        }
    }

    pub fn variant_type(&self) -> TokenStream {
        let n: &Ident = &self.var_enum;
        match self.need_lifetime {
            true => quote!(#n<'a>),
            false => quote!(#n),
        }
    }

    pub fn var_enum_generics(&self) -> Option<TokenStream> {
        match self.need_lifetime {
            false => None,
            true => Some(quote!(<'a>)),
        }
    }
}

pub struct Enum {
    pub ident: Ident,
}
impl Enum {
    pub(crate) fn new(key: &str, _spec: &kaitai_struct_types::EnumSpec) -> Self {
        let ident = format_ident!("{}", key.to_upper_camel_case());
        Self { ident }
    }
}

pub(crate) fn uint_ty(width: usize) -> TokenStream {
    match width {
        // FIXME uses cases
        1 => quote!(u8),
        2 => quote!(u16),
        4 => quote!(u32),
        8 => quote!(u64),
        _ => panic!("width not supported"),
    }
}

pub(crate) fn sint_ty(width: usize) -> TokenStream {
    match width {
        // FIXME uses cases
        1 => quote!(i8),
        2 => quote!(i16),
        4 => quote!(i32),
        8 => quote!(i64),
        _ => panic!("width not supported"),
    }
}

pub(crate) fn float_ty(width: usize) -> TokenStream {
    match width {
        // FIXME uses cases
        4 => quote!(f32),
        8 => quote!(f64),
        _ => panic!("width not supported"),
    }
}