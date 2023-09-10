//! Parser handling of [`ObligationTree`]

use heck::ToUpperCamelCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::{
    ctx::NamingContext,
    r#type::{float_ty, ident_of, sint_ty, uint_ty, ObligationTree, ResolvedTypeKind, Type},
};

pub(super) fn parent_obligation_params(
    self_ty: &Type,
    parser_args: &mut Vec<TokenStream>,
) -> Option<TokenStream> {
    (!self_ty.parent_obligations.is_empty()).then(|| {
        let mut parent_values = Vec::<TokenStream>::new();
        let mut parent_fields = Vec::<TokenStream>::new();
        let mut prev = Vec::<TokenStream>::new();
        for field in self_ty.parent_obligations.fields() {
            let field_ident = format_ident!("{}", field);
            let (field_ty, field_val) = if field == "_parent" {
                let pp_ident = format_ident!("_ParentParent");
                let mut pp_values = Vec::<TokenStream>::new();
                let mut pp_fields = Vec::<TokenStream>::new();
                let obligations = self_ty.parent_obligations.get("_parent").unwrap();
                for fields in obligations.fields() {
                    let pp_field_ty = quote!(u32);
                    let pp_field_id = format_ident!("{}", fields);
                    let pp_field = quote!(#pp_field_id: #pp_field_ty);
                    parser_args.push(pp_field.clone());
                    pp_fields.push(pp_field);
                    pp_values.push(pp_field_id.to_token_stream());
                }
                prev.push(quote!(struct #pp_ident {
                    #(#pp_fields),*
                }));
                (
                    quote!(#pp_ident),
                    quote!(#pp_ident {
                        #(#pp_values),*
                    }),
                )
            } else {
                (quote!(u32), field_ident.to_token_stream())
            };
            let field_decl = quote!(#field_ident: #field_ty);
            if field != "_parent" {
                parser_args.push(field_decl.clone());
            }
            parent_values.push(quote!(#field_ident: #field_val));
            parent_fields.push(field_decl);
        }
        assert_eq!(parent_fields.len(), parent_values.len());
        quote!(
            #(#prev)*
            struct _Parent {
                #(#parent_fields),*
            }
            let _parent = _Parent {
                #(#parent_values),*
            };
        )
    })
}

pub(super) fn root_obligation_params(
    self_ty: &Type,
    nc: &NamingContext,
    parser_args: &mut Vec<TokenStream>,
) -> Option<TokenStream> {
    if self_ty.root_obligations.is_empty() || self_ty.root_obligations.local_or_all_local() {
        return None;
    }
    let mut struct_defs = Vec::new();
    let context_ty = nc.get_root().unwrap();
    let context_id = format_ident!("_Root");
    let obligations = &self_ty.root_obligations;
    let val = visit_root_obligation(
        obligations,
        context_ty,
        &context_id,
        parser_args,
        &mut struct_defs,
        nc,
    );
    Some(quote!(
        #(#struct_defs)*
        let _root = #val;
    ))
}

fn visit_root_obligation(
    obligations: &ObligationTree,
    context_ty: &Type,
    context_id: &Ident,
    parser_args: &mut Vec<TokenStream>,
    struct_defs: &mut Vec<TokenStream>,
    nc: &NamingContext,
) -> TokenStream {
    let mut fields = Vec::new();
    let mut values = Vec::new();
    for (f, inner) in obligations.entries() {
        let f_name = format_ident!("{}", f);
        let mut f_is_parser_arg = true;
        let mut _dbg = format!("{}::{}", context_id, f);
        let (f_ty, f_val) = match &context_ty.type_of_field(f).expect(&_dbg).kind {
            ResolvedTypeKind::UInt { width, .. } => (uint_ty(*width), quote!(#f_name)),
            ResolvedTypeKind::SInt { width, .. } => (sint_ty(*width), quote!(#f_name)),
            ResolvedTypeKind::Float { width, .. } => (float_ty(*width), quote!(#f_name)),
            ResolvedTypeKind::Magic => todo!(),
            ResolvedTypeKind::Str { .. } => todo!(),
            ResolvedTypeKind::Bytes { .. } => todo!(),
            ResolvedTypeKind::Enum(_, _) => todo!(),
            ResolvedTypeKind::User(name) => {
                let id = format_ident!("{}{}", context_id, name.to_upper_camel_case());
                f_is_parser_arg = false;
                let inner_ty = nc.resolve(name).unwrap();
                let val = visit_root_obligation(inner, inner_ty, &id, parser_args, struct_defs, nc);
                (id.into_token_stream(), quote!(#f_name: #val))
            }
            ResolvedTypeKind::Dynamic(_, _) => todo!(),
        };
        if f_is_parser_arg {
            parser_args.push(quote!(#f_name: #f_ty,));
        }
        values.push(f_val);
        fields.push(quote!(#f_name: #f_ty,));
    }
    let _doc = format!("local: {:?}", obligations.is_local());
    struct_defs.push(quote!(
        #[doc = #_doc]
        struct #context_id {
            #(#fields)*
        }
    ));
    quote!(#context_id {
        #(#values)*
    })
}

/// Get the values passed into a root obligation by the parent parser
pub(super) fn root_obligation_values(
    named_ty: &Type,
    values: &mut Vec<TokenStream>,
    is_root_parser: bool,
) {
    let base = match is_root_parser {
        true => None,
        false => Some(quote!(_root)),
    };
    push_root_obligation_values(&named_ty.root_obligations, values, base.as_ref());
}

fn push_root_obligation_values(
    obligations: &ObligationTree,
    values: &mut Vec<TokenStream>,
    base: Option<&TokenStream>,
) {
    for (f, inner) in obligations.entries() {
        if inner.is_local() {
            continue;
        }
        let f_id = format_ident!("{}", f);
        let id = base
            .map(|b| quote!(#b.#f_id))
            .unwrap_or_else(|| Ident::into_token_stream(f_id));
        let inner = obligations.get(f).unwrap();
        if inner.is_empty() {
            values.push(id);
        } else {
            push_root_obligation_values(inner, values, Some(&id))
        }
    }
}

/// Get the values passed into a parent obligation by the parent parser
pub(super) fn parent_obligation_values(
    named_ty: &Type,
    values: &mut Vec<TokenStream>,
    in_parent: bool,
) {
    for obligation in named_ty.parent_obligations.fields() {
        match obligation.as_str() {
            "_parent" => {
                for obligation in named_ty
                    .parent_obligations
                    .get(obligation)
                    .unwrap()
                    .fields()
                {
                    match obligation.as_str() {
                        "_parent" => todo!(),
                        _ => {
                            let i = ident_of(obligation);
                            values.push(if in_parent {
                                i.into_token_stream()
                            } else {
                                quote!(_parent.#i)
                            });
                        }
                    }
                }
            }
            _ => {
                values.push(ident_of(obligation).into_token_stream());
            }
        }
    }
}
