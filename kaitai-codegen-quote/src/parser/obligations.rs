use heck::ToUpperCamelCase;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::{
    ctx::NamingContext,
    r#type::{float_ty, sint_ty, uint_ty, ObligationTree, ResolvedType, Type},
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
    if self_ty.root_obligations.is_empty() {
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
    let mut obligation_fields = Vec::new();
    let mut root_values = Vec::new();
    for f in obligations.fields() {
        let f_name = format_ident!("{}", f);
        let mut f_is_parser_arg = true;
        let mut _dbg = format!("{}::{}", context_id, f);
        let (f_ty, f_val) = match context_ty.type_of_field(f).expect(&_dbg) {
            ResolvedType::UInt { width, .. } => (uint_ty(*width), quote!(#f_name)),
            ResolvedType::SInt { width, .. } => (sint_ty(*width), quote!(#f_name)),
            ResolvedType::Float { width, .. } => (float_ty(*width), quote!(#f_name)),
            ResolvedType::Magic => todo!(),
            ResolvedType::Str { .. } => todo!(),
            ResolvedType::Bytes { .. } => todo!(),
            ResolvedType::Enum(_, _) => todo!(),
            ResolvedType::User(name) => {
                let id = format_ident!("{}{}", context_id, name.to_upper_camel_case());
                f_is_parser_arg = false;
                let inner = obligations.get(f).unwrap();
                let inner_ty = nc.resolve(name).unwrap();
                let val = visit_root_obligation(inner, inner_ty, &id, parser_args, struct_defs, nc);
                (id.into_token_stream(), quote!(#f_name: #val))
            }
            ResolvedType::Dynamic(_, _) => todo!(),
        };
        if f_is_parser_arg {
            parser_args.push(quote!(#f_name: #f_ty,));
        }
        root_values.push(f_val);
        obligation_fields.push(quote!(#f_name: #f_ty,));
    }
    struct_defs.push(quote!(
        struct #context_id {
            #(#obligation_fields)*
        }
    ));
    quote!(#context_id {
        #(#root_values)*
    })
}
