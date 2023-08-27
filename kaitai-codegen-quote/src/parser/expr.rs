use heck::ToUpperCamelCase;
use kaitai_expr::{parse_expr, Expr, Op};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::r#type::ident_of;

struct ExprCodegen {
    in_parent: bool,
}

impl ExprCodegen {
    fn new() -> Self {
        Self { in_parent: false }
    }

    fn codegen_expr(&self, _expr: &Expr) -> TokenStream {
        match _expr {
            Expr::Path(_root, _components) => {
                let i_enum = format_ident!("{}", _root.to_upper_camel_case());
                let i_var = format_ident!("{}", _components[0].to_upper_camel_case());
                quote!((#i_enum::#i_var))
            }
            Expr::Input(i, fields) => {
                let mut field_iter = fields.iter();
                let mut ts = ident_of(match *i {
                    "_parent" if self.in_parent => field_iter.next().unwrap(),
                    _ => i,
                })
                .into_token_stream();
                for field in field_iter {
                    let f = ident_of(field);
                    ts = quote!(#ts.#f);
                }
                ts
            }
            Expr::If(args) => {
                let cond = self.codegen_expr(&args.0);
                let then_case = self.codegen_expr(&args.1);
                let else_case = self.codegen_expr(&args.2);
                quote!(match #cond { true => #then_case, false => #else_case })
            }
            Expr::Number(u) => Literal::u64_unsuffixed(*u).into_token_stream(),
            Expr::BinOp { op, args } => {
                let lhs = self.codegen_expr(&args.0);
                let rhs = self.codegen_expr(&args.1);
                match op {
                    Op::Mul => quote!((#lhs * #rhs)),
                    Op::Div => quote!((#lhs / #rhs)),
                    Op::Sub => quote!((#lhs - #rhs)),
                    Op::Add => quote!((#lhs + #rhs)),
                    Op::Dot => quote!((#lhs).#rhs),
                    Op::GtEq => quote!((#lhs >= #rhs)),
                    Op::Gt => quote!((#lhs > #rhs)),
                    Op::LtEq => quote!((#lhs <= #rhs)),
                    Op::Lt => quote!((#lhs < #rhs)),
                    Op::Eq => quote!((#lhs == #rhs)),
                    Op::NEq => quote!((#lhs != #rhs)),
                    Op::And => quote!((#lhs && #rhs)),
                    Op::Or => quote!((#lhs || #rhs)),
                    Op::Path | Op::LParen | Op::RParen | Op::TernaryTrue | Op::TernaryFalse => {
                        quote!(0xBEEF)
                    }
                }
            }
        }
    }
}

pub(super) fn codegen_expr_str(expr: &str) -> TokenStream {
    ExprCodegen::new().codegen_expr(&parse_expr(expr).expect(expr))
}

pub(super) fn codegen_expr_str_with(expr: &str, in_parent: bool) -> TokenStream {
    ExprCodegen { in_parent }.codegen_expr(&parse_expr(expr).expect(expr))
}
