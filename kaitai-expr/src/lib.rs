use std::{iter::Peekable, str::FromStr};

use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Input(&'a str, Vec<&'a str>),
    Number(u64),
    BinOp {
        op: Op,
        args: Box<(Expr<'a>, Expr<'a>)>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Dot,

    GtEq,
    Gt,
    LtEq,
    Lt,
    Eq,

    And,
    Or,

    LParen,
    RParen,
}

impl Op {
    fn is_left_associative(&self) -> bool {
        use Op::*;
        matches!(self, Dot | GtEq | Gt | LtEq | Lt | Eq | And | Or)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(&'a str),
    BinOp(Op),
    Number(u64),
}

impl<'a> Token<'a> {
    fn new_str(str: &'a str) -> Self {
        match str {
            "or" => Self::BinOp(Op::Or),
            "and" => Self::BinOp(Op::And),
            _ => Self::Ident(str),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    /// No tokens in the expression
    NoTokens,
    /// Does not start with a value
    NoValueStart,
    /// Value following Expr without operator in between
    ValAfterExpr,
}

/// Symbolic Stack Machine that constructs [Expr]s.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct ExprVM<'a> {
    stack: Vec<Expr<'a>>,
}

impl<'a> ExprVM<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn stack(&self) -> &[Expr<'a>] {
        &self.stack
    }

    pub fn exec(&mut self, t: Token<'a>) {
        match t {
            Token::Ident(i) => self.stack.push(Expr::Input(i, Vec::new())),
            Token::Number(n) => self.stack.push(Expr::Number(n)),
            Token::BinOp(op) => {
                let rhs = self.stack.pop().unwrap();
                let lhs = self.stack.pop().unwrap();
                self.stack.push(match (lhs, rhs) {
                    (Expr::Input(l, mut vl), Expr::Input(r, mut vr)) => {
                        vl.push(r);
                        vl.append(&mut vr);
                        Expr::Input(l, vl)
                    }
                    args => Expr::BinOp {
                        op,
                        args: Box::new(args),
                    },
                });
            }
        }
    }

    pub fn result(&mut self) -> Expr<'a> {
        assert_eq!(self.stack.len(), 1);
        self.stack.pop().unwrap()
    }
}

/// Iterator implementation of the [Shunting yard algorithm]
///
/// [Shunting yard algorithm]: https://en.wikipedia.org/wiki/Shunting_yard_algorithm#The_algorithm_in_detail
pub struct ShuntingYard<'a> {
    inner: Peekable<Lexer<'a>>,
    op_stack: Vec<Op>,
}

impl<'a> From<Lexer<'a>> for ShuntingYard<'a> {
    fn from(inner: Lexer<'a>) -> Self {
        Self {
            inner: inner.peekable(),
            op_stack: Vec::new(),
        }
    }
}

impl<'a> Iterator for ShuntingYard<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.inner.peek().copied() {
                Some(Token::Ident(_) | Token::Number(_)) => break self.inner.next(),
                Some(Token::BinOp(o1)) => match self.op_stack.last().copied() {
                    Some(o2)
                        if o2 != Op::LParen
                            && (o2 < o1 || (o1 == o2 && o1.is_left_associative())) =>
                    {
                        break self.op_stack.pop().map(Token::BinOp)
                    }

                    _ => {
                        self.inner.next();
                        self.op_stack.push(o1);
                    }
                },
                None => break self.op_stack.pop().map(Token::BinOp),
            }
        }
    }
}

pub struct Lexer<'a> {
    rest: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(rest: &'a str) -> Self {
        Self { rest }
    }

    pub fn parse_expr(self) -> Result<Expr<'a>, Error> {
        let yard = ShuntingYard::from(self);
        let mut vm = ExprVM::new();
        for token in yard {
            vm.exec(token);
        }
        Ok(vm.result())
    }
}

pub fn parse_expr(expr: &str) -> Result<Expr<'_>, Error> {
    Lexer::new(expr).parse_expr()
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let mut start = self.rest;
        let mut chars = start.chars();
        loop {
            match chars.next() {
                Some('>') => {
                    self.rest = chars.as_str();
                    match chars.next() {
                        Some('=') => {
                            self.rest = chars.as_str();
                            break Some(Token::BinOp(Op::GtEq));
                        }
                        Some(_) | None => break Some(Token::BinOp(Op::Gt)),
                    }
                }
                Some('<') => {
                    self.rest = chars.as_str();
                    match chars.next() {
                        Some('=') => {
                            self.rest = chars.as_str();
                            break Some(Token::BinOp(Op::LtEq));
                        }
                        Some(_) | None => break Some(Token::BinOp(Op::Lt)),
                    }
                }
                Some('=') => {
                    self.rest = chars.as_str();
                    match chars.next() {
                        Some('=') => {
                            self.rest = chars.as_str();
                            break Some(Token::BinOp(Op::Eq));
                        }
                        Some(_) | None => todo!("Error"),
                    }
                }
                Some('.') => {
                    self.rest = chars.as_str();
                    break Some(Token::BinOp(Op::Dot));
                }
                Some('0'..='9') => {
                    break Some(Token::Number(
                        u64::from_str(loop {
                            self.rest = chars.as_str();
                            break match chars.next() {
                                Some('0'..='9') => continue,
                                Some(_) => &start[..(start.len() - self.rest.len())],
                                None => start,
                            };
                        })
                        .unwrap(),
                    ))
                }
                Some(ch) if is_xid_start(ch) || ch == '_' => {
                    break Some(Token::new_str(loop {
                        self.rest = chars.as_str();
                        break match chars.next() {
                            Some(ch) if is_xid_continue(ch) => continue,
                            Some(_) => &start[..(start.len() - self.rest.len())],
                            None => start,
                        };
                    }))
                }
                Some(ch) if ch.is_whitespace() => {
                    start = chars.as_str();
                    self.rest = start;
                }
                Some(_ch) => todo!("{}", _ch),
                None => break None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ExprVM;

    use super::{parse_expr, Expr, Lexer, Op, ShuntingYard, Token};

    const COMPLEX_EXPR_STR: &str = "_root.file_version >= 33 or _root.file_version < 30";
    fn complex_expr() -> Expr<'static> {
        Expr::BinOp {
            op: Op::Or,
            args: Box::new((
                Expr::BinOp {
                    op: Op::GtEq,
                    args: Box::new((Expr::Input("_root", vec!["file_version"]), Expr::Number(33))),
                },
                Expr::BinOp {
                    op: Op::Lt,
                    args: Box::new((Expr::Input("_root", vec!["file_version"]), Expr::Number(30))),
                },
            )),
        }
    }

    #[test]
    fn test_tokenizer_simple() {
        let mut t = Lexer::new("file_version >= 38");
        assert_eq!(t.next(), Some(Token::Ident("file_version")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::GtEq)));
        assert_eq!(t.next(), Some(Token::Number(38)));
        assert_eq!(t.next(), None);
    }

    #[test]
    fn test_tokenizer_complex() {
        let mut t = Lexer::new("_root.file_version >= 33 or _root.file_version < 30");
        assert_eq!(t.next(), Some(Token::Ident("_root")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::Dot)));
        assert_eq!(t.next(), Some(Token::Ident("file_version")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::GtEq)));
        assert_eq!(t.next(), Some(Token::Number(33)));
        assert_eq!(t.next(), Some(Token::BinOp(Op::Or)));
        assert_eq!(t.next(), Some(Token::Ident("_root")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::Dot)));
        assert_eq!(t.next(), Some(Token::Ident("file_version")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::Lt)));
        assert_eq!(t.next(), Some(Token::Number(30)));
        assert_eq!(t.next(), None);
    }

    #[test]
    fn test_shunting_yard_simple() {
        let mut yard = ShuntingYard::from(Lexer::new("file_version >= 38"));
        assert_eq!(yard.next(), Some(Token::Ident("file_version")));
        assert_eq!(yard.next(), Some(Token::Number(38)));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::GtEq)));
        assert_eq!(yard.next(), None);
    }

    #[test]
    fn test_shunting_yard_complex() {
        let mut yard = ShuntingYard::from(Lexer::new(
            "_root.file_version >= 33 or _root.file_version < 30",
        ));
        assert_eq!(yard.next(), Some(Token::Ident("_root")));
        assert_eq!(yard.next(), Some(Token::Ident("file_version")));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::Dot)));
        assert_eq!(yard.next(), Some(Token::Number(33)));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::GtEq)));
        assert_eq!(yard.next(), Some(Token::Ident("_root")));
        assert_eq!(yard.next(), Some(Token::Ident("file_version")));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::Dot)));
        assert_eq!(yard.next(), Some(Token::Number(30)));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::Lt)));
        assert_eq!(yard.next(), Some(Token::BinOp(Op::Or)));
        assert_eq!(yard.next(), None);
    }

    #[test]
    fn test_expr_vm_simple() {
        let mut vm = ExprVM::new();
        vm.exec(Token::Ident("file_version"));
        assert_eq!(vm.stack(), &[Expr::Input("file_version", vec![])]);
        vm.exec(Token::Number(38));
        assert_eq!(
            vm.stack(),
            &[Expr::Input("file_version", vec![]), Expr::Number(38)]
        );
        vm.exec(Token::BinOp(Op::GtEq));
        assert_eq!(
            vm.stack(),
            &[Expr::BinOp {
                op: Op::GtEq,
                args: Box::new((Expr::Input("file_version", vec![]), Expr::Number(38)))
            }]
        );
    }

    #[test]
    fn test_expr_vm_complex() {
        let mut vm = ExprVM::new();
        vm.exec(Token::Ident("_root"));
        vm.exec(Token::Ident("file_version"));
        vm.exec(Token::BinOp(Op::Dot));
        vm.exec(Token::Number(33));
        vm.exec(Token::BinOp(Op::GtEq));
        vm.exec(Token::Ident("_root"));
        vm.exec(Token::Ident("file_version"));
        vm.exec(Token::BinOp(Op::Dot));
        vm.exec(Token::Number(30));
        vm.exec(Token::BinOp(Op::Lt));
        vm.exec(Token::BinOp(Op::Or));
        assert_eq!(vm.stack(), &[complex_expr()]);
    }

    #[test]
    fn test_parse_expr_simple() {
        assert_eq!(
            parse_expr("file_version >= 38"),
            Ok(Expr::BinOp {
                op: Op::GtEq,
                args: Box::new((Expr::Input("file_version", vec![]), Expr::Number(38)))
            })
        );
    }

    #[test]
    fn test_parse_expr_complex() {
        assert_eq!(parse_expr(COMPLEX_EXPR_STR), Ok(complex_expr()));
    }
}
