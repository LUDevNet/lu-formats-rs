use std::str::FromStr;

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

impl<'a> Expr<'a> {
    fn try_from_token(token: Token<'a>) -> Result<Expr<'a>, Error> {
        match token {
            Token::Ident(i) => Ok(Expr::Input(i, vec![])),
            Token::Number(n) => Ok(Expr::Number(n)),
            _ => Err(Error::NoValueStart),
        }
    }
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

struct Tokenizer<'a> {
    rest: &'a str,
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

/// Invariant: Always one more expr than op
struct ExprBuilder<'a> {
    expr_stack: Vec<Expr<'a>>,
    op_stack: Vec<Op>,
}

impl<'a> ExprBuilder<'a> {
    fn new(expr: Expr<'a>) -> Self {
        Self {
            op_stack: vec![],
            expr_stack: vec![expr],
        }
    }

    fn can_merge(&mut self, new_op: Op) -> bool {
        self.op_stack.last().is_some_and(|&op| op < new_op)
    }

    fn push(&mut self, op: Op, expr: Expr<'a>) {
        if op == Op::Dot {
            let top = self.expr_stack.last_mut().unwrap();
            if let Expr::Input(field, _) = expr {
                if let Expr::Input(_, fields) = top {
                    fields.push(field);
                    return;
                }
            }
        }
        while self.can_merge(op) {
            self.merge_last();
        }
        self.op_stack.push(op);
        self.expr_stack.push(expr);
    }

    fn finish(&mut self) -> Expr<'a> {
        while !self.op_stack.is_empty() {
            self.merge_last();
        }
        self.expr_stack.pop().unwrap()
    }

    fn merge_last(&mut self) {
        let op = self.op_stack.pop().unwrap();
        let rhs = self.expr_stack.pop().unwrap();
        let lhs = self.expr_stack.pop().unwrap();
        self.expr_stack.push(Expr::BinOp {
            op,
            args: Box::new((lhs, rhs)),
        });
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(rest: &'a str) -> Self {
        Self { rest }
    }

    pub fn parse_expr(mut self) -> Result<Expr<'a>, Error> {
        let next_token = self.next().ok_or(Error::NoTokens)?;
        let expr = Expr::try_from_token(next_token)?;
        let mut stack = ExprBuilder::new(expr);

        Ok(loop {
            if let Some(t1) = self.next() {
                if let Token::BinOp(new_op) = t1 {
                    let t2 = self.next().ok_or(Error::NoValueStart)?;
                    let expr = Expr::try_from_token(t2)?;
                    stack.push(new_op, expr);
                } else {
                    return Err(Error::ValAfterExpr);
                }
            } else {
                break stack.finish();
            }
        })
    }
}

pub fn parse_expr(expr: &str) -> Result<Expr<'_>, Error> {
    Tokenizer::new(expr).parse_expr()
}

impl<'a> Iterator for Tokenizer<'a> {
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
    use crate::expr::{parse_expr, Expr, Op, Token};

    use super::Tokenizer;

    #[test]
    fn test_tokenizer_simple() {
        let mut t = Tokenizer::new("file_version >= 38");
        assert_eq!(t.next(), Some(Token::Ident("file_version")));
        assert_eq!(t.next(), Some(Token::BinOp(Op::GtEq)));
        assert_eq!(t.next(), Some(Token::Number(38)));
        assert_eq!(t.next(), None);
    }

    #[test]
    fn test_tokenizer_complex() {
        let mut t = Tokenizer::new("_root.file_version >= 33 or _root.file_version < 30");
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
        assert_eq!(
            parse_expr("_root.file_version >= 33 or _root.file_version < 30"),
            Ok(Expr::BinOp {
                op: Op::Or,
                args: Box::new((
                    Expr::BinOp {
                        op: Op::GtEq,
                        args: Box::new((
                            Expr::Input("_root", vec!["file_version"]),
                            Expr::Number(33)
                        ))
                    },
                    Expr::BinOp {
                        op: Op::Lt,
                        args: Box::new((
                            Expr::Input("_root", vec!["file_version"]),
                            Expr::Number(30)
                        ))
                    }
                ))
            })
        );
    }
}
