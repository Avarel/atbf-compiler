extern crate pom;

use pom::parser::*;
use pom::Error;

use crate::common::BinOp;
use crate::common::CmpOp;
use crate::common::UnOp;

use super::lang_parse;
use super::lang_parse::Exp;
use super::lang_parse::Spanned;
use super::lexer::Token;

/// Success when current input symbol equals `t`.
pub fn sym<'a>(t: Token<'a>) -> Parser<'a, Spanned<Token<'a>>, Spanned<Token<'a>>> {
    Parser::new(move |input: &'a [Spanned<Token<'a>>], start: usize| {
        if let Some(st @ Spanned { inner: s, .. }) = input.get(start) {
            if t == *s {
                Ok((st.clone(), start + 1))
            } else {
                Err(Error::Mismatch {
                    message: format!("expect: {}, found: {}", t, s),
                    position: start,
                })
            }
        } else {
            Err(Error::Incomplete)
        }
    })
}

pub fn one_of<'a>(t: &'a [Token<'a>]) -> Parser<'a, Spanned<Token<'a>>, Spanned<Token<'a>>> {
    Parser::new(move |input: &'a [Spanned<Token<'a>>], start: usize| {
        if let Some(st @ Spanned { inner: s, .. }) = input.get(start) {
            if t.into_iter().any(|t| t == s) {
                Ok((st.clone(), start + 1))
            } else {
                Err(Error::Mismatch {
                    message: format!("expect: {:?}, found: {}", t, s),
                    position: start,
                })
            }
        } else {
            Err(Error::Incomplete)
        }
    })
}

pub fn num<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<i64>> {
    Parser::new(move |input: &[Spanned<Token>], start: usize| {
        if let Some(st @ Spanned { inner: s, .. }) = input.get(start) {
            if let Token::Num(i) = s {
                Ok((Spanned::new(*i, st.span.clone()), start + 1))
            } else {
                Err(Error::Mismatch {
                    message: format!("expected number, found: {}", s),
                    position: start,
                })
            }
        } else {
            Err(Error::Incomplete)
        }
    })
}

pub fn ident<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<&'a str>> {
    Parser::new(move |input: &[Spanned<Token>], start: usize| {
        if let Some(st @ Spanned { inner: s, .. }) = input.get(start) {
            if let Token::Ident(i) = s {
                Ok((Spanned::new(*i, st.span.clone()), start + 1))
            } else {
                Err(Error::Mismatch {
                    message: format!("expected number, found: {}", s),
                    position: start,
                })
            }
        } else {
            Err(Error::Incomplete)
        }
    })
}

fn atom<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    sym(Token::True).map(|Spanned { inner: _, span }| Spanned::new(Exp::Bool(true), span))
        | sym(Token::False).map(|Spanned { inner: _, span }| Spanned::new(Exp::Bool(false), span))
        | num().map(|Spanned { inner, span }| Spanned::new(Exp::Int(inner), span))
        | ident().map(|Spanned { inner, span }| Spanned::new(Exp::Var(inner.to_owned()), span))
        | (sym(Token::LParen) * call(expr) - sym(Token::RParen))
        | call(block)
}

// /// Parse separated list.
// pub fn list<'a, I, O, U>(
//     parser: Parser<'a, I, O>,
//     separator: Parser<'a, I, U>,
// ) -> Parser<'a, I, Vec<O>>
// where
//     O: 'a,
//     U: 'a,
// {
//     Parser::new(move |input: &'a [I], start: usize| {
//         let mut items = vec![];
//         let mut pos = start;
//         if let Ok((first_item, first_pos)) = (parser.method)(input, pos) {
//             items.push(first_item);
//             pos = first_pos;
//             while let Ok((_, sep_pos)) = (separator.method)(input, pos) {
//                 match (parser.method)(input, sep_pos) {
//                     Ok((more_item, more_pos)) => {
//                         items.push(more_item);
//                         pos = more_pos;
//                     }
//                     Err(e) => {
//                         return Err(match e {
//                             Error::Incomplete => Error::Incomplete,
//                             Error::Mismatch { message, position } => Error::Mismatch {
//                                 message,
//                                 position: position,
//                             },
//                             Error::Conversion { message, position } => Error::Conversion {
//                                 message,
//                                 position: position,
//                             },
//                             Error::Expect { message, inner, position } => Error::Expect {
//                                 message,
//                                 position: position,
//                                 inner,
//                             },
//                             Error::Custom { message, inner, position } => Error::Custom {
//                                 message,
//                                 position: position,
//                                 inner,
//                             },
//                         })
//                     }
//                 }
//             }
//         }
//         Ok((items, pos))
//     })
// }

fn func_call<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let expr_list = list(call(expr), sym(Token::Comma));
    (ident() + sym(Token::LParen) * expr_list - sym(Token::RParen)).map(|(ident, args)| {
        let span = ident.span.start..args.last().map(|a| a.span.end).unwrap_or(ident.span.end);
        Spanned::new(
            Exp::Call {
                name: ident.inner.to_owned(),
                args,
            },
            span,
        )
    })
}

fn primary<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    func_call() | call(if_expr) | atom()
}

fn factor<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let ops = one_of(&[Token::Not, Token::Minus]).map(|t| {
        Spanned::new(
            match t.inner {
                Token::Not => UnOp::Not,
                Token::Minus => UnOp::Negate,
                _ => unreachable!(),
            },
            t.span,
        )
    });
    (ops + primary()).map(|(op, arg)| {
        let span = op.span.start..arg.span.end;
        Spanned::new(
            Exp::UnOp {
                op: op.inner,
                arg: Box::new(arg),
            },
            span,
        )
    }) | primary()
}

fn sum<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let ops = one_of(&[Token::Plus, Token::Minus]).map(|t| match t.inner {
        Token::Plus => lang_parse::CoreOp::Bin(BinOp::Add),
        Token::Minus => lang_parse::CoreOp::Bin(BinOp::Sub),
        _ => unreachable!(),
    });
    (factor() + (ops + factor()).repeat(0..)).map(|(left, right)| {
        right.into_iter().fold(left, |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Spanned::new(
                Exp::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            )
        })
    })
}

fn comp<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let ops = one_of(&[
        Token::Eq,
        Token::Neq,
        Token::Ge,
        Token::Gt,
        Token::Le,
        Token::Lt,
    ])
    .map(|t| match t.inner {
        Token::Eq => lang_parse::CoreOp::Cmp(CmpOp::Eq),
        Token::Neq => lang_parse::CoreOp::Cmp(CmpOp::Neq),
        Token::Ge => lang_parse::CoreOp::Cmp(CmpOp::Ge),
        Token::Gt => lang_parse::CoreOp::Cmp(CmpOp::Gt),
        Token::Le => lang_parse::CoreOp::Cmp(CmpOp::Le),
        Token::Lt => lang_parse::CoreOp::Cmp(CmpOp::Lt),
        _ => unreachable!(),
    });
    (sum() + (ops + sum()).repeat(0..)).map(|(left, right)| {
        right.into_iter().fold(left, |left, (op, right)| {
            let span = left.span.start..right.span.end;
            Spanned::new(
                Exp::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            )
        })
    })
}

fn conjunct<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    (comp() + (sym(Token::And) * comp()).repeat(0..)).map(|(left, right)| {
        right.into_iter().fold(left, |left, right| {
            let span = left.span.start..right.span.end;
            Spanned::new(
                Exp::BinOp {
                    op: lang_parse::CoreOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            )
        })
    })
}

fn disjunct<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    (conjunct() + (sym(Token::Or) * conjunct()).repeat(0..)).map(|(left, right)| {
        right.into_iter().fold(left, |left, right| {
            let span = left.span.start..right.span.end;
            Spanned::new(
                Exp::BinOp {
                    op: lang_parse::CoreOp::Or,
                    left: Box::new(left),
                    right: Box::new(right),
                },
                span,
            )
        })
    })
}

fn expr<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    disjunct()
}

fn let_expr<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    (sym(Token::Let) + ident() - sym(Token::Assign) + expr()).map(|((t, id), exp)| {
        let span = t.span.start..exp.span.end;
        Spanned::new(
            Exp::Let {
                var: id.inner.to_owned(),
                expr: Box::new(exp),
            },
            span,
        )
    })
}

fn set_expr<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    (ident() - sym(Token::Assign) + expr()).map(|(id, exp)| {
        let span = id.span.start..exp.span.end;
        Spanned::new(
            Exp::Set {
                var: id.inner.to_owned(),
                expr: Box::new(exp),
            },
            span,
        )
    })
}

fn if_expr<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let else_clause = sym(Token::Else) * (call(block) | call(if_expr));
    (sym(Token::If) + expr() + call(block) + else_clause.opt()).map(
        |(((t, cond), then_), else_)| {
            let span = t.span.start..else_.as_ref().map(|e| e.span.end).unwrap_or(then_.span.end);
            Spanned::new(
                Exp::If {
                    cond: Box::new(cond),
                    else_: else_.map(Box::new).unwrap_or_else(|| {
                        Box::new(Spanned::new(Exp::Void, then_.span.end..then_.span.end + 1))
                    }),
                    then_: Box::new(then_),
                },
                span,
            )
        },
    )
}

fn while_expr<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    (sym(Token::While) + expr() + call(block)).map(|((t, cond), body)| {
        let span = t.span.start..body.span.end;
        Spanned::new(
            Exp::While {
                cond: Box::new(cond),
                body: Box::new(body),
            },
            span,
        )
    })
}

fn sequence<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let semi_expr = let_expr() | set_expr() | expr();
    let free_stmt = if_expr() | while_expr() | call(block);

    let parse = (free_stmt - sym(Token::Semi).opt() + call(sequence).opt())
        | (semi_expr + (sym(Token::Semi) * call(sequence)).opt());
    parse.map(|(base, tail)| match tail {
        None => {
            let span = base.span.clone();
            Spanned::new(Exp::Block { body: vec![base] }, span)
        }
        Some(Spanned {
            inner: Exp::Block { mut body },
            ..
        }) => {
            let span = base.span.start..body.last().unwrap().span.end;
            body.insert(0, base);
            Spanned::new(Exp::Block { body }, span)
        }
        Some(tail) => {
            let span = base.span.start..tail.span.end;
            Spanned::new(
                Exp::Block {
                    body: vec![base, tail],
                },
                span,
            )
        }
    })
}

fn block<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    let void = (sym(Token::LBrace) + sym(Token::RBrace)).map(|(l, r)| {
        let span = l.span.start..r.span.end;
        Spanned::new(Exp::Void, span)
    });

    let block = (sym(Token::LBrace) + sequence() + sym(Token::RBrace)).map(|((l, body), r)| {
        let span = l.span.start..r.span.end;
        Spanned::new(body.inner, span)
    });

    void | block
}

pub fn parser<'a>() -> Parser<'a, Spanned<Token<'a>>, Spanned<Exp>> {
    block() - end()
}