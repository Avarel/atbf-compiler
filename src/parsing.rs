use chumsky::prelude::*;

use crate::common::{BaseOp, PrimOp};
use crate::lparse::SpannedExp;
use crate::lparse::{self, Block};
// use std::{collections::HashMap, env, fmt, fs};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpToken {
    // arithmetic
    Add,
    Sub,
    Neg,
    // logical
    Negate,
    // comparisons
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

impl std::fmt::Display for OpToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OpToken::Add => "+",
                OpToken::Sub => "-",
                OpToken::Neg => "-",
                OpToken::Negate => "!",
                OpToken::Eq => "==",
                OpToken::Lt => "<",
                OpToken::Le => "<=",
                OpToken::Gt => ">",
                OpToken::Ge => ">=",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Bool(bool),
    Num(String),
    Op(OpToken),
    Ident(String),
    Let,
    Ctrl(char),
    If,
    While,
    Assign,
    Else,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bool(b) => write!(f, "{b}"),
            Token::Num(n) => write!(f, "{n}"),
            Token::Op(op) => write!(f, "{op}"),
            Token::Ident(id) => write!(f, "{id}"),
            Token::Let => write!(f, "let"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::If => write!(f, "if"),
            Token::While => write!(f, "while"),
            Token::Else => write!(f, "else"),
            Token::Assign => write!(f, "="),
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    // A parser for operators

    let op = just("+")
        .to(OpToken::Add)
        .or(just("-").to(OpToken::Sub))
        .or(just("!").to(OpToken::Negate))
        .or(just("==").to(OpToken::Eq))
        .or(just("<=").to(OpToken::Le))
        .or(just(">=").to(OpToken::Ge))
        .or(just("<").to(OpToken::Lt))
        .or(just(">").to(OpToken::Gt))
        .map(Token::Op);

    let assign = just("=").to(Token::Assign);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,").map(|c| Token::Ctrl(c));

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "let" => Token::Let,
        "if" => Token::If,
        "while" => Token::While,
        "else" => Token::Else,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num
        .or(op)
        .or(assign)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

pub fn expr_parser() -> impl Parser<Token, SpannedExp, Error = Simple<Token>> {
    recursive(|expr: Recursive<Token, SpannedExp, _>| {
        let raw_expr = recursive(|raw_expr| {
            let val = select! {
                Token::Bool(x) => lparse::Exp::Bool(x),
                Token::Num(n) => lparse::Exp::Int(n.parse().unwrap()),
            }
            .labelled("value");

            let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            // A let expression
            let let_ = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Assign))
                .then(raw_expr)
                // .then_ignore(just(Token::Ctrl(';')))
                // .then(expr.clone())
                .map(|(name, val)| lparse::Exp::Let {
                    var: name,
                    expr: Box::new(val),
                });

            // 'Atoms' are expressions that contain no ambiguity
            let atom = val
                .or(ident.map(lparse::Exp::Var))
                .or(let_)
                .map_with_span(|expr, span: Span| (expr, span))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (lparse::Exp::Error, span),
                ))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (lparse::Exp::Error, span),
                ));

            // Function calls have very high precedence so we prioritise them
            let call = ident
                .map_with_span(|id, span: Span| (id, span))
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span: Span| (args, span)),
                )
                .map(|(f, args)| {
                    let span = f.1.start..args.1.end;
                    (
                        lparse::Exp::Prim {
                            op: PrimOp::Func(f.0),
                            args: args.0,
                        },
                        span,
                    )
                });

            let base = atom.or(call);

            // // Product ops (multiply and divide) have equal precedence
            // let op = just(Token::Op("*".to_string()))
            //     .to(BinaryOp::Mul)
            //     .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            // let product = call
            //     .clone()
            //     .then(op.then(call).repeated())
            //     .foldl(|a, (op, b)| {
            //         let span = a.1.start..b.1.end;
            //         (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            //     });

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op(OpToken::Add))
                .to(BaseOp::Add)
                .or(just(Token::Op(OpToken::Sub)).to(BaseOp::Sub));
            let sum = base
                .clone()
                .then(op.then(base).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (
                        lparse::Exp::Prim {
                            op: PrimOp::Base(op),
                            args: vec![a, b],
                        },
                        span,
                    )
                });

            // // Comparison ops (equal, not-equal) have equal precedence
            // let op = just(Token::Op("==".to_string()))
            //     .to(BinaryOp::Eq)
            //     .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));
            // let compare = sum
            //     .clone()
            //     .then(op.then(sum).repeated())
            //     .foldl(|a, (op, b)| {
            //         let span = a.1.start..b.1.end;
            //         (Expr::Binary(Box::new(a), op, Box::new(b)), span)
            //     });

            // compare
            // atom
            sum
            // todo!()
        });

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (lparse::Exp::Error, span),
            ));

        let if_ = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((cond, a), b), span: Span| {
                    (
                        lparse::Exp::If {
                            cond: Box::new(cond),
                            then_: Box::new(a),
                            else_: Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => (lparse::Exp::Void, span.clone()),
                            }),
                        },
                        span,
                    )
                })
        });

        // // Both blocks and `if` are 'block expressions' and can appear in the place of statements
        let block_expr = block.or(if_).labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                // (
                //     match a.0 {
                //         lparse::Exp::Block(Block { mut body, tail }) => lparse::Exp::Block(Block {
                //             body: {
                //                 body.push(*tail);
                //                 body
                //             },
                //             tail: Box::new(b),
                //         }),
                //         e => lparse::Exp::Block(Block {
                //             body: vec![(e, a.1)],
                //             tail: Box::new(b),
                //         }),
                //     },
                //     span,
                // )
                (
                    match b.0 {
                        lparse::Exp::Block(Block { mut body, tail }) => lparse::Exp::Block(Block {
                            body: {
                                body.insert(0, a);
                                body
                            },
                            tail,
                        }),
                        e => lparse::Exp::Block(Block {
                            body: vec![a],
                            tail: Box::new((e, b.1)),
                        }),
                    },
                    span,
                )
            });

        block_chain
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|a, b| {
                if let Some(b) = b {
                    let span = a.1.start..b.1.end;
                    (
                        match b.0 {
                            lparse::Exp::Block(Block { mut body, tail }) => {
                                lparse::Exp::Block(Block {
                                    body: {
                                        body.insert(0, a);
                                        body
                                    },
                                    tail,
                                })
                            }
                            e => lparse::Exp::Block(Block {
                                body: vec![a],
                                tail: Box::new((e, b.1)),
                            }),
                        },
                        span,
                    )
                } else {
                    a
                }
            })
    })
}
