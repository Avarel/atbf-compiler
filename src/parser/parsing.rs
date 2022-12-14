use chumsky::prelude::*;

use crate::common::{BaseOp, CmpOp, UnOp};
use crate::parser::lang_parse::{self, Exp, Span};
use crate::parser::lang_parse::{Spanned, SpannedExp};

use super::lang_parse::CoreOp;
// use std::{collections::HashMap, env, fmt, fs};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OpToken {
    // arithmetic
    Add,
    Sub,
    // logical
    Not,
    And,
    Or,
    // comparisons
    Eq,
    Neq,
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
                OpToken::Not => "!",
                OpToken::Eq => "==",
                OpToken::Neq => "!=",
                OpToken::Lt => "<",
                OpToken::Le => "<=",
                OpToken::Gt => ">",
                OpToken::Ge => ">=",
                OpToken::And => "&&",
                OpToken::Or => "||",
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
    let op = choice((
        just("+").to(OpToken::Add),
        just("-").to(OpToken::Sub),
        just("!=").to(OpToken::Neq),
        just("!").to(OpToken::Not),
        just("==").to(OpToken::Eq),
        just("<=").to(OpToken::Le),
        just(">=").to(OpToken::Ge),
        just("<").to(OpToken::Lt),
        just(">").to(OpToken::Gt),
        just("&&").to(OpToken::And),
        just("||").to(OpToken::Or),
    ))
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
    let token = choice((num, op, assign, ctrl, ident)).recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

pub fn expr_parser() -> impl Parser<Token, SpannedExp, Error = Simple<Token>> {
    let delimiter_recovery = nested_delimiters(
        Token::Ctrl('('),
        Token::Ctrl(')'),
        [
            (Token::Ctrl('['), Token::Ctrl(']')),
            (Token::Ctrl('{'), Token::Ctrl('}')),
        ],
        |span| Spanned {
            inner: Exp::Error,
            span,
        },
    );

    let token_spanner = |token: Token, span: Span| Spanned::new(token, span);
    let spanner = |expr: Exp, span: Span| Spanned::new(expr, span);

    recursive(|expr: Recursive<Token, SpannedExp, _>| {
        let ident = select! { Token::Ident(ident) => ident.clone() }.labelled("identifier");

        let raw_expr = {
            let val_expr = select! {
                Token::Bool(x) => lang_parse::Exp::Bool(x),
                Token::Num(n) => lang_parse::Exp::Int(n.parse().unwrap()),
            }
            .labelled("value")
            .map_with_span(spanner);

            let var_expr = ident.map(Exp::Var).map_with_span(spanner);

            // (raw_expr)
            let paren_expr = expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

            // !raw_expr
            let not_expr = just(Token::Op(OpToken::Not))
                .map_with_span(token_spanner)
                .then(expr.clone())
                .map(|(n, arg)| {
                    let span = n.span.start..arg.span.end;
                    Spanned::new(
                        Exp::UnOp {
                            op: UnOp::Not,
                            arg: Box::new(arg),
                        },
                        span,
                    )
                });

            // -raw_expr
            let neg_expr = just(Token::Op(OpToken::Sub))
                .map_with_span(token_spanner)
                .then(expr.clone())
                .map(|(n, arg)| {
                    let span = n.span.start..arg.span.end;
                    Spanned::new(
                        Exp::UnOp {
                            op: UnOp::Negate,
                            arg: Box::new(arg),
                        },
                        span,
                    )
                });

            // 'Atoms' are expressions that contain no ambiguity
            let atom = choice((val_expr, var_expr, paren_expr, neg_expr, not_expr))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(delimiter_recovery.clone());

            // expr [, expr[,]]
            let items = expr
                .clone()
                .separated_by(just(Token::Ctrl(',')))
                .allow_trailing();

            // Function calls have very high precedence so we prioritise them
            let call = ident
                .map_with_span(|id, span: Span| Spanned::new(id, span))
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span: Span| Spanned::new(args, span)),
                )
                .map(|(f, args)| {
                    let span = f.span.start..args.span.end;
                    Spanned::new(
                        Exp::Call {
                            name: f.inner,
                            args: args.inner,
                        },
                        span,
                    )
                });

            let base = call.or(atom);

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
            let op = choice((
                just(Token::Op(OpToken::Add)).to(BaseOp::Add),
                just(Token::Op(OpToken::Sub)).to(BaseOp::Sub),
            ));
            let sum = base
                .clone()
                .then(op.then(base).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;
                    Spanned::new(
                        Exp::BinOp {
                            op: CoreOp::Base(op),
                            left: Box::new(a),
                            right: Box::new(b),
                        },
                        span,
                    )
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = choice((
                just(Token::Op(OpToken::Eq)).to(CmpOp::Eq),
                just(Token::Op(OpToken::Neq)).to(CmpOp::Neq),
                just(Token::Op(OpToken::Le)).to(CmpOp::Le),
                just(Token::Op(OpToken::Lt)).to(CmpOp::Lt),
                just(Token::Op(OpToken::Ge)).to(CmpOp::Ge),
                just(Token::Op(OpToken::Gt)).to(CmpOp::Gt),
            ));
            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.span.start..b.span.end;
                    Spanned::new(
                        Exp::BinOp {
                            op: CoreOp::Cmp(op),
                            left: Box::new(a),
                            right: Box::new(b),
                        },
                        span,
                    )
                });

            let and = compare
                .clone()
                .then(just(Token::Op(OpToken::And)).then(compare).repeated())
                .foldl(|a, (_, b)| {
                    let span = a.span.start..b.span.end;
                    Spanned::new(
                        Exp::BinOp {
                            op: CoreOp::And,
                            left: Box::new(a),
                            right: Box::new(b),
                        },
                        span,
                    )
                });

            let or = and
                .clone()
                .then(just(Token::Op(OpToken::Or)).then(and).repeated())
                .foldl(|a, (_, b)| {
                    let span = a.span.start..b.span.end;
                    Spanned::new(
                        Exp::BinOp {
                            op: CoreOp::Or,
                            left: Box::new(a),
                            right: Box::new(b),
                        },
                        span,
                    )
                });

            or
        };

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(delimiter_recovery);

        // A let expression
        let let_expr = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Assign))
            .then(raw_expr.clone().or(block.clone()))
            .map(|(name, val)| Exp::Let {
                var: name,
                expr: Box::new(val),
            })
            .map_with_span(spanner);

        let if_expr = recursive(|if_| {
            just(Token::If)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(block.clone().or(if_))
                        .or_not(),
                )
                .map_with_span(|((cond, a), b), span: Span| {
                    Spanned::new(
                        Exp::If {
                            cond: Box::new(cond),
                            then_: Box::new(a),
                            else_: Box::new(match b {
                                Some(b) => b,
                                // If an `if` expression has no trailing `else` block, we magic up one that just produces null
                                None => Spanned::new(Exp::Void, span.clone()),
                            }),
                        },
                        span,
                    )
                })
        });

        // Both blocks and `if` are 'block expressions' and can appear in the place of statements
        let block_expr = choice((block, if_expr, let_expr)).labelled("block");

        block_expr
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|a, b| {
                if let Some(b) = b {
                    let span = a.span.start..b.span.end;
                    Spanned::new(
                        match b.inner {
                            Exp::Block { mut body } => Exp::Block {
                                body: {
                                    body.insert(0, a);
                                    body
                                },
                            },
                            _ => Exp::Block { body: vec![a, b] },
                        },
                        span,
                    )
                } else {
                    a
                }
            })
    })
}
