use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::Parser;
use parser::lang_parse::{self, Spanned};

use crate::passes::{despan::despan, shrink::shrink, uniquify::uniquify};

pub mod common;
pub mod langs;
pub mod parser;
pub mod passes;

fn main() {
    let ast = parse().expect("Parse failure");
    let ast = despan(ast);
    let ast = shrink(ast);
    let ast = uniquify(ast);
    dbg!(ast);
}

fn parse() -> Option<Spanned<lang_parse::Exp>> {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");

    let (tokens, errs) = parser::parsing::lexer().parse_recovery(src.as_str());

    // println!("{tokens:?}");

    let (ast, parse_errs) = if let Some(tokens) = tokens {
        //dbg!(tokens);
        let len = src.chars().count();
        let (ast, parse_errs) = parser::parsing::expr_parser()
            .parse_recovery(chumsky::Stream::from_iter(len..len + 1, tokens.into_iter()));

        (ast, parse_errs)
    } else {
        (None, Vec::new())
    };

    errs.into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .for_each(|e| {
            let report = Report::build(ReportKind::Error, (), e.span().start);

            let report = match e.reason() {
                chumsky::error::SimpleReason::Unclosed { span, delimiter } => report
                    .with_message(format!(
                        "Unclosed delimiter {}",
                        delimiter.fg(Color::Yellow)
                    ))
                    .with_label(
                        Label::new(span.clone())
                            .with_message(format!(
                                "Unclosed delimiter {}",
                                delimiter.fg(Color::Yellow)
                            ))
                            .with_color(Color::Yellow),
                    )
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Must be closed before this {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Unexpected => report
                    .with_message(format!(
                        "{}, expected {}",
                        if e.found().is_some() {
                            "Unexpected token in input"
                        } else {
                            "Unexpected end of input"
                        },
                        if e.expected().len() == 0 {
                            "something else".to_string()
                        } else {
                            e.expected()
                                .map(|expected| match expected {
                                    Some(expected) => expected.to_string(),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        }
                    ))
                    .with_label(
                        Label::new(e.span())
                            .with_message(format!(
                                "Unexpected token {}",
                                e.found()
                                    .unwrap_or(&"end of file".to_string())
                                    .fg(Color::Red)
                            ))
                            .with_color(Color::Red),
                    ),
                chumsky::error::SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                    Label::new(e.span())
                        .with_message(format!("{}", msg.fg(Color::Red)))
                        .with_color(Color::Red),
                ),
            };

            report.finish().print(Source::from(&src)).unwrap();
        });

    ast
}
