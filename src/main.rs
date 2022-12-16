use std::process::exit;

use ariadne::{Color, Label, Report, ReportKind, Source};
use logos::Logos;
use parser::{
    lang_parse::{self, Spanned},
    lexer::Token,
};

pub mod common;
pub mod langs;
pub mod parser;
pub mod passes;

fn main() {
    let ast = parse().expect("Parse failure");
    let ast = passes::despan(ast);
    let ast = passes::shrink(ast);
    let ast = passes::uniquify(ast);
    let ast = passes::flatten(ast);
    let ast = passes::uncover_get(ast);
    let ast = passes::remove_complex(ast);
    let ast = passes::explicate_control(ast);
    // dbg!(ast);

    for (lbl, block) in ast {
        println!("{lbl}:");
        println!("{block}\n");
    }
}

fn lex(src: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<&str>>> {
    let mut lexer = Token::lexer(&src);

    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    while let Some(token) = lexer.next() {
        if let Token::Error = token {
            errors.push(Spanned::new(lexer.slice(), lexer.span()));
        }
        tokens.push(Spanned::new(token, lexer.span()));
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

fn parse() -> Option<Spanned<lang_parse::Exp>> {
    let file_name = &std::env::args().nth(1).expect("Expected file argument");
    let src = std::fs::read_to_string(file_name).expect("Failed to read file");

    let tokens = match lex(&src) {
        Ok(tokens) => tokens,
        Err(errors) => {
            for Spanned { inner: str, span } in errors.into_iter() {
                Report::build(ReportKind::Error, file_name, 0)
                    .with_message(format!("Unknown token \"{str}\""))
                    .with_label(
                        Label::new((file_name, span))
                            .with_message("Unknown token")
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((file_name, Source::from(&src)))
                    .unwrap();
            }
            exit(1)
        }
    };

    let ast = match parser::parser::parser().parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => match e {
            pom::Error::Incomplete => todo!(),
            pom::Error::Mismatch { message, position }
            | pom::Error::Conversion { message, position }
            | pom::Error::Expect {
                message, position, ..
            }
            | pom::Error::Custom {
                message, position, ..
            } => {
                Report::build(ReportKind::Error, file_name, 0)
                    .with_message(&message)
                    .with_label(
                        Label::new((file_name, tokens[position].span.clone()))
                            .with_message(&message)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((file_name, Source::from(&src)))
                    .unwrap();
                exit(1)
            }
        },
    };

    Some(ast)
}
