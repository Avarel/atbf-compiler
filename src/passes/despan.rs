use crate::common::map_box;
use crate::parser::lang_parse::Spanned;

use crate::langs::lang as lout;
use crate::parser::lang_parse as lin;

fn convert_op(op: lin::CoreOp) -> lout::CoreOp {
    type In = lin::CoreOp;
    type Out = lout::CoreOp;
    match op {
        In::Base(b) => Out::Base(b),
        In::Cmp(c) => Out::Cmp(c),
        In::Or => Out::Or,
        In::And => Out::And,
    }
}

fn despan_box(b: Box<Spanned<lin::Exp>>) -> Box<lout::Exp> {
    map_box(b, despan)
}

pub fn despan(exp: Spanned<lin::Exp>) -> lout::Exp {
    type In = lin::Exp;
    type Out = lout::Exp;
    match exp.inner {
        In::Void => Out::Void,
        In::Bool(b) => Out::Bool(b),
        In::Int(i) => Out::Int(i),
        In::Var(v) => Out::Var(v),
        In::Block { body } => Out::Block {
            body: body.into_iter().map(despan).collect(),
        },
        In::If { cond, then_, else_ } => Out::If {
            cond: despan_box(cond),
            then_: despan_box(then_),
            else_: despan_box(else_),
        },
        In::While { cond, body } => Out::While {
            cond: despan_box(cond),
            body: despan_box(body),
        },
        In::Let { var, expr } => Out::Let {
            var,
            expr: despan_box(expr),
        },
        In::Set { var, expr } => Out::Set {
            var,
            expr: despan_box(expr)
        },
        In::Call { name, args } => Out::Call {
            name,
            args: args.into_iter().map(despan).collect(),
        },
        In::BinOp { op, left, right } => Out::BinOp {
            op: convert_op(op),
            left: despan_box(left),
            right: despan_box(right),
        },
        In::UnOp { op, arg } => Out::UnOp {
            op,
            arg: despan_box(arg),
        },
        In::Error => panic!("Cannot convert error code"),
    }
}
