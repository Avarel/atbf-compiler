use crate::common::map_box;

use crate::langs::lang_while as lin;
use crate::langs::lang_while_shrink as lout;

fn convert_op(op: lin::CoreOp) -> lout::CoreOp {
    type In = lin::CoreOp;
    type Out = lout::CoreOp;
    match op {
        In::Base(b) => Out::Base(b),
        In::Func(f) => Out::Func(f),
        In::Cmp(c) => Out::Cmp(c),
        In::Or => unreachable!(),
        In::And => unreachable!(),
    }
}

fn shrink_box(b: Box<lin::Exp>) -> Box<lout::Exp> {
    map_box(b, shrink)
}

pub fn shrink(exp: lin::Exp) -> lout::Exp {
    type In = lin::Exp;
    type Out = lout::Exp;
    match exp {
        In::Void => Out::Void,
        In::Bool(b) => Out::Bool(b),
        In::Int(i) => Out::Int(i),
        In::Var(v) => Out::Var(v),
        In::BinOp {
            op: lin::CoreOp::And,
            left,
            right,
        } => Out::If {
            cond: shrink_box(left),
            then_: shrink_box(right),
            else_: Box::new(Out::Bool(false)),
        },
        In::BinOp {
            op: lin::CoreOp::Or,
            left,
            right,
        } => Out::If {
            cond: shrink_box(left),
            then_: Box::new(Out::Bool(true)),
            else_: shrink_box(right),
        },
        In::Call { name, args } => Out::Call {
            name,
            args: args.into_iter().map(shrink).collect(),
        },
        In::BinOp { op, left, right } => Out::BinOp {
            op: convert_op(op),
            left: shrink_box(left),
            right: shrink_box(right),
        },
        In::UnOp { op, arg } => Out::UnOp {
            op,
            arg: shrink_box(arg),
        },
        In::SetBang { var, expr } => Out::SetBang {
            var,
            expr: shrink_box(expr),
        },
        In::Block { body } => Out::Block {
            body: body.into_iter().map(shrink).collect(),
        },
        In::If { cond, then_, else_ } => Out::If {
            cond: shrink_box(cond),
            then_: shrink_box(then_),
            else_: shrink_box(else_),
        },
        In::While { cond, body } => Out::While {
            cond: shrink_box(cond),
            body: shrink_box(body),
        },
        In::Let { var, expr } => Out::Let {
            var,
            expr: shrink_box(expr),
        },
        In::Set { var, expr } => Out::Set {
            var,
            expr: shrink_box(expr),
        },
    }
}
