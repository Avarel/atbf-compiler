use crate::common::BinOp;
use crate::common::CoreOp;
use crate::common::LogicOp;

use crate::langs::l_ast as lin;
use crate::langs::l_shrink as lout;

fn convert_op(op: BinOp) -> CoreOp {
    match op {
        BinOp::Arith(b) => CoreOp::Arith(b),
        BinOp::Cmp(c) => CoreOp::Cmp(c),
        BinOp::Logic(_) => unreachable!(),
    }
}

fn shrink_box(b: Box<lin::Exp>) -> Box<lout::Exp> {
    Box::new(shrink(*b))
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
            op: BinOp::Logic(LogicOp::And),
            left,
            right,
        } => Out::If {
            cond: shrink_box(left),
            then_branch: shrink_box(right),
            else_branch: Box::new(Out::Bool(false)),
        },
        In::BinOp {
            op: BinOp::Logic(LogicOp::Or),
            left,
            right,
        } => Out::If {
            cond: shrink_box(left),
            then_branch: Box::new(Out::Bool(true)),
            else_branch: shrink_box(right),
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
        In::Block { body } => Out::Block {
            body: body.into_iter().map(shrink).collect(),
        },
        In::If {
            cond,
            then_branch,
            else_branch,
        } => Out::If {
            cond: shrink_box(cond),
            then_branch: shrink_box(then_branch),
            else_branch: shrink_box(else_branch),
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
