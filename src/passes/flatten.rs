use crate::{langs::l_shrink as lang};

fn flatten_box(b: Box<lang::Exp>) -> Box<lang::Exp> {
    Box::new(flatten(*b))
}

pub fn flatten(exp: lang::Exp) -> lang::Exp {
    type In = lang::Exp;
    type Out = lang::Exp;
    match exp {
        In::Void => Out::Void,
        In::Bool(b) => Out::Bool(b),
        In::Int(i) => Out::Int(i),
        In::Var(v) => Out::Var(v),
        In::Call { name, args } => Out::Call {
            name,
            args: args.into_iter().map(flatten).collect(),
        },
        In::BinOp { op, left, right } => Out::BinOp {
            op,
            left: flatten_box(left),
            right: flatten_box(right),
        },
        In::UnOp { op, arg } => Out::UnOp {
            op,
            arg: flatten_box(arg),
        },
        In::Block { mut body } => {
            assert!(!body.is_empty());
            if body.len() == 1 {
                flatten(body.remove(0))
            } else {
                let mut new_body = Vec::new();
                for exp in body {
                    match flatten(exp) {
                        lang::Exp::Block { body } => new_body.extend(body.into_iter().map(flatten)),
                        e => new_body.push(flatten(e)),
                    }
                }
                Out::Block { body: new_body }
            }
        }
        In::If { cond, yes: then_, no: else_ } => Out::If {
            cond: flatten_box(cond),
            yes: flatten_box(then_),
            no: flatten_box(else_),
        },
        In::While { cond, body } => Out::While {
            cond: flatten_box(cond),
            body: flatten_box(body),
        },
        In::Let { var, expr } => Out::Let {
            var,
            expr: flatten_box(expr),
        },
        In::Set { var, expr } => Out::Set {
            var,
            expr: flatten_box(expr),
        },
    }
}
