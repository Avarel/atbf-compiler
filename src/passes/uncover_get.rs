use std::collections::HashSet;

use crate::common::VarName;

use crate::langs::lang_get as lout;
use crate::langs::lang_shrink as lin;
type In = lin::Exp;
type Out = lout::Exp;

fn collect_set_vars(exp: &In) -> HashSet<VarName> {
    fn inner(exp: &In, set: &mut HashSet<VarName>) {
        match exp {
            In::Void => (),
            In::Bool(_) => (),
            In::Int(_) => (),
            In::Var(_) => (),
            In::Block { body } => body.into_iter().for_each(|a| inner(a, set)),
            In::If { cond, then_, else_ } => {
                inner(cond, set);
                inner(then_, set);
                inner(else_, set)
            }
            In::While { cond, body } => {
                inner(cond, set);
                inner(body, set)
            }
            In::Let { expr, .. } => inner(expr, set),
            In::Set { var, expr } => {
                set.insert(var.clone());
                inner(expr, set);
            }
            In::Call { args, .. } => args.into_iter().for_each(|a| inner(a, set)),
            In::BinOp { left, right, .. } => {
                inner(left, set);
                inner(right, set)
            }
            In::UnOp { arg, .. } => inner(arg, set),
        }
    }

    let mut set = HashSet::new();
    inner(exp, &mut set);
    set
}

fn uncover_box(b: Box<In>, set: &HashSet<VarName>) -> Box<Out> {
    Box::new(uncover_get_exp(*b, set))
}

fn uncover_get_exp(exp: In, set: &HashSet<VarName>) -> Out {
    match exp {
        In::Void => Out::Void,
        In::Bool(b) => Out::Bool(b),
        In::Int(i) => Out::Int(i),
        In::Var(var) => {
            if set.contains(&var) {
                Out::Get { var }
            } else {
                Out::Var(var)
            }
        }
        In::Block { body } => Out::Block {
            body: body.into_iter().map(|e| uncover_get_exp(e, set)).collect(),
        },
        In::If { cond, then_, else_ } => Out::If {
            cond: uncover_box(cond, set),
            then_: uncover_box(then_, set),
            else_: uncover_box(else_, set),
        },
        In::While { cond, body } => Out::While {
            cond: uncover_box(cond, set),
            body: uncover_box(body, set),
        },
        In::Let { var, expr } => Out::Let {
            var,
            expr: uncover_box(expr, set),
        },
        In::Set { var, expr } => Out::Set {
            var,
            expr: uncover_box(expr, set),
        },
        In::Call { name, args } => Out::Call {
            name,
            args: args.into_iter().map(|e| uncover_get_exp(e, set)).collect(),
        },
        In::BinOp { op, left, right } => Out::BinOp {
            op,
            left: uncover_box(left, set),
            right: uncover_box(right, set),
        },
        In::UnOp { op, arg } => Out::UnOp {
            op,
            arg: uncover_box(arg, set),
        },
    }
}

pub fn uncover_get(exp: lin::Exp) -> lout::Exp {
    let set = collect_set_vars(&exp);
    uncover_get_exp(exp, &set)
}
