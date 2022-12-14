use std::collections::HashMap;

use crate::langs::lang_shrink as lang;

#[derive(Clone)]
struct UniquifyState {
    counter: HashMap<String, i32>,
    m: HashMap<String, String>,
}

impl UniquifyState {
    fn new() -> Self {
        Self {
            counter: HashMap::new(),
            m: HashMap::new(),
        }
    }

    fn fresh_name(&mut self, base: String) -> String {
        let name = if let Some(count) = self.counter.get_mut(&base) {
            *count += 1;
            format!("{base}.{count}")
        } else {
            self.counter.insert(base.to_owned(), 1);
            format!("{base}.1")
        };
        self.m.insert(base, name.clone());
        name
    }

    fn get_name(&self, base: &str) -> String {
        self.m.get(base).expect("no name?").to_owned()
    }
}

fn uniq_box(b: Box<lang::Exp>, state: &mut UniquifyState) -> Box<lang::Exp> {
    Box::new(uniq(*b, state))
}

fn uniq(exp: lang::Exp, state: &mut UniquifyState) -> lang::Exp {
    type In = lang::Exp;
    type Out = lang::Exp;
    match exp {
        In::Void => Out::Void,
        In::Bool(b) => Out::Bool(b),
        In::Int(i) => Out::Int(i),
        In::Var(v) => Out::Var(state.get_name(&v)),
        In::Call { name, args } => Out::Call {
            name,
            args: args
                .into_iter()
                .map(|e| uniq(e, &mut state.clone()))
                .collect(),
        },
        In::BinOp { op, left, right } => Out::BinOp {
            op,
            left: uniq_box(left, state),
            right: uniq_box(right, state),
        },
        In::UnOp { op, arg } => Out::UnOp {
            op,
            arg: uniq_box(arg, state),
        },
        In::Block { body } => Out::Block {
            body: body.into_iter().map(|e| uniq(e, state)).collect(),
        },
        In::If { cond, then_, else_ } => Out::If {
            cond: uniq_box(cond, state),
            then_: uniq_box(then_, state),
            else_: uniq_box(else_, state),
        },
        In::While { cond, body } => Out::While {
            cond: uniq_box(cond, state),
            body: uniq_box(body, state),
        },
        In::Let { var, expr } => {
            let expr = uniq_box(expr, state);
            let var = state.fresh_name(var);
            Out::Let { var, expr }
        }
        In::Set { var, expr } => Out::Set {
            var: state.get_name(&var),
            expr: uniq_box(expr, state),
        },
    }
}

pub fn uniquify(exp: lang::Exp) -> lang::Exp {
    let mut state = UniquifyState::new();
    uniq(exp, &mut state)
}
