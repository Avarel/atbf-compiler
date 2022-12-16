use std::collections::HashMap;

use crate::langs::l_shrink as lang;

struct UniquifyState<'a> {
    parent: Option<&'a Self>,
    counter: HashMap<String, u32>,
    m: HashMap<String, String>,
}

impl<'a> UniquifyState<'a> {
    fn new() -> Self {
        Self {
            parent: None,
            counter: HashMap::new(),
            m: HashMap::new(),
        }
    }

    fn get_count(&self, base: &str) -> u32 {
        self.counter.get(base).copied().unwrap_or_else(|| {
            if let Some(parent) = self.parent {
                parent.get_count(base)
            } else {
                0
            }
        })
    }

    fn fresh_name(&mut self, base: String) -> String {
        let count = self.get_count(&base) + 1;
        self.counter.insert(base.to_owned(), count);
        let name = format!("{base}.{count}");
        self.m.insert(base, name.clone());
        name
    }

    fn get_name(&self, base: &str) -> String {
        self.m
            .get(base)
            .cloned()
            .unwrap_or_else(|| self.parent.expect("no name?").get_name(base))
    }

    fn scope_in(&'a self) -> Self {
        Self {
            parent: Some(self),
            counter: HashMap::new(),
            m: HashMap::new(),
        }
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
            args: args.into_iter().map(|e| uniq(e, state)).collect(),
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
        In::Block { body } => {
            let state = &mut state.scope_in();
            Out::Block {
                body: body.into_iter().map(|e| uniq(e, state)).collect(),
            }
        }
        In::If {
            cond,
            then_branch,
            else_branch,
        } => Out::If {
            cond: uniq_box(cond, state),
            then_branch: uniq_box(then_branch, state),
            else_branch: uniq_box(else_branch, state),
        },
        In::While { cond, body } => Out::While {
            cond: uniq_box(cond, state),
            body: uniq_box(body, state),
        },
        In::Let { var, expr } => {
            let var = state.fresh_name(var);
            let expr = uniq_box(expr, state);
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
