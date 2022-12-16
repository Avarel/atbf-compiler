use crate::common::VarName;

use crate::langs::l_get as lin;
use crate::langs::l_mon as lout;
use lout::Atm;
type In = lin::Exp;
type Out = lout::Exp;

struct TmpState {
    counter: u32,
}

impl TmpState {
    fn new() -> Self {
        Self { counter: 0 }
    }

    fn get_name(&mut self) -> String {
        self.counter += 1;
        format!("$tmp.{}", self.counter)
    }
}

struct Binding(VarName, lout::Exp);

fn rco_atom_box(e: Box<In>, state: &mut TmpState) -> Box<Out> {
    Box::new(rco_exp(*e, state))
}

fn rco_atom(e: In, state: &mut TmpState) -> (Vec<Binding>, Atm) {
    match e {
        In::Void => (vec![], Atm::Void),
        In::Bool(b) => (vec![], Atm::Bool(b)),
        In::Int(i) => (vec![], Atm::Int(i)),
        In::Var(v) => (vec![], Atm::Var(v)),
        In::Block { body } => {
            let body_ = body.into_iter().map(|e| rco_exp(e, state)).collect();
            let tmp = state.get_name();
            (
                vec![Binding(tmp.clone(), Out::Block { body: body_ })],
                Atm::Var(tmp),
            )
        }
        In::If { cond, yes, no } => {
            let cond = rco_atom_box(cond, state);
            let yes = rco_atom_box(yes, state);
            let no = rco_atom_box(no, state);
            let tmp = state.get_name();
            (
                vec![Binding(tmp.clone(), Out::If { cond, yes, no })],
                Atm::Var(tmp),
            )
        }
        In::While { cond, body } => {
            let cond = rco_atom_box(cond, state);
            let body = rco_atom_box(body, state);
            let tmp = state.get_name();
            (
                vec![Binding(tmp.clone(), Out::While { cond, body })],
                Atm::Var(tmp),
            )
        }
        In::Let { var, expr } => (vec![Binding(var, rco_exp(*expr, state))], Atm::Void),
        In::Set { var, expr } => {
            let expr = rco_atom_box(expr, state);
            let tmp = state.get_name();
            (vec![Binding(tmp, Out::Set { var, expr })], Atm::Void)
        }
        In::Get { var } => {
            let tmp = state.get_name();
            (
                vec![Binding(tmp.clone(), Out::Atm(lout::Atm::Var(var)))],
                Atm::Var(tmp),
            )
        }
        In::Call { name, args } => {
            let (bindings, atoms): (Vec<Vec<Binding>>, Vec<Atm>) =
                args.into_iter().map(|e| rco_atom(e, state)).unzip();
            let mut bindings = bindings.into_iter().flatten().collect::<Vec<_>>();
            let tmp = state.get_name();
            bindings.push(Binding(tmp.clone(), Out::Call { name, args: atoms }));
            (bindings, Atm::Var(tmp))
        }
        In::BinOp { op, left, right } => {
            let (mut lbind, latm) = rco_atom(*left, state);
            let (mut rbind, ratm) = rco_atom(*right, state);
            let tmp = state.get_name();
            lbind.append(&mut rbind);
            lbind.push(Binding(
                tmp.clone(),
                Out::BinOp {
                    op,
                    left: latm,
                    right: ratm,
                },
            ));
            (lbind, Atm::Var(tmp))
        }
        In::UnOp { op, arg } => {
            let (mut bind, atm) = rco_atom(*arg, state);
            let tmp = state.get_name();
            bind.push(Binding(tmp.clone(), Out::UnOp { op, arg: atm }));
            (bind, Atm::Var(tmp))
        }
    }
}

fn convert_bindings(binds: Vec<Binding>) -> Vec<Out> {
    binds
        .into_iter()
        .map(|Binding(var, expr)| Out::Let {
            var,
            expr: Box::new(expr),
        })
        .collect()
}

fn rco_exp(e: In, state: &mut TmpState) -> Out {
    match e {
        In::Void => Out::Atm(Atm::Void),
        In::Bool(b) => Out::Atm(Atm::Bool(b)),
        In::Int(i) => Out::Atm(Atm::Int(i)),
        In::Var(v) => Out::Atm(Atm::Var(v)),
        In::Block { body } => Out::Block {
            body: body
                .into_iter()
                .flat_map(|e| match rco_exp(e, state) {
                    Out::Block { body } => body,
                    e => vec![e],
                })
                .collect(),
        },
        In::If {
            cond,
            yes: then_,
            no: else_,
        } => {
            let cond = rco_atom_box(cond, state);
            let then_ = rco_atom_box(then_, state);
            let else_ = rco_atom_box(else_, state);
            Out::If {
                cond,
                yes: then_,
                no: else_,
            }
        }
        In::While { cond, body } => {
            let cond = rco_atom_box(cond, state);
            let body = rco_atom_box(body, state);
            Out::While { cond, body }
        }
        In::Let { var, expr } => Out::Let {
            var,
            expr: rco_atom_box(expr, state),
        },
        In::Set { var, expr } => {
            let expr = rco_atom_box(expr, state);
            Out::Set { var, expr }
        }
        In::Get { var } => Out::Atm(Atm::Var(var)),
        In::Call { name, args } => {
            let (bindings, atoms): (Vec<Vec<Binding>>, Vec<lout::Atm>) =
                args.into_iter().map(|e| rco_atom(e, state)).unzip();
            let bindings = bindings.into_iter().flatten().collect::<Vec<_>>();
            let mut body = convert_bindings(bindings);
            body.push(Out::Call { name, args: atoms });
            Out::Block { body }
        }
        In::BinOp { op, left, right } => {
            let (mut lbind, latm) = rco_atom(*left, state);
            let (mut rbind, ratm) = rco_atom(*right, state);
            lbind.append(&mut rbind);
            let mut body = convert_bindings(lbind);
            body.push(Out::BinOp {
                op,
                left: latm,
                right: ratm,
            });
            Out::Block { body }
        }
        In::UnOp { op, arg } => {
            let (bind, atm) = rco_atom(*arg, state);
            let mut body = convert_bindings(bind);
            body.push(Out::UnOp { op, arg: atm });
            Out::Block { body }
        }
    }
}

pub fn remove_complex(exp: In) -> Out {
    let mut state = TmpState::new();
    rco_exp(exp, &mut state)
}
