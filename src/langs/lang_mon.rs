use crate::common::{UnOp, VarName};

pub use super::lang_shrink::CoreOp;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atm {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Atm(Atm),
    Block {
        body: Vec<Exp>,
    },
    If {
        cond: Box<Exp>,
        then_: Box<Exp>,
        else_: Box<Exp>,
    },
    While {
        cond: Box<Exp>,
        body: Box<Exp>,
    },
    Let {
        var: VarName,
        expr: Box<Exp>,
    },
    Set {
        var: VarName,
        expr: Box<Exp>,
    },
    Call {
        name: VarName,
        args: Vec<Atm>,
    },
    BinOp {
        op: CoreOp,
        left: Atm,
        right: Atm,
    },
    UnOp {
        op: UnOp,
        arg: Atm,
    },
}
