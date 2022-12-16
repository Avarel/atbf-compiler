use crate::common::{UnOp, VarName, CoreOp};

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
        body: Vec<Self>,
    },
    If {
        cond: Box<Self>,
        then_branch: Box<Self>,
        else_branch: Box<Self>,
    },
    While {
        cond: Box<Self>,
        body: Box<Self>,
    },
    Let {
        var: VarName,
        expr: Box<Self>,
    },
    Set {
        var: VarName,
        expr: Box<Self>,
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
