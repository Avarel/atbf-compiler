use crate::common::{PrimOp, VarName};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
    Prim {
        op: PrimOp,
        args: Vec<Exp>,
    },
    SetBang {
        var: VarName,
        expr: Box<Exp>,
    },
    Begin {
        body: Vec<Exp>,
        tail: Box<Exp>,
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
}