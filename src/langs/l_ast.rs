use crate::common::{VarName, UnOp, BinOp};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
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
        args: Vec<Self>,
    },
    BinOp {
        op: BinOp,
        left: Box<Self>,
        right: Box<Self>
    },
    UnOp {
        op: UnOp,
        arg: Box<Self>,
    },
}