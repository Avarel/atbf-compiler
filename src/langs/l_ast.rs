use crate::common::{VarName, CmpOp, BinOp, UnOp};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Base(BinOp),
    Cmp(CmpOp),
    Or,
    And
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
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
        args: Vec<Exp>,
    },
    BinOp {
        op: CoreOp,
        left: Box<Exp>,
        right: Box<Exp>
    },
    UnOp {
        op: UnOp,
        arg: Box<Exp>,
    },
}