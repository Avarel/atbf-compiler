use crate::common::{VarName, CmpOp, BaseOp, UnOp};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Base(BaseOp),
    Func(VarName),
    Cmp(CmpOp),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
    SetBang {
        var: VarName,
        expr: Box<Exp>,
    },
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