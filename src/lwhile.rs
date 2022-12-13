use crate::common::{VarName, CmpOp, BaseOp};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Base(BaseOp),
    Func(VarName),
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
    Prim {
        op: CoreOp,
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