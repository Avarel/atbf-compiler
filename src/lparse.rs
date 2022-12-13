use crate::{
    common::{PrimOp, VarName},
    parsing::Span,
};

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(u64),
    Var(VarName),
    Prim {
        op: PrimOp,
        args: Vec<SpannedExp>,
    },
    SetBang {
        var: VarName,
        expr: Box<SpannedExp>,
    },
    Block(Block),
    If {
        cond: Box<SpannedExp>,
        then_: Box<SpannedExp>,
        else_: Box<SpannedExp>,
    },
    While {
        cond: Box<SpannedExp>,
        body: Box<SpannedExp>,
    },
    Let {
        var: VarName,
        expr: Box<SpannedExp>,
    },
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<SpannedExp>,
    pub tail: Box<SpannedExp>,
}

pub type SpannedExp = Spanned<Exp>;
