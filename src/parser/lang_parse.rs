use crate::common::{VarName, BaseOp, CmpOp};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}
pub type Span = std::ops::Range<usize>;

impl<T> Spanned<T> {
    pub fn new(expr: T, span: Span) -> Self {
        Self { inner: expr, span }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(u64),
    Var(VarName),
    Prim {
        op: CoreOp,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Base(BaseOp),
    Func(VarName),
    Cmp(CmpOp),
    Or,
    And
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<SpannedExp>,
}

pub type SpannedExp = Spanned<Exp>;
