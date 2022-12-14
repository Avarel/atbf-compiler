use crate::common::{VarName, BaseOp, CmpOp, UnOp};

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
    Int(i64),
    Var(VarName),
    SetBang {
        var: VarName,
        expr: Box<SpannedExp>,
    },
    Block {
        body: Vec<SpannedExp>,
    },
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
    Call {
        name: VarName,
        args: Vec<SpannedExp>,
    },
    BinOp {
        op: CoreOp,
        left: Box<SpannedExp>,
        right: Box<SpannedExp>
    },
    UnOp {
        op: UnOp,
        arg: Box<SpannedExp>,
    },
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Base(BaseOp),
    Cmp(CmpOp),
    Or,
    And
}

pub type SpannedExp = Spanned<Exp>;
