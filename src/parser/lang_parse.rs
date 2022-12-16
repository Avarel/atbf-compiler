use crate::common::{BinOp, UnOp, VarName};

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

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Void,
    Bool(bool),
    Int(i64),
    Var(VarName),
    Block {
        body: Vec<SpannedExp>,
    },
    If {
        cond: Box<SpannedExp>,
        then_branch: Box<SpannedExp>,
        else_branch: Box<SpannedExp>,
    },
    While {
        cond: Box<SpannedExp>,
        body: Box<SpannedExp>,
    },
    Let {
        var: VarName,
        expr: Box<SpannedExp>,
    },
    Set {
        var: VarName,
        expr: Box<SpannedExp>,
    },
    Call {
        name: VarName,
        args: Vec<SpannedExp>,
    },
    BinOp {
        op: BinOp,
        left: Box<SpannedExp>,
        right: Box<SpannedExp>,
    },
    UnOp {
        op: UnOp,
        arg: Box<SpannedExp>,
    },
    Error,
}

pub type SpannedExp = Spanned<Exp>;
