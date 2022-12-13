pub type VarName = String;

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum BaseOp {
    Add,
    Sub,
    Not,
    Negate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrimOp {
    Base(BaseOp),
    Func(VarName),
    Cmp(CmpOp),
}