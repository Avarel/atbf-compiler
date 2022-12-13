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
    Neq,
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
}

