pub type VarName = String;

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    Add,
    Sub,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
    Negate,
}

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Arith(ArithOp),
    Cmp(CmpOp),
}

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum LogicOp {
    And,
    Or
}

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Arith(ArithOp),
    Cmp(CmpOp),
    Logic(LogicOp)
}

