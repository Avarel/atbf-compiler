pub type VarName = String;

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum BaseOp {
    Add,
    Sub,
}

#[derive(Clone, Copy,  Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
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

pub fn map_box<T, R>(b: Box<T>, f: impl FnOnce(T) -> R) -> Box<R> {
    Box::new(f(*b))
}