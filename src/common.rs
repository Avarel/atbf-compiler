pub type VarName = String;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
    Negate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CoreOp {
    Arith(ArithOp),
    Cmp(CmpOp),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Arith(ArithOp),
    Cmp(CmpOp),
    Logic(LogicOp),
}

impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            ArithOp::Add => "+",
            ArithOp::Sub => "-",
        })
    }
}

impl std::fmt::Display for CmpOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            CmpOp::Eq => "==",
            CmpOp::Neq => "!=",
            CmpOp::Lt => "<",
            CmpOp::Le => "<=",
            CmpOp::Gt => ">",
            CmpOp::Ge => ">=",
        })
    }
}

impl std::fmt::Display for LogicOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            LogicOp::And => "&&",
            LogicOp::Or => "||",
        })
    }
}

impl std::fmt::Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            UnOp::Not => "!",
            UnOp::Negate => "-",
        })
    }
}

impl std::fmt::Display for CoreOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoreOp::Arith(o) => o.fmt(f),
            CoreOp::Cmp(o) => o.fmt(f),
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Arith(o) => o.fmt(f),
            BinOp::Cmp(o) => o.fmt(f),
            BinOp::Logic(o) => o.fmt(f),
        }
    }
}
