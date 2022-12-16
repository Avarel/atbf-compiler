use std::collections::VecDeque;

use crate::common::{CmpOp, CoreOp, UnOp, VarName};

pub use super::l_mon::Atm;

impl std::fmt::Display for Atm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atm::Void => f.write_str("void"),
            Atm::Bool(b) => b.fmt(f),
            Atm::Int(i) => i.fmt(f),
            Atm::Var(v) => f.write_str(v),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Atm(Atm),
    Call { name: VarName, args: Vec<Atm> },
    BinOp { op: CoreOp, left: Atm, right: Atm },
    UnOp { op: UnOp, arg: Atm },
}

impl std::fmt::Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Exp::Atm(a) => a.fmt(f),
            Exp::Call { name, args } => {
                name.fmt(f)?;
                f.write_str("(")?;
                if !args.is_empty() {
                    args[0].fmt(f)?;
                    for arg in &args[1..] {
                        arg.fmt(f)?;
                    }
                }
                f.write_str(")")
            }
            Exp::BinOp { op, left, right } => write!(f, "{left} {op} {right}"),
            Exp::UnOp { op, arg } => write!(f, "{op}{arg}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Assign { var: VarName, expr: Exp },
    Call { name: VarName, args: Vec<Atm> },
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Assign { var, expr } => write!(f, "{var} := {expr}"),
            Stmt::Call { name, args } => {
                name.fmt(f)?;
                f.write_str("(")?;
                if !args.is_empty() {
                    args[0].fmt(f)?;
                    for arg in &args[1..] {
                        arg.fmt(f)?;
                    }
                }
                f.write_str(")")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum BlockEnd {
    Return(Exp),
    Goto(String),
    IfStmt {
        cmp_op: CmpOp,
        left: Atm,
        right: Atm,
        then_label: String,
        else_label: String,
    },
}

impl std::fmt::Display for BlockEnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockEnd::Return(e) => write!(f, "return {e}"),
            BlockEnd::Goto(l) => write!(f, "goto {l}"),
            BlockEnd::IfStmt {
                cmp_op,
                left,
                right,
                then_label,
                else_label,
            } => write!(
                f,
                "if {left} {cmp_op} {right} goto {then_label} else {else_label}"
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub body: VecDeque<Stmt>,
    pub end: BlockEnd,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.body {
            f.write_str("    ")?;
            stmt.fmt(f)?;
            f.write_str("\n")?;
        }
        f.write_str("    ")?;
        self.end.fmt(f)
    }
}

impl Block {
    pub fn tail(end: BlockEnd) -> Self {
        Self {
            body: VecDeque::new(),
            end,
        }
    }

    pub fn push(mut self, stmt: Stmt) -> Self {
        self.body.push_front(stmt);
        Self {
            body: self.body,
            end: self.end,
        }
    }
}
