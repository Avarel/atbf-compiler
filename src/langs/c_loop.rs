use std::{
    collections::VecDeque,
    fmt::{Pointer, Write},
};

use crate::common::{CmpOp, CoreOp, UnOp, VarName};

pub use super::l_mon::Atm;

impl std::fmt::Display for Atm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atm::Void => f.write_str("void"),
            Atm::Bool(b) => b.fmt(f),
            Atm::Int(i) => i.fmt(f),
            Atm::Var(v) => write!(f, "var({})", v),
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
                f.write_char('(')?;
                if !args.is_empty() {
                    args[0].fmt(f)?;
                    for arg in &args[1..] {
                        arg.fmt(f)?;
                    }
                }
                f.write_char(')')
            }
            Exp::BinOp { op, left, right } => todo!(),
            Exp::UnOp { op, arg } => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Assign { var: VarName, expr: Exp },
    Call { name: VarName, args: Vec<Atm> },
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

#[derive(Clone, Debug)]
pub struct Block {
    pub body: VecDeque<Stmt>,
    pub end: BlockEnd,
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
