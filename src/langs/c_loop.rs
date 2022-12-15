use std::collections::VecDeque;

use crate::common::{CmpOp, UnOp, VarName};

pub use super::l_shrink::CoreOp;

pub use super::l_mon::Atm;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp {
    Atm(Atm),
    Call { name: VarName, args: Vec<Atm> },
    BinOp { op: CoreOp, left: Atm, right: Atm },
    UnOp { op: UnOp, arg: Atm },
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
            end
        }
    }

    pub fn push(mut self, stmt: Stmt) -> Self {
        self.body.push_front(stmt);
        Self { body: self.body, end: self.end }
    }
}
