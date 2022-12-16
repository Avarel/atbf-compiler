use std::collections::HashMap;

use crate::common::CoreOp;
use crate::common::UnOp;
use crate::common::VarName;

use crate::langs::c_loop as lout;
use crate::langs::l_mon as lin;

use lout::Atm;
use lout::Block;
use lout::BlockEnd;
use lout::Exp;
use lout::Stmt;

struct ExplicateState {
    block_counter: u32,
    loop_counter: u32,
    blocks: HashMap<String, Block>,
}

impl ExplicateState {
    fn new() -> Self {
        Self {
            block_counter: 0,
            loop_counter: 0,
            blocks: HashMap::new(),
        }
    }

    fn get_loop_label(&mut self) -> String {
        self.loop_counter += 1;
        format!("loop_{}", self.loop_counter)
    }

    fn create_block(&mut self, tail: Block) -> String {
        let label = {
            self.block_counter += 1;
            format!("block_{}", self.block_counter)
        };
        self.add_block(label.clone(), tail);
        label
    }

    fn add_block(&mut self, label: String, block: Block) {
        self.blocks.insert(label, block);
    }
}

fn convert_exp(exp: lin::Exp) -> Exp {
    match exp {
        lin::Exp::Atm(atm) => Exp::Atm(atm),

        lin::Exp::Call { name, args } => Exp::Call { name, args },
        lin::Exp::BinOp { op, left, right } => Exp::BinOp { op, left, right },
        lin::Exp::UnOp { op, arg } => Exp::UnOp { op, arg },

        lin::Exp::Block { .. }
        | lin::Exp::If { .. }
        | lin::Exp::While { .. }
        | lin::Exp::Let { .. }
        | lin::Exp::Set { .. } => unreachable!(),
    }
}

fn explicate_assign(
    var: VarName,
    expr: lin::Exp,
    tail: Block,
    state: &mut ExplicateState,
) -> Block {
    match expr {
        lin::Exp::Atm(_)
        | lin::Exp::Call { .. }
        | lin::Exp::BinOp { .. }
        | lin::Exp::UnOp { .. } => tail.push(Stmt::Assign {
            var,
            expr: convert_exp(expr),
        }),
        lin::Exp::Block { mut body } => {
            let tail = explicate_assign(var, body.pop().unwrap(), tail, state);
            explicate_effect(lin::Exp::Block { body }, tail, state)
        }
        lin::Exp::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let tail_goto = Block::tail(BlockEnd::Goto(state.create_block(tail)));
            explicate_pred(
                *cond,
                explicate_assign(var.clone(), *then_branch, tail_goto.clone(), state),
                explicate_assign(var, *else_branch, tail_goto, state),
                state,
            )
        }
        lin::Exp::While { .. } | lin::Exp::Set { .. } | lin::Exp::Let { .. } => explicate_assign(
            var,
            lin::Exp::Atm(Atm::Void),
            explicate_effect(expr, tail, state),
            state,
        ),
    }
}

fn explicate_pred(
    cond: lin::Exp,
    then_tail: Block,
    else_tail: Block,
    state: &mut ExplicateState,
) -> Block {
    match cond {
        lin::Exp::Atm(a) => {
            let then_label = state.create_block(then_tail);
            let else_label = state.create_block(else_tail);
            Block::tail(BlockEnd::IfStmt {
                cmp_op: crate::common::CmpOp::Eq,
                left: a,
                right: Atm::Bool(true),
                then_label,
                else_label,
            })
        }
        lin::Exp::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let then_label = state.create_block(then_tail);
            let else_label = state.create_block(else_tail);
            let goto_then = Block::tail(BlockEnd::Goto(then_label));
            let goto_else = Block::tail(BlockEnd::Goto(else_label));
            explicate_pred(
                *cond,
                explicate_pred(*then_branch, goto_then.clone(), goto_else.clone(), state),
                explicate_pred(*else_branch, goto_then, goto_else, state),
                state,
            )
        }

        lin::Exp::Block { mut body } => {
            let tail = explicate_pred(body.pop().unwrap(), then_tail, else_tail, state);
            explicate_effect(lin::Exp::Block { body }, tail, state)
        }

        lin::Exp::BinOp {
            op: CoreOp::Cmp(op),
            left,
            right,
        } => {
            let then_label = state.create_block(then_tail);
            let else_label = state.create_block(else_tail);
            Block::tail(BlockEnd::IfStmt {
                cmp_op: op,
                left,
                right,
                then_label,
                else_label,
            })
        }
        lin::Exp::UnOp { op: UnOp::Not, arg } => {
            explicate_pred(lin::Exp::Atm(arg), else_tail, then_tail, state)
        }

        lin::Exp::While { .. }
        | lin::Exp::Call { .. }
        | lin::Exp::Let { .. }
        | lin::Exp::Set { .. }
        | lin::Exp::BinOp { .. }
        | lin::Exp::UnOp { .. } => {
            panic!("Not a test expression")
        }
    }
}

fn explicate_effect(expr: lin::Exp, tail: Block, state: &mut ExplicateState) -> Block {
    match expr {
        lin::Exp::Atm(_) | lin::Exp::BinOp { .. } | lin::Exp::UnOp { .. } => tail,
        lin::Exp::Call { name, args } => tail.push(Stmt::Call { name, args }),
        lin::Exp::Block { body } => body
            .into_iter()
            .rfold(tail, |tail, expr| explicate_effect(expr, tail, state)),
        lin::Exp::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let tail = state.create_block(tail);
            let tail_goto = Block::tail(BlockEnd::Goto(tail));
            explicate_pred(
                *cond,
                explicate_effect(*then_branch, tail_goto.clone(), state),
                explicate_effect(*else_branch, tail_goto, state),
                state,
            )
        }
        lin::Exp::While { cond, body } => {
            let loop_label = state.get_loop_label();
            let loop_goto = Block::tail(BlockEnd::Goto(loop_label.clone()));
            let loop_body = explicate_effect(*body, loop_goto.clone(), state);
            let loop_expr = explicate_pred(*cond, loop_body, tail, state);
            state.add_block(loop_label, loop_expr);
            loop_goto
        }
        lin::Exp::Let { var, expr } | lin::Exp::Set { var, expr } => {
            explicate_assign(var, *expr, tail, state)
        }
    }
}

fn explicate_tail(e: lin::Exp, state: &mut ExplicateState) -> Block {
    match e {
        lin::Exp::Atm(_)
        | lin::Exp::Call { .. }
        | lin::Exp::BinOp { .. }
        | lin::Exp::UnOp { .. } => Block::tail(BlockEnd::Return(convert_exp(e))),
        lin::Exp::Block { mut body } => {
            let tail = explicate_tail(body.pop().unwrap(), state);
            explicate_effect(lin::Exp::Block { body }, tail, state)
        }
        lin::Exp::If {
            cond,
            then_branch,
            else_branch,
        } => explicate_pred(
            *cond,
            explicate_tail(*then_branch, state),
            explicate_tail(*else_branch, state),
            state,
        ),
        lin::Exp::Set { .. } | lin::Exp::While { .. } => {
            explicate_effect(e, Block::tail(BlockEnd::Return(Exp::Atm(Atm::Void))), state)
        }
        lin::Exp::Let { var, expr } => explicate_assign(
            var,
            *expr,
            Block::tail(BlockEnd::Return(Exp::Atm(Atm::Void))),
            state,
        ),
    }
}

pub fn explicate_control(e: lin::Exp) -> HashMap<String, Block> {
    let mut state = ExplicateState::new();
    let block = explicate_tail(e, &mut state);
    state.add_block(String::from("start"), block);
    state.blocks
}
