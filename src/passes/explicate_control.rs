use std::collections::HashMap;

use crate::common::UnOp;
use crate::common::VarName;

use crate::langs::c_loop as lout;
use crate::langs::l_mon as lin;

use lout::Atm;
use lout::Block;
use lout::BlockEnd;
use lout::CoreOp;
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

// let rec explicate_assign (e : L.exp) (v : var) (tl : tail) : tail =
//   match e with
//   | Let (v', e1, e2) ->
//       (* Chain the first let bind as an assign, then explicate the
//          body as the tail. *)
//       explicate_assign e1 v' @@ explicate_assign e2 v tl
//   | If (cond, then', else') ->
//       (* (let (y (if e1 e2 e3)) tl) *)
//       (* INTO (if e1 (let (y e2) tl) (let (y e3) tl)) *)
//       let tl_goto = create_block tl in
//       explicate_pred cond
//         (explicate_assign then' v @@ Goto tl_goto)
//         (explicate_assign else' v @@ Goto tl_goto)
//   | Atm _ | Prim _ -> Seq (Assign (v, convert_exp e), tl)
//   | While _ | SetBang _ ->
//       explicate_assign (Atm Void) v @@ explicate_effect e tl
//   | Begin (body, ret) ->
//       List.fold_right explicate_effect body @@ explicate_assign ret v tl

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
            let mut tail = explicate_assign(var, body.pop().unwrap(), tail, state);
            for exp in body.into_iter().rev() {
                tail = explicate_effect(exp, tail, state);
            }
            tail
        }
        lin::Exp::If { cond, then_, else_ } => {
            let tail_goto = Block::tail(BlockEnd::Goto(state.create_block(tail)));
            explicate_pred(
                *cond,
                explicate_assign(var.clone(), *then_, tail_goto.clone(), state),
                explicate_assign(var, *else_, tail_goto, state),
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

// and explicate_pred (e : L.exp) (then_tl : tail) (else_tl : tail) : tail =
//   (* Explicate pred but carries a boolean truth value to compare against
//      for simple atomic cases. *)
//   let rec aux (b : bool) (e : L.exp) (then_tl : tail) (else_tl : tail) : tail =
//     match e with
//     | Atm (Bool t) -> if t == b then then_tl else else_tl
//     | Atm a ->
//         (* (if x then_tl else_tl) *)
//         (* INTO (if (eq? x true) then_tl else_tl) *)
//         let arg1 = convert_atom a in
//         let jump_then = create_block then_tl in
//         let jump_else = create_block else_tl in
//         IfStmt { op = `Eq; arg1; arg2 = Bool b; jump_else; jump_then }
//     | Prim (`Not, [ arg ]) -> aux (not b) (Atm arg) then_tl else_tl
//     | Prim ((#cmp_op as op), [ arg1; arg2 ]) ->
//         (* Simple if statement emit *)
//         let arg1 = convert_atom arg1 in
//         let arg2 = convert_atom arg2 in
//         let jump_then = create_block then_tl in
//         let jump_else = create_block else_tl in
//         IfStmt { op; arg1; arg2; jump_else; jump_then }
//     | Let (v, e1, e2) ->
//         (* (if (let (x e1) e2) then_tl else_tl) *)
//         (* INTO (let (x e1) (if e2 then_tl else_tl)) *)
//         explicate_assign e1 v @@ aux b e2 then_tl else_tl
//     | If (cond, then', else') ->
//         (* (if (if e1 e2 e3) then_tl else_tl) *)
//         (* INTO (if e1 (if e2 then_tl else_tl) (if e3 then_tl else_tl)) *)
//         let jump_then = create_block then_tl in
//         let jump_else = create_block else_tl in
//         aux b cond
//           (aux b then' (Goto jump_then) (Goto jump_else))
//           (aux b else' (Goto jump_then) (Goto jump_else))
//     | Begin (body, ret) ->
//         List.fold_right explicate_effect body @@ aux b ret then_tl else_tl
//     | Prim _ | While _ | SetBang _ -> failwith "Not a test expression"
//   in
//   aux true e then_tl else_tl
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
        lin::Exp::If { cond, then_, else_ } => {
            let then_label = state.create_block(then_tail);
            let else_label = state.create_block(else_tail);
            let goto_then = Block::tail(BlockEnd::Goto(then_label));
            let goto_else = Block::tail(BlockEnd::Goto(else_label));
            explicate_pred(
                *cond,
                explicate_pred(*then_, goto_then.clone(), goto_else.clone(), state),
                explicate_pred(*else_, goto_then.clone(), goto_else.clone(), state),
                state,
            )
        }

        lin::Exp::Block { mut body } => {
            let mut tail = explicate_pred(body.pop().unwrap(), then_tail, else_tail, state);
            for exp in body.into_iter().rev() {
                tail = explicate_effect(exp, tail, state);
            }
            tail
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

// and explicate_effect (e : L.exp) (tl : tail) : tail =
//   match e with
//   | Prim (`Read, []) -> Seq (PrimS (`Read, []), tl)
//   | Prim (`Print, [ a ]) -> Seq (PrimS (`Print, [ convert_atom a ]), tl)
//   | Atm _ | Prim _ -> tl
//   | SetBang (var, exp) -> explicate_assign exp var tl
//   (* If we encoutner a Begin here, the return value is being ignored. *)
//   | Begin (body, ret) ->
//       List.fold_right explicate_effect body @@ explicate_effect ret tl
//   | If (cond, then_exp, else_exp) ->
//       let tl_goto = create_block tl in
//       explicate_pred cond
//         (explicate_effect then_exp @@ Goto tl_goto)
//         (explicate_effect else_exp @@ Goto tl_goto)
//   | While (test, body) ->
//       let loop_label = Label (!fresh ~base:"loop" ~sep:"_") in
//       let loop_body = explicate_effect body @@ Goto loop_label in
//       let loop = explicate_pred test loop_body tl in
//       begin
//         basic_blocks := LabelMap.add loop_label loop !basic_blocks;
//         Goto loop_label
//       end
//   | Let (var, value, tl_exp) ->
//       explicate_assign value var @@ explicate_effect tl_exp tl
fn explicate_effect(expr: lin::Exp, tail: Block, state: &mut ExplicateState) -> Block {
    match expr {
        lin::Exp::Atm(_) | lin::Exp::BinOp { .. } | lin::Exp::UnOp { .. } => tail,
        lin::Exp::Call { name, args } => tail.push(Stmt::Call { name, args }),
        lin::Exp::Block { body } => {
            let mut tail = tail;
            for exp in body.into_iter().rev() {
                tail = explicate_effect(exp, tail, state);
            }
            tail
        }
        lin::Exp::If { cond, then_, else_ } => {
            let tail = state.create_block(tail);
            let tail_goto = Block::tail(BlockEnd::Goto(tail));
            explicate_pred(
                *cond,
                explicate_effect(*then_, tail_goto.clone(), state),
                explicate_effect(*else_, tail_goto, state),
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

// and explicate_tail (e : L.exp) : tail =
//   match e with
//   | Let (v, e1, e2) -> explicate_assign e1 v @@ explicate_tail e2
//   | If (cond, then', else') ->
//       explicate_pred cond (explicate_tail then') (explicate_tail else')
//   | Atm _ | Prim _ -> Return (convert_exp e)
//   | While _ | SetBang _ -> explicate_effect e (Return (Atm Void))
//   | Begin (body, ret) ->
//       List.fold_right explicate_effect body (explicate_tail ret)
fn explicate_tail(e: lin::Exp, state: &mut ExplicateState) -> Block {
    match e {
        lin::Exp::Atm(_)
        | lin::Exp::Call { .. }
        | lin::Exp::BinOp { .. }
        | lin::Exp::UnOp { .. } => Block::tail(BlockEnd::Return(convert_exp(e))),
        lin::Exp::Block { mut body } => {
            let mut tail = explicate_tail(body.pop().unwrap(), state);
            for exp in body.into_iter().rev() {
                tail = explicate_effect(exp, tail, state);
            }
            tail
        }
        lin::Exp::If { cond, then_, else_ } => explicate_pred(
            *cond,
            explicate_tail(*then_, state),
            explicate_tail(*else_, state),
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
