//! IR-level peephole optimisation.

use crate::middle::ir::{Inst, Ir, Label};

pub fn optimize(ir: Ir) -> Ir {
    ir.rewrite(fuse)
}

/// The fusion applicable at `code[i]`, or `None` to leave the instruction as is.
fn fuse(code: &[Inst], i: usize) -> Option<(Inst, usize)> {
    use Inst::*;

    let a0 = code[i];
    let a1 = code.get(i + 1).copied();
    let a2 = code.get(i + 2).copied();
    let a3 = code.get(i + 3).copied();

    // `dst = a + b`
    if let (GetLocal(a), Some(GetLocal(b)), Some(Add), Some(SetLocalPop(dst))) = (a0, a1, a2, a3) {
        return Some((SetLocalAddLocalLocal(dst, a, b), 4));
    }

    // branch if `local <cmp> const`
    if let (GetLocal(l), Some(PushConstant(c)), Some(cmp), Some(JumpIfFalse(t))) = (a0, a1, a2, a3) {
        if let Some(make) = local_const_branch(cmp) {
            return Some((make(t, l, c), 4));
        }
    }
    // branch if `const <cmp> local`
    if let (PushConstant(c), Some(GetLocal(l)), Some(cmp), Some(JumpIfFalse(t))) = (a0, a1, a2, a3) {
        if let Some(make) = local_const_branch(flip(cmp)) {
            return Some((make(t, l, c), 4));
        }
    }

    // Any conditional branch
    if let Some(JumpIfFalse(t)) = a1 {
        if let Some(make) = compare_branch(a0) {
            return Some((make(t), 2));
        }
    }

    // `local +/- const`
    if let (GetLocal(l), Some(PushConstant(c)), Some(Add)) = (a0, a1, a2) {
        return Some((AddLocalConst(l, c), 3));
    }
    if let (GetLocal(l), Some(PushConstant(c)), Some(Subtract)) = (a0, a1, a2) {
        return Some((SubLocalConst(l, c), 3));
    }

    // `const +/- local`
    if let (PushConstant(c), Some(GetLocal(l)), Some(Add)) = (a0, a1, a2) {
        return Some((AddLocalConst(l, c), 3));
    }
    if let (PushConstant(c), Some(GetLocal(l)), Some(Subtract)) = (a0, a1, a2) {
        return Some((SubConstLocal(c, l), 3));
    }

    None
}

/// A comparison's false-branch jump (jump taken when the comparison is false).
fn compare_branch(cmp: Inst) -> Option<fn(Label) -> Inst> {
    use Inst::*;
    Some(match cmp {
        LessThan => JumpIfGe,
        LessThanEqual => JumpIfGt,
        GreaterThan => JumpIfLe,
        GreaterThanEqual => JumpIfLt,
        Equal => JumpIfNeq,
        NotEqual => JumpIfEq,
        _ => return None,
    })
}

fn local_const_branch(cmp: Inst) -> Option<fn(Label, u8, u8) -> Inst> {
    use Inst::*;
    Some(match cmp {
        LessThan => JumpIfGeLocalConst,
        LessThanEqual => JumpIfGtLocalConst,
        GreaterThan => JumpIfLeLocalConst,
        GreaterThanEqual => JumpIfLtLocalConst,
        _ => return None,
    })
}

fn flip(cmp: Inst) -> Inst {
    use Inst::*;
    match cmp {
        LessThan => GreaterThan,
        LessThanEqual => GreaterThanEqual,
        GreaterThan => LessThan,
        GreaterThanEqual => LessThanEqual,
        other => other,
    }
}
