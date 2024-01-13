use std::collections::HashMap;

use crate::rt;


#[derive(Debug, Clone)]
pub(crate) enum Op {
    PushInt(u64),
    PushBool(bool),
    Pick(usize),
    SwapPop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
    Jump(Label),
    JumpIfFalse(Label),
    Return(usize),
    Call,
    Construct { tag: u64, fields: u32 },
    ConstructClosure { addr: Label, fields: u32 },
    Branch(Box<[Branch]>),
    Const(usize),
    Trap(Box<str>),
    StringLen,
    StringConcat,
    StringCharAt,
    StringSubstring,
    StringFromChar,
    IntToString,
    Label(Label),
    Exit,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub(crate) struct Label(pub(crate) u32);

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub(crate) tag: u64,
    pub(crate) dst: Label,
}

pub(crate) fn link(ops: Vec<Op>) -> Vec<rt::Op> {
    let mut positions = HashMap::new();
    let mut pos = 0;
    for op in &ops {
        if let &Op::Label(l) = op {
            if positions.insert(l, rt::CodeAddr(pos)).is_some() {
                panic!("duplicate label");
            }
        } else {
            pos += 1;
        }
    }
    let mut linked = Vec::with_capacity(pos);
    for op in ops {
        linked.push(match op {
            Op::PushInt(x) => rt::Op::PushInt(x),
            Op::PushBool(x) => rt::Op::PushBool(x),
            Op::Pick(x) => rt::Op::Pick(x),
            Op::SwapPop => rt::Op::SwapPop,
            Op::Add => rt::Op::Add,
            Op::Sub => rt::Op::Sub,
            Op::Mul => rt::Op::Mul,
            Op::Div => rt::Op::Div,
            Op::Mod => rt::Op::Mod,
            Op::Less => rt::Op::Less,
            Op::LessEq => rt::Op::LessEq,
            Op::Greater => rt::Op::Greater,
            Op::GreaterEq => rt::Op::GreaterEq,
            Op::Eq => rt::Op::Eq,
            Op::NotEq => rt::Op::NotEq,
            Op::Jump(x) => rt::Op::Jump(positions.get(&x).copied().unwrap_or_else(|| panic!("label {:?} is missing", x))),
            Op::JumpIfFalse(x) => rt::Op::JumpIfFalse(positions[&x]),
            Op::Return(x) => rt::Op::Return(x),
            Op::Call => rt::Op::Call,
            Op::Construct { tag, fields } => rt::Op::Construct { tag, fields },
            Op::ConstructClosure { addr, fields } => {
                let addr = positions[&addr];
                rt::Op::Construct { tag: addr.0 as u64 | rt::FN_TAG, fields }
            }
            Op::Branch(brs) => rt::Op::Branch(Box::new(brs
                .into_iter()
                .map(|b| rt::Branch {
                    tag: b.tag,
                    dst: positions[&b.dst],
                })
                .collect())),
            Op::Const(x) => rt::Op::Const(x),
            Op::Trap(msg) => rt::Op::Trap(Box::new(msg)),
            Op::StringLen => rt::Op::StringLen,
            Op::StringConcat => rt::Op::StringConcat,
            Op::StringCharAt => rt::Op::StringCharAt,
            Op::StringSubstring => rt::Op::StringSubstring,
            Op::StringFromChar => rt::Op::StringFromChar,
            Op::IntToString => rt::Op::IntToString,
            Op::Exit => rt::Op::Exit,
            Op::Label(_) => continue,
        });
    }
    linked
}
