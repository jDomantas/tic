use std::collections::HashMap;

#[derive(Eq, PartialEq, Debug, Hash, Clone, Copy)]
pub(crate) struct Name {
    pub(crate) id: u32,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Program<'a> {
    pub(crate) debug_info: HashMap<Name, &'a str>,
    pub(crate) values: HashMap<Name, Expr>,
    pub(crate) order: Vec<Name>,
    pub(crate) exports: Vec<Export>,
}

#[derive(Debug, Clone)]
pub(crate) struct Export {
    pub(crate) name: Name,
    pub(crate) public_name: String,
}

#[derive(Debug, Clone)]
pub(crate) enum Expr {
    Bool(bool),
    Int(u64),
    Name(Name),
    Call(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Lambda(Name, Box<Expr>),
    Match(Box<Expr>, Vec<Branch>),
    Construct(Ctor, Vec<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
    Trap(String),
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ctor {
    pub(crate) name: Name,
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub(crate) ctor: Ctor,
    pub(crate) bindings: Vec<Name>,
    pub(crate) value: Expr,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Copy, Clone)]
pub(crate) enum Op {
    Add,
    Subtract,
    Multiply,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEqual,
}
