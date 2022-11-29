use std::rc::Rc;

pub(crate) use ticc_core::ir::{self as cir, Op};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum Name {
    Cir(cir::Name),
    Temp(u32),
}

impl From<cir::Name> for Name {
    fn from(v: cir::Name) -> Self {
        Self::Cir(v)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub(crate) stmts: Vec<Stmt>,
    pub(crate) trap: Option<String>,
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
    String(Rc<str>),
    Name(Name),
    Call(Box<Expr>, Vec<Expr>),
    Intrinsic(cir::Intrinsic, Vec<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Construct(cir::Name, Vec<Expr>),
    Lambda(Vec<Name>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub(crate) enum Stmt {
    If(Name, Expr, Block, Block),
    Match(Name, Name, Vec<Branch>),
    ValueDef(Name, Expr),
    Def(Def),
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) stmts: Vec<Stmt>,
    pub(crate) value: Box<BlockEnd>,
}

#[derive(Debug, Clone)]
pub(crate) enum BlockEnd {
    If(Expr, Block, Block),
    Match(Name, Vec<Branch>),
    Value(Expr),
    Trap(String),
}

impl From<Expr> for BlockEnd {
    fn from(v: Expr) -> Self {
        Self::Value(v)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub(crate) ctor: cir::Name,
    pub(crate) bindings: Vec<cir::Name>,
    pub(crate) body: Block,
}

#[derive(Debug, Clone)]
pub(crate) struct Def {
    pub(crate) name: Name,
    pub(crate) params: Vec<Name>,
    pub(crate) body: Block,
}
