use std::{fmt, rc::Rc};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Name {
    pub(crate) id: u64,
}

#[derive(Debug, Hash, Clone)]
pub enum Expr {
    Bool(bool),
    Int(u64),
    String(Rc<str>),
    Name(Name),
    Call(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Intrinsic(Intrinsic, Vec<Expr>),
    Lambda(Vec<LambdaParam>, Box<Expr>),
    Match(Box<Expr>, Vec<Branch>),
    Construct(Name, Vec<Ty>, Vec<Expr>),
    Let(Name, Box<Expr>, Box<Expr>),
    LetRec(Name, Ty, Box<Expr>, Box<Expr>),
    Pi(TyVar, Box<Expr>),
    PiApply(Box<Expr>, Ty),
    Trap(String, Ty),
}

#[derive(Debug, Hash, Clone)]
pub struct LambdaParam {
    pub name: Name,
    pub ty: Ty,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct TyVar(pub(crate) u64);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty {
    Bool,
    Int,
    String,
    Var(TyVar),
    Named(Name, Vec<Ty>),
    Fn(Vec<Ty>, Box<Ty>),
    Abs(TyVar, Box<Ty>),
}

#[derive(Debug, Hash, Clone)]
pub struct Branch {
    pub ctor: Name,
    pub bindings: Vec<Name>,
    pub value: Expr,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Hash, Copy, Clone)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equal,
    NotEqual,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Hash, Copy, Clone)]
pub enum Intrinsic {
    StringLen,
    StringConcat,
    StringCharAt,
    StringSubstring,
    StringFromChar,
    IntToString,
}

#[derive(Debug, Hash, Clone)]
pub struct TyDef {
    pub name: Name,
    pub vars: Vec<TyVar>,
    pub cases: Vec<TyCase>,
}

#[derive(Debug, Hash, Clone)]
pub struct TyCase {
    pub name: Name,
    pub fields: Vec<Ty>,
}

#[derive(Debug, Hash, Clone)]
pub struct ValueDef {
    pub name: Name,
    pub export_name: Option<String>,
    pub ty: Ty,
    pub value: Expr,
}

#[derive(Debug, Hash, Clone)]
pub struct Program<'a> {
    pub names: NameGenerator<'a>,
    pub ty: TyGenerator,
    pub types: Vec<TyDef>,
    pub values: Vec<ValueDef>,
}

impl<'a> Program<'a> {
    pub fn new() -> Program<'a> {
        Program {
            names: NameGenerator::new(),
            ty: TyGenerator::new(),
            types: Vec::new(),
            values: Vec::new(),
        }
    }
}

#[derive(Debug, Hash, Clone)]
pub struct NameGenerator<'a> {
    names: Vec<&'a str>,
}

impl<'a> NameGenerator<'a> {
    pub fn new() -> NameGenerator<'a> {
        NameGenerator {
            names: vec![],
        }
    }

    pub fn next_synthetic(&mut self) -> Name {
        self.new_fresh("$")
    }

    pub fn new_fresh(&mut self, debug_name: &'a str) -> Name {
        self.names.push(debug_name);
        let id = self.names.len() as u64 - 1;
        Name { id }
    }

    pub fn new_copy(&mut self, original: Name) -> Name {
        let idx = original.id as usize;
        self.new_fresh(self.names[idx])
    }

    pub fn show_debug(&self, name: Name) -> impl fmt::Display + 'a {
        ShowName {
            name: self.names[name.id as usize],
            id: name.id,
        }
    }

    pub fn original_name(&self, name: Name) -> &'a str {
        self.names[name.id as usize]
    }

    pub(crate) fn max_id(&self) -> u64 {
        self.names.len() as u64 - 1
    }
}

#[derive(Debug, Hash, Clone)]
pub struct TyGenerator {
    next: TyVar,
}

impl TyGenerator {
    pub fn new() -> TyGenerator {
        TyGenerator { next: TyVar(0) }
    }

    pub fn next(&mut self) -> TyVar {
        let ty = self.next;
        self.next.0 += 1;
        ty
    }
}

struct ShowName<'a> {
    name: &'a str,
    id: u64,
}

impl fmt::Display for ShowName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}_{}", self.name, self.id))
    }
}
