use std::{thread::panicking, collections::HashSet, fmt};

use ticc_core::ir;

use crate::heap::{Addr, Heap};

#[derive(Clone, Copy)]
pub enum Value {
    Int(u64),
    Ptr(Addr),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(x) => f.debug_tuple("Int").field(x).finish(),
            Self::Ptr(x) => f.debug_tuple("Ptr").field(&x.0).finish(),
        }
    }
}

struct Frame {
    ip: CodeAddr,
}

struct Runtime {
    heap: Heap,
    stack: Vec<Value>,
    ops: Vec<Op>,
    frames: Vec<Frame>,
    consts: Vec<Addr>,
}

pub fn eval_int_expr(expr: &ir::Expr) -> u64 {
    let mut emitter = Emitter {
        ops: Vec::new(),
        stack_size: 0,
    };
    let mut heap = Heap::new();
    let mut consts = Vec::new();
    let mut compiler = Compiler {
        emit: &mut emitter,
        consts: &mut consts,
        heap: &mut heap,
    };
    compiler.compile_expr(expr, &Env::Empty);
    emitter.op(Op::Return(0));
    let ops = emitter.ops;
    println!("compiled code:");
    for (i, op) in ops.iter().enumerate() {
        println!("{i:>3}  {op:?}");
    }
    let mut runtime = Runtime {
        heap,
        stack: Vec::new(),
        ops,
        frames: Vec::from([
            Frame { ip: CodeAddr(0) },
        ]),
        consts,
    };
    while runtime.frames.len() > 0 {
        runtime.tick();
    }
    match runtime.stack.as_slice() {
        &[Value::Int(x)] => x,
        _ => panic!("shite"),
    }
}

impl Runtime {
    fn run(&mut self) {
        while self.frames.len() > 0 {
            self.tick();
        }
    }

    fn tick(&mut self) {
        let ip = self.frames.last().unwrap().ip;
        let mut dst = CodeAddr(ip.0 + 1);
        println!("executing op at {}: {:?}, stack = {:?}", ip.0, self.ops[ip.0], self.stack);
        match self.ops[ip.0] {
            Op::PushInt(i) => {
                self.stack.push(Value::Int(i));
            }
            Op::PushBool(b) => {
                self.stack.push(Value::Int(u64::from(b)));
            }
            Op::Pick(depth) => {
                let value = self.stack.iter().rev().nth(depth).unwrap();
                self.stack.push(*value);
            }
            Op::SwapPop => {
                let top = self.stack.pop().unwrap();
                *self.stack.last_mut().unwrap() = top;
            }
            Op::Add => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(a.wrapping_add(b)));
            }
            Op::Sub => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(a.wrapping_sub(b)));
            }
            Op::Mul => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(a.wrapping_mul(b)));
            }
            Op::Div => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(a.checked_div(b).unwrap_or(0)));
            }
            Op::Mod => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(a.checked_rem(b).unwrap_or(a)));
            }
            Op::Less => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a < b)));
            }
            Op::LessEq => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a <= b)));
            }
            Op::Greater => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a > b)));
            }
            Op::GreaterEq => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a >= b)));
            }
            Op::Eq => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a == b)));
            }
            Op::NotEq => {
                let b = self.pop_int();
                let a = self.pop_int();
                self.stack.push(Value::Int(u64::from(a != b)));
            }
            Op::Jump(d) => {
                dst = d;
            }
            Op::JumpIfFalse(d) => {
                if self.pop_int() == 0 {
                    dst = d;
                }
            }
            Op::Return(pops) => {
                let return_value = self.stack.pop().unwrap();
                for _ in 0..pops {
                    self.stack.pop().unwrap();
                }
                self.stack.push(return_value);
                self.frames.pop();
                // don't update ip as the frame was already popped
                return;
            }
            Op::Call => {
                let call_addr = match self.stack.last().unwrap() {
                    &Value::Ptr(addr) => {
                        let (tag, fields) = self.heap.obj_with_fields(addr);
                        for field in fields {
                            self.stack.push(field);
                        }
                        CodeAddr(tag as usize)
                    }
                    _ => unreachable!(),
                };
                self.frames.last_mut().unwrap().ip = dst;
                self.frames.push(Frame {
                    ip: call_addr,
                });
                // don't update ip - we did that manually and then pushed a new frame
                return;
            }
            Op::Construct { tag, fields } => {
                let mut obj = self.heap.alloc_object(tag, fields);
                for i in (0..fields).rev() {
                    let value = self.stack.pop().unwrap();
                    match value {
                        Value::Int(x) => obj.put_int(i as usize, x),
                        Value::Ptr(x) => obj.put_ptr(i as usize, x),
                    }
                }
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::Branch(ref branches) => {
                let obj = match self.stack.pop().unwrap() {
                    Value::Ptr(x) => x,
                    _ => unreachable!(),
                };
                let (tag, fields) = self.heap.obj_with_fields(obj);
                'find_branch: {
                    for br in branches.iter() {
                        if br.tag == tag {
                            for field in fields {
                                self.stack.push(field);
                            }
                            dst = br.dst;
                            break 'find_branch;
                        }
                    }
                    panic!("no branch matched");
                }
            }
            Op::Const(idx) => {
                self.stack.push(Value::Ptr(self.consts[idx]));
            }
            Op::Trap(ref msg) => {
                todo!("got trap: {msg:?}");
            }
            Op::StringLen => {
                let ptr = self.pop_ptr();
                let (len, _) = self.heap.obj_with_fields(ptr);
                self.stack.push(Value::Int(u64::from(len)));
            }
            Op::Placeholder => unreachable!("placeholders should be replaced before end of compilation"),
        }
        self.frames.last_mut().unwrap().ip = dst;
    }

    fn pop_int(&mut self) -> u64 {
        match self.stack.pop().unwrap() {
            Value::Int(x) => x,
            _ => unreachable!(),
        }
    }

    fn pop_ptr(&mut self) -> Addr {
        match self.stack.pop().unwrap() {
            Value::Ptr(x) => x,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
enum Op {
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
    Jump(CodeAddr),
    JumpIfFalse(CodeAddr),
    Return(usize),
    Call,
    Construct { tag: u32, fields: u32 },
    Branch(Box<[Branch]>),
    Const(usize),
    Trap(Box<str>),
    StringLen,
    Placeholder,
}

#[derive(Debug, Clone)]
struct Branch {
    tag: u32,
    dst: CodeAddr,
    fields: u32,
}

#[derive(Debug, Clone, Copy)]
struct CodeAddr(usize);

struct Compiler<'a> {
    emit: &'a mut Emitter,
    consts: &'a mut Vec<Addr>,
    heap: &'a mut Heap,
}

impl Compiler<'_> {
    fn compile_expr(&mut self, expr: &ir::Expr, env: &Env<'_>) {
        match expr {
            ir::Expr::Bool(b) => self.emit.op(Op::PushBool(*b)),
            ir::Expr::Int(i) => self.emit.op(Op::PushInt(*i)),
            ir::Expr::String(s) => {
                let obj = self.heap.alloc_bytes(s.len() as u32, s.len() as u32);
                obj.bytes[..s.as_bytes().len()].copy_from_slice(s.as_bytes());
                self.consts.push(obj.addr);
                self.emit.op(Op::Const(self.consts.len() - 1));
            }
            ir::Expr::Name(n) => self.emit.pick_var(env, *n),
            ir::Expr::Call(f, args) => {
                for arg in args {
                    self.compile_expr(arg, env);
                }
                self.compile_expr(f, env);
                self.emit.op(Op::Call);
                // callee pops args, emitter does not get to see it
                self.emit.stack_size -= args.len();
            }
            ir::Expr::If(c, t, e) => {
                self.compile_expr(c, env);
                let branch = self.emit.placeholder(Op::JumpIfFalse(CodeAddr(0)));
                self.compile_expr(t, env);
                let true_finish = self.emit.placeholder(Op::Jump(CodeAddr(0)));
                self.emit.fill_placeholder(branch, Op::JumpIfFalse(self.emit.pos()));
                self.emit.stack_size -= 1;
                self.compile_expr(e, env);
                self.emit.fill_placeholder(true_finish, Op::Jump(self.emit.pos()));
            }
            ir::Expr::Op(a, op, b) => {
                self.compile_expr(a, env);
                self.compile_expr(b, env);
                self.emit.op(match op {
                    ir::Op::Add => Op::Add,
                    ir::Op::Subtract => Op::Sub,
                    ir::Op::Multiply => Op::Mul,
                    ir::Op::Divide => Op::Div,
                    ir::Op::Modulo => Op::Mod,
                    ir::Op::Less => Op::Less,
                    ir::Op::LessEq => Op::LessEq,
                    ir::Op::Greater => Op::Greater,
                    ir::Op::GreaterEq => Op::GreaterEq,
                    ir::Op::Equal => Op::Eq,
                    ir::Op::NotEqual => Op::NotEq,
                });
            }
            ir::Expr::Intrinsic(i, _) => {
                match i {
                    ir::Intrinsic::StringLen => self.emit.op(Op::StringLen),
                    ir::Intrinsic::StringConcat => self.emit.op(Op::Trap("stringConcat".into())),
                    ir::Intrinsic::StringCharAt => self.emit.op(Op::Trap("stringCharAt".into())),
                    ir::Intrinsic::StringSubstring => self.emit.op(Op::Trap("stringSubstring".into())),
                    ir::Intrinsic::StringFromChar => self.emit.op(Op::Trap("stringFromChar".into())),
                    ir::Intrinsic::IntToString => self.emit.op(Op::Trap("intToString".into())),
                }
            }
            ir::Expr::Lambda(p, e) => {
                let skip_over = self.emit.placeholder(Op::Jump(CodeAddr(0)));
                let captures = free_variables(expr);
                let addr = self.compile_function(&p, None, e, &captures);
                self.emit.fill_placeholder(skip_over, Op::Jump(self.emit.pos()));
                for &c in &captures {
                    self.emit.pick_var(env, c);
                }
                let tag = addr.0 as u32;
                self.emit.op(Op::Construct { tag, fields: captures.len() as u32 })
            }
            ir::Expr::Match(discr, branches) => {
                self.compile_expr(discr, env);
                let mut jumps = Vec::new();
                let mut table = Vec::with_capacity(branches.len());
                let dispatch = self.emit.placeholder(Op::Branch(Box::new([])));
                for br in branches {
                    table.push(Branch {
                        tag: br.ctor.idx as u32,
                        dst: self.emit.pos(),
                        fields: br.bindings.len() as u32,
                    });
                    with_local_list(env, &br.bindings, self.emit.stack_size, |env| {
                        self.emit.stack_size += br.bindings.len();
                        self.compile_expr(&br.value, &env);
                        for _ in &br.bindings {
                            self.emit.op(Op::SwapPop);
                        }
                    });
                    jumps.push(self.emit.placeholder(Op::Jump(CodeAddr(0))));
                    self.emit.stack_size -= 1;
                }
                let end = self.emit.pos();
                for jmp in jumps {
                    self.emit.fill_placeholder(jmp, Op::Jump(end));
                }
                self.emit.fill_placeholder(dispatch, Op::Branch(table.into()));
                self.emit.stack_size += 1;
            }
            ir::Expr::Construct(ctor, _, fields) => {
                for field in fields {
                    self.compile_expr(field, env);
                }
                self.emit.op(Op::Construct { tag: ctor.idx as u32, fields: fields.len() as u32 })
            }
            ir::Expr::Let(n, v, e) => {
                self.compile_expr(v, env);
                let env = env.define(*n, self.emit);
                self.compile_expr(e, &env);
                self.emit.op(Op::SwapPop);
            }
            ir::Expr::LetRec(n, _, v, e) => {
                let v = unwrap_pis(v);
                let ir::Expr::Lambda(p, v) = v else {
                    panic!("let rec value is not lambda");
                };
                // compile lambda value
                let skip_over = self.emit.placeholder(Op::Jump(CodeAddr(0)));
                let mut captures = free_variables(expr);
                captures.retain(|c| c != n);
                println!("let rec captured {:?}", captures);
                let addr = self.compile_function(&p, Some(*n), v, &captures);
                self.emit.fill_placeholder(skip_over, Op::Jump(self.emit.pos()));
                for &c in &captures {
                    self.emit.pick_var(env, c);
                }
                let tag = addr.0 as u32;
                self.emit.op(Op::Construct { tag, fields: captures.len() as u32 });
                // compile let
                let env = env.define(*n, self.emit);
                self.compile_expr(e, &env);
                self.emit.op(Op::SwapPop);
            }
            ir::Expr::Pi(_, e) => self.compile_expr(e, env),
            ir::Expr::PiApply(e, _) => self.compile_expr(e, env),
            ir::Expr::Trap(msg, _) => {
                self.emit.op(Op::Trap(msg.as_str().into()));
            }
        }
    }

    fn compile_function(&mut self, p: &[ir::LambdaParam], rec_name: Option<ir::Name>, e: &ir::Expr, captures: &[ir::Name]) -> CodeAddr {
        let old_stack = std::mem::replace(&mut self.emit.stack_size, 0);
        let addr = self.emit.pos();
        self.compile_function_inner(p, rec_name, e, captures, &Env::Empty);
        self.emit.op(Op::Return(p.len() + captures.len() + 1));
        self.emit.stack_size = old_stack;
        addr
    }

    fn compile_function_inner(&mut self, p: &[ir::LambdaParam], rec_name: Option<ir::Name>, e: &ir::Expr, captures: &[ir::Name], env: &Env<'_>) {
        if let Some((p, rest)) = p.split_first() {
            self.emit.stack_size += 1;
            let env = env.define(p.name, self.emit);
            println!("within closure, param {:?} defined at {}", p.name, self.emit.stack_size - 1);
            let env = if rest.len() == 0 {
                // slot for the function value that was called
                self.emit.stack_size += 1;
                if let Some(rec) = rec_name {
                    env.define(rec, &self.emit)
                } else {
                    env
                }
            } else {
                env
            };
            self.compile_function_inner(rest, rec_name, e, captures, &env);
        } else if let Some((c, rest)) = captures.split_first() {
            self.emit.stack_size += 1;
            let env = env.define(*c, self.emit);
            println!("within closure, capture {:?} defined at {}", c, self.emit.stack_size - 1);
            self.compile_function_inner(p, rec_name, e, rest, &env);
        } else {
            // println!("compiling function, env: {:?}", env);
            self.compile_expr(e, env);
        }
    }
}

fn unwrap_pis(mut expr: &ir::Expr) -> &ir::Expr {
    while let ir::Expr::Pi(_, e) = expr {
        expr = e;
    }
    expr
}

fn with_local_list(env: &Env<'_>, names: &[ir::Name], stack_size: usize, f: impl FnOnce(&Env<'_>)) {
    match names {
        [] => f(env),
        [x, xs @ ..] => {
            let env = env.define_raw(*x, stack_size);
            with_local_list(&env, xs, stack_size + 1, f);
        }
    }
}

#[derive(Clone, Copy)]
enum Env<'a> {
    Empty,
    Cons {
        name: ir::Name,
        stack_pos: usize,
        parent: &'a Env<'a>,
    },
}

impl<'a> Env<'a> {
    fn define(&'a self, name: ir::Name, emitter: &Emitter) -> Env<'a> {
        Env::Cons {
            name,
            stack_pos: emitter.stack_size - 1,
            parent: self,
        }
    }

    fn define_raw(&'a self, name: ir::Name, stack_size: usize) -> Env<'a> {
        Env::Cons {
            name,
            stack_pos: stack_size,
            parent: self,
        }
    }

    fn find(&'a self, name: ir::Name) -> usize {
        let mut cur = self;
        while let Env::Cons { name: n, stack_pos, parent } = cur {
            if *n == name {
                return *stack_pos;
            }
            cur = parent;
        }
        unreachable!("could not find name {}_{}", name.idx, name.copy)
    }
}

struct Emitter {
    ops: Vec<Op>,
    stack_size: usize,
}

impl Emitter {
    fn op(&mut self, op: Op) {
        match op {
            Op::PushInt(_) |
            Op::PushBool(_) |
            Op::Pick(_) => self.stack_size += 1,
            Op::SwapPop |
            Op::Add |
            Op::Sub |
            Op::Mul |
            Op::Div |
            Op::Mod |
            Op::Less |
            Op::LessEq |
            Op::Greater |
            Op::GreaterEq |
            Op::Eq |
            Op::NotEq => self.stack_size -= 1,
            Op::Jump(_) => {}
            Op::JumpIfFalse(_) => self.stack_size -= 1,
            Op::Return(pops) => self.stack_size -= pops,
            Op::Call => {}
            Op::Construct { tag: _, fields } => {
                self.stack_size -= fields as usize;
                self.stack_size += 1;
            }
            Op::Branch(_) => self.stack_size -= 1,
            Op::Const(_) => self.stack_size += 1,
            Op::Trap(_) => {}
            Op::StringLen => {}
            Op::Placeholder => {}
        }
        self.ops.push(op);
    }

    fn pick_var(&mut self, env: &Env<'_>, name: ir::Name) {
        let at = env.find(name);
        let depth = self.stack_size - env.find(name) - 1;
        println!("picking {name:?}, at {at}, depth {depth}, stack {}", self.stack_size);
        self.op(Op::Pick(depth));
    }

    fn placeholder(&mut self, op: Op) -> Placeholder {
        let p = Placeholder(self.pos());
        self.op(op);
        *self.ops.last_mut().unwrap() = Op::Placeholder;
        p
    }

    fn pos(&self) -> CodeAddr {
        CodeAddr(self.ops.len())
    }

    fn fill_placeholder(&mut self, p: Placeholder, op: Op) {
        self.ops[p.0.0] = op;
        std::mem::forget(p);
    }
}

struct Placeholder(CodeAddr);

impl Drop for Placeholder {
    fn drop(&mut self) {
        if !panicking() {
            panic!("placeholder was not filled");
        }
    }
}

fn free_variables(expr: &ir::Expr) -> Vec<ir::Name> {
    fn go(e: &ir::Expr, used: &mut Vec<ir::Name>, defined: &mut HashSet<ir::Name>) {
        match e {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) => {}
            ir::Expr::Name(n) => used.push(*n),
            ir::Expr::Call(a, bs) => {
                go(a, used, defined);
                for b in bs {
                    go(b, used, defined);
                }
            }
            ir::Expr::If(c, t, e) => {
                go(c, used, defined);
                go(t, used, defined);
                go(e, used, defined);
            }
            ir::Expr::Op(a, _, b) => {
                go(a, used, defined);
                go(b, used, defined);
            }
            ir::Expr::Intrinsic(_, args) => {
                for arg in args {
                    go(arg, used, defined);
                }
            }
            ir::Expr::Lambda(ps, e) => {
                for p in ps {
                    defined.insert(p.name);
                }
                go(e, used, defined);
            }
            ir::Expr::Match(d, branches) => {
                go(d, used, defined);
                for br in branches {
                    for &v in &br.bindings {
                        defined.insert(v);
                    }
                    go(&br.value, used, defined);
                }
            }
            ir::Expr::Construct(_, _, xs) => {
                for x in xs {
                    go(x, used, defined);
                }
            }
            ir::Expr::Let(x, v, e) |
            ir::Expr::LetRec(x, _, v, e) => {
                defined.insert(*x);
                go(v, used, defined);
                go(e, used, defined);
            }
            ir::Expr::Pi(_, e) |
            ir::Expr::PiApply(e, _) => go(e, used, defined),
            ir::Expr::Trap(_, _) => {}
        }
    }
    let mut used = Vec::new();
    let mut defined = HashSet::new();
    go(expr, &mut used, &mut defined);
    used.retain(|item| defined.insert(*item));
    used
}

enum TypeInfo {
    Function,
    String,
    Tagged {
        name: String,
    },
}
