use std::{thread::panicking, collections::{HashSet, HashMap}, rc::Rc};

use ticc_core::ir;

use crate::{heap::{Addr, Heap}, Trap};

#[derive(Clone, Copy)]
pub enum Value {
    Int(u64),
    Ptr(Addr),
}

struct Frame {
    ip: CodeAddr,
}

struct Runtime {
    heap: Heap,
    stack: Stack,
    ops: Vec<Op>,
    frames: Vec<Frame>,
    consts: Vec<Addr>,
    tag_names: Vec<ir::Name>,
    bools: Bools,
}

struct Bools {
    t: Addr,
    f: Addr,
}

impl Bools {
    fn get(&self, v: bool) -> Value {
        Value::Ptr(if v {
            self.t
        } else {
            self.f
        })
    }
}

#[derive(Default)]
struct Stack(Vec<Value>);

impl Stack {
    fn pop_int(&mut self) -> u64 {
        match self.0.pop().unwrap() {
            Value::Int(x) => x,
            _ => unreachable!(),
        }
    }

    fn pop_ptr(&mut self) -> Addr {
        match self.0.pop().unwrap() {
            Value::Ptr(x) => x,
            _ => unreachable!(),
        }
    }

    fn pop(&mut self) -> Value {
        self.0.pop().unwrap()
    }

    fn peek_mut(&mut self) -> &mut Value {
        self.0.last_mut().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    fn ptrs<'a>(&'a mut self) -> impl Iterator<Item = &'a mut Addr> + 'a {
        self.0.iter_mut().filter_map(|x| match x {
            Value::Int(_) => None,
            Value::Ptr(x) => Some(x),
        })
    }
}

pub fn eval_expr(expr: &ir::Expr) -> Result<crate::Value, Trap> {
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
        tag_names: Vec::new(),
        name_tags: HashMap::new(),
    };
    compiler.compile_expr(expr, &Env::Empty);
    let tag_names = compiler.tag_names;
    emitter.op(Op::Return(0));
    let ops = emitter.ops;
    // println!("compiled code:");
    // for (i, op) in ops.iter().enumerate() {
    //     println!("{i:>3}  {op:?}");
    // }
    let false_obj = heap.alloc_object(0, 0).addr;
    let true_obj = heap.alloc_object(1, 0).addr;
    let mut runtime = Runtime {
        heap,
        stack: Stack::default(),
        ops,
        frames: Vec::from([
            Frame { ip: CodeAddr(0) },
        ]),
        consts,
        tag_names,
        bools: Bools {
            t: true_obj,
            f: false_obj,
        },
    };
    while runtime.frames.len() > 0 {
        runtime.tick()?;
    }
    match runtime.stack.0.as_slice() {
        &[x] => Ok(runtime.decode_value(x)),
        _ => panic!("shite"),
    }
}

impl Runtime {
    fn decode_value(&self, value: Value) -> crate::Value {
        fn go(rt: &Runtime, dedup: &mut HashMap<Addr, crate::Value>, value: Value) -> crate::Value {
            let ptr = match value {
                Value::Int(x) => return crate::Value::Int(x),
                Value::Ptr(ptr) => ptr,
            };
            if ptr == rt.bools.t {
                return crate::Value::Bool(true);
            }
            if ptr == rt.bools.f {
                return crate::Value::Bool(false);
            }
            if let Some(value) = dedup.get(&ptr) {
                return value.clone();
            }
            let tag = rt.heap.obj_with_bytes(ptr).0;
            let value = if tag & !TAG_MASK == FN_TAG {
                crate::Value::Fn(crate::Function(Rc::new(|_, _| panic!("decoded functions cannot be called"))))
            } else if tag & !TAG_MASK == STRING_TAG {
                let (tag, bytes) = rt.heap.obj_with_bytes(ptr);
                let len = (tag & TAG_MASK) as usize;
                let bytes = &bytes[..len];
                let s = std::str::from_utf8(bytes).expect("invalid utf8 in tic string");
                crate::Value::String(s.into())
            } else if tag & !TAG_MASK == OBJ_TAG {
                let (tag, fields) = rt.heap.obj_with_fields(ptr);
                let idx = (tag & TAG_MASK) as usize;
                let name = rt.tag_names[idx];
                let fields = fields.map(|v| go(rt, dedup, v)).collect::<Vec<_>>();
                crate::Value::Composite(crate::Tagged::from_vec(name, fields))
            } else {
                panic!("probably heap corruption")
            };
            dedup.insert(ptr, value.clone());
            value
        }
        go(self, &mut HashMap::new(), value)
    }

    fn tick(&mut self) -> Result<(), Trap> {
        if self.heap.allocated() > self.heap.capacity() / 2 {
            // panic!("gc!");
            // println!("gc!");
            self.heap.collect(self.stack.ptrs()
                .chain(self.consts.iter_mut())
                .chain(std::iter::once(&mut self.bools.t))
                .chain(std::iter::once(&mut self.bools.f)),
            );
        }

        let ip = self.frames.last().unwrap().ip;
        let mut dst = CodeAddr(ip.0 + 1);
        // println!("executing op at {}: {:?}, stack = {:?}", ip.0, self.ops[ip.0], self.stack.0);
        match self.ops[ip.0] {
            Op::PushInt(i) => {
                self.stack.push(Value::Int(i));
            }
            Op::PushBool(b) => {
                self.stack.push(self.bools.get(b));
            }
            Op::Pick(depth) => {
                let value = self.stack.0.iter().rev().nth(depth).unwrap();
                self.stack.push(*value);
            }
            Op::SwapPop => {
                let top = self.stack.pop();
                *self.stack.peek_mut() = top;
            }
            Op::Add => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(Value::Int(a.wrapping_add(b)));
            }
            Op::Sub => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(Value::Int(a.wrapping_sub(b)));
            }
            Op::Mul => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(Value::Int(a.wrapping_mul(b)));
            }
            Op::Div => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(Value::Int(a.checked_div(b).unwrap_or(0)));
            }
            Op::Mod => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(Value::Int(a.checked_rem(b).unwrap_or(a)));
            }
            Op::Less => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a < b));
            }
            Op::LessEq => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a <= b));
            }
            Op::Greater => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a > b));
            }
            Op::GreaterEq => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a >= b));
            }
            Op::Eq => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a == b));
            }
            Op::NotEq => {
                let b = self.stack.pop_int();
                let a = self.stack.pop_int();
                self.stack.push(self.bools.get(a != b));
            }
            Op::Jump(d) => {
                dst = d;
            }
            Op::JumpIfFalse(d) => {
                if self.stack.pop_ptr() == self.bools.f {
                    dst = d;
                }
            }
            Op::Return(pops) => {
                let return_value = self.stack.pop();
                for _ in 0..pops {
                    self.stack.pop();
                }
                self.stack.push(return_value);
                self.frames.pop();
                // don't update ip as the frame was already popped
                return Ok(());
            }
            Op::Call => {
                let call_addr = match self.stack.peek_mut() {
                    &mut Value::Ptr(addr) => {
                        let (tag, fields) = self.heap.obj_with_fields(addr);
                        for field in fields {
                            self.stack.push(field);
                        }
                        CodeAddr((tag  & TAG_MASK) as usize)
                    }
                    _ => unreachable!(),
                };
                self.frames.last_mut().unwrap().ip = dst;
                self.frames.push(Frame {
                    ip: call_addr,
                });
                // don't update ip - we did that manually and then pushed a new frame
                return Ok(());
            }
            Op::Construct { tag, fields } => {
                let mut obj = self.heap.alloc_object(tag, fields);
                for i in (0..fields).rev() {
                    let value = self.stack.pop();
                    match value {
                        Value::Int(x) => obj.put_int(i as usize, x),
                        Value::Ptr(x) => obj.put_ptr(i as usize, x),
                    }
                }
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::Branch(ref branches) => {
                let obj = match self.stack.pop() {
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
                return Err(Trap { message: msg.to_string() });
            }
            Op::StringLen => {
                let ptr = self.stack.pop_ptr();
                let (tag, _) = self.heap.obj_with_fields(ptr);
                let len = tag & TAG_MASK;
                self.stack.push(Value::Int(len));
            }
            Op::StringCharAt => {
                let ptr = self.stack.pop_ptr();
                let idx = self.stack.pop_int();
                let (tag, bytes) = self.heap.obj_with_bytes(ptr);
                let len = tag & TAG_MASK;
                let result = if idx < len {
                    bytes[idx as usize]
                } else {
                    0
                };
                self.stack.push(Value::Int(u64::from(result)))
            }
            Op::StringConcat => {
                let ptr2 = self.stack.pop_ptr();
                let (tag2, bytes2) = self.heap.obj_with_bytes(ptr2);
                let len2 = tag2 & TAG_MASK;
                let bytes2 = &bytes2[..(len2 as usize)];
                let ptr1 = self.stack.pop_ptr();
                let (tag1, bytes1) = self.heap.obj_with_bytes(ptr1);
                let len1 = tag1 & TAG_MASK;
                let bytes1 = &bytes1[..(len1 as usize)];
                let mut concat = Vec::with_capacity(bytes1.len() + bytes2.len());
                concat.extend(bytes1);
                concat.extend(bytes2);
                let obj = self.heap.alloc_bytes(concat.len() as u64 | STRING_TAG, concat.len() as u32);
                obj.bytes[..concat.len()].copy_from_slice(&concat);
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::StringFromChar => {
                let x = self.stack.pop_int();
                let obj = self.heap.alloc_bytes(1 | STRING_TAG, 1);
                obj.bytes[0] = (x & 0x7F) as u8;
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::IntToString => {
                let x = self.stack.pop_int();
                let s = x.to_string();
                let obj = self.heap.alloc_bytes(s.len() as u64 | STRING_TAG, s.len() as u32);
                obj.bytes[..s.len()].copy_from_slice(s.as_bytes());
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::StringSubstring => {
                let s = self.stack.pop_ptr();
                let (tag, bytes) = self.heap.obj_with_bytes(s);
                let len = tag & TAG_MASK;
                let bytes = &bytes[..(len as usize)];
                let take_len = self.stack.pop_int() as usize;
                let start = self.stack.pop_int() as usize;
                let bytes = bytes.get(start..).unwrap_or(&[]);
                let bytes = bytes.get(..take_len).unwrap_or(bytes);
                let bytes = bytes.to_vec();
                let obj = self.heap.alloc_bytes(bytes.len() as u64 | STRING_TAG, bytes.len() as u32);
                obj.bytes[..bytes.len()].copy_from_slice(&bytes);
                self.stack.push(Value::Ptr(obj.addr));
            }
            Op::Placeholder => unreachable!("placeholders should be replaced before end of compilation"),
        }
        self.frames.last_mut().unwrap().ip = dst;
        Ok(())
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
    Construct { tag: u64, fields: u32 },
    Branch(Box<[Branch]>),
    Const(usize),
    Trap(Box<str>),
    StringLen,
    StringConcat,
    StringCharAt,
    StringSubstring,
    StringFromChar,
    IntToString,
    Placeholder,
}

#[derive(Debug, Clone)]
struct Branch {
    tag: u64,
    dst: CodeAddr,
}

#[derive(Debug, Clone, Copy)]
struct CodeAddr(usize);

struct Compiler<'a> {
    emit: &'a mut Emitter,
    consts: &'a mut Vec<Addr>,
    heap: &'a mut Heap,
    tag_names: Vec<ir::Name>,
    name_tags: HashMap<ir::Name, u64>,
}

const STRING_TAG: u64 = 1 << 48;
const OBJ_TAG: u64 = 2 << 48;
const FN_TAG: u64 = 4 << 48;
const TAG_MASK: u64 = (1 << 48) - 1;

impl Compiler<'_> {
    fn compile_expr(&mut self, expr: &ir::Expr, env: &Env<'_>) {
        match expr {
            ir::Expr::Bool(b) => self.emit.op(Op::PushBool(*b)),
            ir::Expr::Int(i) => self.emit.op(Op::PushInt(*i)),
            ir::Expr::String(s) => {
                let need_bytes = s.len() + 256;
                if self.heap.free_space() < need_bytes {
                    self.heap.collect_and_grow(self.consts.iter_mut(), need_bytes as u32);
                }
                let obj = self.heap.alloc_bytes(s.len() as u64 | STRING_TAG, s.len() as u32);
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
            ir::Expr::Intrinsic(i, args) => {
                for arg in args {
                    self.compile_expr(arg, env);
                }
                match i {
                    ir::Intrinsic::StringLen => self.emit.op(Op::StringLen),
                    ir::Intrinsic::StringConcat => self.emit.op(Op::StringConcat),
                    ir::Intrinsic::StringCharAt => self.emit.op(Op::StringCharAt),
                    ir::Intrinsic::StringSubstring => self.emit.op(Op::StringSubstring),
                    ir::Intrinsic::StringFromChar => self.emit.op(Op::StringFromChar),
                    ir::Intrinsic::IntToString => self.emit.op(Op::IntToString),
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
                let tag = addr.0 as u64 | FN_TAG;
                self.emit.op(Op::Construct { tag, fields: captures.len() as u32 })
            }
            ir::Expr::Match(discr, branches) => {
                self.compile_expr(discr, env);
                let mut jumps = Vec::new();
                let mut table = Vec::with_capacity(branches.len());
                let dispatch = self.emit.placeholder(Op::Branch(Box::new([])));
                for br in branches {
                    table.push(Branch {
                        tag: self.name_to_tag(br.ctor),
                        dst: self.emit.pos(),
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
                let tag = self.name_to_tag(*ctor);
                self.emit.op(Op::Construct { tag, fields: fields.len() as u32 })
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
                // println!("let rec captured {:?}", captures);
                let addr = self.compile_function(&p, Some(*n), v, &captures);
                self.emit.fill_placeholder(skip_over, Op::Jump(self.emit.pos()));
                for &c in &captures {
                    self.emit.pick_var(env, c);
                }
                let tag = addr.0 as u64 | FN_TAG;
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

    fn name_to_tag(&mut self, name: ir::Name) -> u64 {
        let tag_names = &mut self.tag_names;
        *self.name_tags.entry(name).or_insert_with(|| {
            let idx = tag_names.len() as u64;
            tag_names.push(name);
            idx | OBJ_TAG
        })
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
            // println!("within closure, param {:?} defined at {}", p.name, self.emit.stack_size - 1);
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
            // println!("within closure, capture {:?} defined at {}", c, self.emit.stack_size - 1);
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
            Op::StringCharAt => self.stack_size -= 1,
            Op::StringConcat => self.stack_size -= 1,
            Op::StringFromChar => {}
            Op::IntToString => {}
            Op::StringSubstring => self.stack_size -= 2,
            Op::Placeholder => {}
        }
        self.ops.push(op);
    }

    fn pick_var(&mut self, env: &Env<'_>, name: ir::Name) {
        // let at = env.find(name);
        let depth = self.stack_size - env.find(name) - 1;
        // println!("picking {name:?}, at {at}, depth {depth}, stack {}", self.stack_size);
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
