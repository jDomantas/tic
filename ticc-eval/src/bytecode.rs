use std::collections::HashMap;
use ticc_core::ir;
use crate::{heap::{Addr, Heap, Value}, Trap, compile};

struct Runtime {
    heap: Heap,
    stack: Stack,
    ops: Vec<Op>,
    consts: Vec<Addr>,
    ctor_tags: CtorTags,
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

    fn peek(&self) -> Value {
        *self.0.last().unwrap()
    }

    fn pick(&self, depth: usize) -> Value {
        *self.0.iter().rev().nth(depth).unwrap()
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

pub fn eval(program: &ir::Program<'_>, exprs: &[ir::Expr]) -> Result<Vec<crate::Value<()>>, Trap> {
    let mut heap = Heap::new();
    let mut consts = Vec::new();
    let compile_ops = crate::compile::compile(program, exprs);
    let (ops, ctor_tags) = link(&mut heap, &mut consts, compile_ops);
    let false_obj = heap.alloc_object(0, 0).addr;
    let true_obj = heap.alloc_object(1, 0).addr;
    let mut runtime = Runtime {
        heap,
        stack: Stack::default(),
        ops,
        consts,
        ctor_tags,
        bools: Bools {
            t: true_obj,
            f: false_obj,
        },
    };
    runtime.run()?;
    let mut values = Vec::new();
    for _ in exprs {
        let result = runtime.stack.pop();
        values.push(runtime.decode_value(result));
    }
    values.reverse();
    Ok(values)
}

impl Runtime {
    fn decode_value(&self, value: Value) -> crate::Value<()> {
        fn go(rt: &Runtime, dedup: &mut HashMap<Addr, crate::Value<()>>, value: Value) -> crate::Value<()> {
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
                crate::Value::Fn(())
            } else if tag & !TAG_MASK == STRING_TAG {
                let (tag, bytes) = rt.heap.obj_with_bytes(ptr);
                let len = (tag & TAG_MASK) as usize;
                let bytes = &bytes[..len];
                crate::Value::String(bytes.into())
            } else if tag & !TAG_MASK == OBJ_TAG {
                let (tag, fields) = rt.heap.obj_with_fields(ptr);
                let name = rt.ctor_tags.tag_names[&tag];
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

    fn run(&mut self) -> Result<(), Trap> {
        let mut ip = CodeAddr(0);
        loop {
            let mut dst = CodeAddr(ip.0 + 1);
            // println!("executing op at {}: {:?}", ip.0, self.ops[ip.0]);
            match self.ops[ip.0] {
                Op::PushInt(i) => {
                    self.stack.push(Value::Int(i));
                }
                Op::PushBool(b) => {
                    self.stack.push(self.bools.get(b));
                }
                Op::Pick(depth) => {
                    let value = self.stack.pick(depth);
                    self.stack.push(value);
                }
                Op::SwapPop => {
                    let top = self.stack.pop();
                    self.stack.pop();
                    self.stack.push(top);
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
                Op::Return { params, captures } => {
                    let return_value = self.stack.pop();
                    for _ in 0..captures {
                        self.stack.pop();
                    }
                    dst = CodeAddr(self.stack.pop_int() as usize);
                    self.stack.pop();
                    for _ in 0..params {
                        self.stack.pop();
                    }
                    self.stack.push(return_value);
                }
                Op::Call => {
                    let Value::Ptr(closure_addr) = self.stack.peek() else {
                        unreachable!();
                    };
                    self.stack.push(Value::Int(dst.0 as u64));
                    let (tag, _) = self.heap.obj_with_fields(closure_addr);
                    dst = CodeAddr((tag  & TAG_MASK) as usize);
                }
                Op::UnpackUpvalues => {
                    let Value::Ptr(closure_addr) = self.stack.pick(1) else {
                        unreachable!();
                    };
                    for field in self.heap.obj_with_fields(closure_addr).1 {
                        self.stack.push(field);
                    }
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

                    self.maybe_gc();
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

                    self.maybe_gc();
                }
                Op::StringFromChar => {
                    let x = self.stack.pop_int();
                    let obj = self.heap.alloc_bytes(1 | STRING_TAG, 1);
                    obj.bytes[0] = x as u8;
                    self.stack.push(Value::Ptr(obj.addr));

                    self.maybe_gc();
                }
                Op::IntToString => {
                    let x = self.stack.pop_int();
                    let s = x.to_string();
                    let obj = self.heap.alloc_bytes(s.len() as u64 | STRING_TAG, s.len() as u32);
                    obj.bytes[..s.len()].copy_from_slice(s.as_bytes());
                    self.stack.push(Value::Ptr(obj.addr));

                    self.maybe_gc();
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

                    self.maybe_gc();
                }
                Op::Exit => {
                    return Ok(());
                }
            }
            ip = dst;
        }
    }

    fn maybe_gc(&mut self) {
        if self.heap.allocated() > self.heap.capacity() / 2 {
            self.gc();
        }
    }

    fn gc(&mut self) {
        self.heap.collect(self.stack.ptrs()
            .chain(self.consts.iter_mut())
            .chain(std::iter::once(&mut self.bools.t))
            .chain(std::iter::once(&mut self.bools.f)),
        );
    }
}

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
    Jump(CodeAddr),
    JumpIfFalse(CodeAddr),
    Return { captures: u32, params: u32 },
    Call,
    UnpackUpvalues,
    Construct { tag: u64, fields: u32 },
    Branch(Box<Box<[Branch]>>),
    Const(usize),
    Trap(Box<Box<str>>),
    StringLen,
    StringConcat,
    StringCharAt,
    StringSubstring,
    StringFromChar,
    IntToString,
    Exit,
}

#[derive(Debug, Clone)]
pub(crate) struct Branch {
    pub(crate) tag: u64,
    pub(crate) dst: CodeAddr,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CodeAddr(pub(crate) usize);

const STRING_TAG: u64 = 1 << 48;
const OBJ_TAG: u64 = 2 << 48;
pub(crate) const FN_TAG: u64 = 4 << 48;
const TAG_MASK: u64 = (1 << 48) - 1;

fn link(heap: &mut Heap, consts: &mut Vec<Addr>, ops: Vec<compile::Op>) -> (Vec<Op>, CtorTags) {
    let mut ctor_tags = CtorTags::default();
    let mut label_addresses = HashMap::new();
    let mut addr = CodeAddr(0);
    for op in &ops {
        if let compile::Op::Label(l) = *op {
            label_addresses.insert(l, addr);
        } else {
            addr.0 += 1;
        }
    }
    let mut linked = Vec::with_capacity(addr.0 as usize);
    for op in ops {
        linked.push(match op {
            compile::Op::PushInt(x) => Op::PushInt(x),
            compile::Op::PushBool(x) => Op::PushBool(x),
            compile::Op::PushString(s) => {
                // TODO: calculate size properly
                let need_bytes = s.len() + 256;
                if heap.free_space() < need_bytes {
                    heap.collect_and_grow(consts.iter_mut(), need_bytes as u32);
                }
                let obj = heap.alloc_bytes(s.len() as u64 | STRING_TAG, s.len() as u32);
                obj.bytes[..s.len()].copy_from_slice(&s);
                consts.push(obj.addr);
                Op::Const(consts.len() - 1)
            }
            compile::Op::Pick(x) => Op::Pick(x),
            compile::Op::SwapPop => Op::SwapPop,
            compile::Op::Add => Op::Add,
            compile::Op::Sub => Op::Sub,
            compile::Op::Mul => Op::Mul,
            compile::Op::Div => Op::Div,
            compile::Op::Mod => Op::Mod,
            compile::Op::Less => Op::Less,
            compile::Op::LessEq => Op::LessEq,
            compile::Op::Greater => Op::Greater,
            compile::Op::GreaterEq => Op::GreaterEq,
            compile::Op::Eq => Op::Eq,
            compile::Op::NotEq => Op::NotEq,
            compile::Op::Jump(l) => Op::Jump(label_addresses[&l]),
            compile::Op::JumpIfFalse(l) => Op::JumpIfFalse(label_addresses[&l]),
            compile::Op::Return { params, captures } => Op::Return { params, captures },
            compile::Op::Call => Op::Call,
            compile::Op::UnpackUpvalues(_) => Op::UnpackUpvalues,
            compile::Op::Construct { ctor, fields } => {
                let tag = ctor_tags.resolve(ctor);
                Op::Construct { tag, fields: fields }
            }
            compile::Op::ConstructClosure { addr, fields } => {
                let tag = (label_addresses[&addr].0 as u64) | FN_TAG;
                Op::Construct { tag, fields }
            }
            compile::Op::Branch(br) => {
                Op::Branch(Box::new(br
                    .into_iter()
                    .map(|b| Branch {
                        tag: ctor_tags.resolve(b.ctor),
                        dst: label_addresses[&b.dst],
                    })
                    .collect()))
            }
            compile::Op::Trap(x) => Op::Trap(Box::new(x)),
            compile::Op::StringLen => Op::StringLen,
            compile::Op::StringConcat => Op::StringConcat,
            compile::Op::StringCharAt => Op::StringCharAt,
            compile::Op::StringSubstring => Op::StringSubstring,
            compile::Op::StringFromChar => Op::StringFromChar,
            compile::Op::IntToString => Op::IntToString,
            compile::Op::Label(_) => continue,
            compile::Op::Exit => Op::Exit,
        });
    }
    (linked, ctor_tags)
}

#[derive(Default)]
struct CtorTags {
    tags: HashMap<ir::Name, u64>,
    tag_names: HashMap<u64, ir::Name>,
}

impl CtorTags {
    fn resolve(&mut self, ctor: ir::Name) -> u64 {
        let next_tag = (self.tags.len() as u64) | OBJ_TAG;
        let names = &mut self.tag_names;
        *self.tags.entry(ctor).or_insert_with(|| {
            names.insert(next_tag, ctor);
            next_tag
        })
    }
}
