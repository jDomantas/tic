use std::{convert::TryInto, collections::{HashMap, HashSet}};

use iced_x86::{BlockEncoder, InstructionBlock, BlockEncoderOptions};
use ticc_core::ir;

use crate::{compile, heap::{Heap, Addr}};

struct NativeStack {
    raw: Vec<u64>,
}

impl NativeStack {
    fn new() -> NativeStack {
        NativeStack {
            raw: Vec::with_capacity(4096 * 4096 * 16),
        }
    }

    fn next_slot_ptr(&mut self) -> *mut u64 {
        unsafe { self.raw.as_mut_ptr().offset(self.raw.len() as isize) }
    }
}

pub fn eval(program: &ir::Program<'_>, exprs: &[ir::Expr]) -> Result<Vec<crate::Value<()>>, crate::Trap> {
    let ops = crate::compile::compile(program, exprs);
    let values = exec(&ops, exprs.len())?;
    assert_eq!(values.len(), exprs.len());
    Ok(values)
}

fn exec(ops: &[compile::Op], pick: usize) -> Result<Vec<crate::Value<()>>, crate::Trap> {
    let mut heap = Heap::new();
    exec_in_heap(ops, &mut heap, pick)
}

fn exec_in_heap(ops: &[compile::Op], heap: &mut Heap, pick: usize) -> Result<Vec<crate::Value<()>>, crate::Trap> {
    let mut assebmled = assemble(ops, heap).unwrap();
    let (_exec_mmap, exec) = make_executable(&assebmled.code);

    let mut stack = NativeStack::new();
    let mut functions = Vec::with_capacity(assebmled.functions);
    let stack_start = stack.next_slot_ptr() as usize as u64;
    let initial_stack_ptr = stack_start.wrapping_sub(8);
    let false_obj = heap.alloc_object(0, 0).addr;
    let true_obj = heap.alloc_object(1, 0).addr;
    let mut state = ExecState {
        stack_ptr: initial_stack_ptr,
        true_addr: unsafe { std::mem::transmute(true_obj) },
        false_addr: unsafe { std::mem::transmute(false_obj) },
        stack_start,
        function_entrypoints: functions.as_mut_ptr(),
        consts: assebmled.consts.as_mut_ptr().cast::<u64>(),
        trap: u64::MAX,
        heap_base: heap.base(),
        heap,
        num_consts: assebmled.consts.len(),
    };
    let final_stack = unsafe { exec(&mut state) };
    assert!(final_stack >= initial_stack_ptr);
    let diff = final_stack - initial_stack_ptr;
    assert_eq!(diff % 16, 0);
    let stack_size = diff / 16;
    if state.trap == u64::MAX {
        let mut values = Vec::new();
        unsafe { stack.raw.set_len(stack_size as usize * 2) };
        assert!(stack_size >= pick as u64);
        let ctx = DecodeCtx {
            heap: unsafe { &*state.heap },
            true_addr: unsafe { std::mem::transmute::<u64, Addr>(state.true_addr) },
            false_addr: unsafe { std::mem::transmute::<u64, Addr>(state.false_addr) },
            ctor_tags: assebmled.ctor_tags.tag_names,
        };
        for i in (stack_size - pick as u64)..stack_size {
            let tag = stack.raw[i as usize * 2];
            let value = if tag == INT_STACK_TAG as u64 {
                crate::Value::Int(stack.raw[i as usize * 2 + 1])
            } else {
                let ptr = unsafe { std::mem::transmute::<u64, Addr>(stack.raw[i as usize * 2 + 1]) };
                decode_value(&ctx, ptr)
            };
            values.push(value);
        }
        Ok(values)
    } else {
        unsafe { stack.raw.set_len(stack_size as usize * 2) };
        let compile::Op::Trap(trap) = &ops[state.trap as usize] else {
            unreachable!();
        };
        Err(crate::Trap {
            message: (**trap).to_owned(),
        })
    }
}

fn make_executable(ins: &[u8]) -> (memmap2::Mmap, unsafe extern "C" fn(&mut ExecState) -> u64) {
    let mut anon = memmap2::MmapOptions::new()
        .len(ins.len())
        .map_anon()
        .unwrap();
    anon[..ins.len()].copy_from_slice(ins);
    let exec = anon.make_exec().unwrap();
    let f = unsafe {
        std::mem::transmute::<*const u8, unsafe extern "C" fn(&mut ExecState) -> u64>(exec.as_ptr())
    };
    (exec, f)
}

#[repr(C)]
struct ExecState {
    stack_ptr: u64,
    true_addr: u64,
    false_addr: u64,
    heap_base: *mut (),
    function_entrypoints: *mut u64,
    consts: *mut u64,
    trap: u64,
    stack_start: u64,
    heap: *mut Heap,
    num_consts: usize,
}

impl ExecState {
    const STACK_PTR_OFFSET: i32 = 0;
    const TRUE_ADDR_OFFSET: i32 = 8;
    const FALSE_ADDR_OFFSET: i32 = 16;
    const HEAP_BASE_OFFSET: i32 = 24;
    const FUNCTION_ENTRYPOINTS_OFFSET: i32 = 32;
    const CONSTS_OFFSET: i32 = 40;
    const TRAP_OFFSET: i32 = 48;

    fn pick_ptr(&self, nth: usize) -> Addr {
        unsafe {
            (self.stack_ptr as *const Addr).sub(nth * 2).read()
        }
    }

    fn pick_int(&self, nth: usize) -> u64 {
        unsafe {
            (self.stack_ptr as *const u64).sub(nth * 2).read()
        }
    }

    fn pop(&mut self) {
        self.stack_ptr -= 16;
    }

    fn push_ptr(&mut self, ptr: Addr) {
        unsafe {
            self.stack_ptr += 16;
            let p = self.stack_ptr as *mut Addr;
            p.sub(1).cast::<u64>().write(PTR_STACK_TAG as u64);
            p.write(ptr);
        }
    }

    fn push_int(&mut self, x: u64) {
        unsafe {
            self.stack_ptr += 16;
            let p = self.stack_ptr as *mut u64;
            p.sub(1).write(INT_STACK_TAG as u64);
            p.write(x);
        }
    }

    fn maybe_gc(&mut self) {
        let heap = unsafe { &mut *self.heap };
        if heap.allocated() > heap.capacity() / 2 {
            self.gc();
        }
    }

    fn gc(&mut self) {
        unsafe {
            let stack_size = ((self.stack_ptr + 8 - self.stack_start) / 16) as usize;
            let stack_ptr = self.stack_start as *mut u64;
            let stack_ptrs = (0..stack_size).filter_map(|i| {
                let tag = stack_ptr.add(i * 2).read();
                if tag == PTR_STACK_TAG as u64 {
                    let addr = stack_ptr.add(i * 2 + 1).cast::<Addr>();
                    Some(&mut *addr)
                } else {
                    None
                }
            });
            let consts = std::slice::from_raw_parts_mut(self.consts.cast::<Addr>(), self.num_consts);
            let heap = &mut *self.heap;
            fn make_addr(x: &mut u64) -> &mut Addr {
                unsafe { std::mem::transmute(x) }
            }
            heap.collect(stack_ptrs
                .chain(consts.iter_mut())
                .chain(std::iter::once(make_addr(&mut self.true_addr)))
                .chain(std::iter::once(make_addr(&mut self.false_addr))),
            );
            self.heap_base = heap.base();
        }
    }
}

const INT_STACK_TAG: i32 = 0;
const PTR_STACK_TAG: i32 = 1;

struct Assembled {
    code: Vec<u8>,
    functions: usize,
    ctor_tags: CtorTags,
    consts: Vec<Addr>,
}

fn assemble(ops: &[compile::Op], heap: &mut Heap) -> Result<Assembled, iced_x86::IcedError> {
    use iced_x86::code_asm::*;
    let mut asm = CodeAssembler::new(64)?;
    let mut consts = Vec::new();
    let mut ctor_tags = CtorTags::default();
    let stack_ptr = r12;
    let tmp1 = r8;
    let tmp2 = r9;
    let tmp3 = r10;
    let tmp4 = r11;
    let exec_state = r13;
    let true_addr = r14;
    let false_addr = r15;
    let heap_base = rdi;
    let function_entrypoints = rsi;
    // must save odd number of registers to keep stack aligned
    let saved_registers = [
        stack_ptr,
        true_addr,
        false_addr,
        exec_state,
        heap_base,
        function_entrypoints,
        function_entrypoints,
    ];
    for reg in saved_registers {
        asm.push(reg)?;
    }
    asm.mov(exec_state, rcx)?;
    asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
    asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
    asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
    asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
    asm.mov(function_entrypoints, qword_ptr(exec_state + ExecState::FUNCTION_ENTRYPOINTS_OFFSET))?;
    let mut labels = HashMap::new();
    let mut initialized_labels = HashSet::new();
    let mut function_indices = HashMap::new();

    for op in ops {
        let compile::Op::ConstructClosure { addr, .. } = op else {
            continue;
        };
        if function_indices.contains_key(addr) {
            continue;
        }
        let idx = function_indices.len() as i32;
        function_indices.insert(*addr, idx);
        let label = *labels.entry(*addr).or_insert_with(|| asm.create_label());
        asm.lea(tmp1, qword_ptr(label))?;
        asm.mov(qword_ptr(function_entrypoints + idx * 8), tmp1)?;
    }

    let alloc_fn_ptr = unsafe { std::mem::transmute::<extern "C" fn (&mut ExecState, u64, u64), u64>(alloc_obj) };

    for (idx, op) in ops.iter().enumerate() {
        asm.nop()?;
        match op {
            compile::Op::PushInt(x) => {
                asm.add(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr - 8), INT_STACK_TAG)?;
                if let Ok(x) = (*x).try_into() {
                    let x: u32 = x;
                    asm.mov(qword_ptr(stack_ptr), x as i32)?;
                } else {
                    asm.mov(tmp1, *x)?;
                    asm.mov(qword_ptr(stack_ptr), tmp1)?;
                }
            }
            compile::Op::PushBool(x) => {
                asm.add(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                let b = if *x { true_addr } else { false_addr };
                asm.mov(qword_ptr(stack_ptr), b)?;
            }
            compile::Op::PushString(s) => {
                let need_bytes = s.len() + 256;
                if heap.free_space() < need_bytes {
                    heap.collect_and_grow(consts.iter_mut(), need_bytes as u32);
                }

                let str = heap.alloc_bytes(s.len() as u64 + STRING_TAG, s.len() as u32);
                str.bytes[..s.len()].copy_from_slice(s);
                let const_idx = consts.len();
                consts.push(str.addr);
                asm.mov(tmp1, qword_ptr(exec_state + ExecState::CONSTS_OFFSET))?;
                asm.mov(tmp1, qword_ptr(tmp1 + (const_idx as i32 * 8)))?;
                asm.add(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::Pick(n) => {
                let offset: u32 = (*n).try_into().unwrap();
                let offset = offset.checked_mul(16).unwrap();
                asm.lea(tmp1, qword_ptr(stack_ptr - offset))?;
                asm.mov(tmp2, qword_ptr(tmp1 - 8))?;
                asm.mov(tmp1, qword_ptr(tmp1))?;
                asm.add(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr - 8), tmp2)?;
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::SwapPop => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.mov(tmp2, qword_ptr(stack_ptr - 8))?;
                asm.sub(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
                asm.mov(qword_ptr(stack_ptr - 8), tmp2)?;
            }
            compile::Op::Add => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.add(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::Sub => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.sub(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::Mul => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.imul_2(tmp1, qword_ptr(stack_ptr))?;
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::Div => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(tmp1, 0)?;
                let mut zero_case = asm.create_label();
                let mut div_end = asm.create_label();
                asm.je(zero_case)?;
                asm.xor(rdx, rdx)?;
                asm.mov(rax, qword_ptr(stack_ptr))?;
                asm.div(tmp1)?;
                asm.jmp(div_end)?;
                asm.set_label(&mut zero_case)?;
                asm.xor(rax, rax)?;
                asm.set_label(&mut div_end)?;
                asm.mov(qword_ptr(stack_ptr), rax)?;
            }
            compile::Op::Mod => {
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(tmp1, 0)?;
                let mut div_end = asm.create_label();
                asm.je(div_end)?;
                asm.xor(rdx, rdx)?;
                asm.mov(rax, qword_ptr(stack_ptr))?;
                asm.div(tmp1)?;
                asm.mov(qword_ptr(stack_ptr), rdx)?;
                asm.set_label(&mut div_end)?;
            }
            compile::Op::Less => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmovb(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::LessEq => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmovbe(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::Greater => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmova(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::GreaterEq => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmovae(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::Eq => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmove(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::NotEq => {
                asm.mov(tmp2, false_addr)?;
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(qword_ptr(stack_ptr), tmp1)?;
                asm.cmovne(tmp2, true_addr)?;
                asm.mov(qword_ptr(stack_ptr - 8), PTR_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp2)?;
            }
            compile::Op::Jump(l) => {
                let l = *labels.entry(*l).or_insert_with(|| asm.create_label());
                asm.jmp(l)?;
            }
            compile::Op::JumpIfFalse(l) => {
                let l = *labels.entry(*l).or_insert_with(|| asm.create_label());
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.sub(stack_ptr, 16)?;
                asm.cmp(tmp1, true_addr)?;
                asm.jne(l)?;
            }
            compile::Op::Return { params, captures } => {
                // return value
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.mov(tmp2, qword_ptr(stack_ptr - 8))?;
                // pop captures and return value
                let to_pop = *captures + 1;
                asm.sub(stack_ptr, to_pop as i32 * 16)?;
                // get return address
                asm.mov(tmp3, qword_ptr(stack_ptr))?;
                // pop return address, fn, and params,
                // minus one because return value will overwrite last value
                let to_pop = *params + 1;
                asm.sub(stack_ptr, to_pop as i32 * 16)?;
                // write return value
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
                asm.mov(qword_ptr(stack_ptr - 8), tmp2)?;
                // jmp to return address
                asm.jmp(tmp3)?;
            }
            compile::Op::Call => {
                // calculate closure object address
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.add(tmp1, heap_base)?;
                // get closure tag (function index with closure tag)
                asm.mov(tmp2, qword_ptr(tmp1 + 16))?;
                // push return address to stack
                asm.add(stack_ptr, 16)?;
                asm.mov(qword_ptr(stack_ptr - 8), INT_STACK_TAG)?;
                let mut return_label = asm.create_label();
                asm.lea(tmp3, qword_ptr(return_label))?;
                asm.mov(qword_ptr(stack_ptr), tmp3)?;
                // todo: push upvalues to stack
                asm.and(tmp2, 0x7fff_ffff)?;
                asm.mov(tmp2, qword_ptr(function_entrypoints + tmp2 * 8))?;
                asm.jmp(tmp2)?;
                asm.set_label(&mut return_label)?;
            }
            compile::Op::UnpackUpvalues(count) => {
                // calculate closure object address (skip top stack slot due to return address)
                asm.mov(tmp1, qword_ptr(stack_ptr - 16))?;
                asm.add(tmp1, heap_base)?;
                // get pointer bitset
                asm.mov(tmp2, qword_ptr(tmp1 + 8))?;
                // push fields to stack
                for field in 0..(*count) {
                    asm.mov(tmp3, tmp2)?;
                    asm.and(tmp3, 1)?;
                    asm.shr(tmp2, 1)?;
                    asm.add(stack_ptr, 16)?;
                    asm.mov(qword_ptr(stack_ptr - 8), tmp3)?;
                    asm.mov(tmp3, qword_ptr(tmp1 + (field * 8 + 24)))?;
                    asm.mov(qword_ptr(stack_ptr), tmp3)?;
                }
            }
            compile::Op::Construct { ctor, fields } => {
                let idx = ctor_tags.resolve(*ctor);
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(rdx, idx + OBJ_TAG)?;
                asm.mov(r8, *fields as u64)?;
                asm.mov(tmp2, alloc_fn_ptr)?;
                asm.call(tmp2)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::ConstructClosure { addr, fields } => {
                let idx = function_indices[addr];
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(rdx, idx as u64 + FN_TAG)?;
                asm.mov(r8, *fields as u64)?;
                asm.mov(tmp2, alloc_fn_ptr)?;
                asm.call(tmp2)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::Branch(branches) => {
                // calculate object address
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.add(tmp1, heap_base)?;
                // get pointer bitset and tag
                asm.mov(tmp2, qword_ptr(tmp1 + 8))?;
                asm.mov(tmp3, qword_ptr(tmp1 + 16))?;
                for br in branches.iter() {
                    let mut next_branch = asm.create_label();
                    let tag = ctor_tags.resolve(br.ctor) + OBJ_TAG;
                    asm.mov(tmp4, tag)?;
                    asm.cmp(tmp3, tmp4)?;
                    asm.jne(next_branch)?;
                    if br.bindings > 0 {
                        for idx in 0..br.bindings {
                            asm.mov(tmp3, tmp2)?;
                            asm.and(tmp3, 1)?;
                            asm.shr(tmp2, 1)?;
                            if idx > 0 {
                                asm.add(stack_ptr, 16)?;
                            }
                            asm.mov(qword_ptr(stack_ptr - 8), tmp3)?;
                            asm.mov(tmp3, qword_ptr(tmp1 + (idx as i32 * 8 + 24)))?;
                            asm.mov(qword_ptr(stack_ptr), tmp3)?;
                        }
                    } else {
                        asm.sub(stack_ptr, 16)?;
                    }
                    let l = *labels.entry(br.dst).or_insert_with(|| asm.create_label());
                    asm.jmp(l)?;
                    asm.set_label(&mut next_branch)?;
                }
                asm.ud2()?;
            }
            compile::Op::Trap(_) => {
                asm.mov(tmp1, idx as u64)?;
                asm.mov(qword_ptr(exec_state + ExecState::TRAP_OFFSET), tmp1)?;
                asm.mov(rax, stack_ptr)?;
                for &reg in saved_registers.iter().rev() {
                    asm.pop(reg)?;
                }
                asm.ret()?;
            }
            compile::Op::StringLen => {
                // string object address
                asm.mov(tmp1, qword_ptr(stack_ptr))?;
                asm.add(tmp1, heap_base)?;
                // get tag (length + mask)
                asm.mov(tmp1, qword_ptr(tmp1 + 16))?;
                // length without tag
                asm.mov(tmp2, STRING_TAG)?;
                asm.sub(tmp1, tmp2)?;
                // overwrite top value on the stack
                asm.mov(qword_ptr(stack_ptr - 8), INT_STACK_TAG)?;
                asm.mov(qword_ptr(stack_ptr), tmp1)?;
            }
            compile::Op::StringConcat => {
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(tmp1, intrinsic_address(string_concat))?;
                asm.call(tmp1)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::StringCharAt => {
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(tmp1, intrinsic_address(string_char_at))?;
                asm.call(tmp1)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::StringSubstring => {
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(tmp1, intrinsic_address(string_substring))?;
                asm.call(tmp1)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::StringFromChar => {
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(tmp1, intrinsic_address(string_from_char))?;
                asm.call(tmp1)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::IntToString => {
                asm.mov(qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET), stack_ptr)?;
                asm.mov(rcx, exec_state)?;
                asm.mov(tmp1, intrinsic_address(int_to_string))?;
                asm.call(tmp1)?;
                asm.mov(stack_ptr, qword_ptr(exec_state + ExecState::STACK_PTR_OFFSET))?;
                asm.mov(true_addr, qword_ptr(exec_state + ExecState::TRUE_ADDR_OFFSET))?;
                asm.mov(false_addr, qword_ptr(exec_state + ExecState::FALSE_ADDR_OFFSET))?;
                asm.mov(heap_base, qword_ptr(exec_state + ExecState::HEAP_BASE_OFFSET))?;
            }
            compile::Op::Exit => {
                asm.mov(rax, stack_ptr)?;
                for &reg in saved_registers.iter().rev() {
                    asm.pop(reg)?;
                }
                asm.ret()?;
            }
            compile::Op::Label(l) => {
                initialized_labels.insert(*l);
                let l = labels
                    .entry(*l)
                    .or_insert_with(|| asm.create_label());
                asm.set_label(l)?;
            }
        }
    }
    assert_eq!(labels.len(), initialized_labels.len());
    let block = InstructionBlock::new(asm.instructions(), 0x112233440000);
    let res = BlockEncoder::encode(64, block, BlockEncoderOptions::NONE)?;
    Ok(Assembled {
        code: res.code_buffer,
        functions: function_indices.len(),
        ctor_tags,
        consts,
    })
}

extern "C" fn alloc_obj(state: &mut ExecState, tag: u64, fields: u64) {
    let heap = unsafe { &mut *state.heap };
    let mut obj = heap.alloc_object(tag, fields as u32);
    if fields > 0 {
        let used_stack_start = state.stack_ptr - fields * 16 + 8;
        let ptr = used_stack_start as *mut u64;
        for i in 0..fields {
            unsafe {
                let tag = ptr.offset(i as isize * 2).read();
                if tag == INT_STACK_TAG as u64 {
                    obj.put_int(i as usize, ptr.offset(i as isize * 2 + 1).read());
                } else {
                    obj.put_ptr(i as usize, ptr.offset(i as isize * 2 + 1).cast::<Addr>().read());
                }
            }
        }
        unsafe {
            ptr.write(PTR_STACK_TAG as u64);
            ptr.offset(1).cast::<Addr>().write(obj.addr);
        }
        state.stack_ptr -= (fields - 1) as u64 * 16;
    } else {
        state.stack_ptr += 16;
        let ptr = state.stack_ptr as *mut u64;
        unsafe {
            ptr.offset(-1).write(PTR_STACK_TAG as u64);
            ptr.cast::<Addr>().write(obj.addr);
        }
    }

    state.maybe_gc();
}

fn intrinsic_address(i: extern "C" fn(&mut ExecState)) -> u64 {
    unsafe { std::mem::transmute(i) }
}

extern "C" fn string_concat(state: &mut ExecState) {
    let heap = unsafe { &mut *state.heap };
    let ptr1 = state.pick_ptr(1);
    let ptr2 = state.pick_ptr(0);
    state.pop();
    state.pop();

    let (tag2, bytes2) = heap.obj_with_bytes(ptr2);
    let len2 = tag2 & TAG_MASK;
    let bytes2 = &bytes2[..(len2 as usize)];
    let (tag1, bytes1) = heap.obj_with_bytes(ptr1);
    let len1 = tag1 & TAG_MASK;
    let bytes1 = &bytes1[..(len1 as usize)];
    let mut concat = Vec::with_capacity(bytes1.len() + bytes2.len());
    concat.extend(bytes1);
    concat.extend(bytes2);
    let obj = heap.alloc_bytes(concat.len() as u64 | STRING_TAG, concat.len() as u32);
    obj.bytes[..concat.len()].copy_from_slice(&concat);
    state.push_ptr(obj.addr);

    state.maybe_gc();
}

extern "C" fn string_char_at(state: &mut ExecState) {
    let heap = unsafe { &mut *state.heap };
    let ptr = state.pick_ptr(0);
    let idx = state.pick_int(1);
    state.pop();
    state.pop();
    let (tag, bytes) = heap.obj_with_bytes(ptr);
    let len = tag & TAG_MASK;
    let result = if idx < len {
        bytes[idx as usize]
    } else {
        0
    };
    state.push_int(u64::from(result));

    state.maybe_gc();
}

extern "C" fn string_from_char(state: &mut ExecState) {
    let heap = unsafe { &mut *state.heap };
    let x = state.pick_int(0);
    state.pop();
    let obj = heap.alloc_bytes(1 | STRING_TAG, 1);
    obj.bytes[0] = x as u8;
    state.push_ptr(obj.addr);

    state.maybe_gc();
}

extern "C" fn int_to_string(state: &mut ExecState) {
    let heap = unsafe { &mut *state.heap };
    let x = state.pick_int(0);
    state.pop();
    let s = x.to_string();
    let obj = heap.alloc_bytes(s.len() as u64 | STRING_TAG, s.len() as u32);
    obj.bytes[..s.len()].copy_from_slice(s.as_bytes());
    state.push_ptr(obj.addr);

    state.maybe_gc();
}

extern "C" fn string_substring(state: &mut ExecState) {
    let heap = unsafe { &mut *state.heap };
    let s = state.pick_ptr(0);
    state.pop();
    let (tag, bytes) = heap.obj_with_bytes(s);
    let len = tag & TAG_MASK;
    let bytes = &bytes[..(len as usize)];
    let take_len = state.pick_int(0) as usize;
    state.pop();
    let start = state.pick_int(0) as usize;
    state.pop();
    let bytes = bytes.get(start..).unwrap_or(&[]);
    let bytes = bytes.get(..take_len).unwrap_or(bytes);
    let bytes = bytes.to_vec();
    let obj = heap.alloc_bytes(bytes.len() as u64 | STRING_TAG, bytes.len() as u32);
    obj.bytes[..bytes.len()].copy_from_slice(&bytes);
    state.push_ptr(obj.addr);

    state.maybe_gc();
}

struct DecodeCtx<'a> {
    heap: &'a Heap,
    true_addr: Addr,
    false_addr: Addr,
    ctor_tags: HashMap<u64, ir::Name>,
}

fn decode_value(ctx: &DecodeCtx<'_>, addr: Addr) -> crate::Value<()> {
    fn go(ctx: &DecodeCtx<'_>, dedup: &mut HashMap<Addr, crate::Value<()>>, addr: Addr) -> crate::Value<()> {
        if addr == ctx.true_addr {
            return crate::Value::Bool(true);
        }
        if addr == ctx.false_addr {
            return crate::Value::Bool(false);
        }
        if let Some(value) = dedup.get(&addr) {
            return value.clone();
        }
        let tag = ctx.heap.obj_with_bytes(addr).0;
        let value = if tag & !TAG_MASK == FN_TAG {
            crate::Value::Fn(())
        } else if tag & !TAG_MASK == STRING_TAG {
            let (tag, bytes) = ctx.heap.obj_with_bytes(addr);
            let len = (tag & TAG_MASK) as usize;
            let bytes = &bytes[..len];
            crate::Value::String(bytes.into())
        } else if tag & !TAG_MASK == OBJ_TAG {
            let (tag, fields) = ctx.heap.obj_with_fields(addr);
            let name = ctx.ctor_tags[&(tag - OBJ_TAG)];
            let fields = fields
                .map(|v| match v {
                    crate::heap::Value::Int(i) => crate::Value::Int(i),
                    crate::heap::Value::Ptr(v) => go(ctx, dedup, v),
                })
                .collect();
            crate::Value::Composite(crate::Tagged::from_vec(name, fields))
        } else {
            panic!("probably heap corruption")
        };
        dedup.insert(addr, value.clone());
        value
    }
    go(ctx, &mut HashMap::new(), addr)
}

const STRING_TAG: u64 = 1 << 48;
const OBJ_TAG: u64 = 2 << 48;
pub(crate) const FN_TAG: u64 = 4 << 48;
const TAG_MASK: u64 = (1 << 48) - 1;

#[derive(Default)]
struct CtorTags {
    tags: HashMap<ir::Name, u64>,
    tag_names: HashMap<u64, ir::Name>,
}

impl CtorTags {
    fn resolve(&mut self, ctor: ir::Name) -> u64 {
        let next_tag = self.tags.len() as u64;
        let names = &mut self.tag_names;
        *self.tags.entry(ctor).or_insert_with(|| {
            names.insert(next_tag, ctor);
            next_tag
        })
    }
}
