mod gen_core;
mod opt;

use ticc_core::ir;
use crate::CompilationUnit;

#[derive(Debug, Clone, Copy)]
pub struct Options {
    pub verify: bool,
    pub optimize: bool,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            verify: true,
            optimize: true,
        }
    }
}

pub(crate) fn emit_ir(compilation: &mut CompilationUnit) -> String {
    let program = emit_core(compilation);
    let pretty = ticc_core::pretty_print(&program);
    pretty
}

pub(crate) fn emit_core(compilation: &mut CompilationUnit) -> ir::Program<'_> {
    compilation.compile_to_end();
    let mut program = gen_core::generate_core(compilation);
    let verify = |p: &ir::Program| if compilation.options.verify { ticc_core::assert_valid(p); };
    verify(&program);
    opt::optimize(compilation.options, verify, &mut program);
    program
}
