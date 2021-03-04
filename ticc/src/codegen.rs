pub(crate) mod cir;
mod gen_cir;
mod pretty;
mod opt;

use crate::Compilation;

#[derive(Debug, Clone, Copy)]
pub struct Options {
    pub optimize_lambda: bool,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            optimize_lambda: true,
        }
    }
}

pub(crate) fn emit_ir(compilation: &mut Compilation) -> String {
    compilation.compile_to_end();
    let mut program = gen_cir::generate_cir(compilation);
    opt::optimize(compilation.options, &mut program);
    let pretty = pretty::pretty_print_cir(&program);
    pretty
}
