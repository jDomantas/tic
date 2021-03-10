pub(crate) mod cir;
mod gen_cir;
mod pretty;
mod opt;
mod verify;

use crate::Compilation;

#[derive(Debug, Clone, Copy)]
pub struct Options {
    pub verify: bool,
    // optimization options
    pub inline: bool,
    pub inline_simple: bool,
    pub reduce_apply: bool,
    pub remove_dead_code: bool,
    pub move_match: bool,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            verify: true,
            inline: true,
            inline_simple: true,
            reduce_apply: true,
            remove_dead_code: true,
            move_match: true,
        }
    }
}

pub(crate) fn emit_ir(compilation: &mut Compilation) -> String {
    compilation.compile_to_end();
    let mut program = gen_cir::generate_cir(compilation);
    let verify = |p: &cir::Program| if compilation.options.verify { verify::verify(p); };
    opt::optimize(compilation.options, verify, &mut program);
    let pretty = pretty::pretty_print_cir(&program);
    pretty
}
