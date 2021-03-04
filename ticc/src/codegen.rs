pub mod cir;
pub mod gen_cir;
pub mod pretty;

use crate::Compilation;

pub(crate) fn emit_ir(compilation: &mut Compilation) -> String {
    compilation.compile_to_end();
    let program = gen_cir::generate_cir(compilation);
    let pretty = pretty::pretty_print_cir(&program);
    pretty
}
