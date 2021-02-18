pub mod cir;
pub mod gen_cir;
pub mod gen_js;

use crate::Compilation;

pub(crate) fn emit_js(compilation: &mut Compilation) -> String {
    compilation.compile_to_end();
    let program = gen_cir::generate_cir(compilation);
    let js = gen_js::emit(&program);
    js
}
