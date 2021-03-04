mod inline_lambda;

use crate::codegen::{cir, Options};

pub(crate) fn optimize(options: Options, program: &mut cir::Program) {
    if options.optimize_lambda {
        inline_lambda::optimize(program);
    }
}
