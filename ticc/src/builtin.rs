use std::sync::Mutex;

use crate::{CompleteUnit, CompilationUnit, NoopModuleResolver, compiler::ir};

struct LazyModule {
    source: &'static str,
    module: Mutex<Option<CompleteUnit>>,
}

impl LazyModule {
    const fn new(source: &'static str) -> LazyModule {
        LazyModule {
            source,
            module: Mutex::new(None),
        }
    }

    fn force(&self) -> CompleteUnit {
        self.module
            .lock()
            .unwrap()
            .get_or_insert_with(|| {
                let mut unit = CompilationUnit::new(
                    self.source,
                    Default::default(),
                    NoopModuleResolver::new(),
                );
                if unit.diagnostics().any(|_| true) {
                    panic!("builtin module has diagnostics");
                }
                unit.complete()
            })
            .clone()
    }
}

pub(crate) fn lookup_module(name: &str) -> Option<CompleteUnit> {
    static INTRINSICS: LazyModule = LazyModule::new(include_str!("std/intrinsics.tic"));
    static OPTION: LazyModule = LazyModule::new(include_str!("std/option.tic"));
    static RESULT: LazyModule = LazyModule::new(include_str!("std/result.tic"));
    static LIST: LazyModule = LazyModule::new(include_str!("std/list.tic"));
    match name {
        "std/intrinsics" => Some(INTRINSICS.force()),
        "std/option" => Some(OPTION.force()),
        "std/result" => Some(RESULT.force()),
        "std/list" => Some(LIST.force()),
        _ => None,
    }
}

pub(crate) fn intrinsic_symbols() -> IntrinsicSymbols {
    static SYMBOLS: Mutex<Option<IntrinsicSymbols>> = Mutex::new(None);
    *SYMBOLS.lock().unwrap().get_or_insert_with(get_intrinsic_symbols)
}

fn get_intrinsic_symbols() -> IntrinsicSymbols {
    let module = lookup_module("std/intrinsics").unwrap();
    let mut iterate = None;
    for item in &module.props.unit.items {
        for def in &item.defs {
            let name = &module.props.unit.src[def.span.source_range()];
            match name {
                "iterate" => iterate = Some(def.symbol),
                _ => {}
            }
        }
    }
    IntrinsicSymbols {
        iterate: iterate.expect("no iterate intrinsic"),
    }
}

#[derive(Clone, Copy)]
pub(crate) struct IntrinsicSymbols {
    pub(crate) iterate: ir::Symbol,
}
