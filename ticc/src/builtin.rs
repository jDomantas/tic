use std::sync::Mutex;

use crate::{CompleteUnit, CompilationUnit, NoopModuleResolver};

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
    static OPTION: LazyModule = LazyModule::new(include_str!("std/option.tic"));
    static RESULT: LazyModule = LazyModule::new(include_str!("std/result.tic"));
    static LIST: LazyModule = LazyModule::new(include_str!("std/list.tic"));
    match name {
        "std/option" => Some(OPTION.force()),
        "std/result" => Some(RESULT.force()),
        "std/list" => Some(LIST.force()),
        _ => None,
    }
}
