use std::sync::Mutex;

use crate::{CompleteUnit, CompilationUnit, NoopModuleResolver, compiler::ir::{self, Visibility}};

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
    static STRING: LazyModule = LazyModule::new(include_str!("std/string.tic"));
    match name {
        "std/intrinsics" => Some(INTRINSICS.force()),
        "std/option" => Some(OPTION.force()),
        "std/result" => Some(RESULT.force()),
        "std/list" => Some(LIST.force()),
        "std/string" => Some(STRING.force()),
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
    let mut string_length = None;
    let mut string_concat = None;
    let mut string_char_at = None;
    let mut string_substring = None;
    let mut string_from_char = None;
    for item in &module.props.unit.items {
        for def in &item.defs {
            if def.vis != Visibility::Export {
                continue;
            }
            let name = &module.props.unit.src[def.span.source_range()];
            match name {
                "iterate" => iterate = Some(def.symbol),
                "stringLength" => string_length = Some(def.symbol),
                "stringConcat" => string_concat = Some(def.symbol),
                "stringCharAt" => string_char_at = Some(def.symbol),
                "stringSubstring" => string_substring = Some(def.symbol),
                "stringFromChar" => string_from_char = Some(def.symbol),
                _ => panic!("unknown intrinsic: {}", name),
            }
        }
    }
    IntrinsicSymbols {
        iterate: iterate.expect("no iterate intrinsic"),
        string_length: string_length.expect("no string_length intrinsic"),
        string_concat: string_concat.expect("no string_concat intrinsic"),
        string_char_at: string_char_at.expect("no string_char_at intrinsic"),
        string_substring: string_substring.expect("no string_substring intrinsic"),
        string_from_char: string_from_char.expect("no string_from_char intrinsic"),
    }
}

#[derive(Clone, Copy)]
pub(crate) struct IntrinsicSymbols {
    pub(crate) iterate: ir::Symbol,
    pub(crate) string_length: ir::Symbol,
    pub(crate) string_concat: ir::Symbol,
    pub(crate) string_char_at: ir::Symbol,
    pub(crate) string_substring: ir::Symbol,
    pub(crate) string_from_char: ir::Symbol,
}
