#[macro_use]
pub(crate) mod error;
pub(crate) mod builtin;
pub(crate) mod codegen;
pub(crate) mod compiler;
pub(crate) mod import;
pub(crate) mod interpreter;
pub(crate) mod lint;
pub(crate) mod api;

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use std::sync::Arc;
use compiler::{DefSet, Scope, ir, parser};
pub use ticc_syntax::{Pos, Span};
pub(crate) use crate::error::RawDiagnostic;
pub use crate::api::tokens::{Token, TokenKind};
pub use crate::api::info::Info;
pub use crate::api::completion::Completion;
pub use crate::codegen::Options;

pub struct CompilationUnit {
    key: ModuleKey,
    src: Arc<str>,
    items: Vec<ir::Item>,
    options: Options,
    modules: Arc<dyn ModuleResolver + Send + Sync>,
}

impl CompilationUnit {
    pub fn new(
        src: &str,
        options: Options,
        modules: Arc<dyn ModuleResolver + Send + Sync>,
    ) -> CompilationUnit {
        CompilationUnit {
            key: ModuleKey::fresh_key(),
            src: src.into(),
            items: Vec::new(),
            options,
            modules,
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        api::tokens::tokens(self)
    }

    pub fn diagnostics(&mut self) -> impl Iterator<Item = Diagnostic> + '_ {
        api::errors::diagnostics(self)
    }

    pub fn find_definition(&mut self, pos: Pos) -> Option<Span> {
        api::navigation::find_definition(self, pos)
    }

    pub fn find_references(&mut self, pos: Pos) -> Option<Vec<Span>> {
        api::navigation::find_references(self, pos)
    }

    pub fn info(&mut self, pos: Pos) -> Option<Info> {
        api::info::info_at(self, pos)
    }

    pub fn completions(&mut self, pos: Pos) -> Option<Vec<Completion>> {
        api::completion::completions_at(self, pos)
    }

    pub fn emit_ir(&mut self) -> String {
        codegen::emit_ir(self)
    }

    pub fn interpret(&mut self, runner: Runner) -> Result<String, Trap> {
        interpreter::eval(self, runner)
    }

    pub fn interpret_main(&mut self, runner: Runner, input: &[u8]) -> Result<Vec<u8>, InterpretError> {
        interpreter::eval_main(self, runner, input)
    }

    pub fn complete(mut self) -> CompleteUnit {
        self.compile_to_end();
        let exports = import::collect_exports(&self);
        let types = import::collect_types(&self);
        CompleteUnit {
            props: Arc::new(CompleteUnitProps {
                unit: self,
                exports,
                types,
            }),
        }
    }

    fn compile_to_end(&mut self) {
        self.compile_up_to(self.src.len());
    }

    fn compile_up_to(&mut self, offset: usize) {
        if self.compiled_length() >= offset {
            return;
        }

        let mut scope = Scope::new();
        let mut defs = DefSet::new();
        for item in &self.items {
            scope.add_item(&self.src, item, false);
            defs.add_item(item);
        }

        loop {
            let compiled = self.compiled_length();
            if compiled >= offset || compiled == self.src.len() {
                break;
            }

            let tail = &self.src[compiled..];
            let item_start = Pos::new(compiled as u32);
            let mut item = parser::parse_one_item(tail, item_start);
            compiler::resolve::resolve(self.key, &mut item, &scope, self.modules.clone());
            defs.add_item(&item);
            compiler::numck::check_numbers(&mut item);
            compiler::kindck::kind_check(&mut item, &defs);
            compiler::typeck::type_check(&mut item, &defs);
            compiler::matchck::check_matches(&mut item, &defs);
            self.items.push(item);
            scope.add_item(&self.src, self.items.last().unwrap(), false);
        }
    }

    fn compiled_length(&self) -> usize {
        self.items.last().map(|i| i.span.end().source_pos()).unwrap_or(0)
    }
}

pub enum InterpretError {
    Trap(Trap),
    NoMain,
    InvalidMain,
}

impl fmt::Display for InterpretError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpretError::Trap(trap) => write!(f, "{}", trap.message),
            InterpretError::NoMain => f.write_str("main function is not defined"),
            InterpretError::InvalidMain => f.write_str("main function must have type `string -> string`"),
        }
    }
}

#[derive(Clone)]
pub struct CompleteUnit {
    props: Arc<CompleteUnitProps>,
}

impl fmt::Debug for CompleteUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Dots;
        impl fmt::Debug for Dots {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("...")
            }
        }
        f.debug_struct("CompleteUnit").field("props", &Dots).finish()
    }
}

struct CompleteUnitProps {
    unit: CompilationUnit,
    exports: HashMap<NamespacedName, ir::Def>,
    types: HashMap<ir::Symbol, ir::Def>,
}

impl CompleteUnitProps {
    fn find_export(&self, namespace: Namespace, name: &str) -> Result<&ir::Def, ExportLookupError> {
        let mut name = NamespacedName {
            name: name.to_owned(),
            namespace,
        };
        if let Some(def) = self.exports.get(&name) {
            return Ok(def);
        }
        let mut available_in = Vec::new();
        for ns in [Namespace::Value, Namespace::Type, Namespace::Pattern, Namespace::Module] {
            name.namespace = ns;
            if self.exports.get(&name).is_some() {
                available_in.push(ns);
            }
        }
        if available_in.len() > 0 {
            Err(ExportLookupError::WrongNamespace { available_in })
        } else {
            Err(ExportLookupError::NoExport)
        }
    }
}

enum ExportLookupError {
    NoExport,
    WrongNamespace { available_in: Vec<Namespace> },
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct NamespacedName {
    namespace: Namespace,
    name: String,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
enum Namespace {
    Value,
    Type,
    Pattern,
    Module,
}

impl Namespace {
    const ALL: [Namespace; 4] = [Namespace::Value, Namespace::Type, Namespace::Pattern, Namespace::Module];
}

pub trait ModuleResolver {
    fn lookup(self: Arc<Self>, name: &str) -> Result<CompleteUnit, ImportError>;
}

pub struct NoopModuleResolver;

impl NoopModuleResolver {
    pub fn new() -> Arc<NoopModuleResolver> {
        Arc::new(NoopModuleResolver)
    }
}

impl ModuleResolver for NoopModuleResolver {
    fn lookup(self: Arc<Self>, _name: &str) -> Result<CompleteUnit, ImportError> {
        Err(ImportError::DoesNotExist)
    }
}

#[derive(Debug)]
pub enum ImportError {
    DoesNotExist,
    ImportCycle,
    Io(std::io::Error),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct ModuleKey(u64);

impl ModuleKey {
    fn fresh_key() -> ModuleKey {
        use std::sync::atomic;
        static NEXT: atomic::AtomicU64 = atomic::AtomicU64::new(0);
        let value = NEXT.fetch_add(1, atomic::Ordering::Relaxed);
        ModuleKey(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: Severity,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct Trap {
    pub message: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Runner {
    Lambda,
    Bytecode,
    Treewalk,
    Jit,
}

impl FromStr for Runner {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "lambda" => Ok(Runner::Lambda),
            "bytecode" => Ok(Runner::Bytecode),
            "treewalk" => Ok(Runner::Treewalk),
            "jit" => Ok(Runner::Jit),
            _ => Err("invalid runner"),
        }
    }
}

impl fmt::Display for Runner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Runner::Lambda => "lambda",
            Runner::Bytecode => "bytecode",
            Runner::Treewalk => "treewalk",
            Runner::Jit => "jit",
        })
    }
}
