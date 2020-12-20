mod lexer;
mod tokens;

use std::sync::Arc;
pub use tokens::{Token, TokenKind};

pub struct Compilation {
    src: Arc<str>,
}

impl Compilation {
    pub fn from_source(src: String) -> Compilation {
        Compilation {
            src: src.into(),
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        tokens::tokens(self)
    }
}
