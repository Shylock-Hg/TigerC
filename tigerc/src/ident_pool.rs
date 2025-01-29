use indexmap::IndexSet;
use std::fmt::{Debug, Display, Write};
use std::sync::LazyLock;
use std::sync::Mutex;

use tigerc_macros::symbols;

symbols! {
    Keywords {
        TOK_TYPE: "type",
        TOK_ARRAY: "array",
        TOK_OF: "of",
        TOK_INT: "int",
        TOK_STRING: "string",
        TOK_NIL: "nil",
        TOK_VAR: "var",
        TOK_FUNCTION: "function",
        TOK_IF: "if",
        TOK_THEN: "then",
        TOK_ELSE: "else",
        TOK_WHILE: "while",
        TOK_DO: "do",
        TOK_FOR: "for",
        TOK_TO: "to",
        TOK_BREAK: "break",
        TOK_LET: "let",
        TOK_IN: "in",
        TOK_END: "end",
    }
}

pub mod kw {
    pub use super::kw_generated::*;
}

static IDENT_POOL: LazyLock<Mutex<IdentPool>> = LazyLock::new(|| Mutex::new(IdentPool::new()));

pub fn create_symbol(ident: &str) -> Symbol {
    IDENT_POOL.lock().unwrap().create(ident)
}

pub fn get_str(symbol: &Symbol) -> String {
    // This is useless in normal process of compile
    // So clone cost is acceptable.
    IDENT_POOL.lock().unwrap().get_str(symbol).to_owned()
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct Symbol(u32);

impl Symbol {
    pub fn new(name: &str) -> Self {
        create_symbol(name)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&get_str(self))?;
        f.write_char('(')?;
        f.write_str(&self.0.to_string())?;
        f.write_char(')')
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&get_str(self))
    }
}

struct IdentPool {
    inner: IndexSet<String>,
}

impl IdentPool {
    fn prefill(fills: &[&str]) -> Self {
        IdentPool {
            inner: fills.iter().map(|s| s.to_owned().to_owned()).collect(),
        }
    }

    pub fn create(&mut self, ident: &str) -> Symbol {
        if self.inner.insert(ident.to_owned()) {
            Symbol((self.inner.len() - 1) as u32)
        } else {
            Symbol(self.inner.get_index_of(ident).unwrap() as u32)
        }
    }

    pub fn get_str(&self, s: &Symbol) -> &String {
        self.inner.get_index(s.0 as usize).unwrap()
    }
}

#[cfg(test)]
mod tests {

    use super::IdentPool;
    use super::Symbol;
    use super::PREDEFINED_SIZE;

    #[test]
    fn test_ident_pool() {
        let mut pool = IdentPool::new();
        let symbol1 = pool.create("symbol1");
        assert_eq!(symbol1.0, PREDEFINED_SIZE + 0);
        assert_eq!(pool.get_str(&symbol1), "symbol1");

        // repeat
        pool.create("symbol1");
        assert_eq!(symbol1.0, PREDEFINED_SIZE + 0);
        assert_eq!(pool.get_str(&symbol1), "symbol1");

        // another
        let symbol2 = pool.create("symbol2");
        assert_eq!(symbol2.0, PREDEFINED_SIZE + 1);
        assert_eq!(pool.get_str(&symbol2), "symbol2");

        assert_eq!(pool.get_str(&symbol1), "symbol1");
    }

    #[test]
    fn test_ident_pool_predefined() {
        let mut pool = IdentPool::new();
        let symbol1 = pool.create("type");
        assert_eq!(symbol1.0, 0);
        assert_eq!(pool.get_str(&symbol1), "type");
    }

    #[test]
    fn test_ident_idx_of_type() {
        let pool = IdentPool::new();
        assert_eq!(pool.get_str(&Symbol(0)), "type");
    }
}
