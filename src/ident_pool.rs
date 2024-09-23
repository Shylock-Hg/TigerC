use indexmap::IndexSet;
use std::fmt::{Debug, Display, Write};
use std::sync::LazyLock;
use std::sync::Mutex;

static IDENT_POOL: LazyLock<Mutex<IdentPool>> = LazyLock::new(|| Mutex::new(IdentPool::new()));

pub fn create_symbol(ident: &str) -> Symbol {
    IDENT_POOL.lock().unwrap().create(ident)
}

pub fn get_str(symbol: &Symbol) -> String {
    // This is useless in normal process of compile
    // So clone cost is acceptable.
    IDENT_POOL.lock().unwrap().get_str(symbol).to_owned()
}

#[derive(PartialEq, Eq)]
pub struct Symbol(u32);

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
    pub fn new() -> Self {
        IdentPool {
            inner: IndexSet::default(),
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

    #[test]
    fn test_ident_pool() {
        let mut pool = IdentPool::new();
        let symbol1 = pool.create("symbol1");
        assert_eq!(symbol1.0, 0);
        assert_eq!(pool.get_str(&symbol1), "symbol1");

        // repeat
        pool.create("symbol1");
        assert_eq!(symbol1.0, 0);
        assert_eq!(pool.get_str(&symbol1), "symbol1");

        // another
        let symbol2 = pool.create("symbol2");
        assert_eq!(symbol2.0, 1);
        assert_eq!(pool.get_str(&symbol2), "symbol2");

        assert_eq!(pool.get_str(&symbol1), "symbol1");
    }
}
