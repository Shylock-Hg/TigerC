use indexmap::IndexSet;

struct Symbol(u32);

struct IdentPool {
    id: u32,
    map: IndexSet<String>,
}

impl IdentPool {
    fn new() -> Self {
        IdentPool {
            id: 0,
            map: IndexSet::default(),
        }
    }

    fn create(&mut self, ident: &str) -> Symbol {
        if self.map.insert(ident.to_owned()) {
            Symbol((self.map.len() - 1) as u32)
        } else {
            Symbol(self.map.get_index_of(ident).unwrap() as u32)
        }
    }

    fn get_str(&self, s: &Symbol) -> Option<&String> {
        self.map.get_index(s.0 as usize)
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
        assert_eq!(pool.get_str(&symbol1).unwrap(), "symbol1");

        // repeat
        let symbol1 = pool.create("symbol1");
        assert_eq!(symbol1.0, 0);
        assert_eq!(pool.get_str(&symbol1).unwrap(), "symbol1");

        // another
        let symbol2 = pool.create("symbol2");
        assert_eq!(symbol2.0, 1);
        assert_eq!(pool.get_str(&symbol2).unwrap(), "symbol2");

        // another
        assert_eq!(pool.get_str(&symbol1).unwrap(), "symbol1");
    }
}
