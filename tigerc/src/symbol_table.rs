use std::collections::LinkedList;
use std::hash::{Hash, RandomState};

use crate::ident_pool::Symbol;
use crate::stack::Stack;

struct MultiMap<V, K = Symbol, S = RandomState> {
    inner: std::collections::HashMap<K, LinkedList<V>, S>,
}

impl<V, K> MultiMap<V, K> {
    pub fn new() -> MultiMap<V, K> {
        MultiMap {
            inner: std::collections::HashMap::new(),
        }
    }

    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: Eq + Hash,
    {
        if let Some(list) = self.inner.get(key) {
            list.back()
        } else {
            None
        }
    }

    pub fn insert(&mut self, key: K, value: V)
    where
        K: Eq + Hash,
    {
        self.inner
            .entry(key)
            .or_insert(Default::default())
            .push_back(value);
    }

    pub fn remove(&mut self, key: &K)
    where
        K: Eq + Hash,
    {
        self.inner.get_mut(key).unwrap().pop_back();
    }
}

enum StackRef<K = Symbol> {
    Symbol(K),
    ScopeDelimiter,
}

pub struct SymbolTable<T, K = Symbol> {
    map: MultiMap<T, K>,
    stack: Stack<StackRef<K>>,
}

impl<T, K> SymbolTable<T, K> {
    pub fn new() -> SymbolTable<T, K> {
        SymbolTable {
            map: MultiMap::<T, K>::new(),
            stack: Stack::<StackRef<K>>::new(),
        }
    }

    pub fn get_symbol(&self, symbol: &K) -> Option<&T>
    where
        K: Eq + Hash,
    {
        self.map.get(symbol)
    }

    pub fn insert_symbol(&mut self, symbol: K, value: T)
    where
        K: Eq + Hash + Clone,
    {
        self.map.insert(symbol.clone(), value);
        self.stack.push(StackRef::Symbol(symbol));
    }

    pub fn begin_scope(&mut self) {
        self.stack.push(StackRef::ScopeDelimiter);
    }

    pub fn end_scope(&mut self)
    where
        K: Eq + Hash,
    {
        while let Some(symbol) = self.stack.pop() {
            match symbol {
                StackRef::Symbol(symbol) => {
                    self.map.remove(&symbol);
                }
                StackRef::ScopeDelimiter => {
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ident_pool;

    use super::*;

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();
        let symbol1 = ident_pool::symbol("s1");
        let symbol2 = ident_pool::symbol("s2");
        let symbol3 = ident_pool::symbol("s3");
        table.begin_scope();
        table.insert_symbol(symbol1, 1);
        table.insert_symbol(symbol2, 2);

        assert_eq!(table.get_symbol(&symbol1), Some(&1));
        assert_eq!(table.get_symbol(&symbol3), None);

        table.insert_symbol(symbol3, 3);
        assert_eq!(table.get_symbol(&symbol3), Some(&3));

        table.begin_scope();
        table.insert_symbol(symbol3, 4);

        assert_eq!(table.get_symbol(&symbol3), Some(&4));

        table.end_scope();

        assert_eq!(table.get_symbol(&symbol3), Some(&3));

        table.end_scope();
        assert_eq!(table.get_symbol(&symbol1), None);
        assert_eq!(table.get_symbol(&symbol2), None);
        assert_eq!(table.get_symbol(&symbol3), None);
    }

    #[test]
    fn test_symbol_table_shadow() {
        let mut table = SymbolTable::new();
        let symbol1 = ident_pool::symbol("s1");
        let symbol2 = ident_pool::symbol("s2");
        let symbol3 = ident_pool::symbol("s3");

        table.begin_scope();
        table.insert_symbol(symbol1, 1);
        assert_eq!(table.get_symbol(&symbol1), Some(&1));
        table.insert_symbol(symbol2, 2);
        table.insert_symbol(symbol3, 3);

        table.insert_symbol(symbol1, 3);
        assert_eq!(table.get_symbol(&symbol1), Some(&3));

        table.begin_scope();
        table.insert_symbol(symbol3, 4);
        assert_eq!(table.get_symbol(&symbol3), Some(&4));

        table.end_scope();
        assert_eq!(table.get_symbol(&symbol3), Some(&3))
    }
}
