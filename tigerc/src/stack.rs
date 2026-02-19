use std::collections::LinkedList;

pub struct Stack<T> {
    list: LinkedList<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack {
            list: LinkedList::new(),
        }
    }

    pub fn push(&mut self, value: T) {
        self.list.push_back(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.list.pop_back()
    }

    pub fn contains(&self, t: &T) -> bool
    where
        T: Eq,
    {
        self.list.contains(t)
    }
}
