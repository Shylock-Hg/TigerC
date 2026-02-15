// index to node in graph vector
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Entry(pub usize);

#[derive(Debug)]
pub struct Node<T> {
    value: T,
    income: Vec<Entry>,
    outcome: Vec<Entry>,
}

impl<T> Node<T> {
    pub fn new(value: T, income: Vec<Entry>, outcome: Vec<Entry>) -> Self {
        Self {
            value,
            income,
            outcome,
        }
    }

    pub fn add_income(&mut self, node: Entry) {
        self.income.push(node);
    }

    pub fn income(&self) -> &Vec<Entry> {
        &self.income
    }

    pub fn add_outcome(&mut self, node: Entry) {
        self.outcome.push(node);
    }

    pub fn outcome(&self) -> &Vec<Entry> {
        &self.outcome
    }

    pub fn value(&self) -> &T {
        &self.value
    }
}

#[derive(Debug)]
pub struct Graph<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Graph<T> {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn add_node(&mut self, node: Node<T>) {
        self.nodes.push(node);
    }

    pub fn mut_node(&mut self, entry: &Entry) -> &mut Node<T> {
        self.nodes.get_mut(entry.0).unwrap()
    }

    pub fn get_node(&self, t: &T) -> Option<Entry>
    where
        T: PartialEq,
    {
        // TODO speed up by HashTable
        self.nodes
            .iter()
            .position(|v| *v.value() == *t)
            .map(|v| Entry(v))
    }

    pub fn node(&self, entry: &Entry) -> &Node<T> {
        self.nodes.get(entry.0).unwrap()
    }

    pub fn last_node(&self) -> &Node<T> {
        self.nodes.last().unwrap()
    }

    pub fn last_node_mut(&mut self) -> &mut Node<T> {
        self.nodes.last_mut().unwrap()
    }

    pub fn last_entry(&self) -> Entry {
        Entry(self.nodes.len() - 1)
    }

    pub fn nodes(&self) -> &Vec<Node<T>> {
        &self.nodes
    }
}
