// index to node in graph vector
pub struct Entry(pub usize);

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

    pub fn add_outcome(&mut self, node: Entry) {
        self.outcome.push(node);
    }
}

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
}
