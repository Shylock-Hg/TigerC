use crate::symbol_table::SymbolTable;
use crate::type_ast::Type;

struct SymbolValue {
    pub ty: Type,
}

pub struct TypeInference {
    // for type declaration
    type_symbol_table: SymbolTable<SymbolValue>,
    // for variable/function declaration
    variable_symbol_table: SymbolTable<SymbolValue>,
}
