use crate::ir::LowerIdent;

// for variable
#[derive(Debug, PartialEq, Eq)]
pub struct Temp(pub LowerIdent);

// for function or code block location
#[derive(Debug, PartialEq, Eq)]
pub struct Label(pub LowerIdent);
