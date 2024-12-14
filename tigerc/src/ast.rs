use crate::ident_pool::Symbol;

pub struct Ast {}

// TypeDecl => "type" IDENT := Ty
// Ty => IDENT
//    => "{" TyFields "}"
//    => "array" "of" IDENT
// TypeFields => %empty
//            => IDENT: IDENT [, IDENT: IDENT...]
pub struct TypeDecl {
    pub type_name: Symbol,
    pub ty: Ty,
}

pub enum Ty {
    Name(Symbol),
    Struct(TyStruct),
    Array(Symbol),
}

pub struct TyStruct(pub Vec<Field>);

pub struct Field {
    pub name: Symbol,
    pub ty: Symbol,
}
