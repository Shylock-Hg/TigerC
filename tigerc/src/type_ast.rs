use crate::ident_pool::Symbol;
use indexmap::IndexMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Nothing,
    Nil,
    Int,
    Str,
    Record(Record),
    Array(Box<Type>),
    Function(Function),
    Name(Symbol),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Record {
    pub fields: IndexMap<Symbol, Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<Type>,
    pub return_ty: Box<Type>,
}

// typed AST...

#[derive(Debug, PartialEq, Eq)]
pub enum TypeAst {
    TypeDecl(TypeDecl),
    TypeExpr(TypeExpr),
}

use crate::ast::Value;

#[derive(Debug, PartialEq, Eq)]
pub enum Unary {
    Negative(Box<TypeExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Binary {
    Add(Box<TypeExpr>, Box<TypeExpr>),
    Minus(Box<TypeExpr>, Box<TypeExpr>),
    Multiply(Box<TypeExpr>, Box<TypeExpr>),
    Divide(Box<TypeExpr>, Box<TypeExpr>),

    Eq(Box<TypeExpr>, Box<TypeExpr>),
    Ne(Box<TypeExpr>, Box<TypeExpr>),
    Gt(Box<TypeExpr>, Box<TypeExpr>),
    Ge(Box<TypeExpr>, Box<TypeExpr>),
    Lt(Box<TypeExpr>, Box<TypeExpr>),
    Le(Box<TypeExpr>, Box<TypeExpr>),

    And(Box<TypeExpr>, Box<TypeExpr>),
    Or(Box<TypeExpr>, Box<TypeExpr>),
}

// RecordExpr => <ident> "{" [<ident> "=" expr, ...] "}"
#[derive(Debug, PartialEq, Eq)]
pub struct RecordExpr {
    pub ty: Symbol,
    pub init: Vec<(Symbol, TypeExpr)>,
}

// ArrayExpr => <ident> "[" <expr> "]" "of" <expr>
#[derive(Debug, PartialEq, Eq)]
pub struct ArrayExpr {
    // element type
    pub ty: Symbol,
    pub len: Box<TypeExpr>,
    pub init: Box<TypeExpr>,
}

// "if" expr "then" expr "else" expr
#[derive(Debug, PartialEq, Eq)]
pub struct IfThenElseExpr {
    pub condition: Box<TypeExpr>,
    pub then: Box<TypeExpr>,
    pub el: Box<TypeExpr>,
}

// "if" expr "then" expr
#[derive(Debug, PartialEq, Eq)]
pub struct IfThen {
    condition: Box<TypeExpr>,
    then: Box<TypeExpr>,
}

// "while" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct While {
    pub condition: Box<TypeExpr>,
    pub body: Box<TypeExpr>,
}

// "for" <ident> ":=" expr "to" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct For {
    pub local: Symbol,
    pub lower: Box<TypeExpr>,
    pub upper: Box<TypeExpr>,
    pub body: Box<TypeExpr>,
}

// "let" decls "in" expr[;expr...] "end"
#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    pub decls: Vec<TypeDecl>,
    pub sequence: Vec<TypeExpr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LeftValue {
    pub left: LeftValue_,
    pub ty: Type,
}

// lvalue => <ident>
//        => lvalue "." <ident>
//        => lvalue "[" expr "]"
#[derive(Debug, PartialEq, Eq)]
pub enum LeftValue_ {
    Variable(Symbol),
    Field(Box<LeftValue>, Symbol),
    Subscript(Box<LeftValue>, Box<TypeExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeExpr_ {
    Literal(Value),
    LeftValue(LeftValue),
    // (exp1; exp2; ...)
    Sequence(Vec<TypeExpr>),
    Unary(Unary),
    Binary(Binary),
    FuncCall(Symbol, Vec<TypeExpr>),
    RecordExpr(RecordExpr),
    ArrayExpr(ArrayExpr),
    Assign(LeftValue, Box<TypeExpr>),
    IfThenElse(IfThenElseExpr),
    IfThen(Box<TypeExpr>, Box<TypeExpr>),
    While(While),
    For(For),
    Break,
    Let(Let),
    Parenthesis(Box<TypeExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeExpr {
    pub expr: TypeExpr_,
    pub ty: Type,
}

// TypeDecl => TypeDecl
//      => VarDecl
//      => FuncDecl
#[derive(Debug, PartialEq, Eq)]
pub enum TypeDecl {
    Type(TyDecl),
    Var(VarDecl),
    Func(FuncDecl),
}

// VarDecl => "var" <ident> ":=" expr
//         => "var" <ident> ":" <ident> := expr
#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub name: Symbol,
    pub ty: Type,
    pub init: TypeExpr,
}

// FuncDecl => "function" <ident> "(" TypeFields ")" "=" expr
//          => "function" <ident> "(" TypeFields ")" ":" <ident> "=" expr
#[derive(Debug, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: Symbol,
    pub args: IndexMap<Symbol, Type>,
    pub ret_ty: Type,
    pub body: TypeExpr,
}

// TypeDecl => "type" IDENT = Ty
// Ty => IDENT
//    => "{" TyFields "}"
//    => "array" "of" IDENT
// TypeFields => %empty
//            => IDENT: IDENT [, IDENT: IDENT...]
#[derive(Debug, PartialEq, Eq)]
pub struct TyDecl {
    pub type_name: Symbol,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ty {
    Name(Symbol),
    Struct(TyStruct),
    Array(Symbol),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TyStruct(pub IndexMap<Symbol, Type>);
