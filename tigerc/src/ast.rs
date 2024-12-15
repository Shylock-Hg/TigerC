use crate::ident_pool::Symbol;

pub struct Ast {}

enum Value {
    // nothing, e.g. `()`
    Nothing,
    // nil
    Nil,
    Int(i64),
    Str(String),
}

enum Unary {
    Negative(Box<Expr>),
}

enum Binary {
    Add(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),

    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),

    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

// RecordExpr => <ident> "{" [<ident> "=" expr, ...] "}"
struct RecordExpr {
    ty: Symbol,
    init: Vec<(Symbol, Expr)>,
}

// ArrayExpr => <ident> [expr] of expr
struct ArrayExpr {
    // element type
    ty: Symbol,
    len: Box<Expr>,
    init: Box<Expr>,
}

// "if" expr "then" expr "else" expr
struct IfThenElseExpr {
    condition: Box<Expr>,
    then: Box<Expr>,
    el: Box<Expr>,
}

// "if" expr "then" expr
struct IfThen {
    condition: Box<Expr>,
    then: Box<Expr>,
}
// "while" expr "do" expr
struct While {
    condition: Box<Expr>,
    body: Box<Expr>,
}

// "for" <ident> ":=" expr "to" expr "do" expr
struct For {
    local: Symbol,
    lower: Box<Expr>,
    upper: Box<Expr>,
    body: Box<Expr>,
}

// "let" decls "in" expr[;expr...] "end"
struct Let {
    decls: Vec<Decl>,
    sequence: Vec<Expr>,
}

// lvalue => <ident>
//        => lvalue "." <ident>
//        => lvalue "[" expr "]"
enum LeftValue {
    Variable(Symbol),
    Field(Box<LeftValue>, Symbol),
    Subscript(Box<LeftValue>, Box<Expr>),
}

enum Expr {
    Literal(Value),
    LeftValue(LeftValue),
    // (exp1, exp2, ...)
    Sequence(Vec<Expr>),
    Unary(Unary),
    Binary(Binary),
    FuncCall(Symbol, Vec<Expr>),
    RecordExpr(RecordExpr),
    ArrayExpr(ArrayExpr),
    Assign(Symbol, Box<Expr>),
    IfThenElse(IfThenElseExpr),
    IfThen(Box<Expr>, Box<Expr>),
    While(While),
    For(For),
    Break,
    Let(Let),
    Parenthesis(Box<Expr>),
}

// Decl => TypeDecl
//      => VarDecl
//      => FuncDecl
enum Decl {
    Type(TypeDecl),
    Var,
    Func,
}

// VarDecl => "var" <ident> ":=" expr
//         => "var" <ident> ":" <ident> := expr
struct VarDecl {
    name: Symbol,
    ty: Option<Symbol>,
    init: Expr,
}

// FuncDecl => "function" <ident> "(" TypeFields ")" "=" expr
//          => "function" <ident> "(" TypeFields ")" ":" <ident> "=" expr
struct FuncDecl {
    name: Symbol,
    args: Vec<Field>,
    ret_ty: Option<Symbol>,
    body: Expr,
}

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
