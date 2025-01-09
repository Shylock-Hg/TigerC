use crate::ident_pool::Symbol;

pub enum Ast {
    Decl(Decl),
    Expr(Expr),
}

pub enum Value {
    // nothing, e.g. `()`
    Nothing,
    // nil
    Nil,
    Int(i64),
    Str(String),
}

pub enum Unary {
    Negative(Box<Expr>),
}

pub enum Binary {
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
pub struct RecordExpr {
    ty: Symbol,
    init: Vec<(Symbol, Expr)>,
}

// ArrayExpr => <ident> [expr] of expr
pub struct ArrayExpr {
    // element type
    ty: Symbol,
    len: Box<Expr>,
    init: Box<Expr>,
}

// "if" expr "then" expr "else" expr
pub struct IfThenElseExpr {
    condition: Box<Expr>,
    then: Box<Expr>,
    el: Box<Expr>,
}

// "if" expr "then" expr
pub struct IfThen {
    condition: Box<Expr>,
    then: Box<Expr>,
}
// "while" expr "do" expr
pub struct While {
    condition: Box<Expr>,
    body: Box<Expr>,
}

// "for" <ident> ":=" expr "to" expr "do" expr
pub struct For {
    local: Symbol,
    lower: Box<Expr>,
    upper: Box<Expr>,
    body: Box<Expr>,
}

// "let" decls "in" expr[;expr...] "end"
pub struct Let {
    decls: Vec<Decl>,
    sequence: Vec<Expr>,
}

// lvalue => <ident>
//        => lvalue "." <ident>
//        => lvalue "[" expr "]"
pub enum LeftValue {
    Variable(Symbol),
    Field(Box<LeftValue>, Symbol),
    Subscript(Box<LeftValue>, Box<Expr>),
}

pub enum Expr {
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
pub enum Decl {
    Type(TypeDecl),
    Var(VarDecl),
    Func(FuncDecl),
}

// VarDecl => "var" <ident> ":=" expr
//         => "var" <ident> ":" <ident> := expr
pub struct VarDecl {
    pub name: Symbol,
    pub ty: Option<Symbol>,
    pub init: Expr,
}

// FuncDecl => "function" <ident> "(" TypeFields ")" "=" expr
//          => "function" <ident> "(" TypeFields ")" ":" <ident> "=" expr
pub struct FuncDecl {
    pub name: Symbol,
    pub args: Vec<Field>,
    pub ret_ty: Option<Symbol>,
    pub body: Expr,
}

// TypeDecl => "type" IDENT = Ty
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
