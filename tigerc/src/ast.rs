use std::fmt::Display;

use crate::ident_pool::Symbol;

#[derive(Debug, PartialEq, Eq)]
pub enum Ast {
    Decl(Decl),
    Expr(Expr),
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ast::Decl(decl) => write!(f, "{}", decl),
            Ast::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    // nothing, e.g. `()`
    Nothing,
    // nil
    Nil,
    Int(i64),
    Str(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nothing => write!(f, "()"),
            Value::Nil => write!(f, "nil"),
            Value::Int(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Unary {
    Negative(Box<Expr>),
}

impl Display for Unary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unary::Negative(expr) => write!(f, "-{}", expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binary::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Binary::Minus(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Binary::Multiply(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Binary::Divide(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),

            Binary::Eq(lhs, rhs) => write!(f, "({} = {})", lhs, rhs),
            Binary::Ne(lhs, rhs) => write!(f, "({} <> {})", lhs, rhs),
            Binary::Gt(lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            Binary::Ge(lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            Binary::Lt(lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            Binary::Le(lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),

            Binary::And(lhs, rhs) => write!(f, "({} & {})", lhs, rhs),
            Binary::Or(lhs, rhs) => write!(f, "({} | {})", lhs, rhs),
        }
    }
}

// RecordExpr => <ident> "{" [<ident> "=" expr, ...] "}"
#[derive(Debug, PartialEq, Eq)]
pub struct RecordExpr {
    pub ty: Symbol,
    pub init: Vec<(Symbol, Expr)>,
}

impl Display for RecordExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.ty)?;
        for (i, (field, expr)) in self.init.iter().enumerate() {
            write!(f, "{} = {}", field, expr)?;
            if i < (self.init.len() - 1) {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

// ArrayExpr => <ident> "[" <expr> "]" "of" <expr>
#[derive(Debug, PartialEq, Eq)]
pub struct ArrayExpr {
    // element type
    pub ty: Symbol,
    pub len: Box<Expr>,
    pub init: Box<Expr>,
}

impl Display for ArrayExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.ty, self.len)?;
        write!(f, " of {}", self.init)
    }
}

// "if" expr "then" expr "else" expr
#[derive(Debug, PartialEq, Eq)]
pub struct IfThenElseExpr {
    pub condition: Box<Expr>,
    pub then: Box<Expr>,
    pub el: Box<Expr>,
}

impl Display for IfThenElseExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if {} then {} else {}",
            self.condition, self.then, self.el
        )
    }
}

// "if" expr "then" expr
#[derive(Debug, PartialEq, Eq)]
pub struct IfThen {
    condition: Box<Expr>,
    then: Box<Expr>,
}

impl Display for IfThen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} then {}", self.condition, self.then)
    }
}

// "while" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct While {
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {} do {}", self.condition, self.body)
    }
}

// "for" <ident> ":=" expr "to" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct For {
    local: Symbol,
    lower: Box<Expr>,
    upper: Box<Expr>,
    body: Box<Expr>,
}

impl Display for For {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "for {} := {} to {} do {}",
            self.local, self.lower, self.upper, self.body
        )
    }
}

// "let" decls "in" expr[;expr...] "end"
#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    decls: Vec<Decl>,
    sequence: Vec<Expr>,
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let ")?;
        for (i, decl) in self.decls.iter().enumerate() {
            write!(f, "{} ", decl)?;
        }
        write!(f, " in ")?;
        for (i, expr) in self.sequence.iter().enumerate() {
            write!(f, "{}", expr)?;
            if i < (self.sequence.len() - 1) {
                write!(f, "; ")?;
            }
        }
        write!(f, " end")
    }
}

// lvalue => <ident>
//        => lvalue "." <ident>
//        => lvalue "[" expr "]"
#[derive(Debug, PartialEq, Eq)]
pub enum LeftValue {
    Variable(Symbol),
    Field(Box<LeftValue>, Symbol),
    Subscript(Box<LeftValue>, Box<Expr>),
}

impl Display for LeftValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LeftValue::Variable(ident) => write!(f, "{}", ident),
            LeftValue::Field(lvalue, field) => write!(f, "{}.{}", lvalue, field),
            LeftValue::Subscript(lvalue, expr) => write!(f, "{}[{}]", lvalue, expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Literal(Value),
    LeftValue(LeftValue),
    // (exp1; exp2; ...)
    Sequence(Vec<Expr>),
    Unary(Unary),
    Binary(Binary),
    FuncCall(Symbol, Vec<Expr>),
    RecordExpr(RecordExpr),
    ArrayExpr(ArrayExpr),
    Assign(LeftValue, Box<Expr>),
    IfThenElse(IfThenElseExpr),
    IfThen(Box<Expr>, Box<Expr>),
    While(While),
    For(For),
    Break,
    Let(Let),
    Parenthesis(Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::LeftValue(lvalue) => write!(f, "{}", lvalue),
            Expr::Sequence(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    write!(f, "{}", expr)?;
                    if i < (exprs.len() - 1) {
                        write!(f, "; ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Unary(unary) => write!(f, "{}", unary),
            Expr::Binary(binary) => write!(f, "{}", binary),
            Expr::FuncCall(func, args) => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < (args.len() - 1) {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::RecordExpr(record) => write!(f, "{}", record),
            Expr::ArrayExpr(array) => write!(f, "{}", array),
            Expr::Assign(lvalue, expr) => write!(f, "{} := {}", lvalue, expr),
            Expr::IfThenElse(ite) => write!(f, "{}", ite),
            Expr::IfThen(cond, then) => write!(f, "if {} then {}", cond, then),
            Expr::While(while_expr) => write!(f, "{}", while_expr),
            Expr::For(for_expr) => write!(f, "{}", for_expr),
            Expr::Break => write!(f, "break"),
            Expr::Let(let_expr) => write!(f, "{}", let_expr),
            Expr::Parenthesis(expr) => write!(f, "({})", expr),
        }
    }
}

// Decl => TypeDecl
//      => VarDecl
//      => FuncDecl
#[derive(Debug, PartialEq, Eq)]
pub enum Decl {
    Type(TypeDecl),
    Var(VarDecl),
    Func(FuncDecl),
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Type(ty) => write!(f, "{}", ty),
            Decl::Var(var) => write!(f, "{}", var),
            Decl::Func(func) => write!(f, "{}", func),
        }
    }
}

// VarDecl => "var" <ident> ":=" expr
//         => "var" <ident> ":" <ident> := expr
#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub name: Symbol,
    pub ty: Option<Symbol>,
    pub init: Expr,
}

impl Display for VarDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            Some(ty) => write!(f, "var {} : {} := {}", self.name, ty, self.init),
            None => write!(f, "var {} := {}", self.name, self.init),
        }
    }
}

// FuncDecl => "function" <ident> "(" TypeFields ")" "=" expr
//          => "function" <ident> "(" TypeFields ")" ":" <ident> "=" expr
#[derive(Debug, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: Symbol,
    pub args: Vec<Field>,
    pub ret_ty: Option<Symbol>,
    pub body: Expr,
}

impl Display for FuncDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {} (", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            write!(f, "{}: {}", arg.name, arg.ty)?;
            if i < (self.args.len() - 1) {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        match &self.ret_ty {
            Some(ty) => write!(f, ": {} = {}", ty, self.body),
            None => write!(f, " = {}", self.body),
        }
    }
}

// TypeDecl => "type" IDENT = Ty
// Ty => IDENT
//    => "{" TyFields "}"
//    => "array" "of" IDENT
// TypeFields => %empty
//            => IDENT: IDENT [, IDENT: IDENT...]
#[derive(Debug, PartialEq, Eq)]
pub struct TypeDecl {
    pub type_name: Symbol,
    pub ty: Ty,
}

impl Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {} = {}", self.type_name, self.ty)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ty {
    Name(Symbol),
    Struct(TyStruct),
    Array(Symbol),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Name(name) => write!(f, "{}", name),
            Ty::Struct(fields) => write!(f, "{}", fields),
            Ty::Array(ty) => write!(f, "array of {}", ty),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TyStruct(pub Vec<Field>);

impl Display for TyStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, field) in self.0.iter().enumerate() {
            write!(f, "{}", field)?;
            if i < (self.0.len() - 1) {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    pub name: Symbol,
    pub ty: Symbol,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}
