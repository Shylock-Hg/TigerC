use std::fmt::Display;

use crate::ident_pool::Symbol;
use indexmap::IndexMap;

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Record {
    pub fields: IndexMap<Symbol, Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: Symbol,
    pub params: Vec<Type>,
    pub return_ty: Box<Type>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeAst {
    TypeDecl(TypeDecl),
    TypeExpr(TypeExpr),
}

impl Display for TypeAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAst::TypeDecl(decl) => write!(f, "{}", decl),
            TypeAst::TypeExpr(expr) => write!(f, "{}", expr),
        }
    }
}

use crate::ast::Value;

#[derive(Debug, PartialEq, Eq)]
pub enum Unary {
    Negative(Box<TypeExpr>),
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
    pub init: Vec<(Symbol, TypeExpr)>,
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
    pub len: Box<TypeExpr>,
    pub init: Box<TypeExpr>,
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
    pub condition: Box<TypeExpr>,
    pub then: Box<TypeExpr>,
    pub el: Box<TypeExpr>,
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
    condition: Box<TypeExpr>,
    then: Box<TypeExpr>,
}

impl Display for IfThen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} then {}", self.condition, self.then)
    }
}

// "while" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct While {
    pub condition: Box<TypeExpr>,
    pub body: Box<TypeExpr>,
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while {} do {}", self.condition, self.body)
    }
}

// "for" <ident> ":=" expr "to" expr "do" expr
#[derive(Debug, PartialEq, Eq)]
pub struct For {
    pub local: Symbol,
    pub lower: Box<TypeExpr>,
    pub upper: Box<TypeExpr>,
    pub body: Box<TypeExpr>,
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
    pub decls: Vec<TypeDecl>,
    pub sequence: Vec<TypeExpr>,
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
    Subscript(Box<LeftValue>, Box<TypeExpr>),
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
pub enum TypeExpr {
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

impl Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeExpr::Literal(value) => write!(f, "{}", value),
            TypeExpr::LeftValue(lvalue) => write!(f, "{}", lvalue),
            TypeExpr::Sequence(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    write!(f, "{}", expr)?;
                    if i < (exprs.len() - 1) {
                        write!(f, "; ")?;
                    }
                }
                write!(f, ")")
            }
            TypeExpr::Unary(unary) => write!(f, "{}", unary),
            TypeExpr::Binary(binary) => write!(f, "{}", binary),
            TypeExpr::FuncCall(func, args) => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < (args.len() - 1) {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            TypeExpr::RecordExpr(record) => write!(f, "{}", record),
            TypeExpr::ArrayExpr(array) => write!(f, "{}", array),
            TypeExpr::Assign(lvalue, expr) => write!(f, "{} := {}", lvalue, expr),
            TypeExpr::IfThenElse(ite) => write!(f, "{}", ite),
            TypeExpr::IfThen(cond, then) => write!(f, "if {} then {}", cond, then),
            TypeExpr::While(while_expr) => write!(f, "{}", while_expr),
            TypeExpr::For(for_expr) => write!(f, "{}", for_expr),
            TypeExpr::Break => write!(f, "break"),
            TypeExpr::Let(let_expr) => write!(f, "{}", let_expr),
            TypeExpr::Parenthesis(expr) => write!(f, "({})", expr),
        }
    }
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

impl Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDecl::Type(ty) => write!(f, "{}", ty),
            TypeDecl::Var(var) => write!(f, "{}", var),
            TypeDecl::Func(func) => write!(f, "{}", func),
        }
    }
}

// VarDecl => "var" <ident> ":=" expr
//         => "var" <ident> ":" <ident> := expr
#[derive(Debug, PartialEq, Eq)]
pub struct VarDecl {
    pub name: Symbol,
    pub ty: Option<Symbol>,
    pub init: TypeExpr,
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
    pub body: TypeExpr,
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
pub struct TyDecl {
    pub type_name: Symbol,
    pub ty: Ty,
}

impl Display for TyDecl {
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
