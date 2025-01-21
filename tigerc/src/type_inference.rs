use std::result;

use indexmap::IndexMap;

use crate::ast;
use crate::ident_pool::{kw, Symbol};
use crate::symbol_table::SymbolTable;
use crate::type_ast;

#[derive(Debug)]
pub struct InferError(pub String);

impl InferError {
    pub fn new(msg: String) -> InferError {
        InferError(msg)
    }

    pub fn conflict_type(t1: &type_ast::Type, t2: &type_ast::Type) -> InferError {
        InferError(format!("Expect type {:?} got {:?}.", t1, t2))
    }
}

type Result<T> = result::Result<T, InferError>;

struct SymbolValue {
    pub ty: type_ast::Type,
}

macro_rules! infer_logical_op {
    ($self:ident, $l:ident, $r:ident) => {{
        let ty_left = $self.infer_expr(&$l)?;
        let ty_right = $self.infer_expr(&$r)?;
        match (&ty_left.ty, &ty_right.ty) {
            (type_ast::Type::Int, type_ast::Type::Int) => {
                let ty = ty_left.ty.clone();
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Binary(type_ast::Binary::And(
                        Box::new(ty_left),
                        Box::new(ty_right),
                    )),
                    ty,
                })
            }
            _ => Err(InferError::new(format!(
                "Expect int type for logical operator, got {:?} and {:?}.",
                ty_left.ty, ty_right.ty
            ))),
        }
    }};
}

macro_rules! infer_compare_gl {
    ($self:ident, $l:ident, $r:ident) => {{
        let ty_left = $self.infer_expr(&$l)?;
        let ty_right = $self.infer_expr(&$r)?;
        if ty_left.ty != ty_right.ty {
            return Err(InferError::new(format!("Expect same type for compare.")));
        }
        match ty_left.ty {
            type_ast::Type::Int => Ok(type_ast::TypeExpr {
                expr: type_ast::TypeExpr_::Binary(type_ast::Binary::Eq(
                    Box::new(ty_left),
                    Box::new(ty_right),
                )),
                ty: type_ast::Type::Int,
            }),
            _ => Err(InferError::new(format!(
                "Expect int type for gt/ge/lt/le, got {:?}.",
                ty_left.ty
            ))),
        }
    }};
}

macro_rules! infer_compare_eq_ne {
    ($self:ident, $l:ident, $r:ident) => {{
        let ty_left = $self.infer_expr(&$l)?;
        let ty_right = $self.infer_expr(&$r)?;
        if ty_left.ty != ty_right.ty {
            return Err(InferError::new(format!("Expect same type for equality.")));
        }
        match ty_left.ty {
            type_ast::Type::Int | type_ast::Type::Record(..) | type_ast::Type::Array(..) => {
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Binary(type_ast::Binary::Eq(
                        Box::new(ty_left),
                        Box::new(ty_right),
                    )),
                    ty: type_ast::Type::Int,
                })
            }
            _ => Err(InferError::new(format!(
                "Expect int/record/array type for (not)equality, got {:?}.",
                ty_left.ty
            ))),
        }
    }};
}

macro_rules! infer_arithmetic {
    ($self:ident, $l:ident, $r:ident) => {{
        let ty_l = $self.infer_expr(&$l)?;
        let ty_r = $self.infer_expr(&$r)?;
        match (&ty_l.ty, &ty_r.ty) {
            (type_ast::Type::Int, type_ast::Type::Int) => {
                let ty = ty_l.ty.clone();
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Binary(type_ast::Binary::Add(
                        Box::new(ty_l),
                        Box::new(ty_r),
                    )),
                    ty,
                })
            }
            _ => Err(InferError::new(format!(
                "Expect int type for arithmetic, got {:?} and {:?}.",
                ty_l.ty, ty_r.ty
            ))),
        }
    }};
}

pub struct TypeInference {
    // for type declaration
    type_symbol_table: SymbolTable<SymbolValue>,
    // for variable/function declaration
    variable_symbol_table: SymbolTable<SymbolValue>,
}

impl TypeInference {
    pub fn new() -> Self {
        let mut ty_table = SymbolTable::new();
        ty_table.insert_symbol(
            kw::TOK_INT,
            SymbolValue {
                ty: type_ast::Type::Int,
            },
        );
        ty_table.insert_symbol(
            kw::TOK_STRING,
            SymbolValue {
                ty: type_ast::Type::Str,
            },
        );
        Self {
            type_symbol_table: ty_table,
            variable_symbol_table: SymbolTable::new(),
        }
    }

    pub fn infer(&mut self, ast: &ast::Ast) -> Result<type_ast::TypeAst> {
        match ast {
            ast::Ast::Decl(decl) => Ok(type_ast::TypeAst::TypeDecl(self.infer_decl(decl)?)),
            ast::Ast::Expr(expr) => Ok(type_ast::TypeAst::TypeExpr(self.infer_expr(expr)?)),
        }
    }

    fn infer_decl(&mut self, decl: &ast::Decl) -> Result<type_ast::TypeDecl> {
        match decl {
            ast::Decl::Type(t) => {
                let ty = match &t.ty {
                    ast::Ty::Array(sub) => {
                        let sub = self.type_symbol_table.get_symbol(sub).unwrap().ty.clone();
                        type_ast::Type::Array(Box::new(sub))
                    }
                    ast::Ty::Name(n) => self.type_symbol_table.get_symbol(n).unwrap().ty.clone(),
                    ast::Ty::Struct(s) => type_ast::Type::Record(type_ast::Record {
                        fields: self.infer_field_list(&s.0),
                    }),
                };
                self.type_symbol_table
                    .insert_symbol(t.type_name, SymbolValue { ty });
                Ok(type_ast::TypeDecl::Type(type_ast::TyDecl {
                    type_name: t.type_name,
                    ty: match &t.ty {
                        ast::Ty::Array(s) => type_ast::Ty::Array(*s),
                        ast::Ty::Name(s) => type_ast::Ty::Name(*s),
                        ast::Ty::Struct(s) => {
                            type_ast::Ty::Struct(type_ast::TyStruct(self.infer_field_list(&s.0)))
                        }
                    },
                }))
            }
            ast::Decl::Var(v) => {
                let typed_init = self.infer_expr(&v.init)?;
                let ty =
                    v.ty.map(|t| self.type_symbol_table.get_symbol(&t).unwrap().ty.clone())
                        .unwrap_or(typed_init.ty.clone());
                if typed_init.ty != ty {
                    return Err(InferError::conflict_type(&ty, &typed_init.ty));
                }
                self.variable_symbol_table
                    .insert_symbol(v.name, SymbolValue { ty: ty.clone() });
                Ok(type_ast::TypeDecl::Var(type_ast::VarDecl {
                    name: v.name,
                    ty,
                    init: typed_init,
                }))
            }
            ast::Decl::Func(f) => {
                let typed_args = self.infer_field_list(&f.args);
                let typed_body = self.infer_expr(&f.body)?;
                let ret_ty = f
                    .ret_ty
                    .map(|t| self.type_symbol_table.get_symbol(&t).unwrap().ty.clone())
                    .unwrap_or(typed_body.ty.clone());
                if typed_body.ty != ret_ty {
                    return Err(InferError::conflict_type(&ret_ty, &typed_body.ty));
                }
                self.variable_symbol_table.insert_symbol(
                    f.name,
                    SymbolValue {
                        ty: type_ast::Type::Function(type_ast::Function {
                            name: f.name,
                            params: typed_args
                                .iter()
                                .map(|(_, t)| t.clone())
                                .collect::<Vec<_>>(),
                            return_ty: Box::new(ret_ty.clone()),
                        }),
                    },
                );
                Ok(type_ast::TypeDecl::Func(type_ast::FuncDecl {
                    name: f.name,
                    args: typed_args,
                    ret_ty,
                    body: typed_body,
                }))
            }
        }
    }

    fn infer_decl_list(&mut self, decl_list: &Vec<ast::Decl>) -> Result<Vec<type_ast::TypeDecl>> {
        let mut typed_decl_list = vec![];
        for decl in decl_list.iter() {
            typed_decl_list.push(self.infer_decl(decl)?);
        }
        Ok(typed_decl_list)
    }

    fn infer_expr(&mut self, expr: &ast::Expr) -> Result<type_ast::TypeExpr> {
        match &expr {
            ast::Expr::Literal(l) => Ok(type_ast::TypeExpr {
                expr: type_ast::TypeExpr_::Literal(l.clone()),
                ty: match l {
                    ast::Value::Nothing => type_ast::Type::Nothing,
                    ast::Value::Nil => type_ast::Type::Nil,
                    ast::Value::Int(_) => type_ast::Type::Int,
                    ast::Value::Str(_) => type_ast::Type::Str,
                },
            }),
            ast::Expr::LeftValue(l) => {
                let ty_left = self.infer_left_value(l)?;
                let ty = ty_left.ty.clone();
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::LeftValue(ty_left),
                    ty,
                })
            }
            ast::Expr::Sequence(list) => {
                let mut ty_list = vec![];
                for l in list.iter() {
                    let ty = self.infer_expr(l)?;
                    ty_list.push(ty);
                }
                let ty = ty_list.last().unwrap().ty.clone();
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Sequence(ty_list),
                    ty,
                })
            }
            ast::Expr::Unary(unary) => match &unary {
                &ast::Unary::Negative(e) => {
                    let ty_sub_expr = self.infer_expr(&e)?;
                    if !matches!(ty_sub_expr.ty, type_ast::Type::Int) {
                        Err(InferError::new(format!(
                            "Expect int type for negative, got {:?}.",
                            ty_sub_expr.ty
                        )))
                    } else {
                        let ty = ty_sub_expr.ty.clone();
                        Ok(type_ast::TypeExpr {
                            expr: type_ast::TypeExpr_::Unary(type_ast::Unary::Negative(Box::new(
                                ty_sub_expr,
                            ))),
                            ty,
                        })
                    }
                }
            },
            ast::Expr::Binary(binary) => match binary {
                ast::Binary::Add(l, r) => {
                    infer_arithmetic!(self, l, r)
                }
                ast::Binary::Minus(l, r) => {
                    infer_arithmetic!(self, l, r)
                }
                ast::Binary::Multiply(l, r) => {
                    infer_arithmetic!(self, l, r)
                }
                ast::Binary::Divide(l, r) => {
                    infer_arithmetic!(self, l, r)
                }
                ast::Binary::Eq(l, r) => {
                    infer_compare_eq_ne!(self, l, r)
                }
                ast::Binary::Ne(l, r) => {
                    infer_compare_eq_ne!(self, l, r)
                }
                ast::Binary::Gt(l, r) => {
                    infer_compare_gl!(self, l, r)
                }
                ast::Binary::Ge(l, r) => {
                    infer_compare_gl!(self, l, r)
                }
                ast::Binary::Lt(l, r) => {
                    infer_compare_gl!(self, l, r)
                }
                ast::Binary::Le(l, r) => {
                    infer_compare_gl!(self, l, r)
                }
                ast::Binary::And(l, r) => {
                    infer_logical_op!(self, l, r)
                }
                ast::Binary::Or(l, r) => {
                    infer_logical_op!(self, l, r)
                }
            },
            ast::Expr::FuncCall(f, args) => {
                let ty_func = self
                    .variable_symbol_table
                    .get_symbol(&f)
                    .unwrap()
                    .ty
                    .clone();

                if let type_ast::Type::Function(tf) = ty_func {
                    if tf.params.len() != args.len() {
                        Err(InferError::new(format!(
                            "Expect {} arguments for function `{}`, got {}.",
                            tf.params.len(),
                            tf.name,
                            args.len(),
                        )))
                    } else {
                        let mut ty_args = vec![];
                        for a in args.iter() {
                            let ty_arg = self.infer_expr(a)?;
                            ty_args.push(ty_arg);
                        }

                        for (p, a) in tf.params.iter().zip(ty_args.iter()) {
                            if p != &a.ty {
                                return Err(InferError::new(format!(
                                    "Expect {:?} type for argument {} but got {:?}.",
                                    p, f, a.ty
                                )));
                            }
                        }
                        Ok(type_ast::TypeExpr {
                            expr: type_ast::TypeExpr_::FuncCall(tf.name, ty_args),
                            ty: *tf.return_ty,
                        })
                    }
                } else {
                    Err(InferError::new(format!(
                        "Expect function type for function call, got {:?}.",
                        ty_func
                    )))
                }
            }
            ast::Expr::RecordExpr(record) => {
                let ty_record = self
                    .type_symbol_table
                    .get_symbol(&record.ty)
                    .unwrap()
                    .ty
                    .clone();
                if let type_ast::Type::Record(ty_record_inner) = &ty_record {
                    let mut init = vec![];
                    for i in 0..ty_record_inner.fields.len() {
                        let (f, t) = ty_record_inner.fields.get_index(i).unwrap();
                        let (ef, ee) = &record.init[i];
                        if *f != *ef {
                            return Err(InferError::new(format!(
                                "Expect field name `{}` but got `{}`.",
                                f, ef,
                            )));
                        }
                        let ty_arg = self.infer_expr(&ee)?;
                        if t != &ty_arg.ty {
                            return Err(InferError::new(format!(
                                "Expect {:?} type for field {}, got {:?}.",
                                t, f, ty_arg.ty,
                            )));
                        }
                        init.push((*f, ty_arg));
                    }
                    Ok(type_ast::TypeExpr {
                        ty: ty_record,
                        expr: type_ast::TypeExpr_::RecordExpr(type_ast::RecordExpr {
                            ty: record.ty,
                            init,
                        }),
                    })
                } else {
                    Err(InferError::new(format!(
                        "Expect Record type bug got {:?}",
                        ty_record
                    )))
                }
            }
            ast::Expr::ArrayExpr(array) => {
                let ty_array = self
                    .type_symbol_table
                    .get_symbol(&array.ty)
                    .unwrap()
                    .ty
                    .clone();
                if let type_ast::Type::Array(ty_array_inner) = &ty_array {
                    let ty_len = self.infer_expr(&array.len)?;
                    if !matches!(ty_len.ty, type_ast::Type::Int) {
                        return Err(InferError::new(format!(
                            "Expect int type for array size, got {:?}.",
                            ty_len.ty
                        )));
                    }
                    let ty_init = self.infer_expr(&array.init)?;
                    if ty_init.ty != **ty_array_inner {
                        return Err(InferError::new(format!(
                            "Expect {:?} type for array init, got {:?}.",
                            ty_array_inner, ty_init.ty
                        )));
                    }
                    Ok(type_ast::TypeExpr {
                        ty: ty_array,
                        expr: type_ast::TypeExpr_::ArrayExpr(type_ast::ArrayExpr {
                            ty: array.ty,
                            len: Box::new(ty_len),
                            init: Box::new(ty_init),
                        }),
                    })
                } else {
                    Err(InferError::new(format!(
                        "Expect Array type but got {:?}",
                        ty_array
                    )))
                }
            }
            ast::Expr::Assign(l, init) => {
                let ty_left = self.infer_left_value(l)?;
                let ty_init = self.infer_expr(init)?;
                if ty_left.ty != ty_init.ty {
                    return Err(InferError::conflict_type(&ty_left.ty, &ty_init.ty));
                }
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Assign(ty_left, Box::new(ty_init)),
                    ty: type_ast::Type::Nothing,
                })
            }
            ast::Expr::IfThenElse(ast::IfThenElseExpr {
                condition,
                then,
                el,
            }) => {
                let ty_condition = self.infer_expr(condition)?;
                if !matches!(ty_condition.ty, type_ast::Type::Int) {
                    return Err(InferError::new(format!(
                        "Expect int type for if condition, got {:?}.",
                        ty_condition.ty
                    )));
                }
                let ty_then = self.infer_expr(then)?;
                let ty_el = self.infer_expr(el)?;
                if ty_then.ty != ty_el.ty {
                    Err(InferError::new(format!(
                        "Expect same type for then and else, but got {:?} and {:?}",
                        ty_then.ty, ty_el.ty
                    )))
                } else {
                    let ty = ty_then.ty.clone();
                    Ok(type_ast::TypeExpr {
                        expr: type_ast::TypeExpr_::IfThenElse(type_ast::IfThenElseExpr {
                            condition: Box::new(ty_condition),
                            then: Box::new(ty_then),
                            el: Box::new(ty_el),
                        }),
                        ty,
                    })
                }
            }
            ast::Expr::IfThen(condition, then) => {
                let ty_condition = self.infer_expr(condition)?;
                if !matches![ty_condition.ty, type_ast::Type::Int] {
                    return Err(InferError::new(format!(
                        "Expect int type for if condition, got {:?}.",
                        ty_condition.ty
                    )));
                }
                let ty_then = self.infer_expr(then)?;
                if !matches!(ty_then.ty, type_ast::Type::Nothing) {
                    return Err(InferError::new(format!(
                        "Expect nothing type for if then, got {:?}.",
                        ty_then.ty
                    )));
                }
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::IfThen(Box::new(ty_condition), Box::new(ty_then)),
                    ty: type_ast::Type::Nothing,
                })
            }
            ast::Expr::While(while_) => {
                let ty_condition = self.infer_expr(&while_.condition)?;
                if !matches![ty_condition.ty, type_ast::Type::Int] {
                    return Err(InferError::new(format!(
                        "Expect int type for while condition, got {:?}.",
                        ty_condition.ty,
                    )));
                }
                let ty_body = self.infer_expr(&while_.body)?;
                if !matches![ty_body.ty, type_ast::Type::Nothing] {
                    return Err(InferError::new(format!(
                        "Expect nothing type for while body, got {:?}.",
                        ty_body.ty,
                    )));
                }
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::While(type_ast::While {
                        condition: Box::new(ty_condition),
                        body: Box::new(ty_body),
                    }),
                    ty: type_ast::Type::Nothing,
                })
            }
            ast::Expr::For(for_) => {
                let ty_lower = self.infer_expr(&for_.lower)?;
                if !matches![ty_lower.ty, type_ast::Type::Int] {
                    return Err(InferError::new(format!(
                        "Expect int type for for loop lower bound, got {:?}.",
                        ty_lower.ty,
                    )));
                }
                let ty_upper = self.infer_expr(&for_.upper)?;
                if !matches![ty_upper.ty, type_ast::Type::Int] {
                    return Err(InferError::new(format!(
                        "Expect int type for for loop upper bound, got {:?}.",
                        ty_upper.ty,
                    )));
                }
                let ty_body = self.infer_expr(&for_.body)?;
                if !matches![ty_body.ty, type_ast::Type::Nothing] {
                    return Err(InferError::new(format!(
                        "Expect nothing type for for loop body, got {:?}.",
                        ty_body.ty,
                    )));
                }
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::For(type_ast::For {
                        local: for_.local,
                        lower: Box::new(ty_lower),
                        upper: Box::new(ty_upper),
                        body: Box::new(ty_body),
                    }),
                    ty: type_ast::Type::Nothing,
                })
            }
            ast::Expr::Break => {
                // TODO check break in a loop here?
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Break,
                    ty: type_ast::Type::Nothing,
                })
            }
            ast::Expr::Let(let_) => {
                let ty_decls = self.infer_decl_list(&let_.decls)?;
                let ty_expr_list = self.infer_expr_list(&let_.sequence)?;
                let ty = ty_expr_list
                    .last()
                    .map(|e| e.ty.clone())
                    .unwrap_or(type_ast::Type::Nothing);
                Ok(type_ast::TypeExpr {
                    expr: type_ast::TypeExpr_::Let(type_ast::Let {
                        decls: ty_decls,
                        sequence: ty_expr_list,
                    }),
                    ty,
                })
            }
            ast::Expr::Parenthesis(e) => {
                // For semantic AST, we don't need parenthesis again.
                let ty_e = self.infer_expr(e)?;
                Ok(ty_e)
            }
        }
    }

    fn infer_expr_list(&mut self, expr_list: &Vec<ast::Expr>) -> Result<Vec<type_ast::TypeExpr>> {
        let mut ty_expr_list = vec![];
        for expr in expr_list {
            let ty_expr = self.infer_expr(expr)?;
            ty_expr_list.push(ty_expr);
        }
        Ok(ty_expr_list)
    }

    fn infer_left_value(&mut self, left: &ast::LeftValue) -> Result<type_ast::LeftValue> {
        match &left {
            ast::LeftValue::Variable(v) => {
                let ty = self
                    .variable_symbol_table
                    .get_symbol(&v)
                    .unwrap()
                    .ty
                    .clone();
                Ok(type_ast::LeftValue {
                    left: type_ast::LeftValue_::Variable(*v),
                    ty,
                })
            }
            ast::LeftValue::Field(l, f) => {
                let ty_left = self.infer_left_value(l)?;
                if let type_ast::Type::Record(r) = &ty_left.ty {
                    let ty = r.fields.get(f).unwrap().clone();
                    Ok(type_ast::LeftValue {
                        left: type_ast::LeftValue_::Field(Box::new(ty_left), *f),
                        ty,
                    })
                } else {
                    Err(InferError::new(format!(
                        "Expect record type for field access, got {:?}.",
                        ty_left.ty
                    )))
                }
            }
            ast::LeftValue::Subscript(l, i) => {
                let ty_left = self.infer_left_value(l)?;
                let ty_i = self.infer_expr(i)?;
                if !matches!(ty_i.ty, type_ast::Type::Int) {
                    return Err(InferError::new(format!(
                        "Expect int type for subscript index, got {:?}.",
                        ty_i.ty
                    )));
                }
                if let type_ast::Type::Array(a) = ty_left.ty.clone() {
                    Ok(type_ast::LeftValue {
                        left: type_ast::LeftValue_::Subscript(Box::new(ty_left), Box::new(ty_i)),
                        ty: *a,
                    })
                } else {
                    Err(InferError::new(format!(
                        "Expect array type for subscript access, got {:?}.",
                        ty_left.ty
                    )))
                }
            }
        }
    }

    fn infer_field_list(&self, fields: &Vec<ast::Field>) -> IndexMap<Symbol, type_ast::Type> {
        fields
            .iter()
            .map(|f| {
                (
                    f.name,
                    self.type_symbol_table.get_symbol(&f.ty).unwrap().ty.clone(),
                )
            })
            .collect::<IndexMap<_, _>>()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::tokenizer::tokenize;

    fn test_arithmetic_type() {
        let doc = "
        (1+1)*2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse();
        let mut ti = TypeInference::new();
        let te = ti.infer(&e).unwrap();
        if let type_ast::TypeAst::TypeExpr(te) = te {
            assert_eq!(te.ty, type_ast::Type::Int);
        } else {
            panic!("Unexpected decl.");
        }
    }
}
