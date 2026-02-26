use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use crate::ast;
use crate::ident_pool::Symbol;

pub struct Escape {
    e: Option<Rc<RefCell<bool>>>,
}

impl Default for Escape {
    fn default() -> Self {
        Self::new()
    }
}

impl Escape {
    pub fn new() -> Self {
        Escape { e: None }
    }

    // mark escape parameters/local variables
    pub fn escape(&mut self, ast: &mut ast::Ast) {
        self.escape_expr(&mut ast.0, 0, 0, &None);
    }

    // depth of nested function
    fn escape_expr(
        &mut self,
        expr: &mut ast::Expr,
        depth: u32,
        var_depth: u32,
        var: &Option<Symbol>,
    ) {
        match expr {
            ast::Expr::Literal(_) => {}
            ast::Expr::LeftValue(l) => {
                self.escape_left_value(l, depth, var_depth, var);
            }
            ast::Expr::Sequence(seq) => {
                for e in seq {
                    self.escape_expr(e, depth, var_depth, var);
                }
            }
            ast::Expr::Unary(ast::Unary::Negative(e)) => {
                self.escape_expr(e, depth, var_depth, var);
            }
            ast::Expr::Binary(binary) => match binary {
                ast::Binary::Add(l, r)
                | ast::Binary::Minus(l, r)
                | ast::Binary::Multiply(l, r)
                | ast::Binary::Divide(l, r)
                | ast::Binary::Eq(l, r)
                | ast::Binary::Ne(l, r)
                | ast::Binary::Gt(l, r)
                | ast::Binary::Ge(l, r)
                | ast::Binary::Lt(l, r)
                | ast::Binary::Le(l, r)
                | ast::Binary::And(l, r)
                | ast::Binary::Or(l, r) => {
                    self.escape_expr(l, depth, var_depth, var);
                    self.escape_expr(r, depth, var_depth, var);
                }
            },
            ast::Expr::FuncCall(_, args) => {
                for a in args {
                    self.escape_expr(a, depth, var_depth, var);
                }
            }
            ast::Expr::RecordExpr(r) => {
                for (_, e) in &mut r.init {
                    self.escape_expr(e, depth, var_depth, var);
                }
            }
            ast::Expr::ArrayExpr(a) => {
                self.escape_expr(&mut a.len, depth, var_depth, var);
                self.escape_expr(&mut a.init, depth, var_depth, var);
            }
            ast::Expr::Assign(l, e) => {
                self.escape_left_value(l, depth, var_depth, var);
                self.escape_expr(e, depth, var_depth, var);
            }
            ast::Expr::IfThenElse(i) => {
                self.escape_expr(&mut i.condition, depth, var_depth, var);
                self.escape_expr(&mut i.then, depth, var_depth, var);
                self.escape_expr(&mut i.el, depth, var_depth, var);
            }
            ast::Expr::IfThen(cond, then) => {
                self.escape_expr(cond, depth, var_depth, var);
                self.escape_expr(then, depth, var_depth, var);
            }
            ast::Expr::While(w) => {
                self.escape_expr(&mut w.condition, depth, var_depth, var);
                self.escape_expr(&mut w.body, depth, var_depth, var);
            }
            ast::Expr::For(f) => {
                self.escape_expr(&mut f.lower, depth, var_depth, var);
                self.escape_expr(&mut f.upper, depth, var_depth, var);
                self.escape_expr(&mut f.body, depth, var_depth, var);
            }
            ast::Expr::Break => {}
            ast::Expr::Let(ast::Let { decls, sequence }) => {
                let mut new_vars: Vec<(Symbol, Rc<RefCell<bool>>)> = vec![];
                for d in decls.iter_mut() {
                    match d {
                        ast::Decl::Var(v) => {
                            let new_var = v.name;
                            self.e = Some(v.escape.clone());
                            for e in sequence.iter_mut() {
                                self.escape_expr(e, depth, depth, &Some(new_var));
                            }
                            for (new_var, escape) in &new_vars {
                                self.e = Some(escape.clone());
                                self.escape_expr(&mut v.init, depth, depth, &Some(*new_var));
                            }
                            new_vars.push((new_var, v.escape.clone()));
                        }
                        ast::Decl::Func(f) => {
                            self.escape_expr(&mut f.body, depth + 1, var_depth, var);
                            for p in &mut f.args {
                                self.e = Some(p.escape.clone());
                                self.escape_expr(&mut f.body, depth + 1, depth + 1, &Some(p.name));
                            }
                            for (new_var, escape) in &new_vars {
                                self.e = Some(escape.clone());
                                self.escape_expr(
                                    &mut f.body,
                                    depth + 1,
                                    var_depth,
                                    &Some(*new_var),
                                );
                            }
                        }
                        ast::Decl::Type(_) => {}
                    }
                }
            }
            ast::Expr::Parenthesis(e) => {
                self.escape_expr(e, depth, var_depth, var);
            }
        }
    }

    fn escape_left_value(
        &mut self,
        l: &mut ast::LeftValue,
        depth: u32,
        var_depth: u32,
        var: &Option<Symbol>,
    ) {
        match l {
            ast::LeftValue::Variable(s) => {
                // escape!(depth, var_depth, s, var, self.e);
                if let Some(v) = var {
                    if depth > var_depth && v == s {
                        if let Some(e) = &self.e {
                            e.replace(true);
                        }
                    }
                }
            }
            ast::LeftValue::Field(l, _) => {
                self.escape_left_value(l, depth, var_depth, var);
            }
            ast::LeftValue::Subscript(l, e) => {
                self.escape_left_value(l, depth, var_depth, var);
                self.escape_expr(e, depth, var_depth, var);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ident_pool;
    use crate::parser::Parser;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_escape() {
        let doc = "
        let type r1 = {f1: int, f2: string} var v1 := r1 {f1 = 1, f2 = \"hello\"} in  end
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let mut ast = parser.parse();
        let expected = ast::Ast(ast::Expr::Let(ast::Let {
            decls: vec![
                ast::Decl::Type(ast::TypeDecl {
                    type_name: ident_pool::symbol("r1"),
                    ty: ast::Ty::Record(ast::TyRecord(vec![
                        ast::Field {
                            name: ident_pool::symbol("f1"),
                            ty: ident_pool::symbol("int"),
                        },
                        ast::Field {
                            name: ident_pool::symbol("f2"),
                            ty: ident_pool::symbol("string"),
                        },
                    ])),
                }),
                ast::Decl::Var(ast::VarDecl {
                    name: ident_pool::symbol("v1"),
                    ty: None,
                    init: ast::Expr::RecordExpr(ast::RecordExpr {
                        ty: ident_pool::symbol("r1"),
                        init: vec![
                            (
                                ident_pool::symbol("f1"),
                                ast::Expr::Literal(ast::Value::Int(1)),
                            ),
                            (
                                ident_pool::symbol("f2"),
                                ast::Expr::Literal(ast::Value::Str("hello".to_owned())),
                            ),
                        ],
                    }),
                    escape: Rc::new(RefCell::new(false)),
                }),
            ],
            sequence: vec![],
        }));
        Escape::new().escape(&mut ast);
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_escape_var() {
        let doc = "
        let
          type r1 = {f1: int, f2: string} var v1 := r1 {f1 = 1, f2 = \"hello\"}
          function f1(x: int) = x + v1.f1
        in
        end
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let mut ast = parser.parse();
        Escape::new().escape(&mut ast);
        let escape = match ast {
            ast::Ast(ast::Expr::Let(ast::Let { decls, .. })) => {
                let decl = decls.get(1).unwrap();
                match decl {
                    ast::Decl::Var(v) => *v.escape.borrow(),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };
        assert!(escape);
    }

    #[test]
    fn test_escape_parameter() {
        let doc = "
        let
          type r1 = {f1: int, f2: string} var v1 := r1 {f1 = 1, f2 = \"hello\"}
          function f1(x: int) =
            let function f2(y: int) = y + x
            in
            end
        in
        end
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let mut ast = parser.parse();
        Escape::new().escape(&mut ast);
        let escape = match ast {
            ast::Ast(ast::Expr::Let(ast::Let { decls, .. })) => {
                let decl = decls.get(2).unwrap();
                match decl {
                    ast::Decl::Func(f) => *f.args[0].escape.borrow(),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };
        assert!(escape);
    }
}
