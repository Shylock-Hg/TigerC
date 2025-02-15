use std::cell::RefCell;
use std::rc::Rc;

use indexmap::IndexMap;

use crate::ast;
use crate::data_layout;
use crate::frame;
use crate::frame::Access;
use crate::frame::Frame;
use crate::ident_pool;
use crate::ident_pool::Symbol;
use crate::ir;
use crate::ir::Statement;
use crate::ir::Variable;
use crate::ir_gen;
use crate::stack::Stack;
use crate::symbol_table::SymbolTable;
use crate::temp::Label;
use crate::temp::Temp;
use crate::type_ast;

#[derive(Debug)]
pub struct Level<F> {
    pub current: Rc<RefCell<F>>,
    parent: Option<Box<Level<F>>>,
}

impl<F> Clone for Level<F> {
    fn clone(&self) -> Self {
        Self {
            current: self.current.clone(),
            parent: self.parent.clone(),
        }
    }
}

impl<F: PartialEq> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

impl<F: Frame> Level<F> {
    pub fn new(
        parent: &Level<F>,
        name: Label,
        parameters: IndexMap<ir::LowerIdent, ir::Variable>,
    ) -> Level<F> {
        Level {
            current: Rc::new(RefCell::new(F::new(name, parameters))),
            parent: Some(Box::new(parent.clone())),
        }
    }

    pub fn outermost() -> Level<F> {
        Level {
            current: Rc::new(RefCell::new(F::new(
                Label::new_named(ident_pool::symbol("__main")),
                Default::default(),
            ))),
            parent: None,
        }
    }
}

struct VarEntry<F> {
    level: Level<F>,
    var: frame::Variable,
}

enum Fragment<F: Frame> {
    Function {
        label: Label,
        frame: Rc<RefCell<F>>,
        body: Statement,
    },
    StringLiteral(Label, String),
}

macro_rules! translate_binary_op {
    ($self:ident, $level:ident, $l:ident, $op:path, $r:ident) => {{
        let l_exp = $self.translate_expr($level, $l);
        let r_exp = $self.translate_expr($level, $r);
        ir::Exp::BinOp {
            left: Box::new(l_exp),
            op: $op,
            right: Box::new(r_exp),
        }
    }};
}

macro_rules! translate_relation_op {
    ($self:ident, $level:ident, $l:ident, $op:path, $r:ident) => {{
        let l_exp = $self.translate_expr($level, $l);
        let r_exp = $self.translate_expr($level, $r);
        let result = ir::Exp::Temp(Temp::new());
        let t_label = Label::new();
        let f_label = Label::new();
        let c_jump = ir::Statement::CJump {
            op: $op,
            left: l_exp,
            right: r_exp,
            then: t_label,
            else_: f_label,
        };
        let stmt = ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(t_label)),
            s2: Box::new(ir::Statement::Move {
                dst: result.clone(),
                val: ir::Exp::Const(1),
            }),
        };
        let stmt = ir::Statement::Seq {
            s1: Box::new(stmt),
            s2: Box::new(ir::Statement::Label(f_label)),
        };
        let exp = ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(result.clone()),
        };
        let exp = ir::Exp::ExpSeq {
            stmt: Box::new(c_jump),
            exp: Box::new(exp),
        };
        ir::Exp::ExpSeq {
            stmt: Box::new(ir::Statement::Move {
                dst: result,
                val: ir::Exp::Const(0),
            }),
            exp: Box::new(exp),
        }
    }};
}

pub struct Translate<F: Frame> {
    // the level variable defined in
    // TODO use HashMap<Temp, VarEntry<F>>
    var_table: SymbolTable<VarEntry<F>>,
    fragments: Vec<Fragment<F>>,

    // latest done label of loop(for/while)
    done_label: Stack<Label>,
}

impl<F: Frame + PartialEq + Eq> Translate<F> {
    pub fn new() -> Self {
        Self {
            var_table: SymbolTable::new(),
            fragments: Default::default(),
            done_label: Stack::new(),
        }
    }

    pub fn translate(&mut self, ty_ast: &type_ast::TypeAst) -> ir::Exp {
        let level = Level::<F>::outermost();
        match ty_ast {
            type_ast::TypeAst::TypeDecl(decl) => unreachable!(),
            type_ast::TypeAst::TypeExpr(exp) => self.translate_expr(&level, exp),
        }
    }

    fn translate_decl(&mut self, level: &Level<F>, decl: &type_ast::TypeDecl) -> Option<Statement> {
        match decl {
            type_ast::TypeDecl::Func(f) => {
                self.translate_function_decl(level, f);
                None
            }
            type_ast::TypeDecl::Var(v) => self.translate_var_decl(level, v),
            // It's done in type inference
            type_ast::TypeDecl::Type(_) => None,
        }
    }

    fn translate_function_decl(&mut self, level: &Level<F>, f: &type_ast::FuncDecl) {
        self.var_table.begin_scope();
        let params = f
            .args
            .iter()
            .map(|(name, v)| (ir::LowerIdent::new(*name), Variable(v.escape)))
            .collect::<IndexMap<_, _>>();
        let new_level = Level::new(level, Label::new_named(f.name), params);
        for i in 0..f.args.len() {
            let v = new_level.current.borrow().parameters()[i].clone();
            self.var_table.insert_symbol(
                f.args.keys().nth(i).unwrap().clone(),
                VarEntry {
                    level: new_level.clone(),
                    var: v,
                },
            );
        }
        let body = self.translate_expr(&new_level, &f.body);
        self.var_table.end_scope();
        let body = if matches!(f.ret_ty, type_ast::Type::Nothing) {
            ir::Statement::Exp(body)
        } else {
            Self::translate_return_value(body)
        };
        let body = new_level.current.borrow_mut().proc_entry_exit1(body);
        self.fragments.push(Fragment::<F>::Function {
            label: Label::new_named(f.name),
            frame: new_level.current.clone(),
            body,
        });
    }

    fn translate_var_decl(&mut self, level: &Level<F>, v: &type_ast::VarDecl) -> Option<Statement> {
        let var = level
            .current
            .borrow_mut()
            .allocate_local(Variable(v.escape));
        let s = match v.ty {
            type_ast::Type::Nothing | type_ast::Type::Name(_) | type_ast::Type::Function(_) => {
                unreachable!()
            }
            _ => {
                // In fact, all variables are WORD in tiger (integer or pointer)
                let init_exp = self.translate_expr(&level, &v.init);
                let assign = ir::Statement::Move {
                    dst: Self::translate_var(&var),
                    val: init_exp,
                };
                Some(assign)
            }
        };
        self.var_table.insert_symbol(
            v.name,
            VarEntry {
                level: level.clone(),
                var,
            },
        );
        s
    }

    fn translate_expr(&mut self, level: &Level<F>, expr: &type_ast::TypeExpr) -> ir::Exp {
        match &expr.expr {
            type_ast::TypeExpr_::Literal(l) => self.translate_value(l),
            type_ast::TypeExpr_::LeftValue(l) => self.translate_left_value(level, &l),
            type_ast::TypeExpr_::Sequence(seq) => {
                // by syntax
                assert!(seq.len() >= 2);
                self.translate_seq(level, seq)
            }
            type_ast::TypeExpr_::Unary(unary) => self.translate_unary(level, unary),
            type_ast::TypeExpr_::Binary(binary) => self.translate_binary(level, binary),
            type_ast::TypeExpr_::FuncCall(f, args) => {
                let args = args.iter().map(|e| self.translate_expr(level, e)).collect();
                Self::translate_function_call(*f, args)
            }
            type_ast::TypeExpr_::RecordExpr(r) => self.translate_record_ctor(level, r),
            type_ast::TypeExpr_::ArrayExpr(a) => self.translate_array_ctor(level, a),
            type_ast::TypeExpr_::Assign(l, v) => self.translate_assign(level, l, v),
            type_ast::TypeExpr_::IfThenElse(f) => self.translate_if_then_else(level, f),
            type_ast::TypeExpr_::IfThen(cond, then) => self.translate_if_then(level, cond, then),
            type_ast::TypeExpr_::While(w) => self.translate_while(level, w),
            type_ast::TypeExpr_::For(f) => self.translate_for(level, f),
            type_ast::TypeExpr_::Break => self.translate_break(),
            type_ast::TypeExpr_::Let(let_) => self.translate_let(level, let_),
            type_ast::TypeExpr_::Parenthesis(e) => self.translate_expr(level, e),
        }
    }

    fn translate_let(&mut self, level: &Level<F>, let_: &type_ast::Let) -> ir::Exp {
        self.var_table.begin_scope();
        let decls = let_
            .decls
            .iter()
            .map(|d| self.translate_decl(level, d))
            .filter(|v| v.is_some())
            .map(|v| v.unwrap())
            .collect();
        let stmt = ir_gen::combine_statements(decls);
        let seq = self.translate_seq(level, &let_.sequence);
        self.var_table.end_scope();
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(seq),
        }
    }

    fn translate_break(&mut self) -> ir::Exp {
        let done_label = self.done_label.pop().expect("break outside of loop");
        ir::Exp::ExpSeq {
            stmt: Box::new(Self::goto(done_label)),
            exp: Box::new(Self::unit()),
        }
    }

    fn goto(label: Label) -> ir::Statement {
        ir::Statement::Jump {
            exp: ir::Exp::Name(label),
            labels: vec![label],
        }
    }

    fn translate_for(&mut self, level: &Level<F>, f: &type_ast::For) -> ir::Exp {
        let done_label = Label::new();
        self.done_label.push(done_label);
        let lower = self.translate_expr(level, &f.lower);
        let upper = self.translate_expr(level, &f.upper);
        let var = level
            .current
            .borrow_mut()
            .allocate_local(ir::Variable(false));
        self.var_table.begin_scope();
        self.var_table.insert_symbol(
            f.local,
            VarEntry {
                level: level.clone(),
                var: var.clone(),
            },
        );
        let body = self.translate_expr(level, &f.body);
        self.var_table.end_scope();
        let body = ir::Statement::Exp(body);
        let for_ = Self::for_loop(Self::temp_of(&var), lower, upper, body, done_label);
        ir::Exp::ExpSeq {
            stmt: Box::new(for_),
            exp: Box::new(Self::unit()),
        }
    }

    fn temp_of(var: &frame::Variable) -> Temp {
        match var.access {
            Access::Frame(_) => panic!(),
            Access::Register(t) => t,
        }
    }

    fn translate_while(&mut self, level: &Level<F>, w: &type_ast::While) -> ir::Exp {
        let test_label = Label::new();
        let done_label = Label::new();
        let body_label = Label::new();
        self.done_label.push(done_label);
        let cond = self.translate_expr(level, &w.condition);
        let body = self.translate_expr(level, &w.body);
        let test = ir::Statement::CJump {
            op: ir::CompareOp::Ne,
            left: cond,
            right: ir::Exp::Const(0),
            then: body_label,
            else_: done_label,
        };
        let body = ir::Statement::Exp(body);
        let stmt = Self::while_loop(test, body, test_label, done_label, body_label);
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(Self::unit()),
        }
    }

    fn translate_if_then(
        &mut self,
        level: &Level<F>,
        cond: &type_ast::TypeExpr,
        then: &type_ast::TypeExpr,
    ) -> ir::Exp {
        let cond = self.translate_expr(level, cond);
        let then = self.translate_expr(level, then);
        let then_label = Label::new();
        let end_label = Label::new();
        let then = ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(then_label)),
            s2: Box::new(ir::Statement::Exp(then)),
        };
        let then = ir::Statement::Seq {
            s1: Box::new(then),
            s2: Box::new(ir::Statement::Label(end_label)),
        };
        let stmt = ir::Statement::CJump {
            op: ir::CompareOp::Ne,
            left: cond,
            right: ir::Exp::Const(0),
            then: then_label,
            else_: end_label,
        };
        let stmt = ir::Statement::Seq {
            s1: Box::new(stmt),
            s2: Box::new(then),
        };
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(Self::unit()),
        }
    }

    fn translate_if_then_else(
        &mut self,
        level: &Level<F>,
        f: &type_ast::IfThenElseExpr,
    ) -> ir::Exp {
        let cond = self.translate_expr(level, &f.condition);
        let then = self.translate_expr(level, &f.then);
        let else_ = self.translate_expr(level, &f.el);
        let then_label = Label::new();
        let else_label = Label::new();
        let end_label = Label::new();
        let result = ir::Exp::Temp(Temp::new());
        let then = ir::Statement::Move {
            dst: result.clone(),
            val: then,
        };
        let else_ = ir::Statement::Move {
            dst: result.clone(),
            val: else_,
        };
        let then = ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(then_label)),
            s2: Box::new(then),
        };
        let then = ir::Statement::Seq {
            s1: Box::new(then),
            s2: Box::new(Self::goto(end_label)),
        };
        let else_ = ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(else_label)),
            s2: Box::new(else_),
        };
        let stmt = ir::Statement::Seq {
            s1: Box::new(then),
            s2: Box::new(else_),
        };
        let stmt = ir::Statement::Seq {
            s1: Box::new(stmt),
            s2: Box::new(ir::Statement::Label(end_label)),
        };

        let c_jump = ir::Statement::CJump {
            op: ir::CompareOp::Ne,
            left: cond,
            right: ir::Exp::Const(0),
            then: then_label,
            else_: else_label,
        };

        let stmt = ir::Statement::Seq {
            s1: Box::new(c_jump),
            s2: Box::new(stmt),
        };
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(result),
        }
    }

    fn translate_assign(
        &mut self,
        level: &Level<F>,
        l: &type_ast::LeftValue,
        v: &type_ast::TypeExpr,
    ) -> ir::Exp {
        let l_exp = self.translate_left_value(level, l);
        let v_exp = self.translate_expr(level, v);
        ir::Exp::ExpSeq {
            stmt: Box::new(ir::Statement::Move {
                dst: l_exp,
                val: v_exp,
            }),
            exp: Box::new(Self::unit()),
        }
    }

    fn unit() -> ir::Exp {
        ir::Exp::Const(0)
    }

    fn translate_array_ctor(&mut self, level: &Level<F>, a: &type_ast::ArrayExpr) -> ir::Exp {
        let len = self.translate_expr(level, &a.len);
        let len_temp = ir::Exp::Temp(Temp::new());
        let len_stmt = ir::Statement::Move {
            dst: len_temp.clone(),
            val: len,
        };
        let size = ir::Exp::BinOp {
            op: ir::BinOp::Multiply,
            left: Box::new(len_temp.clone()),
            right: Box::new(ir::Exp::Const(F::word_size())),
        };
        let pt = Self::malloc(size);
        let p = ir::Exp::Temp(Temp::new());
        let len_stmt = ir::Statement::Seq {
            s1: Box::new(len_stmt),
            s2: Box::new(ir::Statement::Move {
                dst: p.clone(),
                val: pt,
            }),
        };

        let init = self.translate_expr(level, &a.init);
        let init_temp = ir::Exp::Temp(Temp::new());
        let init_stmt = ir::Statement::Move {
            dst: init_temp.clone(),
            val: init,
        };

        let iter = Temp::new();
        let fill = ir::Statement::Move {
            dst: ir::Exp::Mem(Box::new(ir::Exp::BinOp {
                op: ir::BinOp::Plus,
                left: Box::new(p.clone()),
                right: Box::new(ir::Exp::Temp(iter)),
            })),
            val: init_temp,
        };

        let stmt = ir::Statement::Seq {
            s1: Box::new(init_stmt),
            s2: Box::new(len_stmt),
        };

        let loop_ = Self::for_loop(Temp::new(), ir::Exp::Const(0), len_temp, fill, Label::new());

        let stmt = ir::Statement::Seq {
            s1: Box::new(stmt),
            s2: Box::new(loop_),
        };
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(p),
        }
    }

    fn for_loop(
        var: Temp,
        start: ir::Exp,
        end: ir::Exp,
        body: ir::Statement,
        done_label: Label,
    ) -> ir::Statement {
        let var = ir::Exp::Temp(var);
        let init = ir::Statement::Move {
            dst: var.clone(),
            val: start,
        };

        let test_label = Label::new();
        let body_label = Label::new();

        let test = ir::Statement::CJump {
            op: ir::CompareOp::SignedLt,
            left: var.clone(),
            right: end,
            then: body_label,
            else_: done_label,
        };
        let inc_var = ir::Statement::Move {
            dst: var.clone(),
            val: ir::Exp::BinOp {
                op: ir::BinOp::Plus,
                left: Box::new(var.clone()),
                right: Box::new(ir::Exp::Const(1)),
            },
        };
        let body = ir::Statement::Seq {
            s1: Box::new(inc_var),
            s2: Box::new(body),
        };
        let loop_ = Self::while_loop(test, body, test_label, done_label, body_label);
        ir::Statement::Seq {
            s1: Box::new(init),
            s2: Box::new(loop_),
        }
    }

    fn while_loop(
        test: ir::Statement,
        body: ir::Statement,
        test_label: Label,
        done_label: Label,
        body_label: Label,
    ) -> ir::Statement {
        assert!(matches!(test, ir::Statement::CJump { .. }));
        let body = ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(body_label)),
            s2: Box::new(body),
        };
        let body = ir::Statement::Seq {
            s1: Box::new(body),
            s2: Box::new(ir::Statement::Jump {
                exp: ir::Exp::Name(test_label),
                labels: vec![test_label],
            }),
        };
        ir::Statement::Seq {
            s1: Box::new(ir::Statement::Label(test_label)),
            s2: Box::new(ir::Statement::Seq {
                s1: Box::new(test),
                s2: Box::new(ir::Statement::Seq {
                    s1: Box::new(body),
                    s2: Box::new(ir::Statement::Label(done_label)),
                }),
            }),
        }
    }

    fn size_of_record(r: &type_ast::RecordExpr) -> i64 {
        r.init.len() as i64 * F::word_size()
    }

    fn malloc(size: ir::Exp) -> ir::Exp {
        let args = vec![size];
        Self::translate_function_call(ident_pool::symbol("malloc"), args)
    }

    fn translate_record_ctor(&mut self, level: &Level<F>, r: &type_ast::RecordExpr) -> ir::Exp {
        let size = Self::size_of_record(r);
        let p = Self::malloc(ir::Exp::Const(size));
        let init_exp = r.init.iter().enumerate().map(|(i, (f, v))| {
            let v = self.translate_expr(level, v);
            let offset = F::word_size() * i as i64;
            (offset, v)
        });
        let result = Temp::new();
        let mut stmt = ir::Statement::Move {
            dst: ir::Exp::Temp(result),
            val: p.clone(),
        };
        for i_exp in init_exp {
            let (offset, v) = i_exp;
            let mv = ir::Statement::Move {
                dst: ir::Exp::Mem(Box::new(ir::Exp::BinOp {
                    op: ir::BinOp::Plus,
                    left: Box::new(p.clone()),
                    right: Box::new(ir::Exp::Const(offset)),
                })),
                val: v,
            };
            stmt = ir::Statement::Seq {
                s1: Box::new(stmt),
                s2: Box::new(mv),
            };
        }
        ir::Exp::ExpSeq {
            stmt: Box::new(stmt),
            exp: Box::new(ir::Exp::Temp(result)),
        }
    }

    fn translate_binary(&mut self, level: &Level<F>, binary: &type_ast::Binary) -> ir::Exp {
        match binary {
            type_ast::Binary::Add(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::Plus, r)
            }
            type_ast::Binary::Minus(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::Minus, r)
            }
            type_ast::Binary::Multiply(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::Multiply, r)
            }
            type_ast::Binary::Divide(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::Divide, r)
            }
            type_ast::Binary::And(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::And, r)
            }
            type_ast::Binary::Or(l, r) => {
                translate_binary_op!(self, level, l, ir::BinOp::Or, r)
            }
            type_ast::Binary::Eq(l, r) => {
                if let type_ast::Type::Str = l.ty {
                    let args = vec![self.translate_expr(level, l), self.translate_expr(level, r)];
                    Self::translate_function_call(ident_pool::symbol("stringEqual"), args)
                } else {
                    translate_relation_op!(self, level, l, ir::CompareOp::Eq, r)
                }
            }
            type_ast::Binary::Ne(l, r) => {
                if let type_ast::Type::Str = l.ty {
                    let args = vec![self.translate_expr(level, l), self.translate_expr(level, r)];
                    let eq = Self::translate_function_call(ident_pool::symbol("stringEqual"), args);
                    ir::Exp::BinOp {
                        op: ir::BinOp::Minus,
                        left: Box::new(ir::Exp::Const(1)),
                        right: Box::new(eq),
                    }
                } else {
                    translate_relation_op!(self, level, l, ir::CompareOp::Ne, r)
                }
            }
            type_ast::Binary::Gt(l, r) => {
                translate_relation_op!(self, level, l, ir::CompareOp::SignedGt, r)
            }
            type_ast::Binary::Ge(l, r) => {
                translate_relation_op!(self, level, l, ir::CompareOp::SignedGe, r)
            }
            type_ast::Binary::Lt(l, r) => {
                translate_relation_op!(self, level, l, ir::CompareOp::SignedLt, r)
            }
            type_ast::Binary::Le(l, r) => {
                translate_relation_op!(self, level, l, ir::CompareOp::SignedLe, r)
            }
        }
    }

    fn translate_unary(&mut self, level: &Level<F>, unary: &type_ast::Unary) -> ir::Exp {
        match unary {
            type_ast::Unary::Negative(e) => {
                let e = self.translate_expr(level, e);
                ir::Exp::BinOp {
                    left: Box::new(ir::Exp::Const(0)),
                    op: ir::BinOp::Minus,
                    right: Box::new(e),
                }
            }
        }
    }

    fn translate_seq(&mut self, level: &Level<F>, seq: &Vec<type_ast::TypeExpr>) -> ir::Exp {
        let mut s = ir_gen::no_op();
        for e in &seq[0..seq.len() - 1] {
            s = ir::Statement::Seq {
                s1: Box::new(s),
                s2: Box::new(ir::Statement::Exp(self.translate_expr(level, e))),
            };
        }
        ir::Exp::ExpSeq {
            stmt: Box::new(s),
            exp: Box::new(self.translate_expr(level, seq.last().unwrap())),
        }
    }

    fn translate_value(&mut self, l: &ast::Value) -> ir::Exp {
        match l {
            ast::Value::Int(i) => ir::Exp::Const(*i),
            ast::Value::Str(s) => {
                let label = Label::new();
                self.fragments
                    .push(Fragment::StringLiteral(label, s.clone()));
                ir::Exp::Name(label)
            }
            ast::Value::Nil => ir::Exp::Const(0),
            ast::Value::Nothing => {
                // TODO Maybe?
                unreachable!()
            }
        }
    }

    fn translate_left_value(&mut self, level: &Level<F>, lv: &type_ast::LeftValue) -> ir::Exp {
        match &lv.left {
            type_ast::LeftValue_::Variable(v) => self.translate_simple_var(*v, level),
            type_ast::LeftValue_::Field(l, f) => {
                // lv is pointer to record
                let lv = self.translate_left_value(level, &l);
                let offset = if let type_ast::Type::Record(r) = &l.ty {
                    let offset = r.fields.get_index_of(f).unwrap() as i64 * F::word_size();
                    ir::Exp::Const(offset)
                } else {
                    unreachable!()
                };
                ir::Exp::Mem(Box::new(ir::Exp::BinOp {
                    left: Box::new(lv),
                    op: ir::BinOp::Plus,
                    right: Box::new(offset),
                }))
            }
            type_ast::LeftValue_::Subscript(l, index) => {
                // lv is a pointer to array
                let lv = self.translate_left_value(level, l);
                let offset_exp = self.translate_expr(level, index);
                ir::Exp::Mem(Box::new(ir::Exp::BinOp {
                    left: Box::new(ir::Exp::BinOp {
                        op: ir::BinOp::Minus,
                        left: Box::new(lv),
                        right: Box::new(ir::Exp::Const(data_layout::ARRAY_HEADER_SIZE)),
                    }),
                    op: ir::BinOp::Plus,
                    right: Box::new(ir::Exp::BinOp {
                        left: Box::new(offset_exp),
                        op: ir::BinOp::Multiply,
                        right: Box::new(ir::Exp::Const(F::word_size())),
                    }),
                }))
            }
        }
    }

    fn translate_simple_var(&self, v: Symbol, level: &Level<F>) -> ir::Exp {
        let mut level = level;
        let var = self.var_table.get_symbol(&v).unwrap();
        let mut fp = ir::Exp::Temp(F::fp());
        while level.current != var.level.current {
            fp =
                Self::translate_access_var(level.current.borrow().parameters().last().unwrap(), fp);
            level = level.parent.as_ref().unwrap();
        }
        let exp = Self::translate_access_var(&var.var, fp);
        exp
    }

    fn translate_var(var: &frame::Variable) -> ir::Exp {
        Self::translate_access_var(var, ir::Exp::Temp(F::fp()))
    }

    fn translate_access_var(var: &frame::Variable, fp: ir::Exp) -> ir::Exp {
        F::access_var(var, fp)
    }

    fn translate_return_value(exp: ir::Exp) -> Statement {
        ir::Statement::Move {
            dst: ir::Exp::Temp(F::return_value()),
            val: exp,
        }
    }

    fn translate_function_call(name: Symbol, args: Vec<ir::Exp>) -> ir::Exp {
        ir::Exp::Call {
            func: Box::new(ir::Exp::Name(Label::new_named(name))),
            args,
        }
    }
}
