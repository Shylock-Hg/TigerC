use std::result;

use crate::ast;
use crate::ident_pool::{kw, Symbol};
use crate::tokenizer::{Posed, Token};

#[derive(PartialEq, PartialOrd, Clone, Copy, Debug)]
enum Precedence {
    Lowest = 0,
    Assign = 1,
    Or = 2,             // |
    And = 3,            // &
    Compare = 4,        // ==, !=, <=, >=, <, >
    PlusMinus = 5,      // +, -
    MultiplyDivide = 6, // *, /
    Prefix = 7,         // -
}

impl Precedence {
    fn from_token(tok: &Token) -> Precedence {
        match tok {
            Token::Or => Precedence::Or,
            Token::And => Precedence::And,
            Token::Eq | Token::Ne | Token::Le | Token::Ge | Token::Lt | Token::Gt => {
                Precedence::Compare
            }
            Token::Plus | Token::Minus => Precedence::PlusMinus,
            Token::Star | Token::Slash => Precedence::MultiplyDivide,
            Token::Assign => Precedence::Assign,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
}

impl ParserError {
    pub fn unexpected_token(tok: &Posed<Token>) -> Self {
        Self {
            message: format!("unexpected token: {:?}", tok),
        }
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub struct Parser<'a> {
    cursor: std::iter::Peekable<Box<dyn Iterator<Item = Posed<Token>> + 'a>>,
}

type Result<T> = result::Result<T, ParserError>;

impl<'a> Parser<'a> {
    pub fn new(cursor: Box<dyn Iterator<Item = Posed<Token>> + 'a>) -> Self {
        Parser {
            cursor: cursor.peekable(),
        }
    }

    pub fn parse(&mut self) -> ast::Ast {
        ast::Ast(self.parse_expr())
    }

    fn bump(&mut self) -> Option<Posed<Token>> {
        self.cursor.next()
    }

    fn look(&mut self) -> Option<&Posed<Token>> {
        self.cursor.peek()
    }

    // return true we look expected token
    fn look_expect(&mut self, expect: &Token) -> bool {
        self.look().map(|tok| tok.node() == expect).unwrap_or(false)
    }

    // return true we look expected keyword
    fn look_expect_keyword(&mut self, expect: &Symbol) -> bool {
        self.look()
            .map(|tok| matches!(tok.node(), Token::Ident(s) if s == expect))
            .unwrap_or(false)
    }

    fn eat_keyword(&mut self, keyword: Symbol) {
        self.eat_expect(Token::Ident(keyword));
    }

    fn eat_expect(&mut self, expected: Token) {
        let next = self.bump().unwrap();
        if next.node() != &expected {
            Self::unexpected_token(&next)
        }
    }

    fn eat_ident(&mut self) -> Symbol {
        let next = self.bump().unwrap();
        if let Token::Ident(s) = next.node() {
            *s
        } else {
            Self::unexpected_token(&next)
        }
    }

    fn eat_field(&mut self) -> Option<ast::Field> {
        let current = self.look().cloned().unwrap();
        if let Token::Ident(name) = current.node() {
            self.bump();
            self.eat_expect(Token::Colon);
            let ty = self.eat_ident();
            Some(ast::Field { name: *name, ty })
        } else {
            None
        }
    }

    // [<ident> ":" <ident> "," <ident> ":" <ident> ...]
    fn eat_field_list(&mut self) -> Vec<ast::Field> {
        let mut fields = vec![];
        while let Some(field) = self.eat_field() {
            fields.push(field);
            if self.look().unwrap().node() == &Token::Comma {
                self.bump();
            } else {
                break;
            }
        }
        fields
    }

    fn eat_field_init(&mut self) -> (Symbol, ast::Expr) {
        let field_name = self.eat_ident();
        self.eat_expect(Token::Eq);
        let init = self.parse_expr();
        (field_name, init)
    }

    // [<ident> "=" <ident> "," <ident> "=" <ident>...] "}"
    fn eat_field_init_list_suffix(&mut self) -> Vec<(Symbol, ast::Expr)> {
        self.eat_token_separated_list_suffix(&Token::Comma, &Token::CloseBrace, |parser| {
            parser.eat_field_init()
        })
    }

    // expr[; expr...] "end"
    fn eat_expr_list_end_suffix(&mut self) -> Vec<ast::Expr> {
        self.eat_token_separated_list_suffix(
            &Token::SemiColon,
            &Token::Ident(kw::TOK_END),
            |parser| parser.parse_expr(),
        )
    }

    // [ <T> "sep" <T> ...  ] "end"
    fn eat_token_separated_list_suffix<T: std::fmt::Debug, F>(
        &mut self,
        sep: &Token,
        end: &Token,
        mut f: F,
    ) -> Vec<T>
    where
        F: FnMut(&mut Parser) -> T,
    {
        let mut v = vec![];
        loop {
            let current = self.look().unwrap();
            match current.node() {
                tok if tok == end => {
                    self.bump();
                    break;
                }
                _ => {
                    v.push(f(self));
                    let look = self.look().unwrap();
                    if look.node() == sep {
                        self.bump();
                    } else if look.node() == end {
                        self.bump();
                        return v;
                    } else {
                        Self::unexpected_token(look);
                    }
                }
            }
        }
        v
    }

    // "." <ident>
    // "[" <expr> "]"
    fn parse_left_value_suffix(&mut self, left: ast::LeftValue) -> ast::LeftValue {
        match self.look().unwrap().node() {
            Token::Dot => {
                self.bump();
                let ident = self.eat_ident();
                self.parse_left_value_suffix(ast::LeftValue::Field(Box::new(left), ident))
            }
            Token::OpenBracket => {
                self.bump();
                let expr = self.parse_expr();
                self.eat_expect(Token::CloseBracket);
                self.parse_left_value_suffix(ast::LeftValue::Subscript(
                    Box::new(left),
                    Box::new(expr),
                ))
            }
            _ => left,
        }
    }

    // [";" <expr>; ... <expr>] ")"
    fn parse_sequence_suffix(&mut self, v: &mut Vec<ast::Expr>) {
        loop {
            match self.look().unwrap().node() {
                Token::SemiColon => {
                    self.bump();
                    let next = self.parse_expr();
                    v.push(next);
                }
                Token::CloseParen => {
                    self.bump();
                    return;
                }
                _ => Self::unexpected_token(self.look().unwrap()),
            }
        }
    }

    // [<expr>, <expr>, ...] ")"
    fn parse_function_arguments_suffix(&mut self) -> Vec<ast::Expr> {
        let mut args = vec![];
        loop {
            let next = self.look().unwrap();
            match next.node() {
                Token::CloseParen => {
                    self.bump();
                    break;
                }
                Token::Comma => {
                    self.bump();
                }
                _ => args.push(self.parse_expr()),
            }
        }
        args
    }

    fn parse_expr(&mut self) -> ast::Expr {
        self.parse_sub_expr(Precedence::Lowest)
    }

    // parse expression until lower precedence
    fn parse_sub_expr(&mut self, precedence: Precedence) -> ast::Expr {
        let mut prefix = self.parse_expr_prefix();
        loop {
            let next_precedence = self.next_precedence();
            if precedence >= next_precedence {
                break;
            }
            prefix = self.parse_expr_suffix(prefix, next_precedence);
        }
        prefix
    }

    fn next_precedence(&mut self) -> Precedence {
        self.look()
            .map(|n| Precedence::from_token(n.node()))
            .unwrap_or(Precedence::Lowest)
    }

    fn parse_expr_prefix(&mut self) -> ast::Expr {
        let current = self.bump().unwrap();
        match current.node() {
            Token::Number(n) => ast::Expr::Literal(ast::Value::Int(*n)),
            Token::Str(s) => ast::Expr::Literal(ast::Value::Str(s.clone())),
            Token::Ident(s) => {
                match s {
                    v if v == &kw::TOK_NIL => {
                        return ast::Expr::Literal(ast::Value::Nil);
                    }
                    v if v == &kw::TOK_IF => {
                        let condition = self.parse_expr();
                        self.eat_keyword(kw::TOK_THEN);
                        let then = self.parse_expr();
                        return if self.look_expect_keyword(&kw::TOK_ELSE) {
                            self.bump();
                            let else_ = self.parse_expr();
                            ast::Expr::IfThenElse(ast::IfThenElseExpr {
                                condition: Box::new(condition),
                                then: Box::new(then),
                                el: Box::new(else_),
                            })
                        } else {
                            ast::Expr::IfThen(Box::new(condition), Box::new(then))
                        };
                    }
                    v if v == &kw::TOK_WHILE => {
                        let condition = self.parse_expr();
                        self.eat_keyword(kw::TOK_DO);
                        let body = self.parse_expr();
                        return ast::Expr::While(ast::While {
                            condition: Box::new(condition),
                            body: Box::new(body),
                        });
                    }
                    v if v == &kw::TOK_FOR => {
                        let local = self.eat_ident();
                        self.eat_expect(Token::Assign);
                        let lower = self.parse_expr();
                        self.eat_keyword(kw::TOK_TO);
                        let upper = self.parse_expr();
                        self.eat_keyword(kw::TOK_DO);
                        let body = self.parse_expr();
                        return ast::Expr::For(ast::For {
                            local,
                            lower: Box::new(lower),
                            upper: Box::new(upper),
                            body: Box::new(body),
                        });
                    }
                    v if v == &kw::TOK_BREAK => {
                        return ast::Expr::Break;
                    }
                    v if v == &kw::TOK_LET => {
                        let decls = self.parse_decl_list();
                        self.eat_keyword(kw::TOK_IN);
                        let sequence = self.eat_expr_list_end_suffix();
                        return ast::Expr::Let(ast::Let { decls, sequence });
                    }
                    _ => (),
                };
                let next = self.look().unwrap();
                match next.node() {
                    Token::OpenParen => {
                        self.bump();
                        let args = self.parse_function_arguments_suffix();
                        ast::Expr::FuncCall(*s, args)
                    }
                    Token::OpenBrace => {
                        self.bump();
                        let field_inits = self.eat_field_init_list_suffix();
                        ast::Expr::RecordExpr(ast::RecordExpr {
                            ty: *s,
                            init: field_inits,
                        })
                    }
                    Token::OpenBracket => {
                        self.bump();
                        let len = self.parse_expr();
                        self.eat_expect(Token::CloseBracket);
                        let n = self.look().to_owned().unwrap();
                        match n.node() {
                            Token::Ident(kw::TOK_OF) => {
                                self.bump();
                                let init = self.parse_expr();
                                ast::Expr::ArrayExpr(ast::ArrayExpr {
                                    ty: *s,
                                    len: Box::new(len),
                                    init: Box::new(init),
                                })
                            }
                            _ => {
                                let lv = ast::LeftValue::Subscript(
                                    Box::new(ast::LeftValue::Variable(*s)),
                                    Box::new(len),
                                );
                                ast::Expr::LeftValue(self.parse_left_value_suffix(lv))
                            }
                        }
                    }
                    _ => {
                        let prev = ast::LeftValue::Variable(*s);
                        let lv = self.parse_left_value_suffix(prev);
                        if self.look().unwrap().node() == &Token::Assign {
                            self.bump();
                            let rhs = self.parse_expr();
                            ast::Expr::Assign(lv, Box::new(rhs))
                        } else {
                            ast::Expr::LeftValue(lv)
                        }
                    }
                }
            }
            Token::OpenParen => {
                let look = self.look().unwrap();
                if look.node() == &Token::CloseParen {
                    self.bump();
                    ast::Expr::Literal(ast::Value::Nothing)
                } else {
                    let expr = self.parse_expr();
                    let next = self.bump().unwrap();
                    if next.node() == &Token::CloseParen {
                        ast::Expr::Parenthesis(Box::new(expr))
                    } else if next.node() == &Token::SemiColon {
                        let expr2 = self.parse_expr();
                        let mut v = vec![expr, expr2];
                        self.parse_sequence_suffix(&mut v);
                        ast::Expr::Sequence(v)
                    } else {
                        Self::unexpected_token(&next);
                    }
                }
            }
            Token::Minus => {
                let expr = self.parse_expr_prefix();
                ast::Expr::Unary(ast::Unary::Negative(Box::new(expr)))
            }
            _ => unimplemented!("{:?}", current),
        }
    }

    fn parse_expr_suffix(&mut self, left: ast::Expr, precedence: Precedence) -> ast::Expr {
        let look = self.bump().unwrap();
        match look.node() {
            Token::Plus => ast::Expr::Binary(ast::Binary::Add(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Minus => ast::Expr::Binary(ast::Binary::Minus(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Star => ast::Expr::Binary(ast::Binary::Multiply(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Slash => ast::Expr::Binary(ast::Binary::Divide(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Eq => ast::Expr::Binary(ast::Binary::Eq(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Ne => ast::Expr::Binary(ast::Binary::Ne(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Le => ast::Expr::Binary(ast::Binary::Le(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Ge => ast::Expr::Binary(ast::Binary::Ge(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Lt => ast::Expr::Binary(ast::Binary::Lt(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Gt => ast::Expr::Binary(ast::Binary::Gt(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::And => ast::Expr::Binary(ast::Binary::And(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Or => ast::Expr::Binary(ast::Binary::And(
                Box::new(left),
                Box::new(self.parse_sub_expr(precedence)),
            )),
            Token::Assign => {
                if let ast::Expr::LeftValue(lv) = left {
                    ast::Expr::Assign(lv, Box::new(self.parse_sub_expr(precedence)))
                } else {
                    panic!("Only left value is allowed in left of assignment.")
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_decl_list(&mut self) -> Vec<ast::Decl> {
        let mut decls = vec![];
        while let Ok(d) = self.parse_decl() {
            decls.push(d);
        }
        decls
    }

    fn parse_decl(&mut self) -> Result<ast::Decl> {
        let current = self.look().unwrap();
        match current.node() {
            Token::Ident(s) => match *s {
                kw::TOK_TYPE => Ok(ast::Decl::Type(self.parse_type_decl())),
                kw::TOK_VAR => Ok(ast::Decl::Var(self.parse_var_decl())),
                kw::TOK_FUNCTION => Ok(ast::Decl::Func(self.parse_function_decl())),
                _ => Err(ParserError::unexpected_token(current)),
            },
            _ => Err(ParserError::unexpected_token(current)),
        }
    }

    fn parse_function_decl(&mut self) -> ast::FuncDecl {
        self.eat_keyword(kw::TOK_FUNCTION);
        let name = self.eat_ident();
        self.eat_expect(Token::OpenParen);
        let fields = self.eat_field_list();
        self.eat_expect(Token::CloseParen);
        let ret_ty = match self.look().unwrap().node() {
            Token::Colon => {
                self.bump();
                Some(self.eat_ident())
            }
            _ => None,
        };
        self.eat_expect(Token::Eq);
        let body = self.parse_expr();

        ast::FuncDecl {
            name,
            args: fields.into_iter().map(|f| f.into()).collect::<Vec<_>>(),
            ret_ty,
            body,
        }
    }

    fn parse_var_decl(&mut self) -> ast::VarDecl {
        self.eat_keyword(kw::TOK_VAR);
        let name = self.eat_ident();
        let ty: Option<Symbol> = match self.look().unwrap().node() {
            Token::Colon => {
                self.bump();
                Some(self.eat_ident())
            }
            Token::Assign => None,
            _ => Self::unexpected_token(self.look().unwrap()),
        };
        self.eat_expect(Token::Assign);
        let init = self.parse_expr();
        ast::VarDecl::new(name, ty, init)
    }

    fn parse_type_decl(&mut self) -> ast::TypeDecl {
        self.eat_keyword(kw::TOK_TYPE);
        let type_name = self.eat_ident();
        self.eat_expect(Token::Eq);
        let ty = self.parse_ty();
        ast::TypeDecl { type_name, ty }
    }

    fn parse_ty(&mut self) -> ast::Ty {
        let next = self.bump().unwrap();
        match next.node() {
            Token::Ident(s) => {
                if *s == kw::TOK_ARRAY {
                    // arrary type
                    self.eat_keyword(kw::TOK_OF);
                    ast::Ty::Array(self.eat_ident())
                } else {
                    // type name
                    ast::Ty::Name(*s)
                }
            }
            Token::OpenBrace => {
                // struct fields
                let look1 = self.look().unwrap();
                match look1.node() {
                    Token::CloseBrace => {
                        self.bump();
                        // empty fields
                        ast::Ty::Record(ast::TyRecord(vec![]))
                    }
                    Token::Ident(..) => {
                        let field_list = self.parse_field_list();
                        ast::Ty::Record(ast::TyRecord(field_list))
                    }
                    _ => Self::unexpected_token(look1),
                }
            }
            _ => Self::unexpected_token(&next),
        }
    }

    fn parse_field_list(&mut self) -> Vec<ast::Field> {
        let mut field_list = vec![];
        loop {
            let field = self.parse_field();
            field_list.push(field);
            let look1 = self.look().unwrap();
            match look1.node() {
                Token::CloseBrace => {
                    self.bump();
                    return field_list;
                }
                Token::Comma => {
                    self.bump();
                }
                _ => Self::unexpected_token(look1),
            }
        }
    }

    fn parse_field(&mut self) -> ast::Field {
        let name = self.eat_ident();
        self.eat_expect(Token::Colon);
        let ty = self.eat_ident();
        ast::Field { name, ty }
    }

    fn unexpected_token(tok: &Posed<Token>) -> ! {
        panic!("Unexpected token: {:?}", tok)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast;
    use crate::ident_pool;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_parser_new() {
        let it = tokenize("");
        let _parser = Parser::new(Box::new(it));
    }

    #[test]
    fn test_parse_literal_nil() {
        let doc = "
      nil
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let ast = parser.parse();
        assert_eq!(ast, ast::Ast(ast::Expr::Literal(ast::Value::Nil)));
    }

    #[test]
    fn test_parse_literal_nothing() {
        let doc = "
      (  )
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let ast = parser.parse();
        assert_eq!(ast, ast::Ast(ast::Expr::Literal(ast::Value::Nothing)));
    }

    #[test]
    fn test_parse_ne_expr() {
        let doc = "
       1 <> 2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let ast = parser.parse();
        assert_eq!(
            ast,
            ast::Ast(ast::Expr::Binary(ast::Binary::Ne(
                Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                Box::new(ast::Expr::Literal(ast::Value::Int(2))),
            )))
        );
    }

    #[test]
    fn test_parse_type_decl() {
        let doc = "
        type TypeName = Type2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_type_decl();
    }

    #[test]
    fn test_parse_type_decl_struct() {
        let doc = "
        type TypeName = { f1: t1, f2: t2 }
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_type_decl();
    }

    #[test]
    fn test_parse_type_decl_empty_struct() {
        let doc = "
        type TypeName = {}
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_type_decl();
    }

    #[test]
    fn test_parse_type_decl_array() {
        let doc = "
        type TypeName = array of t1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_type_decl();
    }

    #[test]
    fn test_parse_func_decl() {
        let doc = "
        function funcName (arg1: int, arg2: string) = 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_function_decl();
    }

    #[test]
    fn test_parse_func_decl_with_return_type() {
        let doc = "
        function funcName (arg1: int, arg2: string) : int = 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_function_decl();
    }

    #[test]
    fn test_parse_var_decl() {
        let doc = "
        var varName := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_var_decl();
    }

    #[test]
    fn test_parse_var_decl_with_type() {
        let doc = "
        var varName : int := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_var_decl();
    }

    #[test]
    fn test_parse_decl() {
        let doc = "
        var varName : int := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _decl = parser.parse_decl().unwrap();
    }

    #[test]
    fn test_parse_left_value() {
        // this will panic without tailing ";"
        // but in a real program, expression won't end with EOF, so it won't happen
        let doc = "
        varName.fieldName[1].field2;
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _ = parser.parse_expr();
    }

    #[test]
    fn test_parse_sequence() {
        // this will panic without tailing ";"
        // but in a real program, expression won't end with EOF, so it won't happen
        let doc = "
        (1; 2; 3)
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let seq = parser.parse_expr();
        assert_eq!(format!("{}", seq), "(1; 2; 3)");
    }

    #[test]
    fn test_parse_unary_negative() {
        // this will panic without tailing ";"
        // but in a real program, expression won't end with EOF, so it won't happen
        let doc = "
        - 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let negative = parser.parse_expr();
        assert_eq!(format!("{}", negative), "-1");
    }

    #[test]
    fn test_parse_binary_expr() {
        let doc = "
        1 + 2 * 4 - 5 / 3 > 6 | 1 & 0
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let negative = parser.parse_expr();
        assert_eq!(
            format!("{}", negative),
            "((((1 + (2 * 4)) - (5 / 3)) > 6) & (1 & 0))"
        );
    }

    #[test]
    fn test_parse_binary_unary_expr() {
        let doc = "
        -1 + 2 * 4 - 5 / 3 > 6 | 1 & 0
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let negative = parser.parse_expr();
        assert_eq!(
            format!("{}", negative),
            "((((-1 + (2 * 4)) - (5 / 3)) > 6) & (1 & 0))"
        );
    }

    #[test]
    fn test_parse_binary_unary_mixed_expr() {
        let doc = "
        -1 + 2 * 4 - 5 / -3 > 6 | 1 & 0
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let negative = parser.parse_expr();
        assert_eq!(
            format!("{}", negative),
            "((((-1 + (2 * 4)) - (5 / -3)) > 6) & (1 & 0))"
        );
    }

    #[test]
    fn test_parse_minus_unary_minus_mixed_expr() {
        let doc = "
        -1 + 2 * 4 - - 5 / -3 > 6 | 1 & 0
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let negative = parser.parse_expr();
        assert_eq!(
            format!("{}", negative),
            "((((-1 + (2 * 4)) - (-5 / -3)) > 6) & (1 & 0))"
        );
    }

    #[test]
    fn test_parse_expr() {
        let doc = "
        1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "1");
    }

    #[test]
    fn test_func_call_expr() {
        let doc = "
        funcName(1, 2, 3)
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "funcName(1, 2, 3)");
    }

    #[test]
    fn test_func_call_1_expr() {
        let doc = "
        funcName(\"hello\")
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "funcName(\"hello\")");
    }

    #[test]
    fn test_func_call_0_expr() {
        let doc = "
        funcName()
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "funcName()");
    }

    #[test]
    fn test_record_construct_0_expr() {
        let doc = "
        RecordName{}
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "RecordName {}");
    }

    #[test]
    fn test_record_construct_1_expr() {
        let doc = "
        RecordName{field0 = 1}
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "RecordName {field0 = 1}");
    }

    #[test]
    fn test_record_construct_expr() {
        let doc = "
        RecordName{field0 = 1, field1 = 2, field3 = 3}
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(
            format!("{}", e),
            "RecordName {field0 = 1, field1 = 2, field3 = 3}"
        );
    }

    #[test]
    fn test_array_construct_expr() {
        let doc = "
        int[3] of 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "int[3] of 1");
    }

    #[test]
    fn test_assign_expr() {
        let doc = "
        a[3] := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "a[3] := 1");
    }

    #[test]
    fn test_assign_1_expr() {
        let doc = "
        a[3].b := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "a[3].b := 1");
    }

    #[test]
    fn test_assign_2_expr() {
        let doc = "
        a := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        assert_eq!(format!("{}", e), "a := 1");
    }

    #[test]
    fn test_assign_3_expr() {
        let doc = "
        a := 1
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::Assign(
            ast::LeftValue::Variable(ident_pool::symbol("a")),
            Box::new(ast::Expr::Literal(ast::Value::Int(1))),
        );
        assert_eq!(e, expected);
    }

    #[test]
    fn test_if_expr() {
        let doc = "
        if 1 then 2 else 3
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::IfThenElse(ast::IfThenElseExpr {
            condition: Box::new(ast::Expr::Literal(ast::Value::Int(1))),
            then: Box::new(ast::Expr::Literal(ast::Value::Int(2))),
            el: Box::new(ast::Expr::Literal(ast::Value::Int(3))),
        });
        assert_eq!(e, expected);
    }

    #[test]
    fn test_if_2_expr() {
        let doc = "
        if 1 then 2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::IfThen(
            Box::new(ast::Expr::Literal(ast::Value::Int(1))),
            Box::new(ast::Expr::Literal(ast::Value::Int(2))),
        );
        assert_eq!(e, expected);
    }

    #[test]
    fn test_if_nested_expr() {
        let doc = "
        if if 1 then 2 then 2 else 3
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::IfThenElse(ast::IfThenElseExpr {
            condition: Box::new(ast::Expr::IfThen(
                Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                Box::new(ast::Expr::Literal(ast::Value::Int(2))),
            )),
            then: Box::new(ast::Expr::Literal(ast::Value::Int(2))),
            el: Box::new(ast::Expr::Literal(ast::Value::Int(3))),
        });
        assert_eq!(e, expected);
    }

    #[test]
    fn test_while_expr() {
        let doc = "
        while 1 do var1 := 3
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::While(ast::While {
            condition: Box::new(ast::Expr::Literal(ast::Value::Int(1))),
            body: Box::new(ast::Expr::Assign(
                ast::LeftValue::Variable(ident_pool::symbol("var1")),
                Box::new(ast::Expr::Literal(ast::Value::Int(3))),
            )),
        });
        assert_eq!(e, expected);
    }

    #[test]
    fn test_for_expr() {
        let doc = "
        for it := 1 to 10 do var1 := 3
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::For(ast::For {
            local: ident_pool::symbol("it"),
            lower: Box::new(ast::Expr::Literal(ast::Value::Int(1))),
            upper: Box::new(ast::Expr::Literal(ast::Value::Int(10))),
            body: Box::new(ast::Expr::Assign(
                ast::LeftValue::Variable(ident_pool::symbol("var1")),
                Box::new(ast::Expr::Literal(ast::Value::Int(3))),
            )),
        });
        assert_eq!(e, expected);
    }

    #[test]
    fn test_break_expr() {
        let doc = "
        break
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::Break;
        assert_eq!(e, expected);
    }

    #[test]
    fn test_let_expr() {
        let doc = "
        let var b := 1 type t1 = t2 in 1; 1+1; \"hello\" end
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::Let(ast::Let {
            decls: vec![
                ast::Decl::Var(ast::VarDecl::new(
                    ident_pool::symbol("b"),
                    None,
                    ast::Expr::Literal(ast::Value::Int(1)),
                )),
                ast::Decl::Type(ast::TypeDecl {
                    type_name: ident_pool::symbol("t1"),
                    ty: ast::Ty::Name(ident_pool::symbol("t2")),
                }),
            ],
            sequence: vec![
                ast::Expr::Literal(ast::Value::Int(1)),
                ast::Expr::Binary(ast::Binary::Add(
                    Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                    Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                )),
                ast::Expr::Literal(ast::Value::Str("hello".to_string())),
            ],
        });
        assert_eq!(e, expected);
    }

    #[test]
    fn test_parenthesis_expr() {
        let doc = "
        (1+1)*2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let e = parser.parse_expr();
        let expected = ast::Expr::Binary(ast::Binary::Multiply(
            Box::new(ast::Expr::Parenthesis(Box::new(ast::Expr::Binary(
                ast::Binary::Add(
                    Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                    Box::new(ast::Expr::Literal(ast::Value::Int(1))),
                ),
            )))),
            Box::new(ast::Expr::Literal(ast::Value::Int(2))),
        ));
        assert_eq!(e, expected);
    }

    #[test]
    #[should_panic]
    fn test_let2_expr() {
        let doc = "
        let type r1 = {f1: int, f2: string} in var v1 := r1{f1=1, f2=\"hello\"} end
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _e = parser.parse_expr();
    }
}
