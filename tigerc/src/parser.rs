use crate::ast;
use crate::ident_pool::{kw, Symbol};
use crate::tokenizer::{Posed, Token};

pub struct Parser {
    cursor: std::iter::Peekable<Box<dyn Iterator<Item = Posed<Token>>>>,
}

impl Parser {
    pub fn new(cursor: Box<dyn Iterator<Item = Posed<Token>>>) -> Self {
        Parser {
            cursor: cursor.peekable(),
        }
    }

    fn bump(&mut self) -> Option<Posed<Token>> {
        self.cursor.next()
    }

    fn look(&mut self) -> Option<&Posed<Token>> {
        self.cursor.peek()
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

    fn parse_expr(&mut self) -> ast::Expr {
        let current = self.bump().unwrap();
        match current.node() {
            Token::Number(n) => ast::Expr::Literal(ast::Value::Int(*n)),
            Token::Str(s) => ast::Expr::Literal(ast::Value::Str(s.clone())),
            Token::Ident(s) => {
                let prev = ast::LeftValue::Variable(*s);
                ast::Expr::LeftValue(self.parse_left_value_suffix(prev))
            }
            _ => unimplemented!(),
        }
    }

    fn parse_decl(&mut self) -> ast::Decl {
        let current = self.look().unwrap();
        match current.node() {
            Token::Ident(s) => match s {
                &kw::TOK_TYPE => ast::Decl::Type(self.parse_type_decl()),
                &kw::TOK_VAR => ast::Decl::Var(self.parse_var_decl()),
                &kw::TOK_FUNCTION => {
                    unimplemented!()
                }
                _ => Self::unexpected_token(current),
            },
            _ => Self::unexpected_token(current),
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
            args: fields,
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
        ast::VarDecl { name, ty, init }
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
                        // empty fields
                        ast::Ty::Struct(ast::TyStruct(vec![]))
                    }
                    Token::Ident(..) => {
                        let field_list = self.parse_field_list();
                        ast::Ty::Struct(ast::TyStruct(field_list))
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
    use crate::tokenizer::{debug_token_stream, tokenize};

    #[test]
    fn test_parser_new() {
        let it = tokenize("");
        let _parser = Parser::new(Box::new(it));
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
        let _decl = parser.parse_decl();
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
}
