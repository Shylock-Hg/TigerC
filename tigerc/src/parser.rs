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

    fn parse_type_decl(&mut self) -> ast::TypeDecl {
        let tok = self.bump().unwrap();
        if let Token::Ident(symbol) = tok.node() {
            if *symbol == kw::TOK_TYPE {
                let type_name = self.eat_ident();
                self.eat_expect(Token::Assign);
                let ty = self.parse_ty();
                ast::TypeDecl { type_name, ty }
            } else {
                Self::unexpected_token(&tok)
            }
        } else {
            Self::unexpected_token(&tok)
        }
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
        type TypeName := Type2
        ";
        let it = tokenize(doc);
        let mut parser = Parser::new(Box::new(it));
        let _type_decl = parser.parse_type_decl();
    }
}
