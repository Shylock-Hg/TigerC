#[cfg(test)]
mod test {

    use tiger_c::parser::Parser;
    use tiger_c::tokenizer::{tokenize, Token};

    fn test_file(path: &str) {
        let content = std::fs::read_to_string(path).unwrap();
        let tok = tokenize(content.as_str());
        let mut parser = Parser::new(Box::new(tok));
        let _ast = parser.parse();
    }

    #[test]
    fn test_parse() {
        let path = "tests/testcases/merge.tig";
        test_file(path);
    }

    #[test]
    fn test_parse2() {
        let path = "tests/testcases/queens.tig";
        test_file(path);
    }
}
