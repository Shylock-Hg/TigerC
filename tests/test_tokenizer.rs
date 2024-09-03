#[cfg(test)]
mod test {

    use tiger_c::tokenizer;
    use tiger_c::tokenizer::Token;

    fn test_file(path: &str) {
        let content = std::fs::read_to_string(path).unwrap();
        let tok = tokenizer::tokenize(&content);
        for t in tok {
            if let Token::Error(e) = t.node() {
                panic!("Unexpected error: {:?}", e);
            }
        }
    }

    #[test]
    fn test_tokenizer() {
        let path = "tests/testcases/merge.tig";
        test_file(path);
    }

    #[test]
    fn test_tokenizer2() {
        let path = "tests/testcases/queens.tig";
        test_file(path);
    }
}
