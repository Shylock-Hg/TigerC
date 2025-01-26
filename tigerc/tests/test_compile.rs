#[cfg(test)]
mod test {

    use tiger_c::compile_file;

    #[test]
    fn test_compile() {
        let path = "tests/testcases/merge.tig";
        compile_file(path);
    }

    #[test]
    fn test_compile2() {
        let path = "tests/testcases/queens.tig";
        compile_file(path);
    }
}
