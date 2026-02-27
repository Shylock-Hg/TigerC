#[cfg(test)]
mod test {

    use tiger_c::compile_file;

    #[test]
    fn test_compile1() {
        let path = "tests/testcases/merge.tig";
        compile_file(path, "tests/testcases/merge.s");
    }

    #[test]
    fn test_compile2() {
        let path = "tests/testcases/queens.tig";
        compile_file(path, "tests/testcases/queens.s");
        let output = std::process::Command::new("./tests/testcases/queens.t")
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}, {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    fn test_compile3() {
        let path = "tests/testcases/hello.tig";
        compile_file(path, "tests/testcases/hello.s");
        let output = std::process::Command::new("./tests/testcases/hello.t")
            .output()
            .unwrap();
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello world!");
    }
}
