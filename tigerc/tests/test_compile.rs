#[cfg(test)]
mod test {

    use insta::assert_snapshot;
    use tiger_c::compile_file;
    use tiger_c::TIGER_RUNTIME_LIB;

    #[test]
    fn test_compile1() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
        let path = "tests/testcases/merge.tig";
        compile_file(path, "tests/testcases/merge.s");
    }

    #[test]
    fn test_compile2() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
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
        assert_snapshot!(String::from_utf8_lossy(&output.stdout));
    }

    #[test]
    fn test_compile3() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
        let path = "tests/testcases/hello.tig";
        compile_file(path, "tests/testcases/hello.s");
        let output = std::process::Command::new("./tests/testcases/hello.t")
            .output()
            .unwrap();
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello world!");
    }
}
