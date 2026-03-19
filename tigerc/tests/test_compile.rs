#[cfg(test)]
mod test {

    use std::io::Write;
    use std::process::Stdio;

    use insta::assert_snapshot;
    use tiger_c::compile_file;
    use tiger_c::TIGER_RUNTIME_LIB;

    #[test]
    fn test_compile1() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
        let path = "tests/testcases/merge.tig";
        compile_file(path, "tests/testcases/merge.t");
        let output = io("./tests/testcases/merge.t", "1\n2\n4\n7\na\n8\n9\nb\n");
        assert!(output.status.success());
        assert_snapshot!(String::from_utf8_lossy(&output.stdout));
    }

    #[test]
    fn test_compile2() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
        let path = "tests/testcases/queens.tig";
        compile_file(path, "tests/testcases/queens.t");
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
        compile_file(path, "tests/testcases/hello.t");
        let output = std::process::Command::new("./tests/testcases/hello.t")
            .output()
            .unwrap();
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello world!");
    }

    fn io(cmd: &str, input: &str) -> std::process::Output {
        let mut child = std::process::Command::new(cmd)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        if let Some(mut stdin) = child.stdin.take() {
            stdin.write_all(input.as_bytes()).unwrap();
        }

        let output = child.wait_with_output().unwrap();
        output
    }

    #[test]
    fn test_compile4() {
        unsafe {
            std::env::set_var(TIGER_RUNTIME_LIB, "../target/debug/libruntime.a");
        }
        let path = "tests/testcases/test_two_level_static_link.tig";
        compile_file(path, "tests/testcases/test_two_level_static_link.t");
        let output = io("./tests/testcases/test_two_level_static_link.t", "h");
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout), "104\n");
    }
}
