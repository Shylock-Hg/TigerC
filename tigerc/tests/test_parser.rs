use insta::assert_snapshot;
use tiger_c::parser::Parser;
use tiger_c::tokenizer::tokenize;

fn parse_snapshot(path: &str) -> String {
    let content = std::fs::read_to_string(path).unwrap();
    let tok = tokenize(content.as_str());
    let mut parser = Parser::new(Box::new(tok));
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse()))
        .map(|ast| format!("{}", ast))
        .unwrap_or_else(|_| "PARSE_ERROR".to_string())
}

#[test]
fn test_parse_merge() {
    assert_snapshot!(parse_snapshot("tests/testcases/merge.tig"));
}

#[test]
fn test_parse_queens() {
    assert_snapshot!(parse_snapshot("tests/testcases/queens.tig"));
}

#[test]
fn test_parse_test1() {
    assert_snapshot!(parse_snapshot("tests/testcases/test1.tig"));
}

#[test]
fn test_parse_test2() {
    assert_snapshot!(parse_snapshot("tests/testcases/test2.tig"));
}

#[test]
fn test_parse_test3() {
    assert_snapshot!(parse_snapshot("tests/testcases/test3.tig"));
}

#[test]
fn test_parse_test4() {
    assert_snapshot!(parse_snapshot("tests/testcases/test4.tig"));
}

#[test]
fn test_parse_test5() {
    assert_snapshot!(parse_snapshot("tests/testcases/test5.tig"));
}

#[test]
fn test_parse_test6() {
    assert_snapshot!(parse_snapshot("tests/testcases/test6.tig"));
}

#[test]
fn test_parse_test7() {
    assert_snapshot!(parse_snapshot("tests/testcases/test7.tig"));
}

#[test]
fn test_parse_test8() {
    assert_snapshot!(parse_snapshot("tests/testcases/test8.tig"));
}

#[test]
fn test_parse_test9() {
    assert_snapshot!(parse_snapshot("tests/testcases/test9.tig"));
}

#[test]
fn test_parse_test10() {
    assert_snapshot!(parse_snapshot("tests/testcases/test10.tig"));
}

#[test]
fn test_parse_test11() {
    assert_snapshot!(parse_snapshot("tests/testcases/test11.tig"));
}

#[test]
fn test_parse_test12() {
    assert_snapshot!(parse_snapshot("tests/testcases/test12.tig"));
}

#[test]
fn test_parse_test13() {
    assert_snapshot!(parse_snapshot("tests/testcases/test13.tig"));
}

#[test]
fn test_parse_test14() {
    assert_snapshot!(parse_snapshot("tests/testcases/test14.tig"));
}

#[test]
fn test_parse_test15() {
    assert_snapshot!(parse_snapshot("tests/testcases/test15.tig"));
}

#[test]
fn test_parse_test16() {
    assert_snapshot!(parse_snapshot("tests/testcases/test16.tig"));
}

#[test]
fn test_parse_test17() {
    assert_snapshot!(parse_snapshot("tests/testcases/test17.tig"));
}

#[test]
fn test_parse_test18() {
    assert_snapshot!(parse_snapshot("tests/testcases/test18.tig"));
}

#[test]
fn test_parse_test19() {
    assert_snapshot!(parse_snapshot("tests/testcases/test19.tig"));
}

#[test]
fn test_parse_test20() {
    assert_snapshot!(parse_snapshot("tests/testcases/test20.tig"));
}

#[test]
fn test_parse_test21() {
    assert_snapshot!(parse_snapshot("tests/testcases/test21.tig"));
}

#[test]
fn test_parse_test22() {
    assert_snapshot!(parse_snapshot("tests/testcases/test22.tig"));
}

#[test]
fn test_parse_test23() {
    assert_snapshot!(parse_snapshot("tests/testcases/test23.tig"));
}

#[test]
fn test_parse_test24() {
    assert_snapshot!(parse_snapshot("tests/testcases/test24.tig"));
}

#[test]
fn test_parse_test25() {
    assert_snapshot!(parse_snapshot("tests/testcases/test25.tig"));
}

#[test]
fn test_parse_test26() {
    assert_snapshot!(parse_snapshot("tests/testcases/test26.tig"));
}

#[test]
fn test_parse_test27() {
    assert_snapshot!(parse_snapshot("tests/testcases/test27.tig"));
}

#[test]
fn test_parse_test28() {
    assert_snapshot!(parse_snapshot("tests/testcases/test28.tig"));
}

#[test]
fn test_parse_test29() {
    assert_snapshot!(parse_snapshot("tests/testcases/test29.tig"));
}

#[test]
fn test_parse_test30() {
    assert_snapshot!(parse_snapshot("tests/testcases/test30.tig"));
}

#[test]
fn test_parse_test31() {
    assert_snapshot!(parse_snapshot("tests/testcases/test31.tig"));
}

#[test]
fn test_parse_test32() {
    assert_snapshot!(parse_snapshot("tests/testcases/test32.tig"));
}

#[test]
fn test_parse_test33() {
    assert_snapshot!(parse_snapshot("tests/testcases/test33.tig"));
}

#[test]
fn test_parse_test34() {
    assert_snapshot!(parse_snapshot("tests/testcases/test34.tig"));
}

#[test]
fn test_parse_test35() {
    assert_snapshot!(parse_snapshot("tests/testcases/test35.tig"));
}

#[test]
fn test_parse_test36() {
    assert_snapshot!(parse_snapshot("tests/testcases/test36.tig"));
}

#[test]
fn test_parse_test37() {
    assert_snapshot!(parse_snapshot("tests/testcases/test37.tig"));
}

#[test]
fn test_parse_test38() {
    assert_snapshot!(parse_snapshot("tests/testcases/test38.tig"));
}

#[test]
fn test_parse_test39() {
    assert_snapshot!(parse_snapshot("tests/testcases/test39.tig"));
}

#[test]
fn test_parse_test40() {
    assert_snapshot!(parse_snapshot("tests/testcases/test40.tig"));
}

#[test]
fn test_parse_test41() {
    assert_snapshot!(parse_snapshot("tests/testcases/test41.tig"));
}

#[test]
fn test_parse_test42() {
    assert_snapshot!(parse_snapshot("tests/testcases/test42.tig"));
}

#[test]
fn test_parse_test43() {
    assert_snapshot!(parse_snapshot("tests/testcases/test43.tig"));
}

#[test]
fn test_parse_test44() {
    assert_snapshot!(parse_snapshot("tests/testcases/test44.tig"));
}

#[test]
fn test_parse_test45() {
    assert_snapshot!(parse_snapshot("tests/testcases/test45.tig"));
}

#[test]
fn test_parse_test46() {
    assert_snapshot!(parse_snapshot("tests/testcases/test46.tig"));
}

#[test]
fn test_parse_test47() {
    assert_snapshot!(parse_snapshot("tests/testcases/test47.tig"));
}

#[test]
fn test_parse_test48() {
    assert_snapshot!(parse_snapshot("tests/testcases/test48.tig"));
}

#[test]
fn test_parse_test49() {
    assert_snapshot!(parse_snapshot("tests/testcases/test49.tig"));
}
