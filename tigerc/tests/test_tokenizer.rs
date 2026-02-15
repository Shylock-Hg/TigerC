use insta::assert_snapshot;
use tiger_c::tokenizer;

fn tokenize_snapshot(path: &str) -> String {
    let content = std::fs::read_to_string(path).unwrap();
    let mut tokens = tokenizer::tokenize(&content);
    let mut output = String::new();
    for tok in tokens {
        output.push_str(&format!(
            "Posed {{ node: {}, pos: Pos {{ line: {}, column: {} }} }}\n",
            tok.node(),
            tok.pos().line,
            tok.pos().column
        ));
    }
    output
}

#[test]
fn test_tokenizer_merge() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/merge.tig"));
}

#[test]
fn test_tokenizer_queens() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/queens.tig"));
}

#[test]
fn test_tokenizer_test1() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test1.tig"));
}

#[test]
fn test_tokenizer_test2() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test2.tig"));
}

#[test]
fn test_tokenizer_test3() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test3.tig"));
}

#[test]
fn test_tokenizer_test4() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test4.tig"));
}

#[test]
fn test_tokenizer_test5() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test5.tig"));
}

#[test]
fn test_tokenizer_test6() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test6.tig"));
}

#[test]
fn test_tokenizer_test7() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test7.tig"));
}

#[test]
fn test_tokenizer_test8() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test8.tig"));
}

#[test]
fn test_tokenizer_test9() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test9.tig"));
}

#[test]
fn test_tokenizer_test10() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test10.tig"));
}

#[test]
fn test_tokenizer_test11() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test11.tig"));
}

#[test]
fn test_tokenizer_test12() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test12.tig"));
}

#[test]
fn test_tokenizer_test13() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test13.tig"));
}

#[test]
fn test_tokenizer_test14() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test14.tig"));
}

#[test]
fn test_tokenizer_test15() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test15.tig"));
}

#[test]
fn test_tokenizer_test16() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test16.tig"));
}

#[test]
fn test_tokenizer_test17() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test17.tig"));
}

#[test]
fn test_tokenizer_test18() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test18.tig"));
}

#[test]
fn test_tokenizer_test19() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test19.tig"));
}

#[test]
fn test_tokenizer_test20() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test20.tig"));
}

#[test]
fn test_tokenizer_test21() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test21.tig"));
}

#[test]
fn test_tokenizer_test22() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test22.tig"));
}

#[test]
fn test_tokenizer_test23() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test23.tig"));
}

#[test]
fn test_tokenizer_test24() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test24.tig"));
}

#[test]
fn test_tokenizer_test25() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test25.tig"));
}

#[test]
fn test_tokenizer_test26() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test26.tig"));
}

#[test]
fn test_tokenizer_test27() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test27.tig"));
}

#[test]
fn test_tokenizer_test28() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test28.tig"));
}

#[test]
fn test_tokenizer_test29() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test29.tig"));
}

#[test]
fn test_tokenizer_test30() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test30.tig"));
}

#[test]
fn test_tokenizer_test31() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test31.tig"));
}

#[test]
fn test_tokenizer_test32() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test32.tig"));
}

#[test]
fn test_tokenizer_test33() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test33.tig"));
}

#[test]
fn test_tokenizer_test34() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test34.tig"));
}

#[test]
fn test_tokenizer_test35() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test35.tig"));
}

#[test]
fn test_tokenizer_test36() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test36.tig"));
}

#[test]
fn test_tokenizer_test37() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test37.tig"));
}

#[test]
fn test_tokenizer_test38() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test38.tig"));
}

#[test]
fn test_tokenizer_test39() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test39.tig"));
}

#[test]
fn test_tokenizer_test40() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test40.tig"));
}

#[test]
fn test_tokenizer_test41() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test41.tig"));
}

#[test]
fn test_tokenizer_test42() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test42.tig"));
}

#[test]
fn test_tokenizer_test43() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test43.tig"));
}

#[test]
fn test_tokenizer_test44() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test44.tig"));
}

#[test]
fn test_tokenizer_test45() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test45.tig"));
}

#[test]
fn test_tokenizer_test46() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test46.tig"));
}

#[test]
fn test_tokenizer_test47() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test47.tig"));
}

#[test]
fn test_tokenizer_test48() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test48.tig"));
}

#[test]
fn test_tokenizer_test49() {
    assert_snapshot!(tokenize_snapshot("tests/testcases/test49.tig"));
}
