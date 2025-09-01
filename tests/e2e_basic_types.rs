// E2E tests for basic types and literals

mod common;

use common::*;

#[test]
fn test_integer_literals() {
    let source = r#"fn main() -> i64: return 42 ."#;

    let executable = run_to_executable(source, "test_integer.yuu")
        .expect("Failed to compile integer literal test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run integer literal test");

    // Should exit with code 42
    assert_eq!(output, 42);
}

#[test]
fn test_float_literals() {
    let source = r#"fn main() -> i64: 
        let x = 3.14;
        return 0 .
    "#;

    let executable =
        run_to_executable(source, "test_float.yuu").expect("Failed to compile float literal test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run float literal test");

    assert_eq!(output, 0);
}

#[test]
fn test_arithmetic_operations() {
    let source = r#"fn main() -> i64:
        let a = 10;
        let b = 5;
        let sum = a + b;
        let diff = a - b;
        let prod = a * b;
        let quot = a / b;
        return sum + diff + prod + quot .
    "#;

    let executable = run_to_executable(source, "test_arithmetic.yuu")
        .expect("Failed to compile arithmetic test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run arithmetic test");

    // sum=15, diff=5, prod=50, quot=2 -> total=72
    assert_eq!(output, 72);
}

#[test]
fn test_comparison_operations() {
    let source = r#"fn main() -> i64:
        let a = 10;
        let b = 5;
        let mut result = 0;
        if a > b: result = 1 .
        else: result = 0 .
        return result .
    "#;

    let executable = run_to_executable(source, "test_comparison.yuu")
        .expect("Failed to compile comparison test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run comparison test");

    assert_eq!(output, 1);
}

#[test]
fn test_unary_operations() {
    let source = r#"fn main() -> i64:
        let x = 5;
        let neg_x = -x;
        let pos_x = +x;
        return neg_x + pos_x .
    "#;

    let executable = run_to_executable(source, "test_unary.yuu")
        .expect("Failed to compile unary operations test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run unary operations test");

    // -5 + 5 = 0
    assert_eq!(output, 0);
}

#[test]
fn test_mixed_type_expressions() {
    let source = r#"fn main() -> i64:
        let int_val = 10;
        let float_val = 3.5;
        return int_val .
    "#;

    let executable =
        run_to_executable(source, "test_mixed.yuu").expect("Failed to compile mixed type test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run mixed type test");

    assert_eq!(output, 10);
}

#[test]
fn test_operator_precedence() {
    let source = r#"fn main() -> i64:
        let result = 2 + 3 * 4;
        return result .
    "#;

    let executable = run_to_executable(source, "test_precedence.yuu")
        .expect("Failed to compile precedence test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run precedence test");

    // 2 + (3 * 4) = 14
    assert_eq!(output, 14);
}

#[test]
fn test_parenthesized_expressions() {
    let source = r#"fn main() -> i64:
        let result = (2 + 3) * 4;
        return result .
    "#;

    let executable =
        run_to_executable(source, "test_parens.yuu").expect("Failed to compile parenthesized test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run parenthesized test");

    // (2 + 3) * 4 = 20
    assert_eq!(output, 20);
}
