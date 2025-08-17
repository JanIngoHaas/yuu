// E2E tests for complex expressions and operator precedence

mod common;

use common::*;

#[test]
fn test_complex_arithmetic_expression() {
    let source = r#"fn main() -> i64:
        let result = 2 + 3 * 4 - 5 / 2 + 1;
        return result .
    "#;

    let executable = run_to_executable(source, "test_complex_arithmetic.yuu")
        .expect("Failed to compile complex arithmetic test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run complex arithmetic test");

    // 2 + (3 * 4) - (5 / 2) + 1 = 2 + 12 - 2 + 1 = 13
    assert_eq!(output, 13);
}

#[test]
fn test_nested_parentheses() {
    let source = r#"fn main() -> i64:
        let result = ((2 + 3) * (4 - 1)) / ((5 + 1) / 2);
        return result .
    "#;

    let executable = run_to_executable(source, "test_nested_parens.yuu")
        .expect("Failed to compile nested parentheses test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run nested parentheses test");

    // ((2+3) * (4-1)) / ((5+1) / 2) = (5 * 3) / (6 / 2) = 15 / 3 = 5
    assert_eq!(output, 5);
}

// #[test]
// fn test_comparison_chain() {
//     let source = r#"fn main() -> i64:
//         let x = 5;
//         let y = 10;
//         let z = 15;
//         let result = if x < y && y < z: 1 else: 0;
//         return result .
//     "#;

//     let executable = run_to_executable(source, "test_comparison_chain.yuu")
//         .expect("Failed to compile comparison chain test");

//     let output =
//         run_executable_with_output(&executable, &[]).expect("Failed to run comparison chain test");

//     assert_eq!(output, 1);
// }

#[test]
fn test_mixed_unary_binary_operators() {
    let source = r#"fn main() -> i64:
        let x = 5;
        let result = -x + 3 * (-2 + 4);
        return result .
    "#;

    let executable = run_to_executable(source, "test_mixed_operators.yuu")
        .expect("Failed to compile mixed operators test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run mixed operators test");

    // -5 + 3 * (-2 + 4) = -5 + 3 * 2 = -5 + 6 = 1
    assert_eq!(output, 1);
}

#[test]
fn test_function_call_in_expression() {
    let source = r#"
        fn double(x: i64) -> i64: return x * 2 .
        fn add_(a: i64, b: i64) -> i64: return a + b .
        
        fn main() -> i64:
            let result = double(5) + add_(3, 4) * 2;
        return result .
    "#;

    let executable = run_to_executable(source, "test_func_in_expr.yuu")
        .expect("Failed to compile function in expression test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run function in expression test");

    // double(5) + add(3,4) * 2 = 10 + 7 * 2 = 10 + 14 = 24
    assert_eq!(output, 24);
}

#[test]
fn test_deep_expression_nesting() {
    let source = r#"fn main() -> i64:
        let a = 2;
        let b = 3;
        let c = 4;
        let result = a + (b * (c + (a - b))) - (c / (a + b));
        return result .
    "#;

    let executable = run_to_executable(source, "test_deep_nesting.yuu")
        .expect("Failed to compile deep nesting test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run deep nesting test");

    // a=2, b=3, c=4
    // 2 + (3 * (4 + (2 - 3))) - (4 / (2 + 3))
    // 2 + (3 * (4 + (-1))) - (4 / 5)
    // 2 + (3 * 3) - 0 = 2 + 9 - 0 = 11
    assert_eq!(output, 11);
}

// #[test]
// fn test_complex_boolean_expression() {
//     let source = r#"fn main() -> i64:
//         let a = 5;
//         let b = 10;
//         let c = 15;
//         let result = if (a < b) && (b < c) && (a + b > c - 5): 1 else: 0;
//         return result .
//     "#;

//     let executable = run_to_executable(source, "test_complex_boolean.yuu")
//         .expect("Failed to compile complex boolean test");

//     let output =
//         run_executable_with_output(&executable, &[]).expect("Failed to run complex boolean test");

//     // (5 < 10) && (10 < 15) && (5 + 10 > 15 - 5)
//     // true && true && (15 > 10) = true && true && true = true = 1
//     assert_eq!(output, 1);
// }

#[test]
fn test_assignment_in_expression() {
    let source = r#"fn main() -> i64:
        let mut x = 5;
        let result = (x = x + 3) + x;
        return result .
    "#;

    let executable = run_to_executable(source, "test_assignment_expr.yuu")
        .expect("Failed to compile assignment expression test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run assignment expression test");

    // x = 5 + 3 = 8, then 8 + 8 = 16
    assert_eq!(output, 16);
}

// #[test]
// fn test_precedence_with_all_operators() {
//     let source = r#"fn main() -> i64:
//         let result = 1 + 2 * 3 == 7 && 4 > 2;
//         return if result: 1 else: 0 .
//     "#;

//     let executable = run_to_executable(source, "test_all_operators.yuu")
//         .expect("Failed to compile all operators test");

//     let output =
//         run_executable_with_output(&executable, &[]).expect("Failed to run all operators test");

//     // 1 + 2 * 3 == 7 && 4 > 2
//     // 1 + 6 == 7 && true
//     // 7 == 7 && true
//     // true && true = true = 1
//     assert_eq!(output, 1);
// }

#[test]
fn test_expression_with_struct_access() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        end
        
        fn main() -> i64:
            let p1 = Point { x: 3, y: 4 };
            let p2 = Point { x: 1, y: 2 };
            let result = (p1.x + p2.x) * (p1.y - p2.y);
            return result .
    "#;

    let executable = run_to_executable(source, "test_struct_access_expr.yuu")
        .expect("Failed to compile struct access expression test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run struct access expression test");

    // (3 + 1) * (4 - 2) = 4 * 2 = 8
    assert_eq!(output, 8);
}
