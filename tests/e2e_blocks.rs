// E2E tests for block expressions and labels

mod common;

use common::*;

#[test]
fn test_simple_block_expression() {
    let source = r#"fn main() -> i64:
        let result = :
            let x = 10;
            let y = 20;
            break x + y .
        return result .
    "#;

    let executable =
        run_to_executable(source, "test_block.yuu").expect("Failed to compile block test");

    let output = run_executable_with_output(&executable, &[]).expect("Failed to run block test");

    assert_eq!(output, 30);
}

#[test]
fn test_labeled_block() {
    let source = r#"fn main() -> i64:
        let result = :outer:
            let x = 5;
            break :outer x * 2 .
        return result .
    "#;

    let executable = run_to_executable(source, "test_labeled_block.yuu")
        .expect("Failed to compile labeled block test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run labeled block test");

    assert_eq!(output, 10);
}

#[test]
fn test_nested_blocks() {
    let source = r#"fn main() -> i64:
        let result = :outer:
            let x = 10;
            let inner_result = :inner:
                let y = 5;
                break :inner y * 2 .
            break :outer x + inner_result .
        return result .
    "#;

    let executable = run_to_executable(source, "test_nested_blocks.yuu")
        .expect("Failed to compile nested blocks test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run nested blocks test");

    // inner: 5 * 2 = 10, outer: 10 + 10 = 20
    assert_eq!(output, 20);
}

#[test]
fn test_break_with_value() {
    let source = r#"fn main() -> i64:
        let result = :
            let mut counter = 0;
            while counter < 10:
                if counter == 5: break counter * 2 .
                counter = counter + 1 .
            break counter .
        return result .
    "#;

    let executable = run_to_executable(source, "test_break_value.yuu")
        .expect("Failed to compile break with value test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run break with value test");

    // breaks with 5 * 2 = 10
    assert_eq!(output, 10);
}

#[test]
fn test_break_from_nested_loop() {
    let source = r#"fn main() -> i64:
        let result = :outer:
            let mut i = 0;
            while i < 5:
                let mut j = 0;
                while j < 5:
                    if i == 2 && j == 3: break :outer i + j .
                    j = j + 1 .
                i = i + 1 .
            break :outer 0 .
        return result .
    "#;

    let executable = run_to_executable(source, "test_break_nested.yuu")
        .expect("Failed to compile break from nested test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run break from nested test");

    // i=2, j=3, so i+j=5
    assert_eq!(output, 5);
}

#[test]
fn test_block_with_local_scope() {
    let source = r#"fn main() -> i64:
        let x = 10;
        let result = :
            let x = 20;  // shadows outer x
            break x .
        return result + x . // uses outer x
    "#;

    let executable = run_to_executable(source, "test_block_scope.yuu")
        .expect("Failed to compile block scope test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run block scope test");

    // inner x=20, outer x=10, so 20+10=30
    assert_eq!(output, 30);
}

#[test]
fn test_block_as_function_body() {
    let source = r#"
        fn compute(x: i64) -> i64: :calc:
            let doubled = x * 2;
            if doubled > 10: break :calc doubled .
            else: break :calc x . .
        
        fn main() -> i64: return compute(8) .
    "#;

    let executable = run_to_executable(source, "test_block_func.yuu")
        .expect("Failed to compile block as function body test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run block as function body test");

    // 8 * 2 = 16 > 10, so returns 16
    assert_eq!(output, 16);
}

#[test]
fn test_multiple_labeled_blocks() {
    let source = r#"fn main() -> i64:
        let result1 = :first:
            break :first 10 .
        
        let result2 = :second:
            break :second 20 .
        
        return result1 + result2 .
    "#;

    let executable = run_to_executable(source, "test_multiple_labels.yuu")
        .expect("Failed to compile multiple labels test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run multiple labels test");

    // 10 + 20 = 30
    assert_eq!(output, 30);
}

#[test]
fn test_block_with_early_return() {
    let source = r#"fn main() -> i64:
        let result = :
            let x = 5;
            if x > 3: break 100 .
            break 0 .
        return result .
    "#;

    let executable = run_to_executable(source, "test_early_return.yuu")
        .expect("Failed to compile early return test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run early return test");

    // x=5 > 3, so breaks with 100
    assert_eq!(output, 100);
}
