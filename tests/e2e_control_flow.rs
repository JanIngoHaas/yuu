// E2E tests for control flow statements

mod common;

use common::*;

#[test]
fn test_simple_if_expression() {
    let source = r#"fn main() -> i64:
        let x = 10;
        let result = if x > 5: 1 else: 0;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_if.yuu")
        .expect("Failed to compile if test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run if test");
    
    assert_eq!(output, 1);
}

#[test]
fn test_if_else_if_chain() {
    let source = r#"fn main() -> i64:
        let x = 7;
        let result = if x < 5: 1 
                     else if x < 10: 2
                     else: 3;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_if_else_if.yuu")
        .expect("Failed to compile if-else-if test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run if-else-if test");
    
    assert_eq!(output, 2);
}

#[test]
fn test_nested_if_expressions() {
    let source = r#"fn main() -> i64:
        let x = 8;
        let y = 3;
        let result = if x > 5: 
                        if y > 2: 10 else: 5
                     else: 0;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_nested_if.yuu")
        .expect("Failed to compile nested if test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run nested if test");
    
    assert_eq!(output, 10);
}

#[test]
fn test_while_loop() {
    let source = r#"fn main() -> i64:
        let mut counter = 0;
        let mut sum = 0;
        while counter < 5:
            sum = sum + counter;
            counter = counter + 1 .
        return sum .
    "#;
    
    let executable = run_to_executable(source, "test_while.yuu")
        .expect("Failed to compile while test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run while test");
    
    // 0 + 1 + 2 + 3 + 4 = 10
    assert_eq!(output, 10);
}

#[test]
fn test_while_with_break() {
    let source = r#"fn main() -> i64:
        let mut counter = 0;
        while counter < 10:
            if counter == 5: break counter .
            counter = counter + 1 .
        return counter .
    "#;
    
    let executable = run_to_executable(source, "test_while_break.yuu")
        .expect("Failed to compile while break test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run while break test");
    
    assert_eq!(output, 5);
}

#[test]
fn test_nested_while_loops() {
    let source = r#"fn main() -> i64:
        let mut i = 0;
        let mut sum = 0;
        while i < 3:
            let mut j = 0;
            while j < 3:
                sum = sum + 1;
                j = j + 1 .
            i = i + 1 .
        return sum .
    "#;
    
    let executable = run_to_executable(source, "test_nested_while.yuu")
        .expect("Failed to compile nested while test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run nested while test");
    
    // 3 * 3 = 9
    assert_eq!(output, 9);
}

#[test]
fn test_if_with_complex_condition() {
    let source = r#"fn main() -> i64:
        let x = 10;
        let y = 5;
        let result = if x > y && x < 15: 1 else: 0;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_complex_condition.yuu")
        .expect("Failed to compile complex condition test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run complex condition test");
    
    assert_eq!(output, 1);
}

#[test]
fn test_while_with_complex_condition() {
    let source = r#"fn main() -> i64:
        let mut x = 0;
        let mut y = 10;
        while x < 5 && y > 5:
            x = x + 1;
            y = y - 1 .
        return x + y .
    "#;
    
    let executable = run_to_executable(source, "test_while_complex.yuu")
        .expect("Failed to compile while complex test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run while complex test");
    
    // x=5, y=5 when loop ends, so x+y=10
    assert_eq!(output, 10);
}

#[test]
fn test_return_in_if() {
    let source = r#"fn main() -> i64:
        let x = 10;
        if x > 5: return 42 .
        return 0 .
    "#;
    
    let executable = run_to_executable(source, "test_return_if.yuu")
        .expect("Failed to compile return in if test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run return in if test");
    
    assert_eq!(output, 42);
}