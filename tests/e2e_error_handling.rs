// E2E tests for error handling and recovery

mod common;

use common::*;

#[test]
fn test_syntax_error_recovery() {
    let source = r#"fn main() -> i64:
        let x = 5
        let y = 10;
        return x + y .
    "#; // Missing semicolon after x = 5
    
    // This should fail to compile
    let result = run_to_executable(source, "test_syntax_error.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to syntax error");
}

#[test]
fn test_type_error_detection() {
    let source = r#"
        fn add_numbers(a: i64, b: i64) -> i64:
            return a + b .
        
        fn main() -> i64:
            let result = add_numbers(5, 3.14);
            return result .
    "#; // Passing float to function expecting int
    
    // This should fail to compile
    let result = run_to_executable(source, "test_type_error.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to type error");
}

#[test]
fn test_undefined_variable_error() {
    let source = r#"fn main() -> i64:
        let x = 5;
        return x + undefined_var .
    "#; // undefined_var is not defined
    
    // This should fail to compile
    let result = run_to_executable(source, "test_undefined_var.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to undefined variable");
}

#[test]
fn test_undefined_function_error() {
    let source = r#"fn main() -> i64:
        return undefined_function(5) .
    "#; // undefined_function is not defined
    
    // This should fail to compile
    let result = run_to_executable(source, "test_undefined_func.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to undefined function");
}

#[test]
fn test_wrong_number_of_arguments() {
    let source = r#"
        fn add(a: i64, b: i64) -> i64:
            return a + b .
        
        fn main() -> i64:
            return add(5) .
    "#; // add expects 2 arguments, got 1
    
    // This should fail to compile
    let result = run_to_executable(source, "test_wrong_args.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to wrong number of arguments");
}

#[test]
fn test_immutable_assignment_error() {
    let source = r#"fn main() -> i64:
        let x = 5;
        x = 10;
        return x .
    "#; // x is immutable, cannot assign
    
    // This should fail to compile
    let result = run_to_executable(source, "test_immutable_assign.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to immutable assignment");
}

#[test]
fn test_struct_field_error() {
    let source = r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn main() -> i64:
            let p = Point { x: 5, y: 10 };
            return p.z .
    "#; // Point doesn't have field z
    
    // This should fail to compile
    let result = run_to_executable(source, "test_struct_field_error.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to unknown struct field");
}

#[test]
fn test_return_type_mismatch() {
    let source = r#"fn main() -> i64:
        return 3.14 .
    "#; // Function returns i64 but trying to return f64
    
    // This should fail to compile
    let result = run_to_executable(source, "test_return_type_mismatch.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to return type mismatch");
}

#[test]
fn test_successful_compilation_after_error_fix() {
    // First try with error
    let source_with_error = r#"fn main() -> i64:
        let x = 5
        return x .
    "#; // Missing semicolon
    
    let result = run_to_executable(source_with_error, "test_error_fix.yuu");
    assert!(result.is_err(), "Expected compilation to fail");
    
    // Now try with fixed version
    let source_fixed = r#"fn main() -> i64:
        let x = 5;
        return x .
    "#;
    
    let executable = run_to_executable(source_fixed, "test_error_fix.yuu")
        .expect("Expected compilation to succeed after fix");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Expected execution to succeed");
    
    assert_eq!(output, 5);
}

#[test]
fn test_multiple_errors_in_source() {
    let source = r#"fn main() -> i64:
        let x = 5
        let y = undefined_var;
        x = 10;
        return x + y .
    "#; // Multiple errors: missing semicolon, undefined variable, immutable assignment
    
    // This should fail to compile
    let result = run_to_executable(source, "test_multiple_errors.yuu");
    assert!(result.is_err(), "Expected compilation to fail due to multiple errors");
}

#[test]
fn test_recursive_function_type_checking() {
    let source = r#"
        fn factorial(n: i64) -> i64:
            if n <= 1: return 1 .
            else: return n * factorial(n - 1) . .
        
        fn main() -> i64:
            return factorial(5) .
    "#; // This should work - recursive function with correct types
    
    let executable = run_to_executable(source, "test_recursive_types.yuu")
        .expect("Expected recursive function to compile successfully");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Expected recursive function to execute successfully");
    
    // 5! = 120
    assert_eq!(output, 120);
}