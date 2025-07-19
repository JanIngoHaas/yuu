// E2E tests for function definitions and calls

mod common;

use common::*;

#[test]
fn test_simple_function_call() {
    let source = r#"
        fn add(a: i64, b: i64) -> i64: return a + b .
        
        fn main() -> i64: return add(5, 3) .
    "#;
    
    let executable = run_to_executable(source, "test_func_call.yuu")
        .expect("Failed to compile function call test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run function call test");
    
    assert_eq!(output, 8);
}

#[test]
fn test_function_with_no_params() {
    let source = r#"
        fn get_answer() -> i64: return 42 .
        
        fn main() -> i64: return get_answer() .
    "#;
    
    let executable = run_to_executable(source, "test_no_params.yuu")
        .expect("Failed to compile no params test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run no params test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_function_with_multiple_params() {
    let source = r#"
        fn calc(a: i64, b: i64, c: i64) -> i64: return a + b * c .
        
        fn main() -> i64: return calc(1, 2, 3) .
    "#;
    
    let executable = run_to_executable(source, "test_multi_params.yuu")
        .expect("Failed to compile multi params test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run multi params test");
    
    // 1 + 2 * 3 = 7
    assert_eq!(output, 7);
}

#[test]
fn test_recursive_function() {
    let source = r#"
        fn factorial(n: i64) -> i64:
            if n <= 1: return 1 .
            else: return n * factorial(n - 1) . .
        
        fn main() -> i64: return factorial(5) .
    "#;
    
    let executable = run_to_executable(source, "test_recursive.yuu")
        .expect("Failed to compile recursive test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run recursive test");
    
    // 5! = 120
    assert_eq!(output, 120);
}

#[test]
fn test_function_with_local_variables() {
    let source = r#"
        fn compute(x: i64) -> i64:
            let doubled = x * 2;
            let plus_ten = doubled + 10;
            return plus_ten .
        
        fn main() -> i64: return compute(5) .
    "#;
    
    let executable = run_to_executable(source, "test_local_vars.yuu")
        .expect("Failed to compile local vars test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run local vars test");
    
    // 5 * 2 + 10 = 20
    assert_eq!(output, 20);
}

#[test]
fn test_nested_function_calls() {
    let source = r#"
        fn add(a: i64, b: i64) -> i64: return a + b .
        fn multiply(a: i64, b: i64) -> i64: return a * b .
        
        fn main() -> i64: return add(multiply(2, 3), multiply(4, 5)) .
    "#;
    
    let executable = run_to_executable(source, "test_nested_calls.yuu")
        .expect("Failed to compile nested calls test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run nested calls test");
    
    // (2*3) + (4*5) = 6 + 20 = 26
    assert_eq!(output, 26);
}

#[test]
fn test_function_with_float_params() {
    let source = r#"
        fn area_circle(radius: f32) -> f32: return 3.14 * radius * radius .
        
        fn main() -> i64: 
            let area = area_circle(2.0);
            return 0 .
    "#;
    
    let executable = run_to_executable(source, "test_float_params.yuu")
        .expect("Failed to compile float params test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run float params test");
    
    assert_eq!(output, 0);
}

#[test]
fn test_function_with_mutable_params() {
    let source = r#"
        fn modify_value(mut x: i64) -> i64:
            x = x + 10;
            return x .
        
        fn main() -> i64: return modify_value(5) .
    "#;
    
    let executable = run_to_executable(source, "test_mut_params.yuu")
        .expect("Failed to compile mut params test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mut params test");
    
    // 5 + 10 = 15
    assert_eq!(output, 15);
}

#[test]
fn test_fibonacci_recursive() {
    let source = r#"
        fn fibonacci(n: i64) -> i64:
            if n <= 1: return n .
            else: return fibonacci(n - 1) + fibonacci(n - 2) . .
        
        fn main() -> i64: return fibonacci(8) .
    "#;
    
    let executable = run_to_executable(source, "test_fibonacci.yuu")
        .expect("Failed to compile fibonacci test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run fibonacci test");
    
    // fib(8) = 21
    assert_eq!(output, 21);
}