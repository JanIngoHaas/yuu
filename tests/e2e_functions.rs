// E2E tests for function definitions and calls

mod common;

use common::*;

#[test]
fn test_simple_function_call() {
    let source = r#"
        fn add_(a: i64, b: i64) -> i64: return a + b .

        fn main() -> i64: return add_(5, 3) .
    "#;

    let executable = run_to_executable(source, "test_func_call.yuu")
        .expect("Failed to compile function call test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run function call test");

    assert_eq!(output, 8);
}

#[test]
fn test_function_with_no_params() {
    let source = r#"
        fn get_answer() -> i64: return 42 .
        
        fn main() -> i64: return get_answer() .
    "#;

    let executable =
        run_to_executable(source, "test_no_params.yuu").expect("Failed to compile no params test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run no params test");

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

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run multi params test");

    // 1 + 2 * 3 = 7
    assert_eq!(output, 7);
}

#[test]
fn test_nested_function_calls() {
    let source = r#"
        fn add_(a: i64, b: i64) -> i64: return a + b .
        
        fn main() -> i64: return id(add_(multiply(2, 3), multiply(4, 5))) .
        fn id(a: i64) -> i64: return a .
        fn multiply(a: i64, b: i64) -> i64: return a * b .
    "#;

    let executable = run_to_executable(source, "test_nested_calls.yuu")
        .expect("Failed to compile nested calls test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run nested calls test");

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

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run float params test");

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

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run mut params test");

    // 5 + 10 = 15
    assert_eq!(output, 15);
}
