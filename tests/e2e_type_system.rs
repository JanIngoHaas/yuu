// E2E tests for type system and type inference

mod common;

use common::*;

#[test]
fn test_type_inference_basic() {
    let source = r#"fn main() -> i64 =>
        let x = 42;  // should infer i64
        let y = 3.14; // should infer f64
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_type_infer.yuu")
        .expect("Failed to compile type inference test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run type inference test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_explicit_type_annotations() {
    let source = r#"fn main() -> i64 =>
        let x: i64 = 42;
        let y: f32 = 3.14;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_explicit_types.yuu")
        .expect("Failed to compile explicit types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run explicit types test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_function_parameter_types() {
    let source = r#"
        fn add_numbers(a: i64, b: i64) -> i64 =>
            return a + b .
        
        fn add_floats(a: f32, b: f32) -> f32 =>
            return a + b .
        
        fn main() -> i64 =>
            let int_result = add_numbers(5, 3);
            let float_result = add_floats(2.5, 1.5);
            return int_result .
    "#;
    
    let executable = run_to_executable(source, "test_param_types.yuu")
        .expect("Failed to compile parameter types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run parameter types test");
    
    assert_eq!(output, 8);
}

#[test]
fn test_return_type_matching() {
    let source = r#"
        fn get_integer() -> i64 =>
            return 42 .
        
        fn get_float() -> f32 =>
            return 3.14 .
        
        fn main() -> i64 =>
            let int_val = get_integer();
            let float_val = get_float();
            return int_val .
    "#;
    
    let executable = run_to_executable(source, "test_return_types.yuu")
        .expect("Failed to compile return types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run return types test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_struct_type_checking() {
    let source = r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn distance_squared(p: Point) -> i64 =>
            return p.x * p.x + p.y * p.y .
        
        fn main() -> i64 =>
            let p = Point { x: 3, y: 4 };
            return distance_squared(p) .
    "#;
    
    let executable = run_to_executable(source, "test_struct_types.yuu")
        .expect("Failed to compile struct types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run struct types test");
    
    assert_eq!(output, 25);
}

#[test]
fn test_mixed_arithmetic_types() {
    let source = r#"fn main() -> i64 =>
        let a: i64 = 10;
        let b: i64 = 3;
        let result = a + b * 2;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_mixed_arithmetic.yuu")
        .expect("Failed to compile mixed arithmetic test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mixed arithmetic test");
    
    // 10 + 3 * 2 = 16
    assert_eq!(output, 16);
}

#[test]
fn test_type_inference_from_function_calls() {
    let source = r#"
        fn return_int() -> i64 => return 42 .
        fn return_float() -> f32 => return 3.14 .
        
        fn main() -> i64 =>
            let x = return_int();  // should infer i64
            let y = return_float(); // should infer f32
            return x .
    "#;
    
    let executable = run_to_executable(source, "test_infer_from_calls.yuu")
        .expect("Failed to compile inference from calls test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run inference from calls test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_comparison_operator_types() {
    let source = r#"fn main() -> i64 =>
        let a = 10;
        let b = 5;
        let greater = a > b;
        let equal = a == b;
        let result = if greater => 1 else => 0;
        return result .
    "#;
    
    let executable = run_to_executable(source, "test_comparison_types.yuu")
        .expect("Failed to compile comparison types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run comparison types test");
    
    assert_eq!(output, 1);
}

#[test]
fn test_nested_type_inference() {
    let source = r#"
        struct Container {
            value: i64,
        }
        
        fn create_container(val: i64) -> Container =>
            return Container { value: val } .
        
        fn main() -> i64 =>
            let container = create_container(42);
            return container.value .
    "#;
    
    let executable = run_to_executable(source, "test_nested_inference.yuu")
        .expect("Failed to compile nested inference test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run nested inference test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_mutability_type_checking() {
    let source = r#"fn main() -> i64 =>
        let mut x = 10;
        let y = 20;
        x = x + y;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_mutability_types.yuu")
        .expect("Failed to compile mutability types test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mutability types test");
    
    assert_eq!(output, 30);
}