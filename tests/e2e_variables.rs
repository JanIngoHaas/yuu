// E2E tests for variable declarations and mutability

mod common;

use common::*;

#[test]
fn test_let_binding_with_type_inference() {
    let source = r#"fn main() -> i64:
        let x = 42;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_let_infer.yuu")
        .expect("Failed to compile let binding test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run let binding test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_let_binding_with_explicit_type() {
    let source = r#"fn main() -> i64:
        let x: i64 = 42;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_let_explicit.yuu")
        .expect("Failed to compile explicit type test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run explicit type test");
    
    assert_eq!(output, 42);
}

#[test]
fn test_mut_variable_assignment() {
    let source = r#"fn main() -> i64:
        let mut x = 10;
        x = 20;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_mut.yuu")
        .expect("Failed to compile mut assignment test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mut assignment test");
    
    assert_eq!(output, 20);
}

#[test]
fn test_multiple_variable_declarations() {
    let source = r#"fn main() -> i64:
        let a = 5;
        let b = 10;
        let c = a + b;
        return c .
    "#;
    
    let executable = run_to_executable(source, "test_multiple_vars.yuu")
        .expect("Failed to compile multiple vars test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run multiple vars test");
    
    assert_eq!(output, 15);
}

#[test]
fn test_variable_shadowing() {
    let source = r#"fn main() -> i64:
        let x = 10;
        let x = x + 5;
        return x .
    "#;
    
    let executable = run_to_executable(source, "test_shadowing.yuu")
        .expect("Failed to compile shadowing test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run shadowing test");
    
    assert_eq!(output, 15);
}

#[test]
fn test_mut_variable_operations() {
    let source = r#"fn main() -> i64:
        let mut counter = 0;
        counter = counter + 1;
        counter = counter * 2;
        return counter .
    "#;
    
    let executable = run_to_executable(source, "test_mut_ops.yuu")
        .expect("Failed to compile mut operations test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mut operations test");
    
    // 0 + 1 = 1, 1 * 2 = 2
    assert_eq!(output, 2);
}

#[test]
fn test_float_variables() {
    let source = r#"fn main() -> i64:
        let x = 3.14;
        let y = 2.5;
        let z = x + y;
        return 0 .
    "#;
    
    let executable = run_to_executable(source, "test_float_vars.yuu")
        .expect("Failed to compile float vars test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run float vars test");
    
    assert_eq!(output, 0);
}

#[test]
fn test_mixed_type_variables() {
    let source = r#"fn main() -> i64:
        let int_var = 42;
        let float_var = 3.14;
        return int_var .
    "#;
    
    let executable = run_to_executable(source, "test_mixed_vars.yuu")
        .expect("Failed to compile mixed type vars test");
    
    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run mixed type vars test");
    
    assert_eq!(output, 42);
}