// E2E tests for struct definitions and usage

mod common;

use common::*;

#[test]
fn test_simple_struct_definition() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        .
        
        fn main() -> i64:
            let p = Point { x: 10, y: 20 };
            return p.x + p.y .
    "#;

    let executable = run_to_executable(source, "test_simple_struct_definition.yuu")
        .expect("Failed to compile struct test");

    let output = run_executable_with_output(&executable, &[]).expect("Failed to run struct test");

    // 10 + 20 = 30
    assert_eq!(output, 30);
}

#[test]
fn test_struct_with_different_types() {
    let source = r#"
        struct Mixed:
            count: i64,
            value: f32,
        .
        
        fn main() -> i64:
            let m = Mixed { count: 5, value: 3.14 };
            return m.count .
    "#;

    let executable = run_to_executable(source, "test_struct_with_different_types.yuu")
        .expect("Failed to compile mixed struct test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run mixed struct test");

    assert_eq!(output, 5);
}

#[test]
fn test_struct_field_access() {
    let source = r#"
        struct Rectangle:
            width: i64,
            height: i64,
        .
        
        fn main() -> i64:
            let rect = Rectangle { width: 10, height: 5 };
            let area = rect.width * rect.height;
            return area .
    "#;

    let executable = run_to_executable(source, "test_field_access.yuu")
        .expect("Failed to compile field access test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run field access test");

    // 10 * 5 = 50
    assert_eq!(output, 50);
}

#[test]
fn test_struct_as_function_parameter() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        .
        
        fn distance_from_origin(p: Point) -> i64:
            return p.x * p.x + p.y * p.y .
        
        fn main() -> i64:
            let p = Point { x: 3, y: 4 };
            return distance_from_origin(p) .
    "#;

    let executable = run_to_executable(source, "test_struct_as_function_parameter.yuu")
        .expect("Failed to compile struct param test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run struct param test");

    // 3*3 + 4*4 = 9 + 16 = 25
    assert_eq!(output, 25);
}

#[test]
fn test_struct_as_return_value() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        .
        
        fn create_point(x: i64, y: i64) -> Point:
            return Point { x: x, y: y } .
        
        fn main() -> i64:
            let p = create_point(7, 8);
            return p.x + p.y .
    "#;

    let executable = run_to_executable(source, "test_struct_as_return_value.yuu")
        .expect("Failed to compile struct return test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run struct return test");

    // 7 + 8 = 15
    assert_eq!(output, 15);
}

#[test]
fn test_nested_struct_fields() {
    let source = r#"
        struct Inner:
            value: i64,
        .
        
        struct Outer:
            inner: Inner,
            count: i64,
        .
        
        fn main() -> i64:
            let inner = Inner { value: 10 };
            let outer = Outer { inner: inner, count: 5 };
            return outer.inner.value + outer.count .
    "#;

    let executable = run_to_executable(source, "test_nested_struct_fields.yuu")
        .expect("Failed to compile nested struct test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run nested struct test");

    // 10 + 5 = 15
    assert_eq!(output, 15);
}

#[test]
fn test_struct_with_multiple_instances() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        .
        
        fn main() -> i64:
            let p1 = Point { x: 1, y: 2 };
            let p2 = Point { x: 3, y: 4 };
            return (p1.x + p1.y) + (p2.x + p2.y) .
    "#;

    let executable = run_to_executable(source, "test_multiple_instances.yuu")
        .expect("Failed to compile multiple instances test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run multiple instances test");

    // (1+2) + (3+4) = 3 + 7 = 10
    assert_eq!(output, 10);
}

#[test]
fn test_struct_field_assignment() {
    let source = r#"
        struct Counter:
            value: i64,
        .
        
        fn main() -> i64:
            let mut counter = Counter { value: 0 };
            counter.value = 42;
            return counter.value .
    "#;

    let executable = run_to_executable(source, "test_field_assignment.yuu")
        .expect("Failed to compile field assignment test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run field assignment test");

    assert_eq!(output, 42);
}

#[test]
fn test_struct_with_computation() {
    let source = r#"
        struct Circle:
            radius: f32,
        .
        
        fn area(c: Circle) -> f32:
            return 3.14 * c.radius * c.radius .
        
        fn main() -> i64:
            let circle = Circle { radius: 2.0 };
            let area_val = area(circle);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_struct_computation.yuu")
        .expect("Failed to compile struct computation test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run struct computation test");

    assert_eq!(output, 0);
}
