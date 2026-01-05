// E2E tests for unified call syntax and method chaining

mod common;

use common::*;

#[test]
fn test_method_style_call() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        end

        fn distance(p: Point) -> i64:
            return p.x * p.x + p.y * p.y .
        
        fn main() -> i64:
            let p = Point { x: 3, y: 4 };
            return p.distance() .
    "#;

    let executable = run_to_executable(source, "test_method_style_call.yuu")
        .expect("Failed to compile method call test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run method call test");

    // 3*3 + 4*4 = 25
    assert_eq!(output, 25);
}

#[test]
fn test_method_with_additional_params() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        end

        fn translate(p: Point, dx: i64, dy: i64) -> Point:
            return Point { x: p.x + dx, y: p.y + dy } .
        
        fn main() -> i64:
            let p = Point { x: 5, y: 10 };
            let translated = p.translate(2, 3);
            return translated.x + translated.y .
    "#;

    let executable = run_to_executable(source, "test_method_with_additional_params.yuu")
        .expect("Failed to compile method with params test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run method with params test");

    // (5+2) + (10+3) = 7 + 13 = 20
    assert_eq!(output, 20);
}

#[test]
fn test_chained_method_calls() {
    let source = r#"
        struct Number:
            value: i64,
        end
        
        fn double(n: Number) -> Number:
            return Number { value: n.value * 2 } .
        
        fn add_ten(n: Number) -> Number:
            return Number { value: n.value + 10 } .
        
        fn main() -> i64:
            let n = Number { value: 5 };
            let result = n.double().add_ten();
            return result.value .
    "#;

    let executable = run_to_executable(source, "test_chained_method_calls.yuu")
        .expect("Failed to compile chained calls test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run chained calls test");

    // 5 * 2 = 10, 10 + 10 = 20
    assert_eq!(output, 20);
}

#[test]
fn test_mixed_call_styles() {
    let source = r#"
        struct Vector:
            x: i64,
            y: i64,
        end

        fn scale(v: Vector, factor: i64) -> Vector:
            return Vector { x: v.x * factor, y: v.y * factor } .
        
        fn dot_product(v1: Vector, v2: Vector) -> i64:
            return v1.x * v2.x + v1.y * v2.y .
        
        fn main() -> i64:
            let v1 = Vector { x: 2, y: 3 };
            let v2 = Vector { x: 4, y: 5 };
            return v1.scale(2).dot_product(v2) .
    "#;

    let executable = run_to_executable(source, "test_mixed_call_styles.yuu")
        .expect("Failed to compile mixed calls test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run mixed calls test");

    // v1 scaled: (4, 6), dot product: 4*4 + 6*5 = 16 + 30 = 46
    assert_eq!(output, 46);
}

#[test]
fn test_long_method_chain() {
    let source = r#"
        struct Calculator:
            value: i64,
        end
        
        fn add(c: Calculator, n: i64) -> Calculator:
            return Calculator { value: c.value + n } .
        
        fn multiply(c: Calculator, n: i64) -> Calculator:
            return Calculator { value: c.value * n } .
        
        fn subtract(c: Calculator, n: i64) -> Calculator:
            return Calculator { value: c.value - n } .
        
        fn main() -> i64:
            let calc = Calculator { value: 10 };
            let result = calc.add(5).multiply(2).subtract(3);
            return result.value .
    "#;

    let executable = run_to_executable(source, "test_long_method_chain.yuu")
        .expect("Failed to compile long chain test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run long chain test");

    // 10 + 5 = 15, 15 * 2 = 30, 30 - 3 = 27
    assert_eq!(output, 27);
}

#[test]
fn test_method_call_with_complex_expression() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        end
        
        fn distance_to(p1: Point, p2: Point) -> i64:
            let dx = p1.x - p2.x;
            let dy = p1.y - p2.y;
            return dx * dx + dy * dy .
        
        fn main() -> i64:
            let p1 = Point { x: 0, y: 0 };
            let p2 = Point { x: 3, y: 4 };
            return p1.distance_to(p2) .
    "#;

    let executable = run_to_executable(source, "test_method_call_with_complex_expression.yuu")
        .expect("Failed to compile complex method test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run complex method test");

    // dx=3, dy=4, distance = 3*3 + 4*4 = 25
    assert_eq!(output, 25);
}

#[test]
fn test_nested_method_calls() {
    let source = r#"
        struct Transform:
            scale: i64,
        end

        fn apply_to_point(t: Transform, x: i64, y: i64) -> i64:
            return (x + y) * t.scale .
        
        fn create_transform(scale: i64) -> Transform:
            return Transform { scale: scale } .
        
        fn main() -> i64:
            let result = create_transform(3).apply_to_point(4, 5);
            return result .
    "#;

    let executable = run_to_executable(source, "test_nested_method_calls.yuu")
        .expect("Failed to compile nested methods test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run nested methods test");

    // (4 + 5) * 3 = 27
    assert_eq!(output, 27);
}

#[test]
fn test_method_call_in_expression() {
    let source = r#"
        struct Number:
            value: i64,
        end

        fn get_value(n: Number) -> i64:
            return n.value .
        
        fn main() -> i64:
            let n1 = Number { value: 10 };
            let n2 = Number { value: 20 };
            let result = n1.get_value() + n2.get_value();
            return result .
    "#;

    let executable = run_to_executable(source, "test_method_call_in_expression.yuu")
        .expect("Failed to compile method in expression test");

    let output = run_executable_with_output(executable, &[])
        .expect("Failed to run method in expression test");

    // 10 + 20 = 30
    assert_eq!(output, 30);
}
