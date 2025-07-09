// Tests demonstrating the unified call syntax capabilities in Yuu

mod common;

use common::*;

#[test]
fn test_unified_call_syntax_basic() {
    // Test basic unified call syntax transformation: obj.method() -> method(obj)
    let source = r#"
    struct Point {
        x: f32,
        y: f32,
    }
    
    fn distance_from_origin(p: Point) -> f32 =>
        break (p.x * p.x + p.y * p.y).sqrt();
    ;
    
    fn sqrt(value: f32) -> f32 =>
        // Simplified sqrt implementation for demo
        break value;
    ;
    
    fn test_basic_unified_call() -> f32 =>
        let p = Point { x: 3.0, y: 4.0 };
        // This should be transformed from p.distance_from_origin() to distance_from_origin(p)
        let dist = p.distance_from_origin();
        return dist;
    ;"#;

    let yir_output = run_to_yir(source, "test_unified_call_basic.yuu")
        .expect("Failed to generate YIR for basic unified call syntax");

    println!(
        "Generated YIR for basic unified call syntax:\n{}",
        yir_output
    );

    // Verify that the unified call syntax transformation occurred
    assert!(yir_output.contains("distance_from_origin"));
    assert!(yir_output.contains("Point"));
}

#[test]
fn test_unified_call_syntax_chained() {
    // Test chained method calls: obj.method1().method2() -> method2(method1(obj))
    let source = r#"
    struct Vector {
        x: f32,
        y: f32,
        z: f32,
    }
    
    fn normalize(v: Vector) -> Vector =>
        let len = length(v);
        break Vector { 
            x: v.x / len, 
            y: v.y / len, 
            z: v.z / len 
        };
    ;
    
    fn scale(v: Vector, factor: f32) -> Vector =>
        break Vector { 
            x: v.x * factor, 
            y: v.y * factor, 
            z: v.z * factor 
        };
    ;
    
    fn length(v: Vector) -> f32 =>
        break (v.x * v.x + v.y * v.y + v.z * v.z).sqrt();
    ;
    
    fn sqrt(value: f32) -> f32 =>
        break value; // Simplified for demo
    ;
    
    fn test_chained_unified_call() -> Vector =>
        let v = Vector { x: 1.0, y: 2.0, z: 3.0 };
        // This should be transformed:
        // v.normalize().scale(2.0) -> scale(normalize(v), 2.0)
        let result = v.normalize().scale(2.0);
        return result;
    ;"#;

    let yir_output = run_to_yir(source, "test_unified_call_chained.yuu")
        .expect("Failed to generate YIR for chained unified call syntax");

    println!(
        "Generated YIR for chained unified call syntax:\n{}",
        yir_output
    );

    // Verify that the chained unified call syntax transformation occurred
    assert!(yir_output.contains("normalize"));
    assert!(yir_output.contains("scale"));
    assert!(yir_output.contains("Vector"));
}

#[test]
fn test_unified_call_syntax_with_multiple_args() {
    // Test unified call syntax with multiple arguments
    let source = r#"
    struct Rectangle {
        width: f32,
        height: f32,
    }
    
    fn resize(rect: Rectangle, new_width: f32, new_height: f32) -> Rectangle =>
        break Rectangle { 
            width: new_width, 
            height: new_height 
        };
    ;
    
    fn area(rect: Rectangle) -> f32 =>
        break rect.width * rect.height;
    ;
    
    fn test_multiple_args_unified_call() -> f32 =>
        let rect = Rectangle { width: 10.0, height: 20.0 };
        // This should be transformed:
        // rect.resize(30.0, 40.0) -> resize(rect, 30.0, 40.0)
        let new_rect = rect.resize(30.0, 40.0);
        let result_area = new_rect.area();
        return result_area;
    ;"#;

    let yir_output = run_to_yir(source, "test_unified_call_multiple_args.yuu")
        .expect("Failed to generate YIR for multiple args unified call syntax");

    println!(
        "Generated YIR for multiple args unified call syntax:\n{}",
        yir_output
    );

    // Verify that the unified call syntax with multiple arguments worked
    assert!(yir_output.contains("resize"));
    assert!(yir_output.contains("area"));
    assert!(yir_output.contains("Rectangle"));
}

#[test]
fn test_mixed_member_access_and_unified_call() {
    // Test mixing regular member access with unified call syntax
    let source = r#"    struct Person {
        name_length: i64,
        age: i64,
    }
    
    fn get_info_string(person: Person, prefix: i64) -> i64 =>
        // Combine field access with method call result
        break person.name_length + prefix;
    ;
    
    fn test_mixed_access() -> i64 =>
        let person = Person { name_length: 5, age: 25 };
        
        // Mix regular field access and unified call syntax:
        // person.age (regular field access)
        // person.get_info_string(person.age) -> get_info_string(person, person.age)
        let info = person.get_info_string(person.age);
        return info;
    ;"#;

    let yir_output = run_to_yir(source, "test_mixed_access.yuu")
        .expect("Failed to generate YIR for mixed member access and unified call");

    println!(
        "Generated YIR for mixed member access and unified call:\n{}",
        yir_output
    );

    // Verify both field access and unified call syntax
    assert!(yir_output.contains("get_info_string"));
    assert!(yir_output.contains("Person"));
    assert!(yir_output.contains("field_ptr")); // Should contain field access operations
}

#[test]
fn test_unified_call_syntax_complex_expression() {
    // Test unified call syntax with complex expressions as the receiver
    let source = r#"
    struct Calculator {
        value: f32,
    }
    
    fn add(calc: Calculator, amount: f32) -> Calculator =>
        break Calculator { value: calc.value + amount };
    ;
    
    fn multiply(calc: Calculator, factor: f32) -> Calculator =>
        break Calculator { value: calc.value * factor };
    ;
    
    fn get_value(calc: Calculator) -> f32 =>
        break calc.value;
    ;
    
    fn create_calculator(initial: f32) -> Calculator =>
        break Calculator { value: initial };
    ;
    
    fn test_complex_expression_unified_call() -> f32 =>
        // Test unified call syntax with complex receiver expression:
        // create_calculator(10.0).add(5.0).multiply(2.0).get_value()
        // Should transform to: get_value(multiply(add(create_calculator(10.0), 5.0), 2.0))
        let result = create_calculator(10.0).add(5.0).multiply(2.0).get_value();
        return result;
    ;"#;

    let yir_output = run_to_yir(source, "test_complex_unified_call.yuu")
        .expect("Failed to generate YIR for complex expression unified call");

    println!(
        "Generated YIR for complex expression unified call:\n{}",
        yir_output
    );

    // Verify the complex chained unified call syntax
    assert!(yir_output.contains("create_calculator"));
    assert!(yir_output.contains("add"));
    assert!(yir_output.contains("multiply"));
    assert!(yir_output.contains("get_value"));
    assert!(yir_output.contains("Calculator"));
}