// Integration tests for YIR printing functionality

mod common;

use common::*;

#[test]
fn test_while_loop_yir() {
    // Test a while loop with a labeled break
    let source = r#"fn test_while() -> i64 =>
        let mut i = 0;
        let result = :outer_block =>
            while i < 10 =>
                if i == 5 =>
                    break :outer_block 100;.
                i = i + 1;.
            break 999;.
        ;
        return result;."#;

    let yir_output = run_to_yir(source, "test_while.yuu")
        .expect("Failed to generate YIR for while loop");

    println!("Generated YIR for while loop:\n{}", yir_output);

    // Basic verification that YIR was generated
    assert!(yir_output.contains("test_while"));
    assert!(yir_output.contains("while"));
}

#[test]
fn test_struct_yir() {
    // Define a struct (three members, f32, i64, f32). Define function which takes a such a struct and returns a struct.
    let source = r#"struct Point {
        x: f32,
        y: i64,
        z: f32,
    }
    fn test() -> Point =>
        let p = create_point(1.0, 2, 3.0);
        return p;
    ;                
    fn create_point(x: f32, y: i64, z: f32) -> Point =>
        break Point { x:x, y:y, z:z };
    ;"#;

    let yir_output = run_to_yir(source, "test_struct.yuu")
        .expect("Failed to generate YIR for struct");

    println!("Generated YIR for struct:\n{}", yir_output);

    // Basic verification that YIR was generated
    assert!(yir_output.contains("Point"));
    assert!(yir_output.contains("create_point"));
}

#[test]
fn test_factorial_yir() {
    // Create the factorial function code
    let source = r#"fn fac(n: i64) -> i64 =>
        break if n == 0 =>
            break 1;
        ;
        else =>
            let n_out = n * fac(n - 1);
            return n_out;
        ;
    ;"#;

    let yir_output = run_to_yir(source, "test_factorial.yuu")
        .expect("Failed to generate YIR for factorial");

    println!("Generated YIR for factorial:\n{}", yir_output);

    // Basic verification that YIR was generated
    assert!(yir_output.contains("fac"));
    assert!(yir_output.contains("call"));
    assert!(yir_output.contains("branch"));
}

#[test]
fn test_member_access_yir() {
    // Test member access on a struct
    let source = r#"struct Point {
        x: f32,
        y: f32,
        z: f32,
    }
    
    fn test_member_access(p: Point) -> f32 =>
        let x_val = p.x;
        let y_val = p.y;
        return x_val + y_val;
    ;"#;

    let yir_output = run_to_yir(source, "test_member_access.yuu")
        .expect("Failed to generate YIR for member access");

    println!("Generated YIR for member access:\n{}", yir_output); 
    
    // Basic verification that YIR was generated correctly
    assert!(yir_output.contains("test_member_access"));
    assert!(yir_output.contains("Point"));
    assert!(yir_output.contains("field_ptr")); // Should contain field pointer operations
    assert!(yir_output.contains("x")); // Field name should appear
    assert!(yir_output.contains("y")); // Field name should appear
}