// Integration tests for source-to-C compilation pipeline

mod common;

use common::*;
use yuu::pass_c_lowering::pass_yir_to_c::{CSourceCode, PassYirToC};
use yuu::pass_parse::{ast::SourceInfo, pass_parse_impl::PassParse};
use yuu::pass_type_inference::PassTypeInference;
use yuu::pass_yir_lowering::PassAstToYir;
use yuu::utils::scheduler::{Schedule, Scheduler};
use yuu::utils::context::Context;
use std::sync::Arc;
use yuu::Pass;

fn create_c_lowering_schedule() -> Schedule {
    let mut schedule = Schedule::new();
    PassParse.install(&mut schedule);
    PassTypeInference.install(&mut schedule);
    PassAstToYir.install(&mut schedule);
    PassYirToC.install(&mut schedule);
    schedule
}

fn run_schedule_and_get_c_code(context: Context, schedule: Schedule) -> anyhow::Result<String> {
    let scheduler = Scheduler::new();
    let result_context = scheduler.run(schedule, context)?;
    let c_code = result_context.get_resource::<CSourceCode>(&PassYirToC);
    let c_code = c_code.lock().unwrap();
    Ok(c_code.0.clone())
}

#[test]
fn test_simple_function_to_c() {
    // Create a simple function that adds two numbers
    let source = r#"fn add(a: i64, b: i64) -> i64 {
        return a + b;
    }"#;

    let context = create_context_with_source(source, "test.yuu");
    let schedule = create_c_lowering_schedule();

    let c_code = run_schedule_and_get_c_code(context, schedule)
        .expect("Failed to generate C code");

    println!("Generated C code:\n\n```C\n{}\n```", c_code);

    // Basic sanity checks
    assert!(c_code.contains("#include <stdint.h>"));
    assert!(c_code.contains("fn_add"));
    assert!(c_code.contains("int64_t"));
    assert!(c_code.contains("INT64_C"));
}

#[test]
fn test_boolean_constants() {
    let source = r#"fn test_bool() -> bool {
        return true;
    }"#;

    let context = create_context_with_source(source, "test_bool.yuu");
    let schedule = create_c_lowering_schedule();

    let c_code = run_schedule_and_get_c_code(context, schedule)
        .expect("Failed to generate C code for boolean test");

    println!("Generated C code for boolean test:\n{}", c_code);

    // Should use 1/0 instead of true/false
    assert!(c_code.contains("1") || c_code.contains("0"));
    assert!(!c_code.contains("true"));
    assert!(!c_code.contains("false"));
}

#[test]
fn test_struct_to_c() {
    let source = r#"struct Point {
        x: f32,
        y: i64,
        z: f32,
    }
    
    fn create_point(x: f32, y: i64, z: f32) -> Point {
        break Point { x:x, y:y, z:z };
    }"#;

    let context = create_context_with_source(source, "test_struct.yuu");
    let schedule = create_c_lowering_schedule();

    let c_code = run_schedule_and_get_c_code(context, schedule)
        .expect("Failed to generate C code for struct");

    println!("Generated C code for struct:\n{}", c_code);

    // Should contain struct definition and function
    assert!(c_code.contains("struct Point"));
    assert!(c_code.contains("float x;"));
    assert!(c_code.contains("int64_t y;"));
    assert!(c_code.contains("float z;"));
    assert!(c_code.contains("fn_create_point"));
}

#[test]
fn test_arithmetic_operations() {
    let source = r#"fn math_ops(a: i64, b: i64) -> i64 {
        let sum = a + b;
        let diff = a - b;
        let prod = sum * diff;
        return prod;
    }"#;

    let context = create_context_with_source(source, "test_math.yuu");
    let schedule = create_c_lowering_schedule();

    let c_code = run_schedule_and_get_c_code(context, schedule)
        .expect("Failed to generate C code for arithmetic");

    println!("Generated C code for arithmetic:\n{}", c_code);

    // Should contain arithmetic operations
    assert!(c_code.contains(" + "));
    assert!(c_code.contains(" - "));
    assert!(c_code.contains(" * "));
    assert!(c_code.contains("fn_math_ops"));
}