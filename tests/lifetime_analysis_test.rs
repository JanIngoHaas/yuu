mod common;
use common::*;
use yuu::pass_lifetime_analysis::LifetimeAnalysis;
use yuu::utils::pipeline::Pipeline;

#[test]
fn test_01_simple_heap_allocation() {
    let source = r#"
fn main() -> i64:
    let heap_ptr = @42;
    return heap_ptr.* .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_01.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");


    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Simple heap allocation should succeed");
}

#[test]
fn test_02_pointer_reassignment_chain() {
    let source = r#"
fn main() -> i64:
    let ptr1 = @100;
    let ptr2 = @200;
    let ptr3 = @300;

    let active = ptr1;
    active = ptr2;
    active = ptr3;

    return active.* .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_02.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");

    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Pointer reassignment chain should succeed");
}

#[test]
fn test_03_branching_with_different_allocations() {
    let source = r#"
fn main() -> i64:
    let result: *i64 = i64@0ptr;

    if 2>1:
        let temp1 = @42;
        let temp2 = @temp1.*;
        result = temp2; 
    end
    else:
        let temp3 = @24;
        result = temp3;
    end

    return result.*
end
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test.yir".to_string()).unwrap();

    let module = pipeline.get_module().unwrap();

    let mut m = String::new();
    module.format_yir(true, &mut m).unwrap();
    println!("{m}");

    LifetimeAnalysis.run(module);

}

#[test]
fn test_04_nested_heap_with_indirection() {
    let source = r#"
fn main() -> i64:
    let level1 = @100;
    let level2 = @level1;
    let level3 = @level2;

    let final_val = level3.*.*.* ;
    return final_val .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_04.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");

    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Nested heap with indirection should succeed");
}

#[test]
fn test_05_loop_with_heap_allocation() {
    let source = r#"
fn main() -> i64:
    let sum = 0;
    let i = 0;

    while i < 3:
        let heap_val = @(i * 10);
        sum = sum + heap_val.*;
        i = i + 1;
    end

    return sum .
    "#;

    let out = run_to_yir(source, "test.yir").unwrap();
    println!("{out}");
}

#[test]
fn test_06_complex_control_flow_with_pointer_merging() {
    let source = r#"
fn main() -> i64:
    let ptr: *i64 = i64@0ptr;
    let choice = 2;

    if choice == 1:
        let temp = @111;
        ptr = temp;
    end
    else if choice == 2:
        let temp = @222;
        ptr = temp;
    end
    else:
        let temp = @333;
        ptr = temp;
    end

    let result = ptr.*;
    return result .
    "#;

    let out = run_to_yir(source, "test.yir").unwrap();
    println!("{out}");

}

#[test]
fn test_07_nested_loops_with_heap_operations() {
    let source = r#"
fn main() -> i64:
    let total = 0;
    let outer = 0;
    let primary_accumulator: *i64 = i64@0ptr;
    let secondary_accumulator: *i64 = i64@0ptr;

    while outer < 3:
        let inner = 0;
        let branch_selector = outer % 2;
        let local_ptr: *i64 = i64@0ptr;

        if branch_selector == 0:
            let temp_val = @(outer * 100);
            primary_accumulator = temp_val;
            local_ptr = primary_accumulator;
        end
        else:
            let temp_val = @(outer * 50);
            secondary_accumulator = temp_val;
            local_ptr = secondary_accumulator;
        end

        while inner < 2:
            let heap_ptr = @(outer * 10 + inner);
            let intermediate = @heap_ptr.*;

            if inner == 0:
                let multiplier = @2;
                intermediate.* = intermediate.* * multiplier.*;
                local_ptr.* = local_ptr.* + intermediate.*;
            end
            else:
                let divisor = @1;
                let adjusted = @(intermediate.* + divisor.*);
                local_ptr.* = local_ptr.* + adjusted.*;
            end

            total = total + local_ptr.*;
            inner = inner + 1;
        end

        if outer == 1:
            let bridge_ptr = primary_accumulator;
            secondary_accumulator.* = secondary_accumulator.* + bridge_ptr.*;
        end

        outer = outer + 1;
    end

    let final_primary = primary_accumulator.*;
    let final_secondary = secondary_accumulator.*;

    return total + final_primary + final_secondary .
    "#;

    let out = run_to_yir(source, "test.yir").unwrap();
    println!("{out}");
}

#[test]
fn test_08_pointer_aliasing_and_modification() {
    let source = r#"
fn main() -> i64:
    let original = @50;
    let alias1 = original;
    let alias2 = alias1;

    alias2.* = alias2.* * 2;
    alias1.* = alias1.* + 10;
    original.* = original.* + 5;

    return original.* + alias1.* + alias2.* .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_08.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");

    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Pointer aliasing and modification should succeed");
}

#[test]
fn test_09_complex_branching_with_loop_and_heap() {
    let source = r#"
fn main() -> i64:
    let mode = 1;
    let result: *i64;

    if mode == 1:
        let accumulator = @0;
        let i = 0;
        while i < 3:
            let temp = @(i * 2);
            accumulator.* = accumulator.* + temp.*;
            i = i + 1;
        end
        result = accumulator;
    else:
        let simple = @999;
        result = simple;
    end

    let final_ptr = @result.*;
    return final_ptr.* .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_09.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");

    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Complex branching with loop and heap should succeed");
}

#[test]
fn test_10_extreme_pointer_web() {
    let source = r#"
fn main() -> i64:
    let a = @10;
    let b = @a;
    let c = @b;
    let d = @20;
    let e = @d;

    let web1 = c;
    let web2 = e;

    if true:
        let temp = @web1.*.*.* ;
        web2.* = temp;
    else:
        let temp = @web2.*.* ;
        web1.*.* = temp;
    end

    let final_a = web1.*.*.* ;
    let final_b = web2.*.* ;

    return final_a + final_b .
    "#;

    let mut pipeline = Pipeline::new(source.to_string(), "test_10.yuu".to_string())
        .expect("Failed to create pipeline");
    let module = pipeline.get_module().expect("Failed to get module");

    let lifetime_pass = LifetimeAnalysis::new();
    let result = lifetime_pass.run(module);
    //assert!(result.is_ok(), "Extreme pointer web should succeed");
}