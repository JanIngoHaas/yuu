mod common;
use common::*;

#[test]
fn test_basic_heap_allocation() {
    let source = r#"
fn main() -> i64:
    let heap_ptr = @42;
    return heap_ptr.* .
    "#;

    let executable =
        run_to_executable(source, "test_basic_heap_allocation.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 42);
}

#[test]
fn test_heap_allocation_with_expressions() {
    let source = r#"
fn main() -> i64:
    let base = 10;
    let heap_ptr = @(base * 4 + 2);
    let out = heap_ptr.*;
    ~heap_ptr;
    return out .
    "#;

    let executable = run_to_executable(source, "test_heap_allocation_with_expressions.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    // Expected: 10 * 4 + 2 = 42
    assert_eq!(result, 42);
}

#[test]
fn test_heap_allocation_struct() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

fn main() -> i64:
    let heap_point = @Point { x: 15, y: 25 };
    let result = heap_point.*.x + heap_point.*.y;
    ~heap_point;
    return result .
    "#;

    let executable =
        run_to_executable(source, "test_heap_allocation_struct.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    // Expected: 15 + 25 = 40
    assert_eq!(result, 40);
}

#[test]
fn test_heap_allocation_modification() {
    let source = r#"
fn main() -> i64:
    let heap_ptr = @100;
    heap_ptr.* = heap_ptr.* + 50;
    heap_ptr.* = heap_ptr.* / 3;
    let result = heap_ptr.*;
    ~heap_ptr;
    return result .
    "#;

    let executable = run_to_executable(source, "test_heap_allocation_modification.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    // Expected: (100 + 50) / 3 = 150 / 3 = 50
    assert_eq!(result, 50);
}

#[test]
fn test_multiple_heap_allocations() {
    let source = r#"
fn main() -> i64:
    let ptr1 = @10;
    let ptr2 = @20;
    let ptr3 = @30;

    let result = ptr1.* + ptr2.* + ptr3.*;
    ~ptr1;
    ~ptr2;
    ~ptr3;
    return result .
    "#;

    let executable =
        run_to_executable(source, "test_multiple_heap_allocations.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    // Expected: 10 + 20 + 30 = 60
    assert_eq!(result, 60);
}

#[test]
fn test_heap_allocation_with_enum() {
    let source = r#"
enum Status:
    Ready,
    Processing: i64,
end

fn main() -> i64:
    let heap_status = @Status::Processing(42);
    let result = 0;
    match heap_status.*:
        Status::Ready: result = 0 .
        Status::Processing(value): result = value .
    end
    ~heap_status;
    return result .
    "#;

    let executable =
        run_to_executable(source, "test_heap_allocation_with_enum.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 42);
}

#[test]
fn test_heap_allocation_in_function() {
    let source = r#"
fn allocate_and_return(value: i64) -> *i64:
    return @value .

fn main() -> i64:
    let ptr = allocate_and_return(99);
    let result = ptr.*;
    ~ptr;
    return result .
    "#;

    let executable = run_to_executable(source, "test_heap_allocation_in_function.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 99);
}

#[test]
fn test_nested_heap_allocations() {
    let source = r#"
fn main() -> i64:
    let ptr_to_ptr = @@42;  // Heap allocate a heap-allocated value
    let inner_ptr = ptr_to_ptr.*;
    let result = inner_ptr.*;
    ~inner_ptr;
    ~ptr_to_ptr;
    return result .
    "#;

    let executable =
        run_to_executable(source, "test_nested_heap_allocations.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 42);
}

#[test]
fn test_heap_free() {
    let source = r#"
fn main() -> i64:
    let heap_ptr = @42;
    let result = heap_ptr.*;
    ~heap_ptr;
    return result .
    "#;

    let executable = run_to_executable(source, "test_heap_free.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 42);
}

#[test]
fn test_heap_free_struct() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

fn main() -> i64:
    let p = @Point { x: 10, y: 20 };
    let val = p.*.x + p.*.y;
    ~p;
    return val .
    "#;

    let executable = run_to_executable(source, "test_heap_free_struct.yuu").expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 30);
}
