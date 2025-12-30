mod common;

use common::{run_executable_with_output, run_to_executable};

#[test]
fn test_pointer_arithmetic_with_arrays() {
    let source = r#"
fn main() -> i64:
    // Create an array of integers
    let arr = [10, 20, 30, 40, 50];

    // Get pointer to start of array
    let base_ptr = arr;

    // Test basic pointer arithmetic: ptr + offset
    let ptr1 = base_ptr + 0;  // points to arr[0] = 10
    let ptr2 = base_ptr + 1;  // points to arr[1] = 20
    let ptr3 = base_ptr + 2;  // points to arr[2] = 30
    let ptr4 = base_ptr + 3;  // points to arr[3] = 40

    // Dereference and sum the values
    let sum = ptr1.* + ptr2.* + ptr3.* + ptr4.*;
    return sum .  // Should be 10 + 20 + 30 + 40 = 100
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_with_arrays.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 100);
}

#[test]
fn test_pointer_arithmetic_negative_offsets() {
    let source = r#"
fn main() -> i64:
    // Create array and get pointer to middle element
    let arr = [1, 2, 3, 4, 5];
    let middle_ptr = arr + 2;  // points to arr[2] = 3

    // Test negative offsets (going backwards)
    let back1 = middle_ptr -1;  // points to arr[1] = 2
    let back2 = middle_ptr + -2;  // points to arr[0] = 1

    // Test forward offsets from middle
    let forward1 = middle_ptr + 1;  // points to arr[3] = 4
    let forward2 = middle_ptr + 2;  // points to arr[4] = 5

    // Sum all accessed values
    let sum = back2.* + back1.* + middle_ptr.* + forward1.* + forward2.*;
    return sum .  // Should be 1 + 2 + 3 + 4 + 5 = 15
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_negative_offsets.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 15);
}

#[test]
fn test_pointer_arithmetic_with_struct_arrays() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

fn main() -> i64:
    // Create array of Point structs
    let points = [Point { x: 1, y: 2 }; 3];

    // Get pointers to different elements
    let p0 = points + 0;
    let p1 = points + 1;
    let p2 = points + 2;

    // Access fields through pointers
    let sum_x = p0.*.x + p1.*.x + p2.*.x;  // Should be 1 + 1 + 1 = 3
    let sum_y = p0.*.y + p1.*.y + p2.*.y;  // Should be 2 + 2 + 2 = 6

    return sum_x + sum_y .  // Should be 3 + 6 = 9
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_with_struct_arrays.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 9);
}

#[test]
fn test_pointer_arithmetic_chaining() {
    let source = r#"
fn main() -> i64:
    // Create a larger array
    let arr = [100, 200, 300, 400, 500, 600, 700];

    // Chain multiple pointer arithmetic operations
    let base = arr;
    let ptr1 = base + 2;      // points to arr[2] = 300
    let ptr2 = ptr1 + 3;      // points to arr[5] = 600
    let ptr3 = ptr2 -1;   // points to arr[4] = 500

    // Alternative: single offset calculation
    let direct_ptr = base + 4;  // should also point to arr[4] = 500

    // Both should access the same value
    let val1 = ptr3.*;
    let val2 = direct_ptr.*;

    return val1 + val2 .  // Should be 500 + 500 = 1000
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_chaining.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 1000);
}

#[test]
fn test_pointer_arithmetic_with_casting() {
    let source = r#"
fn main() -> i64:
    // Test combining casting and pointer arithmetic
    let arr = [42, 84, 126];

    // Cast array start to pointer and do arithmetic
    let base = 1000 as *i64;  // Some base address
    let arr_ptr = arr + 0;    // Get actual array pointer

    // Use pointer arithmetic to access array elements
    let first = arr_ptr + 0;
    let second = arr_ptr + 1;
    let third = arr_ptr + 2;

    let sum = first.* + second.* + third.*;
    return sum .  // Should be 42 + 84 + 126 = 252
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_with_casting.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 252);
}

#[test]
fn test_dynamic_pointer_arithmetic() {
    let source = r#"
fn access_element(arr: *i64, index: i64) -> i64:
    let ptr = arr + index;
    return ptr.* .

fn main() -> i64:
    let numbers = [5, 10, 15, 20, 25];

    // Access elements using dynamic offsets
    let sum = access_element(numbers, 0) +
              access_element(numbers, 1) +
              access_element(numbers, 2) +
              access_element(numbers, 3) +
              access_element(numbers, 4);

    return sum .  // Should be 5 + 10 + 15 + 20 + 25 = 75
    "#;

    let executable = run_to_executable(source, "test_dynamic_pointer_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 75);
}

#[test]
fn test_heap_allocated_array_pointer_arithmetic() {
    let source = r#"
fn main() -> i64:
    // Create heap-allocated array
    let heap_arr = new [100, 200, 300, 400, 500];

    // Use pointer arithmetic on heap-allocated array
    let ptr0 = heap_arr + 0;   // points to heap_arr[0] = 100
    let ptr1 = heap_arr + 1;   // points to heap_arr[1] = 200
    let ptr2 = heap_arr + 2;   // points to heap_arr[2] = 300
    let ptr4 = heap_arr + 4;   // points to heap_arr[4] = 500

    // Sum values accessed through pointer arithmetic
    let sum = ptr0.* + ptr1.* + ptr2.* + ptr4.*;

    // Free the heap memory
    ~heap_arr;

    return sum .  // Should be 100 + 200 + 300 + 500 = 1100
    "#;

    let executable = run_to_executable(source, "test_heap_allocated_array_pointer_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 1100);
}

#[test]
fn test_heap_allocated_struct_array_pointer_arithmetic() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

fn main() -> i64:
    // Create heap-allocated array of structs
    let heap_points = new [Point { x: 10, y: 20 }; 4];

    // Use pointer arithmetic to access different elements
    let p0 = heap_points + 0;
    let p1 = heap_points + 1;
    let p2 = heap_points + 2;
    let p3 = heap_points + 3;

    // Access struct fields through pointers
    let total_x = p0.*.x + p1.*.x + p2.*.x + p3.*.x;  // 10 * 4 = 40
    let total_y = p0.*.y + p1.*.y + p2.*.y + p3.*.y;  // 20 * 4 = 80

    // Free the heap memory
    ~heap_points;

    return total_x + total_y .  // Should be 40 + 80 = 120
    "#;

    let executable = run_to_executable(
        source,
        "test_heap_allocated_struct_array_pointer_arithmetic.yuu",
    )
    .expect("Failed to compile");
    let result = run_executable_with_output(&executable, &[]).expect("Failed to run");
    assert_eq!(result, 120);
}
