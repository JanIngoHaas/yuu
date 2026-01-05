mod common;
use common::*;

#[test]
fn test_basic_pointer_arithmetic() {
    let source = r#"
fn main() -> i64:
    let x = 12;
    let y = 4;
    let ptr_x = &x;
    let ptr_y = &y;

    // Mixed arithmetic operations through pointers
    let result1 = *ptr_x * *ptr_y / 2 + *ptr_x % *ptr_y;
    let result2 = (*ptr_x + *ptr_y) * (*ptr_x - *ptr_y) / *ptr_y;

    return result1 + result2 .
    "#;

    let executable =
        run_to_executable(source, "test_basic_pointer_arithmetic.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: (12 * 4 / 2 + 12 % 4) + ((12 + 4) * (12 - 4) / 4) = (24 + 0) + (16 * 8 / 4) = 24 + 32 = 56
    assert_eq!(result, 56);
}

#[test]
fn test_multi_level_pointer_arithmetic() {
    let source = r#"
fn main() -> i64:
    let base = 8;
    let ptr1 = &base;
    let ptr2 = &ptr1;
    let ptr3 = &ptr2;

    // Complex nested arithmetic
    return ***ptr3 * **ptr2 / *ptr1 + ***ptr3 % 5 .
    "#;

    let executable = run_to_executable(source, "test_multi_level_pointer_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 8 * 8 / 8 + 8 % 5 = 8 + 3 = 11
    assert_eq!(result, 11);
}

#[test]
fn test_pointer_assignment_with_arithmetic() {
    let source = r#"
fn main() -> i64:
    let mut accumulator = 100;
    let acc_ptr = &accumulator;

    *acc_ptr = *acc_ptr * 2 / 3 + 15;
    *acc_ptr = *acc_ptr - *acc_ptr % 7;

    return accumulator .
    "#;

    let executable = run_to_executable(source, "test_pointer_assignment_with_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 100 * 2 / 3 + 15 = 66 + 15 = 81, then 81 - 81 % 7 = 81 - 4 = 77
    assert_eq!(result, 77);
}

#[test]
fn test_function_with_pointer_arithmetic() {
    let source = r#"
fn calculate(a: i64, b: i64) -> i64:
    return a * b + a / b - a % b .

fn main() -> i64:
    let a = 15;
    let b = 3;
    let ptr_a = &a;
    let ptr_b = &b;

    return calculate(*ptr_a * 2, *ptr_b + 1) .
    "#;

    let executable = run_to_executable(source, "test_function_with_pointer_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: calculate(15 * 2, 3 + 1) = calculate(30, 4) = 30 * 4 + 30 / 4 - 30 % 4 = 120 + 7 - 2 = 125
    assert_eq!(result, 125);
}

// #[test]
// fn test_conditional_pointer_arithmetic() {
//     let source = r#"
// fn main() -> i64:
//     let flag: bool = true;
//     let option1: i64 = 20;
//     let option2: i64 = 30;
//     let multiplier: i64 = 3;

//     let selected_ptr = if flag: &option1 . else { &option2 }
//     return *selected_ptr * multiplier / 2 + *selected_ptr % multiplier .
//     "#;

//     let executable = run_to_executable(source, "test_conditional_pointer_arithmetic.yuu").expect("Failed to compile");
//     let result = run_executable_with_output(executable, &[]).expect("Failed to run");
//     // Expected: 20 * 3 / 2 + 20 % 3 = 30 + 2 = 32
//     assert_eq!(result, 32);
// }

#[test]
fn test_quadruple_pointer_arithmetic() {
    let source = r#"
fn main() -> i64:
    let mut deep_value = 6;
    let level1 = &deep_value;
    let level2 = &level1;
    let level3 = &level2;
    let level4 = &level3;

    // Deep arithmetic assignment
    ****level4 = ****level4 * ****level4 / 2 + ****level4 % 3;

    return deep_value .
    "#;

    let executable = run_to_executable(source, "test_quadruple_pointer_arithmetic.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 6 * 6 / 2 + 6 % 3 = 18 + 0 = 18
    assert_eq!(result, 18);
}

#[test]
fn test_pointer_arithmetic_swap() {
    let source = r#"
fn main() -> i64:
    let mut val1 = 45;
    let mut val2 = 18;
    let p1 = &val1;
    let p2 = &val2;

    // Arithmetic swap
    *p1 = *p1 + *p2;
    *p2 = *p1 - *p2;
    *p1 = *p1 - *p2;

    return val1 + val2 .
    "#;

    let executable =
        run_to_executable(source, "test_pointer_arithmetic_swap.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: after swap val1=18, val2=45, so 18 + 45 = 63
    assert_eq!(result, 63);
}

#[test]
fn test_complex_pointer_expression() {
    let source = r#"
fn main() -> i64:
    let x = 10;
    let y = 5;
    let z = 2;

    let px = &x;
    let py = &y;
    let pz = &z;

    let ppy = &py;

    // Complex expression mixing single and double pointers
    return (*px * **ppy / *pz + *px % **ppy) * (*pz + 1) .
    "#;

    let executable = run_to_executable(source, "test_complex_pointer_expression.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: (10 * 5 / 2 + 10 % 5) * (2 + 1) = (25 + 0) * 3 = 75
    assert_eq!(result, 75);
}

#[test]
fn test_pointer_function_modification() {
    let source = r#"
fn modify_through_pointer(ptr: *i64, factor: i64):
    *ptr = *ptr * factor / 2 + factor % 3 .

fn main() -> i64:
    let mut value = 20;
    let ptr = &value;

    modify_through_pointer(ptr, 7);

    return value .
    "#;

    let executable = run_to_executable(source, "test_pointer_function_modification.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 20 * 7 / 2 + 7 % 3 = 70 + 1 = 71
    assert_eq!(result, 71);
}

#[test]
fn test_enum_containing_pointers() {
    let source = r#"
enum Container:
    IntPtr: *i64,
    Value: i64,
end

fn process_container(c: Container) -> i64:
    match c:
        Container::IntPtr(ptr): return *ptr * 2 .
        Container::Value(val): return val + 10 .
    end
end

fn main() -> i64:
    let mut x = 15;
    let ptr = &x;

    let container1 = Container::IntPtr(ptr);
    let container2 = Container::Value(25);

    let result1 = process_container(container1);
    let result2 = process_container(container2);

    return result1 + result2 .
    "#;

    let executable =
        run_to_executable(source, "test_enum_containing_pointers.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: (15 * 2) + (25 + 10) = 30 + 35 = 65
    assert_eq!(result, 65);
}

#[test]
fn test_struct_containing_pointers() {
    let source = r#"
struct PointerPair:
    ptr1: *i64,
    ptr2: *i64,
    offset: i64,
end

fn compute_through_pointers(pp: PointerPair) -> i64:
    return *pp.ptr1 + *pp.ptr2 + pp.offset .

fn main() -> i64:
    let mut a = 10;
    let mut b = 20;

    let pair = PointerPair {
        ptr1: &a,
        ptr2: &b,
        offset: 5
    };

    return compute_through_pointers(pair) .
    "#;

    let executable = run_to_executable(source, "test_struct_containing_pointers.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 10 + 20 + 5 = 35
    assert_eq!(result, 35);
}

#[test]
fn test_pointer_to_enum() {
    let source = r#"
enum Status:
    Ready,
    Processing: i64,
    Done: i64,
end

fn get_status_value(status_ptr: *Status) -> i64:
    match *status_ptr:
        Status::Ready: return 0 .
        Status::Processing(progress): return progress .
        Status::Done(result): return result + 100 .
    end
end

fn main() -> i64:
    let mut status1 = Status::Ready;
    let mut status2 = Status::Processing(50);
    let mut status3 = Status::Done(42);

    let ptr1 = &status1;
    let ptr2 = &status2;
    let ptr3 = &status3;

    let r1 = get_status_value(ptr1);
    let r2 = get_status_value(ptr2);
    let r3 = get_status_value(ptr3);

    return r1 + r2 + r3 .
    "#;

    let executable =
        run_to_executable(source, "test_pointer_to_enum.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 0 + 50 + (42 + 100) = 0 + 50 + 142 = 192
    assert_eq!(result, 192);
}

#[test]
fn test_pointer_to_struct() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

fn distance_squared(p_ptr: *Point) -> i64:
    return p_ptr.x * p_ptr.x + p_ptr.y * p_ptr.y .

fn main() -> i64:
    let mut point = Point { x: 3, y: 4 };
    let ptr = &point;

    return distance_squared(ptr) .
    "#;

    let executable =
        run_to_executable(source, "test_pointer_to_struct.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 3*3 + 4*4 = 9 + 16 = 25
    assert_eq!(result, 25);
}

#[test]
fn test_struct_enum_with_pointers() {
    let source = r#"
struct Data:
    value: i64,
    multiplier: i64,
end

enum DataContainer:
    Empty,
    Reference: *Data,
    Copy: Data,
end

fn process_data_container(dc: DataContainer) -> i64:
    match dc:
        DataContainer::Empty: return 0 .
        DataContainer::Reference(data_ptr):
            return data_ptr.value * data_ptr.multiplier .
        DataContainer::Copy(data):
            return data.value + data.multiplier .
    end
end

fn main() -> i64:
    let mut data = Data { value: 6, multiplier: 7 };
    let data_ptr = &data;

    let container1 = DataContainer::Empty;
    let container2 = DataContainer::Reference(data_ptr);
    let container3 = DataContainer::Copy(Data { value: 10, multiplier: 3 });

    let r1 = process_data_container(container1);
    let r2 = process_data_container(container2);
    let r3 = process_data_container(container3);

    return r1 + r2 + r3 .
    "#;

    let executable =
        run_to_executable(source, "test_struct_enum_with_pointers.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 0 + (6 * 7) + (10 + 3) = 0 + 42 + 13 = 55
    assert_eq!(result, 55);
}

#[test]
fn test_complex_nested_pointer_structures() {
    let source = r#"
struct Node:
    value: i64,
    next: *Node,
end

enum ListOperation:
    Traverse: *Node,
    Sum: i64,
end

fn process_list_operation(op: ListOperation) -> i64:
    match op:
        ListOperation::Sum(total): return total .
        ListOperation::Traverse(node_ptr):
            return node_ptr.value + node_ptr.next.value .
    end
end

fn main() -> i64:
    let mut node2 = Node { value: 20, next: 0 as *Node };
    let mut node1 = Node { value: 10, next: &node2 };

    let op1 = ListOperation::Sum(100);
    let op2 = ListOperation::Traverse(&node1);

    let r1 = process_list_operation(op1);
    let r2 = process_list_operation(op2);

    return r1 + r2 .
    "#;

    let executable = run_to_executable(source, "test_complex_nested_pointer_structures.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 100 + (10 + 20) = 100 + 30 = 130
    assert_eq!(result, 130);
}

#[test]
fn test_pointer_arithmetic_with_structs() {
    let source = r#"
struct Calculator:
    accumulator: i64,
    multiplier: i64,
end

fn operate_through_pointer(calc_ptr: *Calculator, operand: i64):
    calc_ptr.accumulator = calc_ptr.accumulator * calc_ptr.multiplier + operand .

fn main() -> i64:
    let mut calc = Calculator { accumulator: 5, multiplier: 3 };
    let ptr = &calc;

    operate_through_pointer(ptr, 7);

    return calc.accumulator .
    "#;

    let executable = run_to_executable(source, "test_pointer_arithmetic_with_structs.yuu")
        .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 5 * 3 + 7 = 15 + 7 = 22
    assert_eq!(result, 22);
}

#[test]
fn test_enum_variants_with_different_pointer_types() {
    let source = r#"
struct Point:
    x: i64,
    y: i64,
end

enum PointerVariants:
    IntPtr: *i64,
    StructPtr: *Point,
    Nothing,
end

fn extract_value(pv: PointerVariants) -> i64:
    match pv:
        PointerVariants::IntPtr(int_ptr): return *int_ptr .
        PointerVariants::StructPtr(point_ptr): return point_ptr.x + point_ptr.y .
        PointerVariants::Nothing: return -1 .
    end
end

fn main() -> i64:
    let mut num = 42;
    let mut point = Point { x: 10, y: 15 };

    let variant1 = PointerVariants::IntPtr(&num);
    let variant2 = PointerVariants::StructPtr(&point);
    let variant3 = PointerVariants::Nothing;

    let r1 = extract_value(variant1);
    let r2 = extract_value(variant2);
    let r3 = extract_value(variant3);

    return r1 + r2 + r3 .
    "#;

    let executable = run_to_executable(
        source,
        "test_enum_variants_with_different_pointer_types.yuu",
    )
    .expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    // Expected: 42 + (10 + 15) + (-1) = 42 + 25 - 1 = 66
    assert_eq!(result, 66);
}

#[test]
fn test_null_pointer_basics() {
    let source = r#"
fn check_pointer_validity(ptr: *i64) -> i64:
    return 42 .

fn main() -> i64:
    let null_ptr = 0 as *i64;
    return check_pointer_validity(null_ptr) .
    "#;

    let executable =
        run_to_executable(source, "test_null_pointer_basics.yuu").expect("Failed to compile");
    let result = run_executable_with_output(executable, &[]).expect("Failed to run");
    assert_eq!(result, 42);
}
