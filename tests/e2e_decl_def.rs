// E2E tests for decl/def statements (deferred variable declarations and definitions)

mod common;

use common::*;

#[test]
fn test_basic_decl_def() {
    let source = r#"fn main() -> i64:
        dec x;
        def x = 5;
        return x .
    "#;

    let executable = run_to_executable(source, "test_basic_decl_def.yuu")
        .expect("Failed to compile basic decl/def test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run basic decl/def test");

    assert_eq!(output, 5);
}

#[test]
fn test_decl_def_with_heap_allocation() {
    let source = r#"fn main() -> i64:
        dec ptr;
        def ptr = @42;
        let value = ptr.*;
        ~ptr;
        return value .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_heap_allocation.yuu")
        .expect("Failed to compile heap allocation decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run heap allocation decl/def test");

    assert_eq!(output, 42);
}

#[test]
fn test_decl_def_with_stack_arrays() {
    let source = r#"fn main() -> i64:
        dec arr;
        def arr = [10; 5];  // Array of 5 elements, all initialized to 10
        return (arr@2).* .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_stack_arrays.yuu")
        .expect("Failed to compile stack array decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run stack array decl/def test");

    assert_eq!(output, 10);
}

#[test]
fn test_decl_def_with_heap_arrays() {
    let source = r#"fn main() -> i64:
        dec heap_arr;
        def heap_arr = @[7; 3];  // Heap-allocated array of 3 elements, all initialized to 7
        let value = (heap_arr@1).*;
        ~heap_arr;
        return value .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_heap_arrays.yuu")
        .expect("Failed to compile heap array decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run heap array decl/def test");

    assert_eq!(output, 7);
}

#[test]
fn test_decl_def_with_array_literals() {
    let source = r#"fn main() -> i64:
        dec arr;
        def arr = [1, 2, 3, 4, 5];
        return (arr@3).* .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_array_literals.yuu")
        .expect("Failed to compile array literal decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run array literal decl/def test");

    assert_eq!(output, 4);
}

#[test]
fn test_decl_def_with_heap_array_literals() {
    let source = r#"fn main() -> i64:
        dec heap_lit;
        def heap_lit = @[10, 20, 30];
        let value = (heap_lit@1).*;
        ~heap_lit;
        return value .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_heap_array_literals.yuu")
        .expect("Failed to compile heap array literal decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run heap array literal decl/def test");

    assert_eq!(output, 20);
}

#[test]
fn test_decl_def_with_structs() {
    let source = r#"
        struct Point:
            x: i64,
            y: i64,
        end

        fn main() -> i64:
            dec point;
            def point = Point { x: 10i64, y: 20 };
            return point.x + point.y .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_structs.yuu")
        .expect("Failed to compile struct decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run struct decl/def test");

    assert_eq!(output, 30);
}

#[test]
fn test_decl_def_with_heap_structs() {
    let source = r#"
        struct Data:
            value: i64,
        end

        fn main() -> i64:
            dec ptr;
            def ptr = @Data { value: 99 };
            let result = ptr.value;
            ~ptr;
            return result .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_heap_structs.yuu")
        .expect("Failed to compile heap struct decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run heap struct decl/def test");

    assert_eq!(output, 99);
}

#[test]
fn test_decl_def_with_enums() {
    let source = r#"
        enum Option:
            None,
            Some: i64,
        end

        fn main() -> i64:
            dec opt;
            def opt = Option::Some(42);

            match opt:
                Option::Some(value): return value .
                Option::None: return 0 .
            end
        end
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_enums.yuu")
        .expect("Failed to compile enum decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum decl/def test");

    assert_eq!(output, 42);
}

#[test]
fn test_decl_def_complex_mixed_types() {
    let source = r#"
        struct Container:
            data: *i64
        end

        fn main() -> i64:
            dec heap_val;
            dec container;
            dec result;

            def heap_val = @77;
            def container = Container { data: heap_val }; 
            def result = container.data.*;

            ~heap_val;
            return result .
    "#;

    let executable = run_to_executable(source, "test_decl_def_complex_mixed_types.yuu")
        .expect("Failed to compile complex mixed types decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run complex mixed types decl/def test");

    assert_eq!(output, 77);
}

#[test]
fn test_decl_def_with_pointer_arithmetic() {
    let source = r#"fn main() -> i64:
        dec arr;
        dec ptr;
        dec offset_ptr;

        def arr = [5, 10, 15, 20];
        def ptr = arr;
        def offset_ptr = ptr @ 2;

        return offset_ptr.* .
    "#;

    let executable = run_to_executable(source, "test_decl_def_with_pointer_arithmetic.yuu")
        .expect("Failed to compile pointer arithmetic decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run pointer arithmetic decl/def test");

    assert_eq!(output, 15);
}

#[test]
fn test_decl_def_interdependent_variables() {
    let source = r#"fn main() -> i64:
        dec a;
        dec b;
        dec c;

        def a = 5;
        def b = a * 2;
        def c = a + b;

        return c .
    "#;

    let executable = run_to_executable(source, "test_decl_def_interdependent_variables.yuu")
        .expect("Failed to compile interdependent variables test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run interdependent variables test");

    assert_eq!(output, 15); // a=5, b=10, c=15
}

#[test]
fn test_decl_def_in_loops() {
    let source = r#"fn main() -> i64:
        dec sum;
        dec i;

        def sum = 0;
        def i = 0;

        while i < 5:
            sum = sum + i;
            i = i + 1;
        end

        return sum .
    "#;

    let executable = run_to_executable(source, "test_decl_def_in_loops.yuu")
        .expect("Failed to compile loop decl/def test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run loop decl/def test");

    assert_eq!(output, 10); // 0+1+2+3+4 = 10
}

// Error case tests - these should fail to compile

#[test]
fn test_def_without_decl_should_fail() {
    let source = r#"fn main() -> i64:
        def x = 5;  // Error: x was never declared
        return x .
    "#;

    let result = run_to_executable(source, "test_def_without_decl_should_fail.yuu");

    assert!(
        result.is_err(),
        "Expected compilation to fail for def without decl"
    );
}

#[test]
fn test_use_before_def_should_fail() {
    let source = r#"fn main() -> i64:
        dec x;
        let y = x;  // Error: x is declared but not yet defined
        def x = 5;
        return y .
    "#;

    let result = run_to_executable(source, "test_use_before_def_should_fail.yuu");

    assert!(
        result.is_err(),
        "Expected compilation to fail when using variable before def"
    );
}