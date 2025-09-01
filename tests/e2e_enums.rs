// E2E tests for enum definitions, instantiation, and matching

mod common;

use common::*;

#[test]
fn test_simple_enum_definition_and_instantiation() {
    let source = r#"
        enum Color:
            Red,
            Green,
            Blue,
        end
        
        fn color_value(c: Color) -> i64:
            match c:
                Color::Red: return 1 .
                Color::Green: return 2 .
                Color::Blue: return 3 .
            end
        end
        
        fn main() -> i64:
            let color = Color::Red;
            return color_value(color) .
    "#;

    let executable = run_to_executable(source, "test_simple_enum.yuu")
        .expect("Failed to compile simple enum test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run simple enum test");

    assert_eq!(output, 1);
}

#[test]
fn test_enum_with_data_variants() {
    let source = r#"
        enum Option:
            None,
            Some: i64,
        end
        
        fn get_value(opt: Option) -> i64:
            match opt:
                Option::None: return 0 .
                Option::Some(value): return value .
            end
        end
        
        fn main() -> i64:
            let value = Option::Some(42);
            return get_value(value) .
    "#;

    // let yir = run_to_yir(source, "test_enum_data_variants.yuu")
    //     .expect("Failed to compile enum data variants test");

    // println!("Yir: {}", yir);
    // return;

    let executable = run_to_executable(source, "test_enum_data_variants.yuu")
        .expect("Failed to compile enum data variants test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum data variants test");

    assert_eq!(output, 42);
}

#[test]
fn test_enum_match_unit_variants() {
    let source = r#"
        enum Color:
            Red,
            Green,
            Blue,
        end
        
        fn color_to_number(c: Color) -> i64:
            match c:
                Color::Red: return 1 .
                Color::Green: return 2 .
                Color::Blue: return 3 .
            end
        
        fn main() -> i64:
            let red = Color::Red;
            let green = Color::Green;
            let blue = Color::Blue;
            return color_to_number(red) + color_to_number(green) + color_to_number(blue) .
    "#;

    let executable = run_to_executable(source, "test_enum_match_unit.yuu")
        .expect("Failed to compile enum match unit variants test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum match unit variants test");

    // 1 + 2 + 3 = 6
    assert_eq!(output, 6);
}

#[test]
fn test_enum_match_data_variants() {
    let source = r#"
        enum Option:
            None,
            Some(i64),
        end
        
        fn unwrap_or(opt: Option, default: i64) -> i64:
            match opt:
                Option::None: return default .
                Option::Some(value): return value .
            end
        
        fn main() -> i64:
            let some_val = Option::Some(42);
            let none_val = Option::None;
            let result1 = unwrap_or(some_val, 0);
            let result2 = unwrap_or(none_val, 10);
            return result1 + result2 .
    "#;

    let executable = run_to_executable(source, "test_enum_match_data.yuu")
        .expect("Failed to compile enum match data variants test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum match data variants test");

    // 42 + 10 = 52
    assert_eq!(output, 52);
}

#[test]
fn test_nested_enum_variants() {
    let source = r#"
        enum Result:
            Ok(i64),
            Error(i64),
        end
        
        enum Status:
            Processing,
            Complete(Result),
        end
        
        fn process_status(s: Status) -> i64:
            match s:
                Status::Processing: return 0 .
                Status::Complete(result):
                    match result:
                        Result::Ok(value): return value .
                        Result::Error(code): return -code .
                    end
            end
        
        fn main() -> i64:
            let processing = Status::Processing;
            let success = Status::Complete(Result::Ok(100));
            let error = Status::Complete(Result::Error(404));
            
            let r1 = process_status(processing);
            let r2 = process_status(success);
            let r3 = process_status(error);
            
            return r1 + r2 + r3 .
    "#;

    let executable = run_to_executable(source, "test_nested_enum.yuu")
        .expect("Failed to compile nested enum test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run nested enum test");

    // 0 + 100 + (-404) = -304
    assert_eq!(output, -304);
}

#[test]
fn test_enum_with_different_data_types() {
    let source = r#"
        enum Value:
            Integer(i64),
            Float(f32),
            Boolean(bool),
        end
        
        fn value_to_int(v: Value) -> i64:
            match v:
                Value::Integer(i): return i .
                Value::Float(f): return 99 .
                Value::Boolean(b): 
                    if b: return 1 .
                    else: return 0 .
            end
        
        fn main() -> i64:
            let int_val = Value::Integer(42);
            let float_val = Value::Float(3.14);
            let bool_true = Value::Boolean(true);
            let bool_false = Value::Boolean(false);
            
            let r1 = value_to_int(int_val);
            let r2 = value_to_int(float_val);
            let r3 = value_to_int(bool_true);
            let r4 = value_to_int(bool_false);
            
            return r1 + r2 + r3 + r4 .
    "#;

    let executable = run_to_executable(source, "test_enum_mixed_types.yuu")
        .expect("Failed to compile enum mixed types test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run enum mixed types test");

    // 42 + 99 + 1 + 0 = 142
    assert_eq!(output, 142);
}

#[test]
fn test_enum_type_inference() {
    let source = r#"
        enum Maybe:
            Nothing,
            Just(i64),
        end
        
        fn create_maybe(has_value: bool, value: i64) -> Maybe:
            if has_value: return Maybe::Just(value) .
            else: return Maybe::Nothing .
        
        fn main() -> i64:
            let m1 = create_maybe(true, 50);
            
            match m1:
                Maybe::Nothing: return 0 .
                Maybe::Just(v): return v .
            end
    "#;

    let executable = run_to_executable(source, "test_enum_type_inference.yuu")
        .expect("Failed to compile enum type inference test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum type inference test");

    assert_eq!(output, 50);
}

#[test]
fn test_enum_as_struct_field() {
    let source = r#"
        enum State:
            Idle,
            Running(i64),
            Error(i64),
        end
        
        struct Machine:
            id: i64,
            state: State,
        end
        
        fn get_machine_value(m: Machine) -> i64:
            match m.state:
                State::Idle: return m.id .
                State::Running(progress): return m.id + progress .
                State::Error(code): return -code .
            end
        
        fn main() -> i64:
            let machine1 = Machine { id: 100, state: State::Idle };
            let machine2 = Machine { id: 200, state: State::Running(50) };
            let machine3 = Machine { id: 300, state: State::Error(404) };
            
            let r1 = get_machine_value(machine1);
            let r2 = get_machine_value(machine2);
            let r3 = get_machine_value(machine3);
            
            return r1 + r2 + r3 .
    "#;

    let executable = run_to_executable(source, "test_enum_struct_field.yuu")
        .expect("Failed to compile enum as struct field test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum as struct field test");

    // 100 + (200 + 50) + (-404) = 100 + 250 - 404 = -54
    assert_eq!(output, -54);
}

#[test]
fn test_enum_match_with_default_case() {
    let source = r#"
        enum Number:
            Zero,
            One,
            Two,
            Many(i64),
        end
        
        fn classify_number(n: Number) -> i64:
            match n:
                Number::Zero: return 0 .
                Number::One: return 1 .
                default: return 999 .
            end
        
        fn main() -> i64:
            let zero = Number::Zero;
            let one = Number::One;
            let two = Number::Two;
            let many = Number::Many(42);
            
            let r1 = classify_number(zero);
            let r2 = classify_number(one);
            let r3 = classify_number(two);
            let r4 = classify_number(many);
            
            return r1 + r2 + r3 + r4 .
    "#;

    let executable = run_to_executable(source, "test_enum_default_case.yuu")
        .expect("Failed to compile enum default case test");

    let output =
        run_executable_with_output(&executable, &[]).expect("Failed to run enum default case test");

    // 0 + 1 + 999 + 999 = 1999
    assert_eq!(output, 1999);
}

#[test]
fn test_enum_extract_data_direct() {
    let source = r#"
        enum Container:
            Empty,
            Value(i64),
        end
        
        fn main() -> i64:
            let container = Container::Value(123);
            match container:
                Container::Empty: return 0 .
                Container::Value(data): return data .
            end
    "#;

    let executable = run_to_executable(source, "test_enum_extract_direct.yuu")
        .expect("Failed to compile enum extract direct test");

    let output = run_executable_with_output(&executable, &[])
        .expect("Failed to run enum extract direct test");

    assert_eq!(output, 123);
}
