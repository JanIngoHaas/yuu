// E2E tests for mathematical algorithms

mod common;

use common::*;

#[test]
fn test_fibonacci_recursive() {
    let source = r#"
        fn fibonacci(n: i64) -> i64:
            if n <= 1: return n .
            else: return fibonacci(n - 1) + fibonacci(n - 2) .
        end
        fn main() -> i64: return fibonacci(10) .
    "#;

    // let config = ron::ser::PrettyConfig::new();
    // let ron_string =
    //     ron::ser::to_string_pretty(&ast, config).expect("Failed to serialize AST to RON");

    // let yir = run_to_yir(source, "test_fibonacci.yuu").expect("Failed to compile fibonacci test");

    // println!("{}", yir);

    let executable = run_to_executable(source, "test_fibonacci_recursive.yuu")
        .expect("Failed to compile fibonacci test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run fibonacci test");

    // fib(10) = 55
    assert_eq!(output, 55);
}

#[test]
fn test_factorial_recursive() {
    let source = r#"
        fn factorial(n: i64) -> i64:
            if n <= 1: return 1 .
            else: return n * factorial(n - 1) .
        end
        
        fn main() -> i64: return factorial(6) .
    "#;

    let executable = run_to_executable(source, "test_factorial_recursive.yuu")
        .expect("Failed to compile factorial test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run factorial test");

    // 6! = 720
    assert_eq!(output, 720);
}

#[test]
fn test_factorial_iterative() {
    let source = r#"
        fn factorial_iter(n: i64) -> i64:
            let mut result = 1;
            let mut i = 1;
            while i <= n:
                result = result * i;
                i = i + 1;
            end
            return result .
        
        fn main() -> i64: return factorial_iter(5) .
    "#;

    let executable = run_to_executable(source, "test_factorial_iterative.yuu")
        .expect("Failed to compile iterative factorial test");

    let output = run_executable_with_output(executable, &[])
        .expect("Failed to run iterative factorial test");

    // 5! = 120
    assert_eq!(output, 120);
}

#[test]
fn test_gcd_algorithm() {
    let source = r#"
        fn gcd(a: i64, b: i64) -> i64:
            if b == 0: return a .
            else: return gcd(b, a % b) .
        end

        fn main() -> i64: return gcd(48, 18) .
    "#;

    let executable =
        run_to_executable(source, "test_gcd_algorithm.yuu").expect("Failed to compile GCD test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run GCD test");

    // gcd(48, 18) = 6
    assert_eq!(output, 6);
}

#[test]
fn test_power_function() {
    let source = r#"
        fn power(base: i64, exp: i64) -> i64:
            if exp == 0: return 1 .
            else if exp == 1: return base .
            else: return base * power(base, exp - 1) .
        end
        
        fn main() -> i64: return power(2, 8) .
    "#;

    let executable =
        run_to_executable(source, "test_power_function.yuu").expect("Failed to compile power test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run power test");

    // 2^8 = 256
    assert_eq!(output, 256);
}

#[test]
fn test_sum_of_squares() {
    let source = r#"
        fn sum_of_squares(n: i64) -> i64:
            let mut sum = 0;
            let mut i = 1;
            while i <= n:
                sum = sum + i * i;
                i = i + 1;
            end
        return sum .
        
        fn main() -> i64: return sum_of_squares(5) .
    "#;

    let executable = run_to_executable(source, "test_sum_of_squares.yuu")
        .expect("Failed to compile sum of squares test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run sum of squares test");

    // 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 1 + 4 + 9 + 16 + 25 = 55
    assert_eq!(output, 55);
}

#[test]
fn test_is_prime() {
    let source = r#"
        fn is_prime(n: i64) -> i64:
            if n <= 1: return 0 .
            else if n == 2: return 1 .
            else if n % 2 == 0: return 0 .
            else:
                let mut i = 3;
                while i * i <= n:
                    if n % i == 0: return 0 .
                    i = i + 2;
                end
                return 1;
            end
        end
        
        fn main() -> i64: return is_prime(17) .
    "#;

    let executable =
        run_to_executable(source, "test_is_prime.yuu").expect("Failed to compile is_prime test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run is_prime test");

    // 17 is prime, so should return 1
    assert_eq!(output, 1);
}

#[test]
fn test_nth_prime() {
    let source = r#"
        fn is_prime(n: i64) -> i64:
            if n <= 1: return 0 .
            else if n == 2: return 1 .
            else if n % 2 == 0: return 0 .
            else:
                let mut i = 3;
                while i * i <= n:
                    if n % i == 0: return 0 .
                    i = i + 2;
                end
                return 1; 
            end
        end

        fn nth_prime(n: i64) -> i64:
            let mut count = 0;
            let mut candidate = 2;
            while count < n:
                if is_prime(candidate) == 1:
                    count = count + 1;
                    if count == n: return candidate .
                end
                candidate = candidate + 1; 
            end
        return 0 . 
        

        fn main() -> i64: return nth_prime(5) .
    "#;

    let executable =
        run_to_executable(source, "test_nth_prime.yuu").expect("Failed to compile nth prime test");

    let output = run_executable_with_output(executable, &[]).expect("Failed to run nth prime test");

    // 5th prime is 11 (2, 3, 5, 7, 11)
    assert_eq!(output, 11);
}

#[test]
fn test_collatz_sequence() {
    let source = r#"
        fn collatz_length(n: i64) -> i64:
            let mut count = 0;
            let mut current = n;
            while current != 1:
                if current % 2 == 0: current = current / 2 .
                else: current = current * 3 + 1 .
                count = count + 1;
            end
        return count .
        
        fn main() -> i64: return collatz_length(7) .
    "#;

    let executable = run_to_executable(source, "test_collatz_sequence.yuu")
        .expect("Failed to compile collatz test");

    //println!("{}", executable);

    let output = run_executable_with_output(executable, &[]).expect("Failed to run collatz test");

    // Collatz sequence for 7: 7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
    // Length is 16 steps
    assert_eq!(output, 16);
}

#[test]
fn test_digital_root() {
    let source = r#"
        fn digital_root(n: i64) -> i64:
            while n >= 10:
                let mut sum = 0;
                let mut temp = n;
                while temp > 0:
                    sum = sum + temp % 10;
                    temp = temp / 10;
                end
                n = sum;
            end
        return n .
        
        fn main() -> i64: return digital_root(9875) .
    "#;

    let executable = run_to_executable(source, "test_digital_root.yuu")
        .expect("Failed to compile digital root test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run digital root test");

    // 9875 -> 9+8+7+5 = 29 -> 2+9 = 11 -> 1+1 = 2
    assert_eq!(output, 2);
}
