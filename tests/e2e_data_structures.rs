// E2E tests for data structure operations

mod common;

use common::*;

#[test]
fn test_vector_operations() {
    let source = r#"
        struct Vector3D:
            x: f32,
            y: f32,
            z: f32 
        end
        
        fn vector_add(v1: Vector3D, v2: Vector3D) -> Vector3D:
            return Vector3D {
                x: v1.x + v2.x,
                y: v1.y + v2.y,
                z: v1.z + v2.z,
            } .
        
        fn vector_dot(v1: Vector3D, v2: Vector3D) -> f32:
            return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z .
        
        fn main() -> i64:
            let v1 = Vector3D { x: 1.0, y: 2.0, z: 3.0 };
            let v2 = Vector3D { x: 4.0, y: 5.0, z: 6.0 };
            let sum = vector_add(v1, v2);
            let dot = vector_dot(v1, v2);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_vector_operations.yuu")
        .expect("Failed to compile vector operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run vector operations test");

    assert_eq!(output, 0);
}

#[test]
fn test_matrix_operations() {
    let source = r#"
        struct Matrix2x2:
            m00: f32, m01: f32,
            m10: f32, m11: f32,
        end
        
        fn matrix_multiply(a: Matrix2x2, b: Matrix2x2) -> Matrix2x2:
            return Matrix2x2 {
                m00: a.m00 * b.m00 + a.m01 * b.m10,
                m01: a.m00 * b.m01 + a.m01 * b.m11,
                m10: a.m10 * b.m00 + a.m11 * b.m10,
                m11: a.m10 * b.m01 + a.m11 * b.m11,
            } .
        
        fn matrix_determinant(m: Matrix2x2) -> f32:
            return m.m00 * m.m11 - m.m01 * m.m10 .
        
        fn main() -> i64:
            let m1 = Matrix2x2 { m00: 1.0, m01: 2.0, m10: 3.0, m11: 4.0 };
            let m2 = Matrix2x2 { m00: 5.0, m01: 6.0, m10: 7.0, m11: 8.0 };
            let product = matrix_multiply(m1, m2);
            let det = matrix_determinant(m1);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_matrix_operations.yuu")
        .expect("Failed to compile matrix operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run matrix operations test");

    assert_eq!(output, 0);
}

#[test]
fn test_point_distance_calculations() {
    let source = r#"
        struct Point2D: 
            x: f32,
            y: f32,
        end
        
        fn distance_squared(p1: Point2D, p2: Point2D) -> f32:
            let dx = p1.x - p2.x;
            let dy = p1.y - p2.y;
            return dx * dx + dy * dy . 
        
        fn midpoint(p1: Point2D, p2: Point2D) -> Point2D:
            return Point2D {
                x: (p1.x + p2.x) / 2.0,
                y: (p1.y + p2.y) / 2.0,
            } .
        
        fn main() -> i64:
            let p1 = Point2D { x: 0.0, y: 0.0 };
            let p2 = Point2D { x: 3.0, y: 4.0 };
            let dist_sq = distance_squared(p1, p2);
            let mid = midpoint(p1, p2);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_point_distance_calculations.yuu")
        .expect("Failed to compile point operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run point operations test");

    assert_eq!(output, 0);
}

// #[test]
// fn test_rectangle_operations() {
//     let source = r#"
//         struct Rectangle:
//             x: f32,
//             y: f32,
//             width: f32,
//             height: f32,
//         end

//         fn area(rect: Rectangle) -> f32:
//             return rect.width * rect.height .

//         fn perimeter(rect: Rectangle) -> f32:
//             return 2.0 * (rect.width + rect.height) .

//         fn contains_point(rect: Rectangle, px: f32, py: f32) -> i64:
//             if px >= rect.x && px <= rect.x + rect.width:
//                 if py >= rect.y && py <= rect.y + rect.height:
//                     return 1;
//                 end
//             end
//             return 0 .

//         fn main() -> i64:
//             let rect = Rectangle { x: 10.0, y: 20.0, width: 30.0, height: 40.0 };
//             let area_val = area(rect);
//             let perim_val = perimeter(rect);
//             let contains = contains_point(rect, 25.0, 35.0);
//             return contains .
//     "#;

//     let executable = run_to_executable(source, "test_rectangle_ops.yuu")
//         .expect("Failed to compile rectangle operations test");

//     let output = run_executable_with_output(executable, &[])
//         .expect("Failed to run rectangle operations test");

//     // Point (25, 35) is inside rectangle at (10, 20) with size 30x40
//     assert_eq!(output, 1);
// }

#[test]
fn test_circle_operations() {
    let source = r#"
        struct Circle:
            center_x: f32,
            center_y: f32,
            radius: f32,
        end

        fn circle_area(c: Circle) -> f32:
            return 3.14159 * c.radius * c.radius .
        
        fn circle_circumference(c: Circle) -> f32:
            return 2.0 * 3.14159 * c.radius .
        
        fn point_in_circle(c: Circle, px: f32, py: f32) -> i64:
            let dx = px - c.center_x;
            let dy = py - c.center_y;
            let dist_sq = dx * dx + dy * dy;
            let radius_sq = c.radius * c.radius;
            if dist_sq <= radius_sq: return 1 .
            else: return 0 .
        end
        
        fn main() -> i64:
            let circle = Circle { center_x: 0.0, center_y: 0.0, radius: 5.0 };
            let area_val = circle_area(circle);
            let circumf = circle_circumference(circle);
            let inside = point_in_circle(circle, 3.0, 4.0);
            return inside .
    "#;

    let executable = run_to_executable(source, "test_circle_ops.yuu")
        .expect("Failed to compile circle operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run circle operations test");

    // Point (3, 4) is inside circle at (0, 0) with radius 5 (distance = 5)
    assert_eq!(output, 1);
}

#[test]
fn test_complex_number_operations() {
    let source = r#"
        struct Complex:
            real: f32,
            imag: f32,
        end
        
        fn complex_add(a: Complex, b: Complex) -> Complex:
            return Complex {
                real: a.real + b.real,
                imag: a.imag + b.imag,
            } .
        
        fn complex_multiply(a: Complex, b: Complex) -> Complex:
            return Complex {
                real: a.real * b.real - a.imag * b.imag,
                imag: a.real * b.imag + a.imag * b.real,
            } .
        
        fn complex_magnitude_squared(c: Complex) -> f32:
            return c.real * c.real + c.imag * c.imag .
        
        fn main() -> i64:
            let c1 = Complex { real: 3.0, imag: 4.0 };
            let c2 = Complex { real: 1.0, imag: 2.0 };
            let sum = complex_add(c1, c2);
            let product = complex_multiply(c1, c2);
            let mag_sq = complex_magnitude_squared(c1);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_complex_number_operations.yuu")
        .expect("Failed to compile complex operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run complex operations test");

    assert_eq!(output, 0);
}

#[test]
fn test_color_operations() {
    let source = r#"
        struct Color:
            r: f32,
            g: f32,
            b: f32,
            a: f32,
        end
        
        fn color_blend(c1: Color, c2: Color, factor: f32) -> Color:
            let inv_factor = 1.0 - factor;
            return Color {
                r: c1.r * inv_factor + c2.r * factor,
                g: c1.g * inv_factor + c2.g * factor,
                b: c1.b * inv_factor + c2.b * factor,
                a: c1.a * inv_factor + c2.a * factor,
            } .
        
        fn color_brightness(c: Color) -> f32:
            return (c.r + c.g + c.b) / 3.0 .
        
        fn main() -> i64:
            let red = Color { r: 1.0, g: 0.0, b: 0.0, a: 1.0 };
            let blue = Color { r: 0.0, g: 0.0, b: 1.0, a: 1.0 };
            let purple = color_blend(red, blue, 0.5);
            let brightness = color_brightness(purple);
            return 0 .
    "#;

    let executable = run_to_executable(source, "test_color_ops.yuu")
        .expect("Failed to compile color operations test");

    let output =
        run_executable_with_output(executable, &[]).expect("Failed to run color operations test");

    assert_eq!(output, 0);
}

// TODO: Define "as" casting operator
// #[test]
// fn test_statistics_operations() {
//     let source = r#"
//         struct Statistics:
//             sum: f32,
//             count: i64,
//             min: f32,
//             max: f32,
//         end

//         fn stats_add_value(stats: Statistics, value: f32) -> Statistics:
//             let mut new_min = 0.0;
//             let mut new_max = 0.0;

//             if stats.count == 0: new_min = value .
//             else if value < stats.min: new_min = value .
//             else: new_min = stats.min .

//             if stats.count == 0: new_max = value .
//             else if value > stats.max: new_max = value .
//             else: new_max = stats.max .

//             return Statistics {
//                 sum: stats.sum + value,
//                 count: stats.count + 1,
//                 min: new_min,
//                 max: new_max,
//             } .

//         fn stats_average(stats: Statistics) -> f32:
//             if stats.count == 0: return 0.0 .
//             else: return stats.sum / stats.count .
//         end

//         fn main() -> i64:
//             let mut stats = Statistics { sum: 0.0, count: 0, min: 0.0, max: 0.0 };
//             stats = stats_add_value(stats, 10.0);
//             stats = stats_add_value(stats, 20.0);
//             stats = stats_add_value(stats, 5.0);
//             let avg = stats_average(stats);
//             return stats.count .
//     "#;

//     let executable = run_to_executable(source, "test_stats_ops.yuu")
//         .expect("Failed to compile statistics operations test");

//     let output = run_executable_with_output(executable, &[])
//         .expect("Failed to run statistics operations test");

//     // Should return count = 3
//     assert_eq!(output, 3);
// }
