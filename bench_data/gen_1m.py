import re

# Read the source file
with open("bench_data/gigantic_benchmark.yuu", "r") as f:
    content = f.read()

# List of identifiers to rename (structs, enums, functions)
# We exclude 'main' from this list as we handle it separately
identifiers = [
    "Point", "Vector3D", "Rectangle", "Circle", "Complex", "Shape", "MathOp", "Result",
    "ListNode", "TreeNode",
    "add_basic", "sub_basic", "mul_basic", "div_basic", "mod_basic", "abs_value", 
    "max_two", "min_two", "sign_function", "power_of_two",
    "fibonacci_recursive", "fibonacci_iterative", "factorial_recursive", "factorial_iterative",
    "triangular_number", "square_number", "cube_number", "pentagonal_number", "hexagonal_number", "tetrahedral_number",
    "gcd_euclidean", "lcm_calculation", "is_prime", "nth_prime", "sum_of_digits", "digital_root",
    "reverse_number", "is_palindrome", "collatz_sequence_length",
    "catalan_number", "bell_number_approximation", "stirling_approximation", "harmonic_number_approximation", "bernoulli_approximation",
    "point_distance_squared", "rectangle_area", "rectangle_perimeter", "circle_area_approximation", "circle_circumference_approximation",
    "vector_magnitude_squared", "vector_dot_product", "triangle_area_by_base_height", "sphere_volume_approximation", "cube_volume",
    "complex_add", "complex_multiply", "complex_magnitude_squared", "complex_conjugate",
    "shape_area_estimate", "shape_perimeter_estimate", "process_math_operation", "handle_result",
    "bubble_sort_swaps", "insertion_sort_comparisons", "selection_sort_operations", "merge_sort_operations", "quicksort_average_comparisons",
    "binary_search_operations", "linear_search_operations", "heap_sort_operations", "radix_sort_operations",
    "matrix_multiply_operations", "matrix_transpose_operations", "matrix_determinant_operations_2x2", "matrix_determinant_operations_3x3",
    "matrix_addition_operations", "gaussian_elimination_operations", "lu_decomposition_operations", "eigenvalue_approximation_operations",
    "dijkstra_operations", "floyd_warshall_operations", "bellman_ford_operations", "dfs_operations", "bfs_operations",
    "kruskal_operations", "prim_operations", "topological_sort_operations",
    "knapsack_operations", "longest_common_subsequence_operations", "edit_distance_operations", "coin_change_operations",
    "fibonacci_dp_operations", "climbing_stairs_operations", "house_robber_operations", "maximum_subarray_operations",
    "convex_hull_operations", "closest_pair_operations", "line_intersection_operations", "polygon_area_operations", "point_in_polygon_operations",
    "voronoi_diagram_operations", "delaunay_triangulation_operations",
    "newton_raphson_operations", "bisection_method_operations", "simpson_rule_operations", "trapezoidal_rule_operations", "euler_method_operations", "runge_kutta_operations", "monte_carlo_operations",
    "rsa_key_generation_operations", "aes_encryption_operations", "sha256_operations", "diffie_hellman_operations", "elliptic_curve_operations",
    "linear_regression_operations", "logistic_regression_operations", "neural_network_forward_pass", "neural_network_backprop", "k_means_operations", "decision_tree_operations", "svm_operations",
    "arithmetic_stress_test", "sequence_stress_test", "number_theory_stress_test", "algorithm_stress_test", "geometry_stress_test", "data_structure_stress_test",
    "mathematical_benchmark_suite", "algorithm_benchmark_suite", "geometry_benchmark_suite", "advanced_algorithm_benchmark", "computational_benchmark_suite",
    "mega_recursive_computation", "mega_iterative_computation", "mega_data_processing", "mega_algorithm_simulation",
    "ultimate_benchmark_phase_1", "ultimate_benchmark_phase_2", "ultimate_benchmark_phase_3", "comprehensive_system_benchmark",
    "create_sample_data", "process_mathematical_operations", "simulate_advanced_algorithms", "execute_comprehensive_benchmarks",
    "create_linked_list", "sum_linked_list", "free_linked_list", "create_tree", "sum_tree", "free_tree", "heap_stress_test"
]

# Rename 'main' to 'main_template' in the content to make it easier to replace
content = content.replace("fn main() -> i64:", "fn main_template() -> i64:")
identifiers.append("main_template")

output_content = "// 1 MILLION LOC BENCHMARK\n\n"

copies = 850 # 1185 * copies LOC

for i in range(copies):
    suffix = f"_{i}"
    current_chunk = content
    print("Processing chunk", i)
    
    for ident in identifiers:
        # Use regex to replace whole words only
        current_chunk = re.sub(rf"\b{ident}\b", f"{ident}{suffix}", current_chunk)
    
    output_content += f"// --- Block {i} ---\n"
    output_content += current_chunk
    output_content += "\n\n"

# Create the real main function
output_content += "fn main() -> i64:\n"
output_content += "    let mut total = 0;\n"
# Call ALL main_template functions
for i in range(copies):
    output_content += f"    total = total + main_template_{i}();\n"
output_content += "    return total .\n"

with open("bench_data/1m_benchmark.yuu", "w") as f:
    f.write(output_content)

print("Generated bench_data/1m_benchmark.yuu")
