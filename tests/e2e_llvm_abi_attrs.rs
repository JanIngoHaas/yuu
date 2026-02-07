#[cfg(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos")))]
mod sysv_x86_64 {
    use std::fs;
    use std::path::Path;
    use yuu::utils::pipeline::Pipeline;

    fn compile_fixture_to_llvm_ir(path: &str) -> String {
        let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
        let source = fs::read_to_string(&fixture_path).unwrap_or_else(|e| {
            panic!("Failed to read fixture '{}': {}", fixture_path.display(), e)
        });

        Pipeline::new(source, fixture_path.display().to_string())
            .calc_llvm_ir()
            .unwrap_or_else(|e| {
                panic!(
                    "Failed to compile fixture '{}' to LLVM IR: {}",
                    fixture_path.display(),
                    e
                )
            })
    }

    fn assert_has_line_with_all_fragments(ir: &str, description: &str, fragments: &[&str]) {
        let has_match = ir
            .lines()
            .any(|line| fragments.iter().all(|fragment| line.contains(fragment)));

        assert!(
            has_match,
            "Missing ABI pattern: {}\nExpected a line containing all fragments: {:?}",
            description, fragments
        );
    }

    #[test]
    fn stress_fixture_emits_required_abi_attributes() {
        let ir = compile_fixture_to_llvm_ir("tests/godbolt_abi_stress.yuu");

        assert_has_line_with_all_fragments(
            &ir,
            "rotate_big signature must carry sret/byval + align + noundef",
            &[
                "define void @rotate_big(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "sum_big signature must carry noundef+byval+align",
            &[
                "define noundef i64 @sum_big(",
                "ptr noundef byval(",
                "align 8",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "rotate_big callsite must carry sret/byval + align + noundef",
            &[
                "call void @rotate_big(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "direct scalar return should be marked noundef",
            &["define noundef i64 @consume_if64("],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "direct scalar argument callsites should carry noundef",
            &["call { i64, double } @make_if64(", "i64 noundef 8"],
        );
    }

    #[test]
    fn compare_fixture_emits_required_abi_attributes() {
        let ir = compile_fixture_to_llvm_ir("tests/godbolt_abi_compare.yuu");

        assert_has_line_with_all_fragments(
            &ir,
            "rotate_triple signature must carry sret/byval + align + noundef",
            &[
                "define void @rotate_triple(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "sum_triple signature must carry noundef+byval+align",
            &[
                "define noundef i64 @sum_triple(",
                "ptr noundef byval(",
                "align 8",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "rotate_triple callsite must carry sret/byval + align + noundef",
            &[
                "call void @rotate_triple(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "direct scalar return should be marked noundef",
            &["define noundef i64 @add_i64("],
        );
    }

    #[test]
    fn unionish_fixture_emits_required_abi_attributes() {
        let ir = compile_fixture_to_llvm_ir("tests/godbolt_abi_unionish.yuu");

        assert_has_line_with_all_fragments(
            &ir,
            "unionish aggregate return should be 2x8-byte pair",
            &["define { i64, i64 } @make_i("],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "large carrier signature must carry sret/byval + align + noundef",
            &[
                "define void @bounce_union_carrier(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "large carrier callsite must carry sret/byval + align + noundef",
            &[
                "call void @bounce_union_carrier(",
                "ptr sret(",
                "align 8",
                "ptr noundef byval(",
            ],
        );

        assert_has_line_with_all_fragments(
            &ir,
            "direct fallback scalar arg should be marked noundef",
            &["call noundef i64 @read_i_or(", "i64 noundef -100"],
        );
    }
}

#[cfg(not(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos"))))]
#[test]
fn abi_attr_tests_are_sysv_only() {
    eprintln!("Skipping SysV-specific LLVM ABI attribute tests on this target.");
}
