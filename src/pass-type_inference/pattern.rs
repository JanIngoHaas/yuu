use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{EnumPattern, ExprNode, RefutablePatternNode, Spanned},
    pass_type_inference::{
        infer_binding,
        pass_type_inference_impl::TransientData,
    },
    utils::type_info_table::TypeInfo,
};

fn infer_enum_pattern(
    enum_pattern: &EnumPattern,
    scrutinee_ty: &'static TypeInfo,
    scrutinee_expr: &ExprNode,
    block_id: usize,
    data: &mut TransientData,
) {
    // Verify the scrutinee is an enum type and get variant info
    match scrutinee_ty {
        TypeInfo::Enum(enum_type) => {
            // Check if this enum pattern matches the scrutinee's enum
            if enum_type.name != enum_pattern.enum_name {
                let err_msg = YuuError::builder()
                    .kind(ErrorKind::TypeMismatch)
                    .message(format!(
                        "Cannot match '{}' pattern against '{}' enum type",
                        enum_pattern.enum_name, enum_type.name
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(
                        enum_pattern.span.clone(),
                        format!(
                            "this pattern expects '{}' enum type",
                            enum_pattern.enum_name
                        ),
                    )
                    .label(
                        scrutinee_expr.span(),
                        format!(
                            "this expression being matched against has type '{}'",
                            enum_type.name
                        ),
                    )
                    .help("Pattern enum type must match the expression's enum type (what we are matching against)")
                    .build();
                data.errors.push(err_msg);
            }

            let opt_enum = data.type_registry.resolve_enum(enum_pattern.enum_name);

            if opt_enum.is_none() {
                let error = YuuError::builder()
                    .kind(ErrorKind::ReferencedUndefinedEnum)
                    .message(format!("Enum '{}' is not defined", enum_pattern.enum_name))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(enum_pattern.span.clone(), "undefined enum")
                    .help("Make sure the enum is declared and in scope")
                    .build();
                data.errors.push(error);
                return;
            }

            let enum_info = opt_enum.unwrap();

            // Get variant info from the type registry
            if let Some(variant_info) = enum_info.variants.get(&enum_pattern.variant_name) {
                // Check pattern matching validity
                match (&enum_pattern.binding, &variant_info.variant) {
                    (Some(binding), Some(associated_type)) => {
                        // Data variant with binding - valid
                        infer_binding(binding, block_id, associated_type, data);
                    }
                    (Some(_), None) => {
                        // Unit variant with binding - invalid
                        let err_msg = YuuError::builder()
                            .kind(ErrorKind::TypeMismatch)
                            .message(format!(
                                "Unit variant '{}::{}' cannot be destructured",
                                enum_pattern.enum_name, enum_pattern.variant_name
                            ))
                            .source(
                                data.src_code.source.clone(),
                                data.src_code.file_name.clone(),
                            )
                            .span(
                                enum_pattern.span.clone(),
                                "unit variant cannot have binding",
                            )
                            .help("Remove the binding or use a variant with associated data")
                            .build();
                        data.errors.push(err_msg);
                    }
                    (None, Some(_)) => {
                        // Data variant without binding - invalid
                        let err_msg = YuuError::builder()
                            .kind(ErrorKind::TypeMismatch)
                            .message(format!(
                                "Data variant '{}::{}' must be destructured",
                                enum_pattern.enum_name, enum_pattern.variant_name
                            ))
                            .source(
                                data.src_code.source.clone(),
                                data.src_code.file_name.clone(),
                            )
                            .span(enum_pattern.span.clone(), "data variant must have binding")
                            .help("Add a binding to destructure the associated data")
                            .build();
                        data.errors.push(err_msg);
                    }
                    (None, None) => {
                        // Unit variant with no binding - valid
                        // Nothing to do
                    }
                }
            } else {
                let err_msg = YuuError::builder()
                    .kind(ErrorKind::TypeMismatch)
                    .message(format!(
                        "Variant '{}' does not exist on enum '{}'",
                        enum_pattern.variant_name, enum_pattern.enum_name
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(
                        enum_pattern.span.clone(),
                        format!("unknown variant '{}'", enum_pattern.variant_name),
                    )
                    .build();
                data.errors.push(err_msg);
            }
        }
        _ => {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Cannot match enum pattern against non-enum type '{}'",
                    scrutinee_ty
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(enum_pattern.span.clone(), "enum pattern")
                .help("Enum patterns can only match against enum types")
                .build();
            data.errors.push(err_msg);
        }
    }
}

pub fn infer_pattern(
    pattern: &RefutablePatternNode,
    scrutinee_ty: &&'static TypeInfo,
    scrutinee_expr: &ExprNode,
    block_id: usize,
    data: &mut TransientData,
) {
    match pattern {
        RefutablePatternNode::Enum(enum_pattern) => {
            infer_enum_pattern(enum_pattern, scrutinee_ty, scrutinee_expr, block_id, data);
        }
    }
}
