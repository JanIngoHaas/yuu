use crate::pass_diagnostics::{ErrorKind, YuuError, create_no_overload_error};
use crate::pass_parse::add_ids::GetId;
use crate::pass_parse::ast::{
    AssignmentExpr, BinaryExpr, EnumInstantiationExpr, ExprNode, FuncCallExpr, IdentExpr,
    LValueKind, LiteralExpr, MemberAccessExpr, Spanned, StructInstantiationExpr, UnaryExpr,
    UnaryOp,
};
use crate::pass_parse::{AddressOfExpr, DerefExpr, HeapAllocExpr};
use crate::pass_type_inference::type_info::{
    PrimitiveType, TypeInfo, error_type, primitive_f32, primitive_f64, primitive_i64,
    primitive_nil, primitive_u64,
};
use crate::pass_yir_lowering::block::Block;

// const MAX_SIMILAR_NAMES: u64 = 3;
// const MIN_DST_SIMILAR_NAMES: u64 = 3;

use super::pass_type_inference_impl::TransientData;

/// Helper function to extract i64 literal value from an expression, if it's a constant
pub fn try_extract_i64_literal(expr: &crate::pass_parse::ast::ExprNode) -> Option<i64> {
    match expr {
        crate::pass_parse::ast::ExprNode::Literal(lit_expr) => match &lit_expr.lit.kind {
            crate::pass_parse::token::TokenKind::Integer(
                crate::pass_parse::token::Integer::I64(val),
            ) => Some(*val),
            _ => None,
        },
        _ => None, // Not a constant literal
    }
}

fn infer_literal_expr(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
    let out = match lit.lit.kind {
        crate::pass_parse::token::TokenKind::Integer(integer) => match integer {
            crate::pass_parse::token::Integer::I64(_) => primitive_i64(),
            crate::pass_parse::token::Integer::U64(_) => primitive_u64(),
        },
        crate::pass_parse::token::TokenKind::F32(_) => primitive_f32(),
        crate::pass_parse::token::TokenKind::F64(_) => primitive_f64(),
        crate::pass_parse::token::TokenKind::NilKw => primitive_nil(),
        _ => unreachable!("Compiler bug: Literal not implemented"),
    };
    data.type_registry.add_literal(lit.id, out);
    out
}

fn infer_pointer_op_expr(
    pointer_op_expr: &crate::pass_parse::ast::PointerOpExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let lhs_type = infer_expr(&pointer_op_expr.left, block, data, None);
    let rhs_type = infer_expr(&pointer_op_expr.right, block, data, None);

    // Check if LHS is a pointer and RHS is integer
    if let TypeInfo::Pointer(_) = lhs_type {
        if matches!(
            rhs_type,
            TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::U64)
        ) {
            // Pointer arithmetic: returns same pointer type
            data.type_registry
                .type_info_table
                .insert(pointer_op_expr.id, lhs_type);
            return lhs_type;
        }
    }

    // Error case - invalid pointer arithmetic
    let err = YuuError::builder()
        .kind(ErrorKind::TypeMismatch)
        .message(format!(
            "Invalid pointer arithmetic: '{}' @ '{}'",
            lhs_type, rhs_type
        ))
        .source(
            data.src_code.source.clone(),
            data.src_code.file_name.clone(),
        )
        .span(pointer_op_expr.span.clone(), "invalid pointer operation")
        .help("The @ operator requires a pointer on the left and an integer offset on the right. Note: @ is not overloadable")
        .build();
    data.errors.push(err);
    data.type_registry
        .type_info_table
        .insert(pointer_op_expr.id, error_type());
    error_type()
}

fn infer_cast_expr(
    cast_expr: &crate::pass_parse::ast::CastExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let _expr_type = infer_expr(&cast_expr.expr, block, data, None);
    let target_type = crate::pass_type_inference::types::infer_type(&cast_expr.target_type, data);
    data.type_registry
        .type_info_table
        .insert(cast_expr.id, target_type);
    target_type
}

fn infer_free_op(
    unary_expr: &UnaryExpr,
    operand_type: &'static TypeInfo,
    data: &mut TransientData,
) -> &'static TypeInfo {
    if let TypeInfo::Pointer(_) = operand_type {
        primitive_nil()
    } else {
        let err = YuuError::builder()
            .kind(ErrorKind::TypeMismatch)
            .message(format!(
                "Cannot free a non-pointer type: '{}'",
                operand_type
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(unary_expr.operand.span().clone(), "expected a pointer type")
            .help("The `~` operator (free) can only be applied to pointer types.")
            .build();
        data.errors.push(err);
        error_type()
    }
}

fn infer_binary_expr(
    binary_expr: &BinaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let expr_id = binary_expr.id;
    let expr_span = binary_expr.span.clone();
    let op_name = binary_expr.op.static_name();

    // Standard logic for all binary ops
    let lhs_type_actual = infer_expr(&binary_expr.left, block, data, None);
    let rhs_type_actual = infer_expr(&binary_expr.right, block, data, None);
    let resolved_type = resolve_binary_overload(
        op_name,
        lhs_type_actual,
        rhs_type_actual,
        expr_span,
        data,
        expr_id,
    );

    data.type_registry
        .type_info_table
        .insert(expr_id, resolved_type);
    resolved_type
}

fn resolve_binary_overload(
    op_name: ustr::Ustr,
    lhs: &'static TypeInfo,
    rhs: &'static TypeInfo,
    span: logos::Span,
    data: &mut TransientData,
    expr_id: i64,
) -> &'static TypeInfo {
    match data.type_registry.resolve_function(op_name, &[lhs, rhs]) {
        Ok(res) => {
            data.type_registry
                .bindings
                .insert(expr_id, res.binding_info.id);
            res.ty.ret
        }
        Err(candidates) => {
            let err = create_no_overload_error(
                &op_name,
                candidates,
                &[lhs, rhs],
                data.type_registry,
                &data.src_code,
                span,
            );
            data.errors.push(err);
            data.type_registry
                .type_info_table
                .insert(expr_id, error_type());
            error_type()
        }
    }
}

fn infer_unary_expr(
    unary_expr: &UnaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let ty = infer_expr(&unary_expr.operand, block, data, None);
    let op_name = unary_expr.op.static_name();

    let resolved_type = match unary_expr.op {
        UnaryOp::Free => infer_free_op(unary_expr, ty, data),
        _ => {
            let resolution = match data.type_registry.resolve_function(op_name, &[ty]) {
                Ok(res) => res.ty.ret,
                Err(err) => {
                    let err = create_no_overload_error(
                        &op_name,
                        err,
                        &[ty],
                        data.type_registry,
                        &data.src_code,
                        unary_expr.span.clone(),
                    );
                    data.errors.push(err);
                    error_type()
                }
            };
            resolution
        }
    };

    data.type_registry
        .type_info_table
        .insert(unary_expr.id, resolved_type);
    resolved_type
}

fn infer_ident_expr(
    ident_expr: &IdentExpr,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    let err = match function_args {
        Some(args) => match data.type_registry.resolve_function(ident_expr.ident, args) {
            Ok(res) => {
                data.type_registry
                    .type_info_table
                    .insert(ident_expr.id, res.general_ty);
                data.type_registry
                    .bindings
                    .insert(ident_expr.id, res.binding_info.id);
                return res.general_ty;
            }
            Err(err) => Box::new(create_no_overload_error(
                ident_expr.ident.as_str(),
                err,
                args,
                data.type_registry,
                &data.src_code,
                ident_expr.span.clone(),
            )),
        },
        None => {
            match block.resolve_variable(ident_expr.ident, &data.src_code, ident_expr.span.clone())
            {
                Ok(fr) => {
                    let ty = data
                        .type_registry
                        .type_info_table
                        .get(fr.binding_info.id)
                        .expect(
                        "Compiler bug: binding not found in type table - but it should be there",
                    );

                    // Check if the variable was declared but not yet defined (has unknown type)
                    if matches!(ty, TypeInfo::Unknown) {
                        let err = YuuError::builder()
                            .kind(ErrorKind::TypeMismatch)
                            .message(format!(
                                "Cannot infer type of '{}' - variable has unknown type, but should be known at this point",
                                ident_expr.ident
                            ))
                            .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                            .span(ident_expr.span.clone(), "unknown type")
                            .help("Define the variable with 'def' before using it to establish its type")
                            .build();
                        data.errors.push(err);
                        data.type_registry.type_info_table.insert(ident_expr.id, error_type());
                        return error_type();
                    }

                    data.type_registry.type_info_table.insert(ident_expr.id, ty);
                    data.type_registry
                        .bindings
                        .insert(ident_expr.id, fr.binding_info.id);
                    return ty;
                }
                Err(err) => err,
            }
        }
    };
    data.errors.push(*err);
    data.type_registry
        .type_info_table
        .insert(ident_expr.id, error_type());
    error_type()
}

fn infer_func_call_expr(
    func_call_expr: &FuncCallExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let actual_arg_types = func_call_expr
        .args
        .iter()
        .map(|arg| infer_expr(arg, block, data, None))
        .collect::<Vec<_>>();

    let actual_func_ident = infer_expr(&func_call_expr.lhs, block, data, Some(&actual_arg_types));

    let resolved_ret_type = match actual_func_ident {
        TypeInfo::Function(func) => func.ret,
        TypeInfo::BuiltInPrimitive(prim) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Cannot call primitive type '{}' as a function",
                    prim
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "not callable")
                .help("Only function types can be called with parentheses")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Pointer(_) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message("Cannot call a pointer as a function")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "pointer type")
                .help("Function pointers are not supported yet")
                .build();
            data.errors.push(err);
            error_type()
        }

        TypeInfo::Struct(_struct_type) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message("Cannot call a struct type identifier as a function")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "struct type")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Error => error_type(),
        TypeInfo::Unknown => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message("Cannot call an identifier of unknown type as a function")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "unknown type")
                .build();
            data.errors.push(err);
            error_type()
        },
        TypeInfo::Enum(e) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Cannot call enum type '{}' as a function",
                    e.name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "enum type")
                .help("Enums cannot be called as functions. Use enum variant syntax like 'EnumName::Variant' instead")
                .build();
            data.errors.push(err);
            error_type()
        }
    };

    data.type_registry
        .type_info_table
        .insert(func_call_expr.id, resolved_ret_type);

    resolved_ret_type
}

fn infer_assignment(
    assignment_expr: &AssignmentExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    // checking LValueKind is done in parsing already..

    debug_assert!(
        matches!(
            (&*assignment_expr.lhs, &assignment_expr.lvalue_kind),
            (ExprNode::Ident(_), LValueKind::Variable)
                | (ExprNode::MemberAccess(_), LValueKind::FieldAccess)
                | (ExprNode::Deref(_), LValueKind::Dereference)
        ),
        "LValueKind should match the LHS expression structure"
    );

    let ty_lhs = infer_expr(&assignment_expr.lhs, block, data, None);
    let ty_rhs = infer_expr(&assignment_expr.rhs, block, data, None);

    // First unify to check compatibility
    let _unified = match ty_lhs.unify(ty_rhs) {
        Ok(unified) => unified,
        Err(err) => {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Cannot unify type '{}' and type '{}' in assignment expression",
                    err.left, err.right
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    assignment_expr.rhs.span().clone(),
                    format!("has type {}", err.left),
                )
                .label(
                    assignment_expr.lhs.span().clone(),
                    format!("has type {}", err.right),
                )
                .help("The types must be compatible for assignment")
                .build();
            data.errors.push(err_msg);
            error_type()
        }
    };

    // Assignment expressions always have type "nil"
    data.type_registry
        .type_info_table
        .insert(assignment_expr.id, primitive_nil());

    primitive_nil()
}

fn infer_struct_instantiation(
    struct_instantiation_expr: &StructInstantiationExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    for (_, expr_node) in &struct_instantiation_expr.fields {
        infer_expr(expr_node, block, data, None);
    }

    let struct_name = struct_instantiation_expr.struct_name;
    let struct_opt = data.type_registry.resolve_struct(struct_name);

    if struct_opt.is_none() {
        let err = YuuError::builder()
            .kind(ErrorKind::ReferencedUndefinedStruct)
            .message(format!(
                "Cannot instantiate struct '{}' because it has not been defined",
                struct_name
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                struct_instantiation_expr.span.clone(),
                "attempted to instantiate undefined struct",
            )
            .help("Define this struct before using it")
            .build();
        data.errors.push(err);
        data.type_registry
            .type_info_table
            .insert(struct_instantiation_expr.id, error_type());
        return error_type();
    }

    let sinfo = struct_opt.unwrap();
    let struct_type = sinfo.ty;

    for (field, expr_node) in &struct_instantiation_expr.fields {
        let expr_type = data
            .type_registry
            .type_info_table
            .get(expr_node.node_id())
            .expect("Compiler bug: expression type should be in registry");

        if let Some(field_info) = sinfo.fields.get(&field.name) {
            match expr_type.unify(field_info.ty) {
                Ok(_) => {}
                Err(err) => {
                    let err_msg = YuuError::builder()
                        .kind(ErrorKind::TypeMismatch)
                        .message(format!(
                            "Cannot assign {} to field '{}' of type {}",
                            err.left, field.name, err.right
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(expr_node.span().clone(), format!("has type {}", err.left))
                        .label(field.span.clone(), format!("expected type {}", err.right))
                        .help("The types must be compatible for assignment")
                        .build();
                    data.errors.push(err_msg);
                }
            }
        } else {
            let err = YuuError::builder()
                .kind(ErrorKind::ReferencedUndeclaredField)
                .message(format!(
                    "Cannot assign to undeclared field '{}' of struct '{}'",
                    field.name, struct_name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    field.span.clone(),
                    "attempted to assign to undeclared field",
                )
                .help("Define this field in the struct before using it")
                .build();
            data.errors.push(err);
        }
    }

    data.type_registry
        .type_info_table
        .insert(struct_instantiation_expr.id, struct_type);

    struct_type
}

fn infer_member_access(
    member_access_expr: &MemberAccessExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let lhs_ty = infer_expr(&member_access_expr.lhs, block, data, None);

    match lhs_ty {
        TypeInfo::Struct(s) | TypeInfo::Pointer(TypeInfo::Struct(s)) => {
            let si = data
                .type_registry
                .resolve_struct(s.name)
                .expect("Compiler bug: Struct type should be resolved here.");

            let field = si.fields.get(&member_access_expr.field.name).cloned();

            match field {
                Some(field_info) => {
                    data.type_registry
                        .type_info_table
                        .insert(member_access_expr.id, field_info.ty);

                    field_info.ty
                }
                None => {
                    let err = YuuError::builder()
                        .kind(ErrorKind::ReferencedUndeclaredField)
                        .message(format!(
                            "Struct '{}' has no field named '{}'",
                            s.name, member_access_expr.field.name
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(
                            member_access_expr.field.span.clone(),
                            "attempted to access undefined field",
                        )
                        .label(
                            member_access_expr.lhs.span().clone(),
                            format!("this expression has type '{}'", s.name),
                        )
                        .help(format!(
                            "Available fields for struct '{}': {}",
                            s.name,
                            if si.fields.is_empty() {
                                "none".to_string()
                            } else {
                                si.fields
                                    .keys()
                                    .map(|k| k.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .build();
                    data.errors.push(err);

                    data.type_registry
                        .type_info_table
                        .insert(member_access_expr.id, error_type());
                    error_type()
                }
            }
        }
        _ => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Cannot access field '{}' on value of type '{}'",
                    member_access_expr.field.name, lhs_ty
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    member_access_expr.span.clone(),
                    "attempted member access on non-struct type",
                )
                .label(
                    member_access_expr.lhs.span().clone(),
                    format!("this expression has type '{}'", lhs_ty),
                )
                .label(
                    member_access_expr.field.span.clone(),
                    "field access attempted here",
                )
                .help("Member access is only supported on struct types")
                .build();
            data.errors.push(err);

            data.type_registry
                .type_info_table
                .insert(member_access_expr.id, error_type());
            error_type()
        }
    }
}

fn infer_enum_instantiation(
    ei: &EnumInstantiationExpr,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    let enum_type = data.type_registry.resolve_enum(ei.enum_name);
    let inferred_ty = match enum_type {
        Some(enum_info) => {
            let variant_info_registered = enum_info.variants.get(&ei.variant_name);
            match variant_info_registered {
                Some(variant) => {
                    let ty = enum_info.ty;
                    // Left: Maybe data associated with the enum instantiation, i.e. Option::Some(x) where x would be the data (syntactical), Right: Semantic type information about the enum variant, i.e. data type that the variant is associated and registered with
                    match (&ei.data, variant.variant) {
                        // Yes, we have both
                        (Some(dexpr), Some(reg_ty)) => {
                            // Calculate type (semantic) from associated data (syntactical)
                            let ty = infer_expr(dexpr, block, data, function_args);
                            // See if the types match
                            if let Err(err) = ty.unify(reg_ty) {
                                let err_msg = YuuError::builder()
                                    .kind(ErrorKind::TypeMismatch)
                                    .message(format!(
                                        "Cannot assign '{}' to enum variant '{}::{}' which expects '{}'",
                                        err.left, ei.enum_name, ei.variant_name, err.right
                                    ))
                                    .source(
                                        data.src_code.source.clone(),
                                        data.src_code.file_name.clone(),
                                    )
                                    .span(dexpr.span().clone(), format!("has type '{}'", err.left))
                                    .label(ei.span.clone(), format!("expected type '{}'", err.right))
                                    .help("The types must be compatible for enum variant instantiation")
                                    .build();
                                data.errors.push(err_msg);
                            }
                        }
                        // Unit Variant, good!
                        (None, None) => {
                            // Nothing to do here...
                        }

                        // Left: Syntactically, we have data (user made an error and mistakingly provided data), Right: No data was expected
                        (Some(dexpr), None) => {
                            let err = YuuError::builder()
                                .kind(ErrorKind::InvalidExpression)
                                .message(format!(
                                    "Enum variant '{}::{}' does not accept data, but data was provided",
                                    ei.enum_name, ei.variant_name
                                ))
                                .source(
                                    data.src_code.source.clone(),
                                    data.src_code.file_name.clone(),
                                )
                                .span(dexpr.span().clone(), "unexpected data provided here")
                                .label(ei.span.clone(), "for this unit variant")
                                .help(format!("Use '{}::{}' without parentheses for unit variants", ei.enum_name, ei.variant_name))
                                .build();
                            data.errors.push(err);
                        }

                        // Left: No data was provided, Right: But there is associated data registered => User error
                        (None, Some(expected_ty)) => {
                            let err = YuuError::builder()
                                .kind(ErrorKind::InvalidExpression)
                                .message(format!(
                                    "Enum variant '{}::{}' expects data of type '{}', but no data was provided",
                                    ei.enum_name, ei.variant_name, expected_ty
                                ))
                                .source(
                                    data.src_code.source.clone(),
                                    data.src_code.file_name.clone(),
                                )
                                .span(ei.span.clone(), "missing required data")
                                .help(format!("Use '{}::{}(value)' with a value of type '{}'", ei.enum_name, ei.variant_name, expected_ty))
                                .build();
                            data.errors.push(err);
                        }
                    }
                    ty
                }
                None => {
                    let err = YuuError::builder()
                        .kind(ErrorKind::ReferencedUndeclaredVariant)
                        .message(format!(
                            "Enum '{}' has no variant named '{}'",
                            ei.enum_name, ei.variant_name
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(ei.span.clone(), "attempted to use undefined variant")
                        .help(format!(
                            "Available variants for enum '{}': {}",
                            ei.enum_name,
                            if enum_info.variants.is_empty() {
                                "none".to_string()
                            } else {
                                enum_info
                                    .variants
                                    .keys()
                                    .map(|k| k.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .build();
                    data.errors.push(err);
                    error_type()
                }
            }
        }
        None => {
            let err = YuuError::builder()
                .kind(ErrorKind::ReferencedUndefinedEnum)
                .message(format!(
                    "Cannot instantiate enum '{}' because it has not been defined",
                    ei.enum_name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(ei.span.clone(), "attempted to instantiate undefined enum")
                .help("Define this enum before using it")
                .build();
            data.errors.push(err);
            error_type()
        }
    };
    data.type_registry
        .type_info_table
        .insert(ei.id, inferred_ty);
    inferred_ty
}

pub fn infer_expr(
    expr: &ExprNode,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    match expr {
        ExprNode::Literal(lit) => infer_literal_expr(lit, data),
        ExprNode::Binary(binary) => infer_binary_expr(binary, block, data),
        ExprNode::Unary(unary) => infer_unary_expr(unary, block, data),
        ExprNode::Ident(ident) => infer_ident_expr(ident, block, data, function_args),
        ExprNode::FuncCall(func_call) => infer_func_call_expr(func_call, block, data),
        ExprNode::Assignment(assignment) => infer_assignment(assignment, block, data),
        ExprNode::StructInstantiation(struct_instantiation_expr) => {
            infer_struct_instantiation(struct_instantiation_expr, block, data)
        }
        ExprNode::MemberAccess(member_access_expr) => {
            infer_member_access(member_access_expr, block, data)
        }
        ExprNode::EnumInstantiation(ei) => infer_enum_instantiation(ei, block, data, function_args),
        ExprNode::Deref(deref_expr) => infer_deref_expr(deref_expr, block, data),
        ExprNode::AddressOf(address_of_expr) => infer_address_of_expr(address_of_expr, block, data),
        ExprNode::HeapAlloc(heap_alloc_expr) => infer_heap_alloc_expr(heap_alloc_expr, block, data),
        ExprNode::Array(array_expr) => infer_array_expr(array_expr, block, data),
        ExprNode::ArrayLiteral(array_literal_expr) => {
            infer_array_literal_expr(array_literal_expr, block, data)
        }
        ExprNode::PointerOp(pointer_op_expr) => infer_pointer_op_expr(pointer_op_expr, block, data),
        ExprNode::Cast(cast_expr) => infer_cast_expr(cast_expr, block, data),
    }
}

fn infer_heap_alloc_expr(
    heap_alloc_expr: &HeapAllocExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    // Always infer the type of the expression to be allocated
    let value_type = infer_expr(&heap_alloc_expr.value, block, data, None);

    // Special case: for array expressions, use the element type directly
    // REASON: The expression would otherwise immediately decay to a pointer...
    let pointer_type = match &*heap_alloc_expr.value {
        ExprNode::Array(_) | ExprNode::ArrayLiteral(_) => {
            value_type // Here, we already have a pointer, so we just don't do anything...
        }
        _ => {
            // Normal case: heap-allocate space for the value
            value_type.ptr_to()
        }
    };

    data.type_registry
        .type_info_table
        .insert(heap_alloc_expr.id, pointer_type);

    pointer_type
}

fn infer_deref_expr(
    deref_expr: &DerefExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let operand_type = infer_expr(&deref_expr.operand, block, data, None);

    let result_type = match operand_type {
        TypeInfo::Pointer(pointee_type) => pointee_type,
        _ => {
            // Error: trying to dereference non-pointer
            let error = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message("Cannot dereference non-pointer type".to_string())
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    deref_expr.span.clone(),
                    "This expression is not a pointer".to_string(),
                )
                .build();
            data.errors.push(error);
            error_type()
        }
    };

    data.type_registry
        .type_info_table
        .insert(deref_expr.id, result_type);

    result_type
}

fn infer_address_of_expr(
    address_of_expr: &AddressOfExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let operand_type = infer_expr(&address_of_expr.operand, block, data, None);

    let result_type = operand_type.ptr_to();

    data.type_registry
        .type_info_table
        .insert(address_of_expr.id, result_type);

    result_type
}

fn infer_array_expr(
    array_expr: &crate::pass_parse::ast::ArrayExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    use crate::pass_type_inference::type_info::{PrimitiveType, TypeInfo};
    use crate::pass_type_inference::types::infer_type;

    // Determine element type
    let element_type = if let Some(explicit_type) = &array_expr.element_type {
        // Explicit type provided: [value:type; count] or [:type; count]
        infer_type(explicit_type, data)
    } else if let Some(init_value) = &array_expr.init_value {
        // Type inferred from init value: [value; count]
        infer_expr(init_value, block, data, None)
    } else {
        // This should not happen - we need either explicit type or init value
        panic!(
            "Compiler Bug: Array expression must have either explicit type or init value for type inference"
        );
    };

    // Verify size is an integer type
    let size_type = infer_expr(&array_expr.size, block, data, None);
    if !matches!(size_type, TypeInfo::BuiltInPrimitive(PrimitiveType::I64)) {
        // Emit proper error for user mistake
        data.errors.push(
            crate::pass_diagnostics::error::YuuError::builder()
                .kind(crate::pass_diagnostics::error::ErrorKind::TypeMismatch)
                .message(format!("Array size must be i64, found {}", size_type))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(array_expr.size.span(), "expected i64 for array size")
                .build(),
        );
        return crate::pass_type_inference::type_info::error_type();
    }

    // Arrays are treated as pointers to the element type
    let result_type = element_type.ptr_to();

    data.type_registry
        .type_info_table
        .insert(array_expr.id, result_type);

    result_type
}

fn infer_array_literal_expr(
    array_literal_expr: &crate::pass_parse::ast::ArrayLiteralExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    use crate::pass_type_inference::types::infer_type;

    debug_assert!(
        !array_literal_expr.elements.is_empty(),
        "Empty array literals should be rejected during parsing"
    );

    // Determine element type
    let element_type = if let Some(explicit_type) = &array_literal_expr.element_type {
        // Explicit type provided: [1:i64, 2, 3]
        infer_type(explicit_type, data)
    } else {
        // Type inferred from first element: [1, 2, 3]
        infer_expr(&array_literal_expr.elements[0], block, data, None)
    };

    // Type check remaining elements against the determined element type
    for (i, element) in array_literal_expr.elements.iter().enumerate().skip(1) {
        let element_type_inferred = infer_expr(element, block, data, None);

        if element_type_inferred != element_type {
            data.errors.push(
                crate::pass_diagnostics::error::YuuError::builder()
                    .kind(crate::pass_diagnostics::error::ErrorKind::TypeMismatch)
                    .message(format!(
                        "Array element {} has type {}, but expected {}",
                        i, element_type_inferred, element_type
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(element.span(), format!("element {} has wrong type", i))
                    .build(),
            );
        }
    }

    // Array literals are treated as pointers to the element type
    let result_type = element_type.ptr_to();

    data.type_registry
        .type_info_table
        .insert(array_literal_expr.id, result_type);

    result_type
}
