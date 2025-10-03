use crate::pass_diagnostics::{ErrorKind, YuuError, create_no_overload_error};
use crate::pass_parse::add_ids::GetId;
use crate::{
    pass_parse::ast::{
        AssignmentExpr, BinaryExpr, EnumInstantiationExpr, ExprNode, FuncCallExpr,
        IdentExpr, LiteralExpr, MemberAccessExpr, Spanned, StructInstantiationExpr, UnaryExpr,
    },
    pass_type_inference::type_info::{
        TypeInfo, error_type, primitive_f32, primitive_f64, primitive_i64,
    },
    pass_yir_lowering::block::Block,
};
// const MAX_SIMILAR_NAMES: u64 = 3;
// const MIN_DST_SIMILAR_NAMES: u64 = 3;

use super::pass_type_inference_impl::TransientData;

fn infer_literal_expr(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
    let out = match lit.lit.kind {
        crate::pass_parse::token::TokenKind::Integer(integer) => match integer {
            crate::pass_parse::token::Integer::I64(_) => primitive_i64(),
        },
        crate::pass_parse::token::TokenKind::F32(_) => primitive_f32(),
        crate::pass_parse::token::TokenKind::F64(_) => primitive_f64(),
        _ => unreachable!("Compiler bug: Literal not implemented"),
    };
    data.type_registry.add_literal(lit.id, out);
    out
}

fn infer_binary_expr(
    binary_expr: &BinaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let lhs = infer_expr(&binary_expr.left, block, data, None);
    let rhs = infer_expr(&binary_expr.right, block, data, None);

    let op_name = binary_expr.op.static_name();

    let resolution = match data.type_registry.resolve_function(op_name, &[lhs, rhs]) {
        Ok(res) => res,
        Err(err) => {
            let err = create_no_overload_error(
                &op_name,
                err,
                &[lhs, rhs],
                data.type_registry,
                &data.src_code,
                binary_expr.span.clone(),
            );

            data.errors.push(err);
            data.type_registry
                .type_info_table
                .insert(binary_expr.id, error_type());
            return error_type();
        }
    };

    data.type_registry
        .bindings
        .insert(binary_expr.id, resolution.binding_info.id);
    data.type_registry
        .type_info_table
        .insert(binary_expr.id, resolution.ty.ret);
    resolution.ty.ret
}

fn infer_unary_expr(
    unary_expr: &UnaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let ty = infer_expr(&unary_expr.operand, block, data, None);
    let op_name = unary_expr.op.static_name();

    // Replace resolve_function with resolve_function_call
    let resolution = match data.type_registry.resolve_function(op_name, &[ty]) {
        Ok(res) => res,
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
            data.type_registry
                .type_info_table
                .insert(unary_expr.id, error_type());
            return error_type();
        }
    };

    data.type_registry
        .bindings
        .insert(unary_expr.id, resolution.binding_info.id);
    data.type_registry
        .type_info_table
        .insert(unary_expr.id, resolution.ty.ret);
    resolution.ty.ret
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
        TypeInfo::Inactive => {
            // This is a compiler bug - kinda weird...
            panic!("Compiler bug: Inactive type as return type of function")
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
    // let binding = &assignment_expr.lhs;
    // // match_binding_node_to_type(binding, block, ty, data) -> we probably need to call this somehow
    // let binding_ty = data
    //     .type_registry
    //     .type_info_table
    //     .get(binding.node_id())
    //     .expect("Compiler bug: binding not found in type table");

    // TODO: Check mutability - We need to do this in another pass!
    //     let err = YuuError::builder()
    //         .kind(ErrorKind::InvalidExpression)
    //         .message("Cannot assign to immutable binding")
    //         .source(
    //             data.src_code.source.clone(),
    //             data.src_code.file_name.clone(),
    //         )
    //         .span(
    //             assignment_expr.span.clone(),
    //             "assignment to immutable binding",
    //         )
    //         .help("Consider adding 'mut' when declaring this binding")
    //         .build();
    //     data.errors.push(err);
    //     return error_type();

    let ty_lhs = infer_expr(&assignment_expr.lhs, block, data, None);
    let ty_rhs = infer_expr(&assignment_expr.rhs, block, data, None);

    // Prevent binding inactive types
    if matches!(ty_rhs, TypeInfo::Inactive) {
        let err = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
            .message("Cannot bind a value-less expression to a variable")
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                assignment_expr.rhs.span().clone(),
                "this expression doesn't produce a value",
            )
            .help("This happens when all paths in the expression return or break")
            .build();
        data.errors.push(err);
        return error_type();
    }

    // First unify to check compatibility
    let unified = match ty_lhs.unify(ty_rhs) {
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

    // Then insert the unified type and return it

    (match data
        .type_registry
        .type_info_table
        .unify_and_insert(assignment_expr.id, unified)
    {
        Ok(ty) => ty,
        Err(_err) => {
            // This should never happen if the previous unify worked, but just in case
            unreachable!("Compiler bug: We should never have inconsistent types here");
        }
    }) as _
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

        ExprNode::Deref(_deref_expr) => {
            // TODO: Implement pointer dereference type inference
            todo!("Pointer dereference not yet implemented")
        }
    }
}
