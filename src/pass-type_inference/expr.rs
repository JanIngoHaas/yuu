use indexmap::IndexSet;

use crate::pass_diagnostics::{ErrorKind, YuuError, create_no_overload_error};
use crate::pass_parse::RefutablePatternNode;
use crate::pass_parse::add_ids::GetId;
use crate::pass_type_inference::{ExitKind, match_binding_node_to_type};
use crate::{
    pass_parse::ast::{
        AssignmentExpr, BinaryExpr, BlockExpr, EnumInstantiationExpr, ExprNode, FuncCallExpr,
        IdentExpr, IfExpr, LiteralExpr, MatchExpr, MemberAccessExpr, Spanned,
        StructInstantiationExpr, UnaryExpr, WhileExpr,
    },
    pass_type_inference::binding_info::BindingInfo,
    pass_type_inference::type_info::{
        TypeInfo, error_type, inactive_type, primitive_bool, primitive_f32, primitive_f64,
        primitive_i64, primitive_nil,
    },
    pass_yir_lowering::block::Block,
};
// const MAX_SIMILAR_NAMES: u64 = 3;
// const MIN_DST_SIMILAR_NAMES: u64 = 3;

use super::pass_type_inference_impl::TransientData;

fn infer_literal(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
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

fn infer_binary(
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

fn infer_unary(
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

fn infer_ident(
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

pub fn infer_block_no_child_creation(
    block_expr: &BlockExpr,
    root_func_block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let mut span_break_ty = None;

    for stmt in &block_expr.body {
        let out = super::infer_stmt(stmt, root_func_block, data);
        // This is for when we break OUT of the current block!
        match out {
            ExitKind::Break => {
                // Store break type but keep processing
                span_break_ty = Some((stmt.span(), inactive_type()));
            }
            ExitKind::Proceed => {}
        }
    }

    let (span, block_ty) = match span_break_ty {
        Some((span, ty)) => (Some(span), ty),
        None => (None, primitive_nil()),
    };

    // Unify with any existing type from breaks TO this block

    (match data
        .type_registry
        .type_info_table
        .unify_and_insert(block_expr.id, block_ty)
    {
        Ok(ty) => ty,
        Err(err) => {
            let mut err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Block has inconsistent return types: '{}' and '{}'",
                    err.left, err.right
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    block_expr.span.clone(),
                    "block with inconsistent return types",
                );

            if let Some(span) = span {
                err_msg = err_msg.label(span, format!("block returns '{}' here", block_ty));
            }

            let err_msg = err_msg
                .help("All branches of a block must evaluate to compatible types")
                .build();

            // Add label for break statement if found
            // let err_msg = if let Some(span) = break_with_value {
            //     err_msg.label(span, format!("break with value of type {}", err.left))
            //         .label(
            //             block_expr.last_expr.as_ref().map_or(
            //                 block_expr.span.clone(),
            //                 |expr| expr.span().clone()
            //             ),
            //             format!("block yields {}", err.right)
            //         )
            //         .help("Break statements with values must match the block's return type")
            // } else {
            //     err_msg.help(
            //         "This can happen if you have a break statement returning a value that doesn't match the block's yield type"
            //     )
            // }.build();

            data.errors.push(err_msg);
            error_type()
        }
    }) as _
}

pub fn infer_block(
    block_expr: &BlockExpr,
    parent_block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let child_block = if let Some(label) = &block_expr.label {
        let binding = BindingInfo {
            id: block_expr.id,
            src_location: Some(block_expr.span.clone()),
        };
        parent_block.make_child((Some(*label), binding))
    } else {
        parent_block.make_child((
            None,
            BindingInfo {
                id: block_expr.id,
                src_location: Some(block_expr.span.clone()),
            },
        ))
    };

    infer_block_no_child_creation(block_expr, child_block, data)
}

fn infer_func_call(
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
            // This is a compiler bug, so keep the panic
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

fn infer_while(
    while_expr: &WhileExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let cond_ty = infer_expr(&while_expr.condition_block.condition, block, data, None);
    let while_expr_ty = infer_block(&while_expr.condition_block.body, block, data);

    if let Err(err) = cond_ty.unify(primitive_bool()) {
        let err_msg = YuuError::builder()
            .kind(ErrorKind::TypeMismatch)
            .message(format!(
                "While condition must be of type 'bool', got '{}'",
                err.left
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                while_expr.condition_block.condition.span().clone(),
                format!("has type '{}'", err.left),
            )
            .help("Conditions must evaluate to a boolean type")
            .build();
        data.errors.push(err_msg);
        data.type_registry
            .type_info_table
            .insert(while_expr.id, error_type());
        return error_type();
    }

    let unify_res = data
        .type_registry
        .type_info_table
        .unify_and_insert(while_expr.id, while_expr_ty);

    match unify_res {
        Ok(ty) => ty,
        Err(err) => {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message("While loop has inconsistent break types")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    while_expr.condition_block.body.span.clone(),
                    format!("Cannot unify type '{}' and type '{}'", err.left, err.right),
                )
                .help("All break statements within a while loop must have compatible types")
                .build();
            data.errors.push(err_msg);

            data.type_registry
                .type_info_table
                .insert(while_expr.id, error_type());
            error_type()
        }
    }
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
                    match (&ei.data, variant.variant) {
                        (Some(dexpr), Some(reg_ty)) => {
                            let ty = infer_expr(&dexpr, block, data, function_args);
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
                        (None, None) => {
                            // Nothing to do here...
                        }
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

fn infer_match(me: &MatchExpr, block: &mut Block, data: &mut TransientData) -> &'static TypeInfo {
    let scrutinee_ty = infer_expr(&me.scrutinee, block, data, None);

    // Check if the scrutinee type is a refutable pattern
    if !scrutinee_ty.is_refutable_pattern() {
        let err = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
            .message(format!(
                "Cannot match on type '{}' because it is not a refutable pattern",
                scrutinee_ty
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                me.scrutinee.span().clone(),
                format!("this expression has type '{}'", scrutinee_ty),
            )
            .label(
                me.span.clone(),
                "match expression requires a refutable pattern",
            )
            .help("Match expressions can only be used with refutable patterns like enums. Consider using an if-let expression for other types, or ensure the scrutinee is an enum type.")
            .build();
        data.errors.push(err);
        data.type_registry
            .type_info_table
            .insert(me.id, error_type());
        return error_type();
    }

    // Check if we have exhaustive match arms + each arm is of type scrutinee_ty

    let mut matches = IndexSet::new();

    for arm in &me.arms {
        match &*arm.pattern {
            RefutablePatternNode::Enum(enum_pattern) => {
                let enum_info = data.type_registry.resolve_enum(enum_pattern.enum_name);

                match enum_info {
                    Some(enum_info) => {
                        // Check if the enum is the same as the scrutinee type
                        if !scrutinee_ty.is_exact_same_type(enum_info.ty) {
                            let err = YuuError::builder()
                                .kind(ErrorKind::TypeMismatch)
                                .message(format!(
                                    "Cannot match enum pattern '{}' against scrutinee of type '{}'",
                                    enum_info.ty, scrutinee_ty
                                ))
                                .source(
                                    data.src_code.source.clone(),
                                    data.src_code.file_name.clone(),
                                )
                                .span(
                                    enum_pattern.span.clone(),
                                    format!("pattern expects type '{}'", enum_info.ty),
                                )
                                .label(
                                    me.scrutinee.span().clone(),
                                    format!("scrutinee has type '{}'", scrutinee_ty),
                                )
                                .help("All match patterns must be compatible with the scrutinee type. Ensure the enum pattern matches the type being matched on.")
                                .build();
                            data.errors.push(err);
                            continue;
                        }

                        match enum_info.variants.get(&enum_pattern.variant_name) {
                            Some(variant_info) => {
                                if !matches.insert(variant_info.variant_name) {
                                    let err = YuuError::builder()
                                        .kind(ErrorKind::InvalidExpression)
                                        .message(format!(
                                            "Duplicate match arm for enum variant '{}::{}'",
                                            enum_pattern.enum_name, enum_pattern.variant_name
                                        ))
                                        .source(
                                            data.src_code.source.clone(),
                                            data.src_code.file_name.clone(),
                                        )
                                        .span(
                                            enum_pattern.span.clone(),
                                            "duplicate pattern",
                                        )
                                        .help(format!(
                                            "The variant '{}::{}' has already been matched in a previous arm. Remove this duplicate pattern or match on a different variant.",
                                            enum_pattern.enum_name, enum_pattern.variant_name
                                        ))
                                        .build();
                                    data.errors.push(err);
                                    continue;
                                }

                                if let Some(binding) = &enum_pattern.binding {
                                    match variant_info.variant {
                                        Some(var_ty) => {
                                            // Match the binding node to the type of the enum variant
                                            match_binding_node_to_type(
                                                block, binding, var_ty, data,
                                            );
                                        }
                                        None => {
                                            let err = YuuError::builder()
                                                .kind(ErrorKind::InvalidExpression)
                                                .message(format!(
                                                    "Cannot bind value from unit enum variant '{}::{}'",
                                                    enum_pattern.enum_name, enum_pattern.variant_name
                                                ))
                                                .source(
                                                    data.src_code.source.clone(),
                                                    data.src_code.file_name.clone(),
                                                )
                                                .span(
                                                    binding.span().clone(),
                                                    "binding not allowed here",
                                                )
                                                .label(
                                                    enum_pattern.span.clone(),
                                                    "unit variant has no data to bind",
                                                )
                                                .help(format!(
                                                    "The variant '{}::{}' is a unit variant (has no associated data). Remove the binding or use a different variant that carries data.",
                                                    enum_pattern.enum_name, enum_pattern.variant_name
                                                ))
                                                .build();
                                            data.errors.push(err);
                                        }
                                    }
                                }
                            }
                            None => {
                                let err = YuuError::builder()
                                    .kind(ErrorKind::ReferencedUndeclaredVariant)
                                    .message(format!(
                                        "Enum '{}' has no variant named '{}'",
                                        enum_pattern.enum_name, enum_pattern.variant_name
                                    ))
                                    .source(
                                        data.src_code.source.clone(),
                                        data.src_code.file_name.clone(),
                                    )
                                    .span(
                                        enum_pattern.span.clone(),
                                        "undefined variant used in pattern",
                                    )
                                    .help(format!(
                                        "Available variants for enum '{}': {}",
                                        enum_pattern.enum_name,
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
                            }
                        }
                    }
                    None => {
                        let err = YuuError::builder()
                            .kind(ErrorKind::ReferencedUndefinedEnum)
                            .message(format!(
                                "Cannot match on enum '{}' because it has not been defined",
                                enum_pattern.enum_name
                            ))
                            .source(
                                data.src_code.source.clone(),
                                data.src_code.file_name.clone(),
                            )
                            .span(enum_pattern.span.clone(), "undefined enum used in pattern")
                            .help("Define this enum before using it in a match expression")
                            .build();
                        data.errors.push(err);
                    }
                }
            }
        }
    }

    // Now, check for exhaustiveness

    match scrutinee_ty {
        TypeInfo::Enum(enum_ty) => {
            let enum_info = data.type_registry.resolve_enum(enum_ty.name);

            // Check if default is set -> immediately exhaustive - currently doesn't exist...
            // if me.default.is_some() {
            //     todo!("TODO: Claude: What should I do here?")
            // }

            match enum_info {
                Some(enum_info) => {
                    // Check for exhaustiveness - happy path -> no allocation here
                    let mut is_exhaustive = true;
                    for variant_name in enum_info.variants.keys() {
                        if !matches.contains(variant_name) {
                            is_exhaustive = false;
                            break;
                        }
                    }

                    if !is_exhaustive {
                        // Only now allocate the vector since we know there's an error - sad path
                        let missing_variants: Vec<&str> = enum_info
                            .variants
                            .keys()
                            .filter(|variant_name| !matches.contains(*variant_name))
                            .map(|v| v.as_str())
                            .collect();
                        let err = YuuError::builder()
                            .kind(ErrorKind::InvalidExpression)
                            .message(format!(
                                "Match expression is not exhaustive, missing variant(s): {}",
                                missing_variants.join(", ")
                            ))
                            .source(
                                data.src_code.source.clone(),
                                data.src_code.file_name.clone(),
                            )
                            .span(
                                me.span.clone(),
                                "non-exhaustive match expression",
                            )
                            .help(format!(
                                "Add match arms for the missing variant(s): {}. All possible variants of the enum must be covered in a match expression.",
                                missing_variants.join(", ")
                            ))
                            .build();
                        data.errors.push(err);
                        data.type_registry
                            .type_info_table
                            .insert(me.id, error_type());
                        return error_type();
                    }
                }
                None => {
                    // This should never happen since we already checked that scrutinee_ty is a refutable pattern
                    // and the only refutable pattern currently supported is Enum, so the enum must exist
                    unreachable!(
                        "Compiler bug: Enum '{}' should exist since scrutinee type is '{}' which passed is_refutable_pattern()",
                        match scrutinee_ty {
                            TypeInfo::Enum(e) => e.name.as_str(),
                            _ => "unknown",
                        },
                        scrutinee_ty
                    );
                }
            }
        }
        _ => unreachable!(
            "Compiler bug: Match expression should only be used with refutable patterns, but got '{}' - this should have been checked earlier",
            scrutinee_ty
        ),
    }

    let unified = me.arms.iter().fold(inactive_type(), |acc, arm| {
        let arm_ty = infer_expr(&arm.body, block, data, None);

        let unified = acc.unify(arm_ty);

        match unified {
            Ok(ty) => return ty,
            Err(err) => {
                let err_msg = YuuError::builder()
                    .kind(ErrorKind::TypeMismatch)
                    .message(format!(
                        "Match arm has incompatible type: cannot unify '{}' with '{}'",
                        err.left.to_string(),
                        err.right.to_string()
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(arm.body.span(), "this match arm has an incompatible type")
                    .help(format!(
                        "All match arms must return the same type. Expected '{}' but this arm returns '{}'. Consider converting the values to a common type or using a different approach.",
                        err.left.to_string(),
                        err.right.to_string()
                    ))
                    .build();
                data.errors.push(err_msg);
                return error_type();
            }
        }
    });

    // Insert the unified type into the type registry
    let overall_unified = data
        .type_registry
        .type_info_table
        .unify_and_insert(me.id, unified);

    match overall_unified {
        Ok(ty) => return ty,
        Err(err) => {
            // This can happen when break statements in nested expressions within match arms
            // cause type inconsistencies that aren't caught during the fold operation
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Match arms have incompatible types: cannot unify '{}' with '{}'",
                    err.left.to_string(), err.right.to_string()
                ))
                .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                .span(me.span.clone(), "match expression with incompatible arms")
                .help(format!(
                    "All match arms must evaluate to the same type. This error commonly occurs when:\n\
                    1. Different arms return different types ('{}' vs '{}')\n\
                    2. Break statements within nested expressions cause type conflicts\n\
                    3. Control flow statements jump out of deeply nested expressions\n\n\
                    Consider:\n\
                    - Ensuring all arms return the same type\n\
                    - Avoiding break statements in nested match arm expressions",
                    err.left.to_string(), err.right.to_string()
                ))
                .build();
            data.errors.push(err_msg);
            return error_type();
        }
    }
}

fn infer_if_expr(expr: &IfExpr, block: &mut Block, data: &mut TransientData) -> &'static TypeInfo {
    // Check condition
    let src_code = data.src_code.clone();

    let cond_ty = infer_expr(&expr.if_block.condition, block, data, None);
    if let Err(err) = cond_ty.unify(primitive_bool()) {
        let err_msg = YuuError::builder()
            .kind(ErrorKind::TypeMismatch)
            .message(format!(
                "If condition must be of type bool, got {}",
                err.left
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                expr.if_block.condition.span().clone(),
                format!("has type {}", err.left),
            )
            .help("Conditions must evaluate to a boolean type")
            .build();
        data.errors.push(err_msg);
        // Continue execution but mark as error
        data.type_registry
            .type_info_table
            .insert(expr.id, error_type());
        return error_type();
    }

    let then_ty = infer_block(&expr.if_block.body, block, data);

    let if_types = expr.else_if_blocks.iter().map(|x| {
        // Check if the else-if condition is a boolean
        let else_if_cond_ty = infer_expr(&x.condition, block, data, None);
        if let Err(err) = else_if_cond_ty.unify(primitive_bool()) {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Else-if condition must be of type 'bool', got '{}'",
                    err.left
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    x.condition.span().clone(),
                    format!("has type '{}'", err.left),
                )
                .help("Conditions must evaluate to a boolean type")
                .build();
            data.errors.push(err_msg);

            // We mark as error type.
            data.type_registry
                .type_info_table
                .insert(x.condition.node_id(), error_type());

            return error_type();
        }

        infer_block(&x.body, block, data)
    });

    let (out_ty, errors) = if_types.into_iter().enumerate().fold(
        (then_ty, Vec::new()),
        move |(acc, mut errors), (block_index, ty)| {
            if matches!(acc, TypeInfo::Inactive) {
                return (ty, errors);
            }

            // Unify the types and use the unified type
            match acc.unify(ty) {
                Ok(unified) => (unified, errors),
                Err(_) => {
                    let err_msg = YuuError::builder()
                        .kind(ErrorKind::TypeMismatch)
                        .message("If expression branches have incompatible types".to_string())
                        .source(src_code.source.clone(), src_code.file_name.clone())
                        .span(expr.span.clone(), "if with incompatible branch types")
                        .label(
                            expr.if_block.body.span.clone(),
                            format!("this branch returns {}", acc),
                        )
                        .label(
                            expr.else_if_blocks[block_index].body.span.clone(),
                            format!("this branch returns {}", ty),
                        )
                        .help("All branches of an if expression must evaluate to compatible types")
                        .build();
                    errors.push(err_msg);
                    (error_type(), errors)
                }
            }
        },
    );

    data.errors.extend(errors);

    if let Some(else_body) = expr.else_block.as_ref() {
        let else_ty = infer_block(else_body, block, data);
        match out_ty.unify(else_ty) {
            Ok(unified) => {
                data.type_registry.type_info_table.insert(expr.id, unified);
                unified
            }
            Err(_) => {
                let err_msg = YuuError::builder()
                    .kind(ErrorKind::TypeMismatch)
                    .message("If expression branches have incompatible types")
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(expr.span.clone(), "if with incompatible branch types")
                    .label(
                        expr.if_block.body.span.clone(),
                        format!("if branch returns {}", out_ty),
                    )
                    .label(
                        else_body.span.clone(),
                        format!("else branch returns {}", else_ty),
                    )
                    .help("All branches of an if expression must evaluate to compatible types")
                    .build();
                data.errors.push(err_msg);
                error_type()
            }
        }
    } else {
        data.type_registry.type_info_table.insert(expr.id, out_ty);
        out_ty
    }
}

pub fn infer_expr(
    expr: &ExprNode,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    match expr {
        ExprNode::Literal(lit) => infer_literal(lit, data),
        ExprNode::Binary(binary) => infer_binary(binary, block, data),
        ExprNode::Unary(unary) => infer_unary(unary, block, data),
        ExprNode::Ident(ident) => infer_ident(ident, block, data, function_args),
        ExprNode::FuncCall(func_call) => infer_func_call(func_call, block, data),
        ExprNode::Assignment(assignment) => infer_assignment(assignment, block, data),
        ExprNode::StructInstantiation(struct_instantiation_expr) => {
            infer_struct_instantiation(struct_instantiation_expr, block, data)
        }

        ExprNode::MemberAccess(member_access_expr) => {
            infer_member_access(member_access_expr, block, data)
        }

        ExprNode::EnumInstantiation(ei) => infer_enum_instantiation(ei, block, data, function_args),
        ExprNode::Match(me) => infer_match(me, block, data),
    }
}
