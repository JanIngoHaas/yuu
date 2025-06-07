use crate::pass_diagnostics::{ErrorKind, YuuError, create_no_overload_error};
use crate::pass_parse::add_ids::GetId;
use crate::{
    pass_parse::ast::{
        AssignmentExpr, BinaryExpr, BlockExpr, ExprNode, FuncCallExpr, IdentExpr, IfExpr,
        LiteralExpr, Spanned, UnaryExpr,
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

use super::pass_type_inference::TransientData;

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
                &data.type_registry,
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
                &data.type_registry,
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
                &ident_expr.ident.as_str(),
                err,
                args,
                &data.type_registry,
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
    return error_type();
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
        if let super::ExitKind::Break = out {
            // Store break type but keep processing
            span_break_ty = Some((stmt.span(), inactive_type()));
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
        ExprNode::Block(block_expr) => infer_block(block_expr, block, data),
        ExprNode::FuncCall(func_call) => infer_func_call(func_call, block, data),
        ExprNode::If(if_expr) => infer_if_expr(if_expr, block, data),
        ExprNode::Assignment(assignment) => infer_assignment(assignment, block, data),
        ExprNode::StructInstantiation(struct_instantiation_expr) => {
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
                // Get the already inferred expression type from the registry
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

            // Register the type for the struct instantiation expression
            data.type_registry
                .type_info_table
                .insert(struct_instantiation_expr.id, struct_type);

            struct_type
        }
        ExprNode::While(while_expr) => {
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
                // Continue execution but mark as error
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
                            // TODO: I should collect "breaks" in a separate datastructure so we can pinpoint the breaks which are incompatible and have a better error message
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
    }
}
