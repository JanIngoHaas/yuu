use yuu_parse::add_ids::GetId;
use yuu_shared::{
    ast::{
        AssignmentExpr, BinaryExpr, BlockExpr, BreakStmt, ExprNode, FuncCallExpr, IdentExpr,
        IfExpr, LiteralExpr, Spanned, StmtNode, UnaryExpr,
    },
    binding_info::{BindingInfo, BindingInfoKind},
    block::{Block, IdentResolutionKind, FUNC_BLOCK_NAME},
    error::{ErrorKind, YuuError},
    type_info::{
        error_type, inactive_type, primitive_bool, primitive_f32, primitive_f64, primitive_i64,
        primitive_nil, TypeInfo,
    },
};

const MAX_SIMILAR_NAMES: u64 = 3;
const MIN_DST_SIMILAR_NAMES: u64 = 3;

use super::{match_binding_node_to_type, pass_type_inference::TransientData};

fn infer_literal(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
    let out = match lit.lit.kind {
        yuu_shared::token::TokenKind::Integer(integer) => match integer {
            yuu_shared::token::Integer::I64(_) => primitive_i64(),
        },
        yuu_shared::token::TokenKind::F32(_) => primitive_f32(),
        yuu_shared::token::TokenKind::F64(_) => primitive_f64(),
        _ => unreachable!("Compiler bug: Literal not implemented"),
    };
    data.type_info_table.types.insert(lit.id, out);
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

    let resolution = match block.resolve_ident(
        op_name,
        Some(&[lhs, rhs]),
        data.type_info_table,
        &data.src_code,
        binary_expr.span.clone(),
    ) {
        Ok(res) => res,
        Err(err) => {
            data.errors.push(err);
            data.type_info_table
                .types
                .insert(binary_expr.id, error_type());
            return error_type();
        }
    };

    data.type_info_table
        .types
        .insert(binary_expr.id, resolution.contextual_appropriate_type);
    resolution.contextual_appropriate_type
}

fn infer_unary(
    unary_expr: &UnaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let ty = infer_expr(&unary_expr.operand, block, data, None);
    let op_name = unary_expr.op.static_name();

    // Replace resolve_function with resolve_function_call
    let resolution = match block.resolve_ident(
        op_name,
        Some(&[ty]),
        data.type_info_table,
        &data.src_code,
        unary_expr.span.clone(),
    ) {
        Ok(res) => res,
        Err(err) => {
            data.errors.push(err);
            data.type_info_table
                .types
                .insert(unary_expr.id, error_type());
            return error_type();
        }
    };

    data.type_info_table
        .types
        .insert(unary_expr.id, resolution.contextual_appropriate_type);
    resolution.contextual_appropriate_type
}

fn infer_ident(
    ident_expr: &IdentExpr,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    let fr = match block.resolve_ident(
        &ident_expr.ident,
        function_args,
        data.type_info_table,
        &data.src_code,
        ident_expr.span.clone(),
    ) {
        Ok(fr) => fr,
        Err(err) => {
            data.errors.push(err);
            data.type_info_table
                .types
                .insert(ident_expr.id, error_type());
            return error_type();
        }
    };

    let ty: &'static TypeInfo = match fr.kind {
        IdentResolutionKind::ResolvedAsFunction { func_type } => func_type.into(),
        IdentResolutionKind::ResolvedAsVariable { type_info } => type_info,
    };

    data.type_info_table.types.insert(ident_expr.id, ty);
    data.binding_table.insert(ident_expr.id, fr.binding.id);
    ty
}

pub fn infer_block_no_child_creation(
    block_expr: &BlockExpr,
    root_func_block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let mut break_ty = None;

    for stmt in &block_expr.body {
        let out = super::infer_stmt(stmt, root_func_block, data);
        if let super::ExitKind::Break = out {
            // Store break type but keep processing - we'll unify with last expr
            break_ty = Some(inactive_type());
        }
    }

    // Get last expression type if it exists
    let last_expr_ty = if let Some(last_expr) = &block_expr.last_expr {
        let expr = infer_expr(last_expr, root_func_block, data, None);
        Some((last_expr.span(), expr))
    } else {
        None
    };

    // Determine type from breaks and last expression
    let (span, block_ty) = match (break_ty, last_expr_ty) {
        (Some(brk), Some((span, last))) => (Some(span), brk.unify(last).unwrap_or(inactive_type())),
        (Some(brk), None) => (None, brk),
        (None, Some((span, last))) => (Some(span), last),
        (None, None) => (None, primitive_nil()),
    };

    // Unify with any existing type from breaks TO this block
    let out = match data
        .type_info_table
        .unify_and_insert(block_expr.id, block_ty)
    {
        Ok(ty) => ty,
        Err(err) => {
            // Try to find a break statement that might be causing the issue
            // let break_with_value = block_expr.body.iter().find_map(|stmt| {
            //     if let StmtNode::Break(br) = stmt {
            //         Some(br.span.clone())
            //     } else {
            //         None
            //     }
            // });

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
    };
    out
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
            is_mut: false,
        };
        parent_block.make_child(Some((label.clone(), binding)))
    } else {
        parent_block.make_child(None)
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
        TypeInfo::Error => error_type(), // Propagate existing error
    };

    data.type_info_table
        .types
        .insert(func_call_expr.id, resolved_ret_type);

    resolved_ret_type
}

fn requires_mut(binding: &BindingInfo) -> bool {
    binding.is_mut
}

// This is WRONG: lhs needs to be an expression, not a binding!
// TODO: Fix this
fn infer_assignment(
    assignment_expr: &AssignmentExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let binding = &assignment_expr.binding;
    // match_binding_node_to_type(binding, block, ty, data) -> we probably need to call this somehow
    let binding_ty = data
        .type_info_table
        .types
        .get(&binding.node_id())
        .cloned()
        .expect("Compiler bug: binding not found in type table");

    // Check mutability
    if !binding.is_mut() {
        let err = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
            .message("Cannot assign to immutable binding")
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                assignment_expr.span.clone(),
                "assignment to immutable binding",
            )
            .help("Consider adding 'mut' when declaring this binding")
            .build();
        data.errors.push(err);
        return error_type();
    }

    let value = infer_expr(&assignment_expr.rhs, block, data, None);

    // Prevent binding inactive types
    if matches!(value, TypeInfo::Inactive) {
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
    let unified = match value.unify(binding_ty) {
        Ok(unified) => unified,
        Err(err) => {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Cannot assign {} to binding of type {}",
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
                    binding.span().clone(),
                    format!("expected type {}", err.right),
                )
                .help("The types must be compatible for assignment")
                .build();
            data.errors.push(err_msg);
            error_type()
        }
    };

    // Then insert the unified type and return it
    let out = match data
        .type_info_table
        .unify_and_insert(assignment_expr.id, unified)
    {
        Ok(ty) => ty,
        Err(err) => {
            // This should never happen if the previous unify worked, but just in case
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message("Type inconsistency in assignment")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(assignment_expr.span.clone(), "inconsistent types")
                .build();
            data.errors.push(err_msg);
            error_type()
        }
    };

    out
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
        data.type_info_table.types.insert(expr.id, error_type());
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
            data.type_info_table
                .types
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
                        .message(format!("If expression branches have incompatible types"))
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
                data.type_info_table.types.insert(expr.id, unified);
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
        data.type_info_table.types.insert(expr.id, out_ty);
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
    }
}
