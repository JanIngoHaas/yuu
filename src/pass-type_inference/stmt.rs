use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{BlockStmt, IfStmt, LetStmt, MatchStmt, ReturnStmt, Spanned, StmtNode, WhileStmt},
    pass_type_inference::{
        BindingInfo,
        type_info::{TypeInfo, error_type, primitive_bool, primitive_nil},
    },
    pass_yir_lowering::block::Block,
};

use super::{
    infer_binding, infer_expr, pass_type_inference_impl::TransientData, pattern::infer_pattern,
};

fn infer_let_stmt(let_stmt: &LetStmt, block: &mut Block, data: &mut TransientData) {
    let ty_expr = infer_expr(&let_stmt.expr, block, data, None);

    // Prevent binding value-less expressions (Inactive)
    if matches!(ty_expr, TypeInfo::Inactive) {
        let err = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
            .message("Cannot bind a value-less expression to a variable".to_string())
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                let_stmt.expr.span().clone(),
                "this expression doesn't produce a value",
            )
            .help("This happens when you try to bind to a statement. Expressions should generally evaluate to a value.".to_string())
            .build();
        data.errors.push(err);
    }

    // If the expression already has an error type, don't emit another
    // diagnostic here, because the original error has already been reported.
    // We simply propagate the error type to the binding.

    infer_binding(block, &let_stmt.binding, ty_expr, data);
}

fn infer_return_stmt(return_stmt: &ReturnStmt, block: &mut Block, data: &mut TransientData) {
    if let Some(expr) = return_stmt.expr.as_ref() {
        let return_ty = infer_expr(expr, block, data, None);
        let unification = return_ty.unify(data.current_function_return_type);

        // Unify return_ty with function's declared return type
        if let Err(err) = unification {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Return type mismatch: expected '{}', found '{}'",
                    data.current_function_return_type, err.left
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    expr.span().clone(),
                    format!("expression has type '{}'", err.left),
                )
                .help("Return expression must match function's declared return type")
                .build();
            data.errors.push(err_msg);
        };
        return;
    } else {
        // We return "nil"; check if that is also the function's return type...
        if let Err(_err) = primitive_nil().unify(data.current_function_return_type) {
            let error = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Function expects return type '{}', but no explicit return value provided (implicitly returns nil)",
                    data.current_function_return_type
                ))
                .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                .span(return_stmt.span.clone(), "implicit nil return")
                .help("Either add an explicit return value or change the function return type to 'nil'")
                .build();
            data.errors.push(error);
        }
    }
}

pub fn infer_stmt(stmt: &StmtNode, block: &mut Block, data: &mut TransientData) {
    match stmt {
        StmtNode::Let(let_stmt) => infer_let_stmt(let_stmt, block, data),
        StmtNode::Atomic(expr) => {
            let _ = infer_expr(expr, block, data, None);
        }

        StmtNode::Break(_exit) => {
            // TODO: Create a semantic analysis micro pass to validate break statements are inside loops
        }
        StmtNode::If(if_stmt) => infer_if_stmt(if_stmt, block, data),
        StmtNode::While(while_stmt) => infer_while_stmt(while_stmt, block, data),
        StmtNode::Block(block_stmt) => infer_block_stmt(block_stmt, block, data),
        StmtNode::Match(match_stmt) => infer_match_stmt(match_stmt, block, data),
        StmtNode::Return(return_stmt) => infer_return_stmt(return_stmt, block, data),
        StmtNode::Error(_stmt) => { /* Even if we have an error in this context, the return stmt itself still produces no value. */
        }
    }
}

fn infer_if_stmt(if_stmt: &IfStmt, block: &mut Block, data: &mut TransientData) {
    // Check condition
    let cond_ty = infer_expr(&if_stmt.if_block.condition, block, data, None);
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
                if_stmt.if_block.condition.span().clone(),
                format!("has type {}", err.left),
            )
            .help("Conditions must evaluate to a boolean type")
            .build();
        data.errors.push(err_msg);
        data.type_registry
            .type_info_table
            .insert(if_stmt.id, error_type());
    }

    // Infer the if body
    infer_block_stmt(&if_stmt.if_block.body, block, data);

    // Infer else-if blocks
    for else_if_block in &if_stmt.else_if_blocks {
        let cond_ty = infer_expr(&else_if_block.condition, block, data, None);
        if let Err(err) = cond_ty.unify(primitive_bool()) {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeMismatch)
                .message(format!(
                    "Else-if condition must be of type bool, got {}",
                    err.left
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    else_if_block.condition.span().clone(),
                    format!("has type {}", err.left),
                )
                .help("Conditions must evaluate to a boolean type")
                .build();
            data.errors.push(err_msg);
        }
        infer_block_stmt(&else_if_block.body, block, data);
    }

    // Infer else block
    if let Some(else_block) = &if_stmt.else_block {
        infer_block_stmt(else_block, block, data);
    }
}

fn infer_while_stmt(while_stmt: &WhileStmt, block: &mut Block, data: &mut TransientData) {
    let cond_ty = infer_expr(&while_stmt.condition_block.condition, block, data, None);

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
                while_stmt.condition_block.condition.span().clone(),
                format!("has type '{}'", err.left),
            )
            .help("Conditions must evaluate to a boolean type")
            .build();
        data.errors.push(err_msg);
        data.type_registry
            .type_info_table
            .insert(while_stmt.id, error_type());
    }

    // Infer the while body
    infer_block_stmt(&while_stmt.condition_block.body, block, data);
}

pub fn infer_block_stmt(block_stmt: &BlockStmt, block: &mut Block, data: &mut TransientData) {
    // Create a new child scope for the block
    let child_block = block.make_child((
        block_stmt.label,
        BindingInfo {
            id: block_stmt.id,
            src_location: Some(block_stmt.span.clone()),
        },
    ));

    // Process each statement in the block
    for stmt in &block_stmt.body {
        infer_stmt(stmt, child_block, data);
    }
}

fn infer_match_stmt(match_stmt: &MatchStmt, block: &mut Block, data: &mut TransientData) {
    // Infer the scrutinee expression
    let scrutinee_ty = infer_expr(&match_stmt.scrutinee, block, data, None);

    // Collect covered variants for exhaustiveness checking
    let mut covered_variants = indexmap::IndexSet::new();
    
    // Process each match arm
    for arm in &match_stmt.arms {
        // Extract variant name if this is an enum pattern
        if let crate::pass_parse::RefutablePatternNode::Enum(enum_pattern) = arm.pattern.as_ref() {
            covered_variants.insert(enum_pattern.variant_name);
        }
        
        // Infer pattern types and handle bindings
        infer_pattern(
            &arm.pattern,
            &scrutinee_ty,
            &match_stmt.scrutinee,
            block,
            data,
        );

        // Infer the arm body
        infer_block_stmt(&arm.body, block, data);
    }

    // Check exhaustiveness if no default case
    if match_stmt.default_case.is_none() {
        if let TypeInfo::Enum(enum_type) = scrutinee_ty {
            if let Some(enum_info) = data.type_registry.resolve_enum(enum_type.name) {
                let all_variants: indexmap::IndexSet<_> = enum_info.variants.keys().copied().collect();
                let missing_variants: Vec<_> = all_variants.difference(&covered_variants)
                    .map(|v| format!("'{}'", v))
                    .collect();
                
                if !missing_variants.is_empty() {
                    let missing_list = missing_variants.join(", ");
                    let error = YuuError::builder()
                        .kind(ErrorKind::InvalidStatement)
                        .message(format!("Non-exhaustive match: missing variants {}", missing_list))
                        .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                        .span(match_stmt.span.clone(), "match statement is not exhaustive")
                        .help("Either add cases for the missing variants or add a 'default:' case")
                        .build();
                    data.errors.push(error);
                }
            }
        }
    }

    // Process default case if present
    if let Some(default_case) = &match_stmt.default_case {
        infer_block_stmt(default_case, block, data);
    }

    // Match statements produce nil
    data.type_registry
        .type_info_table
        .insert(match_stmt.id, primitive_nil());
}
