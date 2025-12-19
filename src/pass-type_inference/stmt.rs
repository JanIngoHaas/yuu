use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{
        BindingNode, BlockStmt, DeclStmt, DefStmt, IfStmt, LetStmt, MatchStmt,
        RefutablePatternNode, ReturnStmt, Spanned, StmtNode, WhileStmt,
    },
    utils::{
        BindingInfo, BlockTree, collections::UstrIndexSet,
        type_info_table::{TypeInfo, error_type, primitive_bool, primitive_nil, unknown_type},
    },
};

use super::{
    infer_binding, infer_expr, pass_type_inference_impl::TransientDataStructural,
    pattern::infer_pattern,
};

fn infer_let_stmt(let_stmt: &LetStmt, block_id: usize, data: &mut TransientDataStructural) {
    let ty_expr = infer_expr(&let_stmt.expr, block_id, data, None);

    // If the expression already has an error type, don't emit another
    // diagnostic here, because the original error has already been reported.
    // We simply propagate the error type to the binding.

    infer_binding(&let_stmt.binding, block_id, ty_expr, data);
}

fn infer_decl_stmt(decl_stmt: &DeclStmt, block_id: usize, data: &mut TransientDataStructural) {
    infer_binding(
        &BindingNode::Ident(decl_stmt.ident.clone()),
        block_id,
        unknown_type(),
        data,
    );
}

fn infer_def_stmt(def_stmt: &DefStmt, block_id: usize, data: &mut TransientDataStructural) {
    let expr_type = infer_expr(&def_stmt.expr, block_id, data, None);
    let var = data.block_tree.resolve_variable(
        block_id,
        def_stmt.ident.name,
        &data.src_code,
        def_stmt.ident.span.clone(),
    );

    match var {
        Ok(var) => {
            // Check if the variable was already properly defined (i.e., has a non-unknown type)
            let current_type = data
                .type_info_table
                .get(var.binding_info.id)
                .unwrap_or(unknown_type());

            if !matches!(current_type, TypeInfo::Unknown) {
                let err = YuuError::builder()
                    .kind(ErrorKind::InvalidStatement)
                    .message(format!(
                        "Variable '{}' is already defined",
                        def_stmt.ident.name
                    ))
                    .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                    .span(def_stmt.ident.span.clone(), "variable already defined here")
                    .help("Use 'def' only once after 'dec'. For rebinding/reassignment, use 'let' or assignment operator")
                    .build();
                data.errors.push(err);
                return;
            }

            data.type_info_table.insert(var.binding_info.id, expr_type); // Update the "unknown" type of the variable
            data.bindings.insert(def_stmt.ident.id, var.binding_info.id); // Map def statement ident to original binding
        }
        Err(err) => {
            data.errors.push(*err);
        }
    }
}

fn infer_return_stmt(
    return_stmt: &ReturnStmt,
    block_id: usize,
    data: &mut TransientDataStructural,
) {
    if let Some(expr) = return_stmt.expr.as_ref() {
        let return_ty = infer_expr(expr, block_id, data, None);
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
                .help("Type of return expression must match function's declared return type")
                .build();
            data.errors.push(err_msg);
        };
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

pub fn infer_stmt(stmt: &StmtNode, block_id: usize, data: &mut TransientDataStructural) {
    match stmt {
        StmtNode::Let(let_stmt) => {
            infer_let_stmt(let_stmt, block_id, data);
        }
        StmtNode::Atomic(expr) => {
            let _ = infer_expr(expr, block_id, data, None);
        }

        StmtNode::Decl(decl_stmt) => {
            infer_decl_stmt(decl_stmt, block_id, data);
        }

        StmtNode::Def(def_stmt) => {
            infer_def_stmt(def_stmt, block_id, data);
        }

        StmtNode::Break(_exit) => {
            // TODO: Create a semantic analysis micro pass to validate break statements are inside loops
        }
        StmtNode::If(if_stmt) => {
            infer_if_stmt(if_stmt, block_id, data);
        }
        StmtNode::While(while_stmt) => {
            infer_while_stmt(while_stmt, block_id, data);
        }
        StmtNode::Block(block_stmt) => {
            infer_block_stmt(block_stmt, block_id, data);
        }
        StmtNode::Match(match_stmt) => {
            infer_match_stmt(match_stmt, block_id, data);
        }
        StmtNode::Return(return_stmt) => {
            infer_return_stmt(return_stmt, block_id, data);
        }
        StmtNode::Defer(defer_stmt) => {
            let _ = infer_expr(&defer_stmt.expr, block_id, data, None);
            data.type_info_table.insert(defer_stmt.id, primitive_nil());
        }
        StmtNode::Error(_stmt) => {}
    }
}

fn infer_if_stmt(if_stmt: &IfStmt, block_id: usize, data: &mut TransientDataStructural) {
    // Check condition
    let cond_ty = infer_expr(&if_stmt.if_block.condition, block_id, data, None);
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
        data.type_info_table.insert(if_stmt.id, error_type());
    }

    // Infer the if body
    infer_block_stmt(&if_stmt.if_block.body, block_id, data);

    // Infer else-if blocks
    for else_if_block in &if_stmt.else_if_blocks {
        let cond_ty = infer_expr(&else_if_block.condition, block_id, data, None);
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
        infer_block_stmt(&else_if_block.body, block_id, data);
    }

    // Infer else block
    if let Some(else_block) = &if_stmt.else_block {
        infer_block_stmt(else_block, block_id, data);
    }
}

fn infer_while_stmt(while_stmt: &WhileStmt, block_id: usize, data: &mut TransientDataStructural) {
    let cond_ty = infer_expr(&while_stmt.condition_block.condition, block_id, data, None);

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
        // Expression doesn't have a valid type
        // data
        //     .type_info_table
        //     .insert(while_stmt.id, error_type());
    }

    // Infer the while body
    infer_block_stmt(&while_stmt.condition_block.body, block_id, data);
}

pub fn infer_block_stmt(
    block_stmt: &BlockStmt,
    block_id: usize,
    data: &mut TransientDataStructural,
) {
    // Create a new child scope for the block
    let child_block_id = data.block_tree.make_child(
        block_id,
        BindingInfo {
            id: block_stmt.id,
            src_location: Some(block_stmt.span.clone()),
        },
    );

    // Process each statement in the block
    for stmt in &block_stmt.body {
        infer_stmt(stmt, child_block_id, data);
    }
}

fn infer_match_stmt(match_stmt: &MatchStmt, block_id: usize, data: &mut TransientDataStructural) {
    // Infer the scrutinee expression
    let scrutinee_ty = infer_expr(&match_stmt.scrutinee, block_id, data, None);

    // Collect covered variants for exhaustiveness checking
    let mut covered_variants = UstrIndexSet::default();

    // Process each match arm
    for arm in &match_stmt.arms {
        // Extract variant name if this is an enum pattern
        match arm.pattern.as_ref() {
            RefutablePatternNode::Enum(enum_pattern) => {
                covered_variants.insert(enum_pattern.variant_name);
            }
        }

        // Infer pattern types and handle bindings

        infer_pattern(
            &arm.pattern,
            &scrutinee_ty,
            &match_stmt.scrutinee,
            block_id,
            data,
        );

        // Infer the arm body
        infer_block_stmt(&arm.body, block_id, data);
    }

    // Check exhaustiveness if no default case
    if match_stmt.default_case.is_none()
        && let TypeInfo::Enum(enum_type) = scrutinee_ty
        && let Some(enum_info) = data.type_registry.resolve_enum(enum_type.name)
    {
        let all_variants: UstrIndexSet = enum_info.variants.keys().copied().collect();
        let missing_variants: Vec<_> = all_variants
            .difference(&covered_variants)
            .map(|v| format!("'{}'", v))
            .collect();

        if !missing_variants.is_empty() {
            let missing_list = missing_variants.join(", ");
            let error = YuuError::builder()
                .kind(ErrorKind::InvalidStatement)
                .message(format!(
                    "Non-exhaustive match: missing variants {}",
                    missing_list
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(match_stmt.span.clone(), "match statement is not exhaustive")
                .help("Either add cases for the missing variants or add a 'default:' case")
                .build();
            data.errors.push(error);
        }
    }

    if let Some(default_case) = &match_stmt.default_case {
        infer_block_stmt(default_case, block_id, data);
    }
}
