use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{BlockStmt, IfStmt, Spanned, StmtNode, WhileStmt},
    pass_type_inference::type_info::{error_type, primitive_bool},
    pass_yir_lowering::block::Block,
};

use super::{infer_block, infer_expr, match_binding_node_to_type, pass_type_inference_impl::TransientData};

pub enum ExitKind {
    Break,
    Proceed,
}

pub fn infer_stmt(stmt: &StmtNode, block: &mut Block, data: &mut TransientData) -> ExitKind {
    match stmt {
        StmtNode::Let(let_stmt) => {
            let ty_expr = infer_expr(&let_stmt.expr, block, data, None);

            if ty_expr.is_inactive() {
                // TODO: Claude: Error here, as we can not bind inactive (never) type - see below
                panic!("Cannot bind inactive type")
            }

            match_binding_node_to_type(block, &let_stmt.binding, ty_expr, data);
            ExitKind::Proceed
        }
        StmtNode::Atomic(expr) => {
            let exp_ty = infer_expr(expr, block, data, None);
            if exp_ty.is_inactive() {
                return ExitKind::Break; // Code below that is dead code 
            }
            ExitKind::Proceed
        }
        StmtNode::Break(exit) => {
            let ty = infer_expr(&exit.expr, block, data, None);

            if ty.is_inactive() {
                // Error: Cannot break with inactive value....
            }

            let bi = if let Some(target_label) = exit.target {
                match block.get_block_binding(&target_label) {
                    Some(target) => target,
                    None => {
                        let err = YuuError::builder()
                            .kind(ErrorKind::InvalidStatement)
                            .message(format!(
                                "Break statement references nonexistent label '{}'",
                                target_label
                            ))
                            .source(
                                data.src_code.source.clone(),
                                data.src_code.file_name.clone(),
                            )
                            .span(
                                exit.span.clone(),
                                format!("break to undefined label '{}'", target_label),
                            )
                            .help("Make sure the label is defined in an enclosing block or loop")
                            .build();

                        data.errors.push(err);

                        // Set error type for this node
                        data.type_registry
                            .type_info_table
                            .insert(exit.id, error_type());

                        return ExitKind::Proceed; // Continue analysis instead of breaking
                    }
                }
            } else {
                // We break to the parent block
                block.named_block_binding.1.clone()
            };

            data.type_registry.bindings.insert(exit.id, bi.id);

            if let Err(unify_err) = data
                .type_registry
                .type_info_table
                .unify_and_insert(bi.id, ty)
            {
                let err = YuuError::builder()
                    .kind(ErrorKind::TypeMismatch)
                    .message(format!(
                        "Break with incompatible value type: {} cannot be used with block expecting {}",
                        ty, unify_err.right
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(
                        exit.expr.span(),
                        format!("has type {}", ty)
                    )
                    .help(
                        "Break values must be compatible with the block's return type, which may be determined by other break statements or the block's last expression"
                    )
                    .build();

                data.errors.push(err);
                data.type_registry
                    .type_info_table
                    .insert(exit.id, error_type());
                // We still want to return Break to maintain control flow analysis
            }
            ExitKind::Break
        }
        StmtNode::If(if_stmt) => infer_if_stmt(if_stmt, block, data),
        StmtNode::While(while_stmt) => infer_while_stmt(while_stmt, block, data),
        StmtNode::Block(block_stmt) => infer_block_stmt(block_stmt, block, data),
        StmtNode::Error(stmt) => {
            // Set error type
            let ty = error_type();
            data.type_registry.type_info_table.insert(*stmt, ty);
            ExitKind::Proceed
        }
    }
}

fn infer_if_stmt(if_stmt: &IfStmt, block: &mut Block, data: &mut TransientData) -> ExitKind {
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
    infer_block(&if_stmt.if_block.body, block, data);

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
        infer_block(&else_if_block.body, block, data);
    }

    // Infer else block
    if let Some(else_block) = &if_stmt.else_block {
        infer_block(else_block, block, data);
    }

    ExitKind::Proceed
}

fn infer_while_stmt(while_stmt: &WhileStmt, block: &mut Block, data: &mut TransientData) -> ExitKind {
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
    infer_block(&while_stmt.condition_block.body, block, data);

    ExitKind::Proceed
}

fn infer_block_stmt(block_stmt: &BlockStmt, block: &mut Block, data: &mut TransientData) -> ExitKind {
    // Create a new scope for the block
    let mut new_block = Block::new();
    new_block.parent_id = Some(block.id);
    
    // Process each statement in the block
    for stmt in &block_stmt.body {
        let exit_kind = infer_stmt(stmt, &mut new_block, data);
        if let ExitKind::Break = exit_kind {
            // Early exit due to break statement
            return ExitKind::Break;
        }
    }
    
    ExitKind::Proceed
}
