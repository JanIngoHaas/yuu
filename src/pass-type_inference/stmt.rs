use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{Spanned, StmtNode},
    pass_type_inference::type_info::error_type,
    pass_yir_lowering::block::Block,
};

use super::{infer_expr, match_binding_node_to_type, pass_type_inference_impl::TransientData};

pub enum ExitKind {
    Break,
    Proceed,
}

pub fn infer_stmt(stmt: &StmtNode, block: &mut Block, data: &mut TransientData) -> ExitKind {
    match stmt {
        StmtNode::Let(let_stmt) => {
            let ty_expr = infer_expr(&let_stmt.expr, block, data, None);
            match_binding_node_to_type(block, &let_stmt.binding, ty_expr, data);
            ExitKind::Proceed
        }
        StmtNode::Atomic(expr) => {
            infer_expr(expr, block, data, None);
            ExitKind::Proceed
        }
        StmtNode::Break(exit) => {
            let ty = infer_expr(&exit.expr, block, data, None);

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
        StmtNode::Error(stmt) => {
            // Set error type
            let ty = error_type();
            data.type_registry.type_info_table.insert(*stmt, ty);
            ExitKind::Proceed
        }
    }
}
