use yuu_shared::{
    ast::{IdentBinding, NodeId, StmtNode},
    block::Block,
    semantic_error::SemanticError,
    type_info::{TypeInfo, TypeInfoTable},
    Span,
};

use super::{infer_expr, match_binding_node_to_type, pass_type_inference::TransientData};

pub enum ExitKind {
    Break,
    Proceed,
}

pub fn infer_stmt(stmt: &StmtNode, block: &mut Block, data: &mut TransientData) -> ExitKind {
    match stmt {
        StmtNode::Let(let_stmt) => {
            let ty_expr = infer_expr(&let_stmt.expr, block, data, None);
            match_binding_node_to_type(&let_stmt.binding, block, ty_expr, data);
            ExitKind::Proceed
        }
        StmtNode::Atomic(expr) => infer_expr(expr, block, data, None).map(|_| ExitKind::Proceed),
        StmtNode::Break(exit) => {
            let ty = infer_expr(&exit.expr, block, data, None);

            let target_block = block.get_block_binding(&exit.target).expect(
                format!(
                    "User error: Referenced block {} does not exist",
                    exit.target
                )
                .as_str(),
            );

            data.binding_table.insert(exit.id, target_block.id);

            data.type_info_table
                .unify_and_insert(target_block.id, ty)
                .expect("User error: Unification failed");
            return ExitKind::Break;
        }
        StmtNode::Error(stmt) => todo!("Make Semantic Error from Syntax Error"),
    }
}
