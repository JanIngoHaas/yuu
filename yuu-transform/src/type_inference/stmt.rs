use yuu_shared::{
    ast::{IdentBinding, NodeId, StmtNode},
    block::Block,
    semantic_error::SemanticError,
    type_info::{TypeInfo, TypeInfoTable},
};

use super::{infer_expr, match_binding_node_to_type, TransientData};

pub enum ExitKind {
    Break,
    Proceed,
}

pub fn infer_stmt(
    stmt: &StmtNode,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<ExitKind, SemanticError> {
    match stmt {
        StmtNode::Let(let_stmt) => {
            let ty_expr = infer_expr(&let_stmt.expr, block, data, None)?;
            let match_resolver =
                move |ident_binding: &IdentBinding,
                      ty: &'static TypeInfo,
                      ty_info_table: &mut TypeInfoTable| {
                    block.insert_variable(
                        ident_binding.name.clone(),
                        ident_binding.id,
                        ident_binding.span.clone(),
                        ident_binding.is_mut,
                    );

                    ty_info_table.types.insert(ident_binding.id, ty);

                    Ok(())
                };

            match_binding_node_to_type(&let_stmt.binding, ty_expr, match_resolver, data)?;
            Ok(ExitKind::Proceed)
        }
        StmtNode::Atomic(expr) => infer_expr(expr, block, data, None).map(|_| ExitKind::Proceed),
        StmtNode::Break(exit) => {
            let ty = infer_expr(&exit.value, block, data, None)?;

            let target_block = block.get_name(&exit.target).expect(
                format!(
                    "User error: Referenced block {} does not exist",
                    exit.target
                )
                .as_str(),
            );
            data.type_info_table
                .unify_and_insert(target_block.id, ty)
                .expect("User error: Unification failed");
            return Ok(ExitKind::Break);
        }
    }
}
