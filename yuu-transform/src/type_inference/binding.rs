use yuu_shared::{
    ast::{BindingNode, IdentBinding, NodeId},
    binding_info::BindingInfo,
    block::FunctionOverloadError,
    semantic_error::SemanticError,
    type_info::{TypeInfo, TypeInfoTable},
};

use super::TransientData;

pub fn match_binding_node_to_type<'a, T>(
    binding: &BindingNode,
    ty: &'static TypeInfo,
    mut match_resolver: T,
    data: &'a mut TransientData,
) -> Result<(), SemanticError>
where
    T: FnMut(&IdentBinding, &'static TypeInfo, &'a mut TypeInfoTable) -> Result<(), SemanticError>,
{
    match binding {
        BindingNode::Ident(ident) => {
            match_resolver(ident, ty, data.type_info_table)?;
        }
    };
    Ok(())
}

pub fn resolve_function_overload(
    candidate_funcs: &[BindingInfo],
    type_info_table: &TypeInfoTable,
    actual_args: &[&'static TypeInfo],
) -> Result<BindingInfo, FunctionOverloadError> {
    for candidate in candidate_funcs {
        if let Some(TypeInfo::Function(func)) = type_info_table.types.get(&candidate.id) {
            // Check if argument lengths match
            if func.args.len() != actual_args.len() {
                continue;
            }

            // Check if all arguments coerce
            let mut all_args_match = true;
            for (expected, actual) in func.args.iter().zip(actual_args.iter()) {
                if !actual.is_exact_same_type(expected) {
                    all_args_match = false;
                    break;
                }
            }

            if all_args_match {
                return Ok(candidate.clone());
            }
        }
    }

    Err(FunctionOverloadError::NoOverloadFound)
}
