use crate::{
    pass_parse::BindingNode,
    utils::{Block, type_info_table::TypeInfo},
};

use super::pass_type_inference_impl::TransientDataStructural;

// TODO: For non-identifier bindings, we need to consider the possibility that the expression type might not match
// the binding type
pub fn infer_binding(
    binding: &BindingNode,
    block_id: usize,
    expr_type: &'static TypeInfo,
    data: &mut TransientDataStructural,
) {
    match binding {
        BindingNode::Ident(ident_binding) => {
            let block = data.block_tree.get_block_mut(block_id);
            block.insert_variable(
                ident_binding.name,
                ident_binding.id,
                Some(ident_binding.span.clone()),
                false,
            );
            data.type_info_table.insert(ident_binding.id, expr_type);
        }
    }
}

//pub fn resolve_function_overload(
//     candidate_funcs: &[BindingInfo],
//     type_info_table: &TypeInfoTable,
//     actual_args: &[&'static TypeInfo],
// ) -> Result<BindingInfo, FunctionOverloadError> {
//     for candidate in candidate_funcs {
//         if let Some(TypeInfo::Function(func)) = type_info_table.types.get(&candidate.id) {
//             // Check if argument lengths match
//             if func.args.len() != actual_args.len() {
//                 continue;
//             }

//             // Check if all arguments coerce
//             let mut all_args_match = true;
//             for (expected, actual) in func.args.iter().zip(actual_args.iter()) {
//                 if !actual.is_exact_same_type(expected) {
//                     all_args_match = false;
//                     break;
//                 }
//             }

//             if all_args_match {
//                 return Ok(candidate.clone());
//             }
//         }
//     }

//     Err(FunctionOverloadError::NoOverloadFound)
// }
