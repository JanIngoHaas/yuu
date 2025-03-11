use yuu_shared::{
    ast::{BindingNode, IdentBinding, NodeId},
    binding_info::BindingInfo,
    block::Block,
    error::YuuError,
    type_info::{TypeInfo, TypeInfoTable},
    Span,
};

use super::pass_type_inference::TransientData;

pub fn match_binding_node_to_type<'a>(
    binding: &BindingNode,
    block: &mut Block,
    ty: &'static TypeInfo,
    data: &'a mut TransientData,
) {
    match binding {
        BindingNode::Ident(ident_binding) => {
            block.insert_variable(
                ident_binding.name.clone(),
                ident_binding.id,
                ident_binding.span.clone(),
                ident_binding.is_mut,
            );
            data.type_info_table.types.insert(ident_binding.id, ty);
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
