use crate::{pass_parse::BindingNode, pass_type_inference::TypeInfo, pass_yir_lowering::Block};

use super::pass_type_inference_impl::TransientData;

pub fn match_binding_node_to_type(
    block: &mut Block,
    binding: &BindingNode,
    ty: &'static TypeInfo,
    data: &mut TransientData,
) {
    match binding {
        BindingNode::Ident(ident_binding) => {
            data.type_registry.add_variable(
                block,
                ident_binding.name,
                ident_binding.id,
                Some(ident_binding.span.clone()),
                ty,
            );
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
