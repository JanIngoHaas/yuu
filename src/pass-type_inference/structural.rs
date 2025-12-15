use crate::{
    pass_parse::ast::{Arg, InternUstr, NodeId, StructuralNode, TypeNode},
    pass_type_inference::infer_stmt,
    utils::{
        BindingInfo, BlockTree,
        type_info_table::{TypeInfo, error_type, primitive_nil, function_type},
    },
};
use logos::Span;
use ustr::Ustr;

use super::{
    infer_type,
    pass_type_inference_impl::{TransientData, TransientDataStructural},
};

pub fn declare_function(
    name: Ustr,
    args: &[Arg],
    ret_ty: &Option<Box<TypeNode>>,
    id: NodeId,
    span: Span,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let func_arg_types = args
        .iter()
        .map(|arg| {
            let semantic_arg_type = infer_type(
                &arg.ty,
                &data.type_registry,
                &mut data.errors,
                &data.src_code,
            );
            semantic_arg_type
        })
        .collect::<Vec<_>>();

    let ret_type = if let Some(ty) = ret_ty {
        infer_type(ty, &data.type_registry, &mut data.errors, &data.src_code)
    } else {
        primitive_nil()
    };

    data.type_registry.add_function(
        &func_arg_types,
        ret_type,
        name,
        BindingInfo {
            id,
            src_location: Some(span),
        },
    );

    // Create and return the full function type
    let (_, full_function_type) = function_type(&func_arg_types, ret_type);
    full_function_type
}

pub fn infer_structural(
    structural: &StructuralNode,
    current_block_id: usize,
    data: &mut TransientDataStructural,
) {
    match structural {
        StructuralNode::FuncDecl(_decl) => {}
        StructuralNode::FuncDef(def) => {
            let func_block_id = data.block_tree.make_child(
                current_block_id,
                BindingInfo {
                    id: def.body.id,
                    src_location: Some(def.span.clone()),
                },
            );

            {
                let func_block = data.block_tree.get_block_mut(func_block_id);

                for arg in &def.decl.args {
                    func_block.insert_variable(
                        arg.name,
                        arg.id,
                        Some(arg.span.clone()),
                        arg.is_mut,
                    );

                    // Store the argument type in the TypeInfoTable for expression lookups
                    let arg_type = infer_type(
                        &arg.ty,
                        &data.type_registry,
                        &mut data.errors,
                        &data.src_code,
                    );
                    data.type_info_table.insert(arg.id, arg_type);
                }
            }

            // Set the current function's return type for return statement validation
            // We need to infer the return type from the function declaration
            let return_type = if let Some(ret_ty) = &def.decl.ret_ty {
                infer_type(ret_ty, &data.type_registry, &mut data.errors, &data.src_code)
            } else {
                primitive_nil()
            };
            data.set_current_function_return_type(return_type);

            // Process function body separately
            for stmt in &def.body.body {
                infer_stmt(stmt, func_block_id, data);
            }

            // Clear the current function return type after processing
            data.current_function_return_type = error_type();
        }
        StructuralNode::Error(estr) => {
            data.type_info_table.insert(*estr, error_type());
        }
        // Already did that in collect_structural
        StructuralNode::StructDecl(_struct_decl) => {}
        StructuralNode::StructDef(_struct_def) => {}
        StructuralNode::EnumDef(_enum_def) => {}
    }
}
