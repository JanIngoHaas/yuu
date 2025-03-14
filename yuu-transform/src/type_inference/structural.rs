use ustr::Ustr;
use yuu_shared::{
    ast::{Arg, BindingNode, InternUstr, NodeId, StructuralNode, TypeNode},
    binding_info::BindingInfo,
    block::{Block, FUNC_BLOCK_NAME},
    type_info::{error_type, primitive_nil, FunctionType, TypeInfo},
    Span,
};

use super::{
    infer_block_no_child_creation, infer_type, match_binding_node_to_type,
    pass_type_inference::TransientData,
};

pub fn declare_function(
    name: Ustr,
    args: &[Arg],
    ret_ty: &Option<Box<TypeNode>>,
    id: NodeId,
    span: Span,
    block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let func_arg_types = args
        .iter()
        .map(|arg| {
            let semantic_arg_type = infer_type(&arg.ty, data);

            match_binding_node_to_type(block, &arg.binding, semantic_arg_type, data);

            semantic_arg_type
        })
        .collect::<Vec<_>>();

    let ret_type = if let Some(ty) = ret_ty {
        infer_type(ty, data)
    } else {
        primitive_nil()
    };

    data.type_registry.add_function(
        &func_arg_types,
        ret_type,
        name,
        BindingInfo {
            id: id,
            src_location: Some(span),
        },
    );

    ret_type
}

pub fn infer_structural(structural: &StructuralNode, block: &mut Block, data: &mut TransientData) {
    match structural {
        StructuralNode::FuncDecl(_decl) => {}
        StructuralNode::FuncDef(def) => {
            let func_block = block.make_child(Some((
                FUNC_BLOCK_NAME.intern(),
                BindingInfo {
                    id: def.body.id,
                    src_location: Some(def.span.clone()),
                },
            )));

            for arg in &def.decl.args {
                let BindingNode::Ident(ident) = &arg.binding;
                func_block.insert_variable(
                    ident.name.clone(),
                    ident.id,
                    Some(ident.span.clone()),
                    ident.is_mut,
                );
            }

            // Process function body separately
            // for stmt in &def.body.body {
            //     infer_stmt(stmt, func_block, data)?;
            // }

            infer_block_no_child_creation(&def.body, func_block, data);
        }
        StructuralNode::Error(estr) => {
            data.type_registry
                .type_info_table
                .insert(*estr, error_type());
        }
        StructuralNode::StructDecl(struct_decl) => {
            todo!()
        }
        StructuralNode::StructDef(struct_def) => todo!(),
    }
}
