use yuu_shared::{
    ast::{BindingNode, FuncArg, NodeId, StructuralNode, TypeNode},
    binding_info::BindingInfo,
    block::{Block, FUNC_BLOCK_NAME},
    error::{YuuError, YuuErrorBuilder},
    semantic_error::SemanticError,
    type_info::{primitive_nil, FunctionType, TypeInfo},
    Span,
};

use super::{
    infer_block, infer_block_no_child_creation, infer_stmt, infer_type, match_binding_node_to_type,
    pass_type_inference::TransientData,
};

pub fn declare_function(
    name: &str,
    args: &[FuncArg],
    ret_ty: &Option<Box<TypeNode>>,
    id: NodeId,
    span: Span,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, YuuError> {
    block
        .declare_function(name.to_string(), id, span.clone())
        .map_err(|_err| panic!("_"))?;

    let func_arg_types = args
        .iter()
        .map(|arg| {
            let semantic_arg_type = infer_type(&arg.ty, block, data);

            match_binding_node_to_type(&arg.binding, block, semantic_arg_type, data);

            semantic_arg_type
        })
        .collect::<Vec<_>>();

    let ret_type = if let Some(ty) = ret_ty {
        infer_type(ty, block, data)
    } else {
        primitive_nil()
    };

    let func = FunctionType {
        args: func_arg_types,
        ret: ret_type,
    }
    .into();

    data.type_info_table.types.insert(id, func);

    Ok(ret_type)
}

pub fn infer_structural(structural: &StructuralNode, block: &mut Block, data: &mut TransientData) {
    match structural {
        StructuralNode::FuncDecl(decl) => {
            let _ = declare_function(
                &decl.name,
                &decl.args,
                &decl.ret_ty,
                decl.id,
                decl.span.clone(),
                block,
                data,
            )
            .expect("User error: Function declaration failed");
        }
        StructuralNode::FuncDef(def) => {
            let ret_type = declare_function(
                &def.decl.name,
                &def.decl.args,
                &def.decl.ret_ty,
                def.id,
                def.span.clone(),
                block,
                data,
            )
            .expect("User error: Function definition failed");

            let func_block = block.make_child(Some((
                FUNC_BLOCK_NAME.to_string(),
                BindingInfo {
                    id: def.body.id,
                    src_location: Some(def.span.clone()),
                    is_mut: false,
                },
            )));

            // Assign the return type to the _fn binding ID
            data.type_info_table.types.insert(def.body.id, ret_type);

            for arg in &def.decl.args {
                let BindingNode::Ident(ident) = &arg.binding;
                func_block.insert_variable(
                    ident.name.clone(),
                    ident.id,
                    ident.span.clone(),
                    ident.is_mut,
                );
            }

            // Process function body separately
            // for stmt in &def.body.body {
            //     infer_stmt(stmt, func_block, data)?;
            // }

            infer_block_no_child_creation(&def.body, func_block, data);
        }
        StructuralNode::Error(_) => todo!("Make semantic error from syntax error"),
    }
}
