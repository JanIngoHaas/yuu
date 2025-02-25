use yuu_shared::{
    ast::{BindingNode, FuncArg, NodeId, StructuralNode, TypeNode},
    binding_info::BindingInfo,
    block::{Block, FUNC_BLOCK_NAME},
    semantic_error::SemanticError,
    type_info::{primitive_nil, FunctionType, TypeInfo},
    Span,
};

use super::{infer_block, infer_stmt, infer_type, match_binding_node_to_type, TransientData};

pub fn declare_function(
    name: &str,
    args: &[FuncArg],
    ret_ty: &Option<Box<TypeNode>>,
    id: NodeId,
    span: Span,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    block
        .declare_function(name.to_string(), id, span.clone())
        .map_err(|_err| panic!("_"))?;

    let func_arg_types = args
        .iter()
        .map(|arg| {
            let semantic_arg_type = infer_type(&arg.ty, block, data)?;

            match_binding_node_to_type(
                &arg.binding,
                semantic_arg_type,
                |ident_binding, ty, ty_info_table| {
                    block.insert_variable(
                        ident_binding.name.clone(),
                        ident_binding.id,
                        ident_binding.span.clone(),
                        ident_binding.is_mut,
                    );
                    ty_info_table.types.insert(ident_binding.id, ty);

                    Ok(())
                },
                data,
            )?;

            Ok::<_, SemanticError>(semantic_arg_type)
        })
        .collect::<Result<Vec<_>, _>>()?;

    let ret_type = if let Some(ty) = ret_ty {
        infer_type(ty, block, data)?
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

pub fn infer_structural(
    structural: &StructuralNode,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<(), SemanticError> {
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
            )?;
            Ok(())
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
            )?;

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

            infer_block(&def.body, func_block, data)?;

            Ok(())
        }
    }
}
