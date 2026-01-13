use crate::{
    pass_parse::ast::{FuncDefStructural, StructuralNode},
    pass_type_inference::infer_stmt,
    utils::{
        BindingInfo,
        type_info_table::{error_type, primitive_nil},
    },
};

use super::{infer_type, pass_type_inference_impl::TransientData};

fn infer_func_def(def: &FuncDefStructural, current_block_id: usize, data: &mut TransientData) {
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
            func_block.insert_variable(arg.name, arg.id, Some(arg.span.clone()), arg.is_mut);

            let arg_type = infer_type(&arg.ty, data.type_registry, data.errors, data.src_code);
            data.type_info_table.insert(arg.id, arg_type);
        }
    }

    let return_type = if let Some(ret_ty) = &def.decl.ret_ty {
        infer_type(ret_ty, data.type_registry, data.errors, data.src_code)
    } else {
        primitive_nil()
    };
    data.set_current_function_return_type(return_type);

    for stmt in &def.body.body {
        infer_stmt(stmt, func_block_id, data);
    }

    data.current_function_return_type = error_type();
}

pub(crate) fn infer_structural(
    structural: &StructuralNode,
    current_block_id: usize,
    data: &mut TransientData,
) {
    match structural {
        StructuralNode::FuncDecl(_decl) => {}
        StructuralNode::FuncDef(def) => infer_func_def(def, current_block_id, data),
        StructuralNode::Error(estr) => {
            data.type_info_table.insert(*estr, error_type());
        }
        // Already did that in collect_structural
        StructuralNode::StructDecl(_struct_decl) => {}
        StructuralNode::StructDef(_struct_def) => {}
        StructuralNode::EnumDef(_enum_def) => {}
        StructuralNode::LuaMeta(_) => {
            // TODO: Implement Lua meta structural type inference
        }
    }
}
