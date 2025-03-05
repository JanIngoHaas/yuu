use std::ops::{Deref, DerefMut};

use hashbrown::HashMap;
use yuu_parse::add_ids::GetId;
use yuu_shared::{
    ast::{NodeId, TypeNode, AST},
    binding_info::BindingInfo,
    block::Block,
    scheduler::{ResourceId, ResourceName},
    semantic_error::SemanticError,
    type_info::{
        primitive_bool, primitive_f32, primitive_f64, primitive_i64, TypeInfo, TypeInfoTable,
    },
};

use super::pass_type_inference::TransientData;

pub fn infer_type(
    ty: &TypeNode,
    _block: &mut Block,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let semantic_type = match ty {
        TypeNode::BuiltIn(built_in) => match built_in.kind {
            yuu_shared::ast::BuiltInTypeKind::I64 => primitive_i64(),
            yuu_shared::ast::BuiltInTypeKind::F32 => primitive_f32(),
            yuu_shared::ast::BuiltInTypeKind::F64 => primitive_f64(),
            yuu_shared::ast::BuiltInTypeKind::Bool => primitive_bool(),
        },
        TypeNode::Ident(_) => todo!("User defined types not implemented yet"),
    };
    // Add the type to the type info table
    data.type_info_table
        .types
        .insert(ty.node_id(), semantic_type);
    semantic_type
}
