use hashbrown::HashMap;
use yuu_parse::add_ids::GetId;
use yuu_shared::{
    ast::{NodeId, TypeNode, AST},
    binding_info::BindingInfo,
    block::Block,
    semantic_error::SemanticError,
    type_info::{
        primitive_bool, primitive_f32, primitive_f64, primitive_i64, TypeInfo, TypeInfoTable,
    },
};

pub struct BindingTable {
    pub bindings: HashMap<NodeId, BindingInfo>,
}

impl Default for BindingTable {
    fn default() -> Self {
        Self::new()
    }
}

impl BindingTable {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

pub struct TransientData<'a> {
    pub type_info_table: &'a mut TypeInfoTable,
    pub ast: &'a AST,
}

pub fn infer_type(
    ty: &TypeNode,
    _block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
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
    Ok(semantic_type)
}
