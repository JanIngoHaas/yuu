use logos::Span;

use crate::pass_parse::ast::NodeId;

#[derive(Clone, Debug)]
pub struct BindingInfo {
    pub id: NodeId,
    pub src_location: Option<Span>,
}

#[derive(Clone, Debug)]
pub struct VariableBinding {
    pub binding_info: BindingInfo,
    pub is_mut: bool,
}

pub fn is_prebuilt_node_id(id: NodeId) -> bool {
    // Non-expression IDs start from usize::MAX and count down
    id >= (usize::MAX / 2) // Use half-way point as threshold
}