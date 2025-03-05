// Here, we define the built-in functions etc. that are already present in the language.

use super::Span;

use crate::ast::NodeId;

#[derive(Clone, Debug)]
pub struct BindingInfo {
    pub id: NodeId,
    pub src_location: Option<Span>,
    pub is_mut: bool,
}

#[derive(Clone, Debug)]
pub enum BindingInfoKind {
    Function(Vec<BindingInfo>), // Probably function overloading
    Variable(BindingInfo),
}

pub fn is_prebuilt_node_id(id: NodeId) -> bool {
    id < 0
}
