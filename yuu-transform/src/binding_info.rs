// Here, we define the built-in functions etc. that are already present in the language.

use std::{cell::RefCell, rc::Rc};

use yuu_parse::Span;
use yuu_shared::ast::NodeId;

#[derive(Clone)]
pub struct BindingInfo {
    pub id: NodeId,
    pub src_location: Option<Span>,
}

#[derive(Clone)]
pub enum BindingInfoKind {
    Ambiguous(Rc<RefCell<Vec<BindingInfo>>>), // Probably function overloading
    Unique(BindingInfo),
}

pub fn is_prebuilt_node_id(id: NodeId) -> bool {
    id < 0
}
