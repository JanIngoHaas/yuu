// Here, we define the built-in functions etc. that are already present in the language.

use std::{cell::RefCell, rc::Rc};

use yuu_parse::Span;
use yuu_shared::ast::NodeId;

use crate::type_info::BuiltInType;

#[derive(Clone)]
pub struct BindingInfo {
    pub id: NodeId,
    pub src_location: Option<Span>,
}

#[derive(Clone)]
pub enum BindingInfoKind {
    Ambiguous(Rc<RefCell<Vec<BindingInfo>>>),
    Unique(BindingInfo),
}

// We grow the NodeId by -1, -2, -3, -4 to avoid conflicts with the AST NodeIds. This way, we can ensure that these NodeIds are unique and we don't have collisions.
pub const ID_ADD_FUNC_I64: NodeId = -1;
pub const ID_ADD_FUNC_F32: NodeId = -2;
pub const ID_ADD_FUNC_F64: NodeId = -3;
pub const ID_SUB_FUNC_I64: NodeId = -4;
pub const ID_SUB_FUNC_F32: NodeId = -5;
pub const ID_SUB_FUNC_F64: NodeId = -6;
pub const ID_MUL_FUNC_I64: NodeId = -7;
pub const ID_MUL_FUNC_F32: NodeId = -8;
pub const ID_MUL_FUNC_F64: NodeId = -9;
pub const ID_DIV_FUNC_I64: NodeId = -10;
pub const ID_DIV_FUNC_F32: NodeId = -11;
pub const ID_DIV_FUNC_F64: NodeId = -12;

pub const OPERATOR_BINDINGS: [(&str, [BindingInfo; 3]); 4] = [
    (
        "add",
        [
            BindingInfo {
                id: ID_ADD_FUNC_I64,
                src_location: None,
            },
            BindingInfo {
                id: ID_ADD_FUNC_F32,
                src_location: None,
            },
            BindingInfo {
                id: ID_ADD_FUNC_F64,
                src_location: None,
            },
        ],
    ),
    (
        "sub",
        [
            BindingInfo {
                id: ID_SUB_FUNC_I64,
                src_location: None,
            },
            BindingInfo {
                id: ID_SUB_FUNC_F32,
                src_location: None,
            },
            BindingInfo {
                id: ID_SUB_FUNC_F64,
                src_location: None,
            },
        ],
    ),
    (
        "mul",
        [
            BindingInfo {
                id: ID_MUL_FUNC_I64,
                src_location: None,
            },
            BindingInfo {
                id: ID_MUL_FUNC_F32,
                src_location: None,
            },
            BindingInfo {
                id: ID_MUL_FUNC_F64,
                src_location: None,
            },
        ],
    ),
    (
        "div",
        [
            BindingInfo {
                id: ID_DIV_FUNC_I64,
                src_location: None,
            },
            BindingInfo {
                id: ID_DIV_FUNC_F32,
                src_location: None,
            },
            BindingInfo {
                id: ID_DIV_FUNC_F64,
                src_location: None,
            },
        ],
    ),
];

pub const BUILTIN_TYPES: [BuiltInType; 3] = [BuiltInType::I64, BuiltInType::F32, BuiltInType::F64];

pub fn is_prebuilt_node_id(id: NodeId) -> bool {
    id < 0
}
