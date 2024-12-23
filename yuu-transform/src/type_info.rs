use std::{cell::RefCell, fmt::Display, rc::Rc};

use hashbrown::HashMap;
use yuu_shared::ast::*;

use crate::binding_info::BindingInfo;

pub struct TypeInfoTable {
    pub types: HashMap<NodeId, Rc<TypeInfo>>,
}

impl Default for TypeInfoTable {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInfoTable {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum BuiltInType {
    I64,
    F32,
    F64,
    Nil,
    Error,
    Bool,
}

impl Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltInType::I64 => write!(f, "i64"),
            BuiltInType::F32 => write!(f, "f32"),
            BuiltInType::F64 => write!(f, "f64"),
            BuiltInType::Error => write!(f, "<error>"),
            BuiltInType::Nil => write!(f, "nil"),
            BuiltInType::Bool => write!(f, "bool"),
        }
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| format!("{}", arg))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({}) -> {}", args, self.ret)
    }
}

#[derive(Clone)]
pub struct FunctionType {
    pub args: Rc<[Rc<TypeInfo>]>,
    pub ret: Rc<TypeInfo>,
}

#[derive(Clone)]
pub enum FunctionGroupKind {
    Resolved(Rc<TypeInfo>),
    Unresolved(Rc<RefCell<Vec<BindingInfo>>>),
}

#[derive(Clone)]
pub enum TypeInfo {
    //Custom(String),
    BuiltIn(BuiltInType),
    Function(FunctionType),
    FunctionGroup(RefCell<FunctionGroupKind>),
    // pub size: usize,
    // pub align: usize,
}

impl TypeInfo {
    pub fn does_coerce_to_same_type(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeInfo::BuiltIn(a), TypeInfo::BuiltIn(b)) => {
                // For now, we only allow exact type matches
                a == b
            }
            (TypeInfo::Function(a), TypeInfo::Function(b)) => {
                // Functions must match exactly in argument and return types
                if a.args.len() != b.args.len() {
                    return false;
                }

                for (arg_a, arg_b) in a.args.iter().zip(b.args.iter()) {
                    if !arg_a.does_coerce_to_same_type(arg_b) {
                        return false;
                    }
                }

                a.ret.does_coerce_to_same_type(&b.ret)
            }
            // Function groups are not directly comparable
            (TypeInfo::FunctionGroup(_), _) | (_, TypeInfo::FunctionGroup(_)) => false,
            // Different type categories don't coerce
            _ => false,
        }
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::BuiltIn(built_in) => write!(f, "{}", built_in),
            TypeInfo::Function(function_type) => write!(f, "{}", function_type),
            TypeInfo::FunctionGroup(_) => write!(f, "<function group>"),
        }
    }
}
