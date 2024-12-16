// use yuu_shared::ast::{BindingNode, BuiltInType, BuiltInTypeKind, TypeNode};

// use crate::type_info::*;

// pub fn mangle_func_name(name: &str, first_arg: Option<&TypeInfo>) -> String {
//     format!(
//         "{}@{}",
//         name,
//         first_arg
//             .map(|x| {
//                 match x {
//                     TypeInfo::BuiltIn(built_in_type) => built_in_type.to_string(),
//                     TypeInfo::Function(function_type) => function_type.to_string(),
//                 }
//             })
//             .unwrap_or("".to_string())
//     )
// }

// pub fn mangle_func_name_node(name: &str, first_arg: Option<&TypeNode>) -> String {
//     format!(
//         "{}@{}",
//         name,
//         first_arg
//             .map(|x| {
//                 match x {
//                     TypeNode::BuiltIn(built_in_type) => built_in_type.kind.to_string(),
//                     TypeNode::Ident(ident_type) => ident_type.name.clone(),
//                 }
//             })
//             .unwrap_or("".to_string()),
//     )
// }

// pub fn demangle_func_name(name: &str) -> (&str, &str) {
//     let mut split = name.split('@');
//     let name = split.next().unwrap();
//     let first_arg = split.next().unwrap();
//     (name, first_arg)
// }
