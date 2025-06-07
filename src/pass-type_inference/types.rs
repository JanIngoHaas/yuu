use crate::pass_diagnostics::{ErrorKind, YuuError};
use crate::pass_parse::add_ids::GetId;
use crate::{
    pass_parse::ast::TypeNode,
    pass_type_inference::type_info::{
        TypeInfo, error_type, primitive_bool, primitive_f32, primitive_f64, primitive_i64,
    },
};

use super::pass_type_inference::TransientData;

pub fn infer_type(ty: &TypeNode, data: &mut TransientData) -> &'static TypeInfo {
    let semantic_type = match ty {
        TypeNode::BuiltIn(built_in) => match built_in.kind {
            crate::pass_parse::ast::BuiltInTypeKind::I64 => primitive_i64(),
            crate::pass_parse::ast::BuiltInTypeKind::F32 => primitive_f32(),
            crate::pass_parse::ast::BuiltInTypeKind::F64 => primitive_f64(),
            crate::pass_parse::ast::BuiltInTypeKind::Bool => primitive_bool(),
        },
        TypeNode::Ident(ident) => {
            let s = data.type_registry.resolve_struct(ident.name);
            match s {
                Some(structinfo) => structinfo.ty,
                None => {
                    // TODO: Implement type inference for functions
                    let help_msg = data
                        .type_registry
                        .get_similar_names_struct(ident.name, 3)
                        .into_iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<String>>();
                    let help_msg = if !help_msg.is_empty() {
                        Some(format!(
                            "Did you mean one of these?\n{}\n",
                            help_msg.join("\n")
                        ))
                    } else {
                        None
                    };

                    let mut message = YuuError::builder()
                        .kind(ErrorKind::ReferencedUndefinedStruct)
                        .message(format!("Cannot find struct type '{}'", ident.name))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(
                            ident.span.clone(),
                            format!("'{}' is not defined", ident.name),
                        );

                    if let Some(help_msg) = help_msg {
                        message = message.help(help_msg);
                    }
                    let message = message.build();
                    data.errors.push(message);
                    error_type()
                }
            }
        }
    };
    // Add the type to the type info table
    data.type_registry
        .type_info_table
        .insert(ty.node_id(), semantic_type);
    semantic_type
}
