use ustr::{Ustr, ustr};

use crate::pass_diagnostics::{ErrorKind, YuuError};
use crate::pass_parse::InternUstr;
use crate::pass_parse::add_ids::GetId;
use crate::pass_type_inference::pass_type_inference_impl::TransientData;
use crate::{
    pass_parse::ast::TypeNode,
    pass_type_inference::type_info::{
        TypeInfo, error_type, primitive_bool, primitive_f32, primitive_f64, primitive_i64,
    },
};


pub fn infer_type(ty: &TypeNode, data: &mut TransientData) -> &'static TypeInfo {

    let semantic_type = match ty {
        TypeNode::BuiltIn(built_in) => match built_in.kind {
            crate::pass_parse::ast::BuiltInTypeKind::I64 => primitive_i64(),
            crate::pass_parse::ast::BuiltInTypeKind::F32 => primitive_f32(),
            crate::pass_parse::ast::BuiltInTypeKind::F64 => primitive_f64(),
            crate::pass_parse::ast::BuiltInTypeKind::Bool => primitive_bool(),
        },
        TypeNode::Ident(ident) => {
            // First check if it's a built-in type identifier
            match ident.name.as_str() {
                "i64" => primitive_i64(),
                "f32" => primitive_f32(),
                "f64" => primitive_f64(),
                "bool" => primitive_bool(),
                _ => {
                    // Try to resolve as either a struct or enum
                    let type_info = data.type_registry.resolve_struct_or_enum(ident.name);
                    if let Some(info) = type_info {
                        info.ty()
                    } else {
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
        }
        TypeNode::Pointer(pointer) => {
            let pointee_type = infer_type(&pointer.pointee, data);
            pointee_type.ptr_to()
        }
    };
    // Add the type to the type info table
    data.type_registry
        .type_info_table
        .insert(ty.node_id(), semantic_type);
    semantic_type
}
