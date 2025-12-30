use crate::pass_diagnostics::{ErrorKind, YuuError};
use crate::pass_parse::SourceInfo;
use crate::pass_parse::ast::TypeNode;
use crate::utils::TypeRegistry;
use crate::utils::type_info_table::{
    TypeInfo, error_type, primitive_bool, primitive_f32, primitive_f64,
    primitive_i64, primitive_u64,
};

pub fn infer_type(
    ty: &TypeNode,
    registry: &TypeRegistry,
    errors: &mut Vec<YuuError>,
    src_code: &SourceInfo,
) -> &'static TypeInfo {
    // Not an expression - no need to add to
    // type_info_table
    //     .insert(ty.node_id(), semantic_type);
    match ty {
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
                "u64" => primitive_u64(),
                "f32" => primitive_f32(),
                "f64" => primitive_f64(),
                "bool" => primitive_bool(),
                _ => {
                    // Try to resolve as either a struct or enum
                    let type_info = registry.resolve_struct_or_enum(ident.name);
                    if let Some(info) = type_info {
                        info.ty()
                    } else {
                        // TODO: Implement type inference for functions
                        let help_msg = registry
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
                            .kind(ErrorKind::UndefinedStruct)
                            .message(format!("Struct or enum type '{}' not found", ident.name))
                            .source(src_code.source.clone(), src_code.file_name.clone())
                            .span(
                                ident.span.clone(),
                                format!("type '{}' is not defined", ident.name),
                            );

                        if let Some(help_msg) = help_msg {
                            message = message.help(help_msg);
                        }
                        let message = message.build();
                        errors.push(message);
                        error_type()
                    }
                }
            }
        }
        TypeNode::Pointer(pointer) => {
            let pointee_type = infer_type(&pointer.pointee, registry, errors, src_code);
            pointee_type.ptr_to()
        }
        TypeNode::Array(array) => {
            // Arrays are treated as pointers to the element type
            let element_type = infer_type(&array.element_type, registry, errors, src_code);
            element_type.ptr_to()
        }
        TypeNode::LuaMeta(_) => {
            // TODO: Implement Lua meta type inference
            error_type()
        }
    }
}
