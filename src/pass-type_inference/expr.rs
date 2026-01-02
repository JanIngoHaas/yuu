use crate::pass_diagnostics::{ErrorKind, YuuError, create_no_overload_error};
use crate::pass_lexing::{Integer, TokenKind};
use crate::pass_parse::add_ids::GetId;
use crate::pass_parse::ast::{
    NodeId, AssignmentExpr, BinaryExpr, EnumInstantiationExpr, ExprNode, FuncCallExpr, IdentExpr,
    LValueKind, LiteralExpr, MemberAccessExpr, Spanned, StructInstantiationExpr, UnaryExpr,
    UnaryOp,
};
use crate::pass_parse::{AddressOfExpr, ArrayLiteralExpr, BinOp, DerefExpr, HeapAllocExpr};
use crate::pass_type_inference::pass_type_inference_impl::TransientData;
use crate::utils::type_info_table::{PrimitiveType, primitive_bool};
use crate::utils::type_info_table::{
    TypeInfo, error_type, primitive_f32, primitive_f64, primitive_i64, primitive_nil, primitive_u64, unknown_type,
};
use crate::utils::{UnionFieldInfo};
// const MAX_SIMILAR_NAMES: u64 = 3;
// const MIN_DST_SIMILAR_NAMES: u64 = 3;


/// Helper function to extract i64 literal value from an expression, if it's a constant
pub fn try_extract_i64_literal(expr: &crate::pass_parse::ast::ExprNode) -> Option<i64> {
    match expr {
        crate::pass_parse::ast::ExprNode::Literal(lit_expr) => match &lit_expr.lit.kind {
            TokenKind::Integer(
                Integer::I64(val),
            ) => Some(*val),
            _ => None,
        },
        _ => None, // Not a constant literal
    }
}

fn infer_literal_expr(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
    let out = match lit.lit.kind {
        TokenKind::Integer(integer) => match integer {
            Integer::I64(_) => primitive_i64(),
            Integer::U64(_) => primitive_u64(),
        },
        TokenKind::F32(_) => primitive_f32(),
        TokenKind::F64(_) => primitive_f64(),
        TokenKind::NilKw => primitive_nil(),
        _ => unreachable!("Compiler bug: Literal not implemented"),
    };
    data.type_info_table.insert(lit.id, out);
    out
}


fn infer_cast_expr(
    cast_expr: &crate::pass_parse::ast::CastExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let _expr_type = infer_expr(&cast_expr.expr, block_id, data, None);
    let target_type = crate::pass_type_inference::types::infer_type(
        &cast_expr.target_type,
        data.type_registry,
        data.errors,
        data.src_code,
    );
    data.type_info_table.insert(cast_expr.id, target_type);
    target_type
}

fn infer_free_op(
    unary_expr: &UnaryExpr,
    operand_type: &'static TypeInfo,
    data: &mut TransientData,
) -> &'static TypeInfo {
    if let TypeInfo::Pointer(_) = operand_type {
        primitive_nil()
    } else {
        let err = YuuError::builder()
            .kind(ErrorKind::FreeNonPointer)
            .message(format!(
                "Cannot free a non-pointer type '{}'",
                operand_type
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(unary_expr.expr.span().clone(), format!("has type '{}', expected pointer", operand_type))
            .help("The `~` operator (free) can only be applied to pointer types")
            .build();
        data.errors.push(err);
        error_type()
    }
}

/// Fast-path for built-in primitive arithmetic operations.
///
/// This handles operations between identical primitive types (i64 + i64, f32 * f32, etc.)
/// without going through function resolution. This prevents users from overloading
/// core arithmetic operations and provides better performance.
///
/// Returns Some(result_type) if this is a built-in operation, None otherwise.
fn try_builtin_primitive_arithmetic(
    op: BinOp,
    lhs_type: &'static TypeInfo,
    rhs_type: &'static TypeInfo,
) -> Option<&'static TypeInfo> {
    // Only handle operations between identical primitive types
    if let (TypeInfo::BuiltInPrimitive(prim_a), TypeInfo::BuiltInPrimitive(prim_b)) = (lhs_type, rhs_type) && prim_a == prim_b {
        match op {
            // Arithmetic operations return the same type
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide | BinOp::Modulo => {
                Some(lhs_type)
            }
            // Comparison operations return bool
            BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::LtEq | BinOp::Gt | BinOp::GtEq => {
                Some(primitive_bool())
            }
        }
    } else {
        None
    }
}

/// Fast-path for pointer arithmetic operations.
///
/// This handles:
/// - pointer + integer = pointer (offset calculation)
/// - pointer - integer = pointer (offset calculation)
/// - pointer - pointer = integer (distance calculation)
///
/// These operations don't go through function resolution as they're fundamental
/// to the language's pointer semantics.
///
/// Returns Some(result_type) if this is pointer arithmetic, None otherwise.
fn try_pointer_arithmetic(
    op: BinOp,
    lhs_type: &'static TypeInfo,
    rhs_type: &'static TypeInfo,
) -> Option<&'static TypeInfo> {
    use TypeInfo::*;
    use PrimitiveType::*;

    match (op, lhs_type, rhs_type) {
        // pointer + integer = pointer
        (BinOp::Add, Pointer(_), BuiltInPrimitive(I64 | U64)) => Some(lhs_type),

        // pointer - integer = pointer
        (BinOp::Subtract, Pointer(_), BuiltInPrimitive(I64 | U64)) => Some(lhs_type),

        // pointer - pointer = integer (distance)
        (BinOp::Subtract, Pointer(_), Pointer(_)) => Some(primitive_i64()),

        _ => None,
    }
}

fn infer_binary_expr(
    binary_expr: &BinaryExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let expr_id = binary_expr.id;
    let expr_span = binary_expr.span.clone();
    let op_name = binary_expr.op.static_name();

    let lhs_type_actual = infer_expr(&binary_expr.left, block_id, data, None);
    let rhs_type_actual = infer_expr(&binary_expr.right, block_id, data, None);

    // Fast-path 1: Built-in primitive arithmetic (i64 + i64, f32 * f32, etc.)
    // This prevents users from overloading core arithmetic operations
    if let Some(result_type) = try_builtin_primitive_arithmetic(binary_expr.op, lhs_type_actual, rhs_type_actual) {
        data.type_info_table.insert(expr_id, result_type);
        return result_type;
    }

    // Fast-path 2: Pointer arithmetic (ptr + int, ptr - ptr, etc.)
    // These are fundamental language operations, not user-overloadable
    if let Some(result_type) = try_pointer_arithmetic(binary_expr.op, lhs_type_actual, rhs_type_actual) {
        data.type_info_table.insert(expr_id, result_type);
        return result_type;
    }

    // Standard path: Function resolution for user-defined types and mixed operations
    // This handles user-defined operators, type conversions, and error cases
    let resolved_type = resolve_binary_overload(
        op_name,
        lhs_type_actual,
        rhs_type_actual,
        expr_span,
        data,
        expr_id,
    );

    data.type_info_table.insert(expr_id, resolved_type);
    resolved_type
}

fn resolve_binary_overload(
    op_name: ustr::Ustr,
    lhs: &'static TypeInfo,
    rhs: &'static TypeInfo,
    span: logos::Span,
    data: &mut TransientData,
    expr_id: NodeId,
) -> &'static TypeInfo {
    match data.type_registry.resolve_function(op_name, &[lhs, rhs]) {
        Ok(res) => {
            data
                .bindings
                .insert(expr_id, res.binding_info.id);
            res.ty.ret
        }
        Err(candidates) => {
            let err = create_no_overload_error(
                &op_name,
                candidates,
                &[lhs, rhs],
                data.type_registry,
                data.src_code,
                span,
            );
            data.errors.push(err);
            data.type_info_table.insert(expr_id, error_type());
            error_type()
        }
    }
}

fn infer_unary_expr(
    unary_expr: &UnaryExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let ty = infer_expr(&unary_expr.expr, block_id, data, None);
    let op_name = unary_expr.op.static_name();

    let resolved_type = match unary_expr.op {
        UnaryOp::Free => infer_free_op(unary_expr, ty, data),
        _ => {
            match data.type_registry.resolve_function(op_name, &[ty]) {
                Ok(res) => res.ty.ret,
                Err(err) => {
                    let err = create_no_overload_error(
                        &op_name,
                        err,
                        &[ty],
                        data.type_registry,
                        data.src_code,
                        unary_expr.span.clone(),
                    );
                    data.errors.push(err);
                    error_type()
                }
            }
        }
    };

    data.type_info_table.insert(unary_expr.id, resolved_type);
    resolved_type
}

fn resolve_function_ident(
    ident_expr: &IdentExpr,
    args: &[&'static TypeInfo],
    data: &mut TransientData,
) -> Result<&'static TypeInfo, Box<YuuError>> {
    match data.type_registry.resolve_function(ident_expr.ident, args) {
        Ok(res) => {
            data.type_info_table.insert(ident_expr.id, res.general_ty);
            data.bindings.insert(ident_expr.id, res.binding_info.id);
            Ok(res.general_ty)
        }
        Err(err) => Err(Box::new(create_no_overload_error(
            ident_expr.ident.as_str(),
            err,
            args,
            data.type_registry,
            data.src_code,
            ident_expr.span.clone(),
        ))),
    }
}

fn resolve_variable_ident(
    ident_expr: &IdentExpr,
    block_id: usize,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, Box<YuuError>> {
    match data.block_tree.resolve_variable(
        block_id,
        ident_expr.ident,
        data.src_code,
        ident_expr.span.clone(),
    ) {
        Ok(fr) => {
            let ty = data.type_info_table.get(fr.binding_info.id).expect(
                "Compiler bug: binding not found in type table - but it should be there",
            );

            if matches!(ty, TypeInfo::Unknown) {
                let err = YuuError::builder()
                    .kind(ErrorKind::InvalidExpression)
                    .message(format!(
                        "Cannot infer type of '{}' - variable has unknown type",
                        ident_expr.ident
                    ))
                    .source(data.src_code.source.clone(), data.src_code.file_name.clone())
                    .span(ident_expr.span.clone(), "unknown type")
                    .help("Define the variable with 'def' before using it to establish its type")
                    .build();
                return Err(Box::new(err));
            }

            data.type_info_table.insert(ident_expr.id, ty);
            data.bindings.insert(ident_expr.id, fr.binding_info.id);
            Ok(ty)
        }
        Err(err) => Err(err),
    }
}

fn infer_ident_expr(
    ident_expr: &IdentExpr,
    block_id: usize,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    let result = match function_args {
        Some(args) => resolve_function_ident(ident_expr, args, data),
        None => resolve_variable_ident(ident_expr, block_id, data),
    };

    match result {
        Ok(ty) => ty,
        Err(err) => {
            data.errors.push(*err);
            data.type_info_table.insert(ident_expr.id, error_type());
            error_type()
        }
    }
}

fn infer_func_call_expr(
    func_call_expr: &FuncCallExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let actual_arg_types = func_call_expr
        .args
        .iter()
        .map(|arg| infer_expr(arg, block_id, data, None))
        .collect::<Vec<_>>();

    let actual_func_ident =
        infer_expr(&func_call_expr.lhs, block_id, data, Some(&actual_arg_types));

    let resolved_ret_type = match actual_func_ident {
        TypeInfo::Function(func) => func.ret,
        TypeInfo::BuiltInPrimitive(prim) => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message(format!(
                    "Cannot call primitive type '{}' as a function",
                    prim
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), format!("has type '{}', not callable", prim))
                .help("Only function types can be called with parentheses")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Pointer(_) => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message("Cannot call a pointer as a function")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "pointer type, not callable")
                .help("Function pointers are not supported yet")
                .build();
            data.errors.push(err);
            error_type()
        }

        TypeInfo::Struct(struct_type) => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message(format!("Cannot call struct type '{}' as a function", struct_type.name))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), format!("struct type '{}', not callable", struct_type.name))
                .help("Structs are instantiated with braces, like: StructName { field: value }")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Error => error_type(),
        TypeInfo::Unknown => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message("Cannot call an identifier of unknown type as a function")
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), "unknown type, not callable")
                .help("Ensure the identifier is defined and has a known type")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Enum(e) => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message(format!(
                    "Cannot call enum type '{}' as a function",
                    e.name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), format!("enum type '{}', not callable", e.name))
                .help("Enums cannot be called as functions. Use enum variant syntax like 'EnumName::Variant' instead")
                .build();
            data.errors.push(err);
            error_type()
        }
        TypeInfo::Union(u) => {
            let err = YuuError::builder()
                .kind(ErrorKind::NotCallable)
                .message(format!(
                    "Cannot call union type '{}' as a function",
                    u.name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(func_call_expr.lhs.span().clone(), format!("union type '{}', not callable", u.name))
                .help("Unions cannot be called as functions")
                .build();
            data.errors.push(err);
            error_type()
        }
    };

    data.type_info_table
        .insert(func_call_expr.id, resolved_ret_type);

    resolved_ret_type
}

fn infer_assignment(
    assignment_expr: &AssignmentExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    // checking LValueKind is done in parsing already..

    debug_assert!(
        matches!(
            (&*assignment_expr.lhs, &assignment_expr.lvalue_kind),
            (ExprNode::Ident(_), LValueKind::Variable)
                | (ExprNode::MemberAccess(_), LValueKind::FieldAccess)
                | (ExprNode::Deref(_), LValueKind::Dereference)
        ),
        "LValueKind should match the LHS expression structure"
    );

    let ty_lhs = infer_expr(&assignment_expr.lhs, block_id, data, None);
    let ty_rhs = infer_expr(&assignment_expr.rhs, block_id, data, None);

    // First unify to check compatibility
    let _unified = match ty_lhs.unify(ty_rhs) {
        Ok(unified) => unified,
        Err(err) => {
            let err_msg = YuuError::builder()
                .kind(ErrorKind::TypeIncompatible)
                .message(format!(
                    "Cannot assign incompatible types: '{}' and '{}'",
                    err.left, err.right
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    assignment_expr.rhs.span().clone(),
                    format!("has type '{}'", err.left),
                )
                .label(
                    assignment_expr.lhs.span().clone(),
                    format!("has type '{}'", err.right),
                )
                .help("The types must be compatible for assignment")
                .build();
            data.errors.push(err_msg);
            error_type()
        }
    };

    // Assignment expressions always have type "nil"
    data.type_info_table
        .insert(assignment_expr.id, primitive_nil());

    primitive_nil()
}

fn infer_struct_instantiation(
    struct_instantiation_expr: &StructInstantiationExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    for (_, expr_node) in &struct_instantiation_expr.fields {
        infer_expr(expr_node, block_id, data, None);
    }

    let struct_name = struct_instantiation_expr.struct_name;
    let struct_opt = data.type_registry.resolve_struct(struct_name);

    if struct_opt.is_none() {
        let err = YuuError::builder()
            .kind(ErrorKind::UndefinedStruct)
            .message(format!(
                "Cannot instantiate undefined struct '{}'",
                struct_name
            ))
            .source(
                data.src_code.source.clone(),
                data.src_code.file_name.clone(),
            )
            .span(
                struct_instantiation_expr.span.clone(),
                format!("struct '{}' is not defined", struct_name),
            )
            .help("Define this struct before using it")
            .build();
        data.errors.push(err);
        data.type_info_table
            .insert(struct_instantiation_expr.id, error_type());
        return error_type();
    }

    let sinfo = struct_opt.unwrap();
    let struct_type = sinfo.ty;

    for (field, expr_node) in &struct_instantiation_expr.fields {
        let expr_type = data
            .type_info_table
            .get(expr_node.node_id())
            .expect("Compiler bug: expression type should be in registry");

        if let Some(field_info) = sinfo.fields.get(&field.name) {
            match expr_type.unify(field_info.ty) {
                Ok(_) => {}
                Err(err) => {
                    let err_msg = YuuError::builder()
                        .kind(ErrorKind::TypeIncompatible)
                        .message(format!(
                            "Cannot assign incompatible type '{}' to field '{}' of type '{}'",
                            err.left, field.name, err.right
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(expr_node.span().clone(), format!("has type '{}'", err.left))
                        .label(field.span.clone(), format!("expected type '{}'", err.right))
                        .help("The types must be compatible for assignment")
                        .build();
                    data.errors.push(err_msg);
                }
            }
        } else {
            let err = YuuError::builder()
                .kind(ErrorKind::UndefinedField)
                .message(format!(
                    "Cannot assign to undeclared field '{}' of struct '{}'",
                    field.name, struct_name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    field.span.clone(),
                    "attempted to assign to undeclared field",
                )
                .help("Define this field in the struct before using it")
                .build();
            data.errors.push(err);
        }
    }

    data.type_info_table
        .insert(struct_instantiation_expr.id, struct_type);

    struct_type
}

fn infer_member_access(
    member_access_expr: &MemberAccessExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let lhs_ty = infer_expr(&member_access_expr.lhs, block_id, data, None);

    match lhs_ty {
        TypeInfo::Struct(s) | TypeInfo::Pointer(TypeInfo::Struct(s)) => {
            let si = data
                .type_registry
                .resolve_struct(s.name)
                .expect("Compiler bug: Struct type should be resolved here.");

            let field = si.fields.get(&member_access_expr.field.name).cloned();

            match field {
                Some(field_info) => {
                    data.type_info_table
                        .insert(member_access_expr.id, field_info.ty);

                    field_info.ty
                }
                None => {
                    let err = YuuError::builder()
                        .kind(ErrorKind::UndefinedField)
                        .message(format!(
                            "Struct '{}' has no field named '{}'",
                            s.name, member_access_expr.field.name
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(
                            member_access_expr.field.span.clone(),
                            format!("field '{}' not defined", member_access_expr.field.name),
                        )
                        .label(
                            member_access_expr.lhs.span().clone(),
                            format!("this expression has type '{}'", s.name),
                        )
                        .help(format!(
                            "Available fields for struct '{}': {}",
                            s.name,
                            if si.fields.is_empty() {
                                "none".to_string()
                            } else {
                                si.fields
                                    .keys()
                                    .map(|k| k.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .build();
                    data.errors.push(err);

                    data.type_info_table
                        .insert(member_access_expr.id, error_type());
                    error_type()
                }
            }
        }
        _ => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Cannot access field '{}' on value of type '{}'",
                    member_access_expr.field.name, lhs_ty
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    member_access_expr.span.clone(),
                    "attempted member access on non-struct type",
                )
                .label(
                    member_access_expr.lhs.span().clone(),
                    format!("this expression has type '{}'", lhs_ty),
                )
                .label(
                    member_access_expr.field.span.clone(),
                    "field access attempted here",
                )
                .help("Member access is only supported on struct types")
                .build();
            data.errors.push(err);

            data.type_info_table
                .insert(member_access_expr.id, error_type());
            error_type()
        }
    }
}

fn validate_enum_variant_binding(
    ei: &EnumInstantiationExpr,
    variant_info: &UnionFieldInfo,
    block_id: usize,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) {
    // Left: Maybe data associated with the enum instantiation, i.e. Option::Some(x) where x would be the data (syntactical), Right: Semantic type information about the enum variant, i.e. data type that the variant is associated and registered with
    match (&ei.data, variant_info.ty.as_option_if_nil()) {
        // Yes, we have both
        (Some(dexpr), Some(reg_ty)) => {
            // Calculate type (semantic) from associated data (syntactical)
            let ty = infer_expr(dexpr, block_id, data, function_args);
            // See if the types match
            if let Err(err) = ty.unify(reg_ty) {
                let err_msg = YuuError::builder()
                    .kind(ErrorKind::TypeIncompatible)
                    .message(format!(
                        "Cannot assign incompatible type '{}' to enum variant '{}::{}' which expects '{}'",
                        err.left, ei.enum_name, ei.variant_name, err.right
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(dexpr.span().clone(), format!("has type '{}'", err.left))
                    .label(ei.span.clone(), format!("expected type '{}'", err.right))
                    .help("The types must be compatible for enum variant instantiation")
                    .build();
                data.errors.push(err_msg);
            }
        }
        // Unit Variant, good!
        (None, None) => {
            // Nothing to do here...
        }

        // Left: Syntactically, we have data (user made an error and mistakingly provided data), Right: No data was expected
        (Some(dexpr), None) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Enum variant '{}::{}' does not accept data, but data was provided",
                    ei.enum_name, ei.variant_name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(dexpr.span().clone(), "unexpected data provided here")
                .label(ei.span.clone(), "for this unit variant")
                .help(format!("Use '{}::{}' without parentheses for unit variants", ei.enum_name, ei.variant_name))
                .build();
            data.errors.push(err);
        }

        // Left: No data was provided, Right: But there is associated data registered => User error
        (None, Some(expected_ty)) => {
            let err = YuuError::builder()
                .kind(ErrorKind::InvalidExpression)
                .message(format!(
                    "Enum variant '{}::{}' expects data of type '{}', but no data was provided",
                    ei.enum_name, ei.variant_name, expected_ty
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(ei.span.clone(), "missing required data")
                .help(format!("Use '{}::{}(value)' with a value of type '{}'", ei.enum_name, ei.variant_name, expected_ty))
                .build();
            data.errors.push(err);
        }
    }
}

fn infer_enum_instantiation(
    ei: &EnumInstantiationExpr,
    block_id: usize,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    let enum_type = data.type_registry.resolve_enum(ei.enum_name);
    let inferred_ty = match enum_type {
        Some(enum_info) => {
            let variant_info_registered = enum_info.variants_info.fields.get(&ei.variant_name);
            match variant_info_registered {
                Some(variant) => {
                    validate_enum_variant_binding(ei, variant, block_id, data, function_args);
                    enum_info.wrapper_info.ty
                }
                None => {
                    let err = YuuError::builder()
                        .kind(ErrorKind::UndefinedVariant)
                        .message(format!(
                            "Enum '{}' has no variant named '{}'",
                            ei.enum_name, ei.variant_name
                        ))
                        .source(
                            data.src_code.source.clone(),
                            data.src_code.file_name.clone(),
                        )
                        .span(ei.span.clone(), format!("variant '{}' not defined", ei.variant_name))
                        .help(format!(
                            "Available variants for enum '{}': {}",
                            ei.enum_name,
                            if enum_info.variants_info.fields.is_empty() {
                                "none".to_string()
                            } else {
                                enum_info
                                    .variants_info
                                    .fields
                                    .keys()
                                    .map(|k| k.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ))
                        .build();
                    data.errors.push(err);
                    error_type()
                }
            }
        }
        None => {
            let err = YuuError::builder()
                .kind(ErrorKind::UndefinedEnum)
                .message(format!(
                    "Cannot instantiate undefined enum '{}'",
                    ei.enum_name
                ))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(ei.span.clone(), format!("enum '{}' not defined", ei.enum_name))
                .help("Define this enum before using it")
                .build();
            data.errors.push(err);
            error_type()
        }
    };
    data.type_info_table.insert(ei.id, inferred_ty);
    inferred_ty
}

pub fn infer_expr(
    expr: &ExprNode,
    block_id: usize,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> &'static TypeInfo {
    match expr {
        ExprNode::Literal(lit) => infer_literal_expr(lit, data),
        ExprNode::Binary(binary) => infer_binary_expr(binary, block_id, data),
        ExprNode::Unary(unary) => infer_unary_expr(unary, block_id, data),
        ExprNode::Ident(ident) => infer_ident_expr(ident, block_id, data, function_args),
        ExprNode::FuncCall(func_call) => infer_func_call_expr(func_call, block_id, data),
        ExprNode::Assignment(assignment) => infer_assignment(assignment, block_id, data),
        ExprNode::StructInstantiation(struct_instantiation_expr) => {
            infer_struct_instantiation(struct_instantiation_expr, block_id, data)
        }
        ExprNode::MemberAccess(member_access_expr) => {
            infer_member_access(member_access_expr, block_id, data)
        }
        ExprNode::EnumInstantiation(ei) => {
            infer_enum_instantiation(ei, block_id, data, function_args)
        }
        ExprNode::Deref(deref_expr) => infer_deref_expr(deref_expr, block_id, data),
        ExprNode::AddressOf(address_of_expr) => {
            infer_address_of_expr(address_of_expr, block_id, data)
        }
        ExprNode::HeapAlloc(heap_alloc_expr) => {
            infer_heap_alloc_expr(heap_alloc_expr, block_id, data)
        }
        ExprNode::Array(array_expr) => infer_array_expr(array_expr, block_id, data),
        ExprNode::ArrayLiteral(array_literal_expr) => {
            infer_array_literal_expr(array_literal_expr, block_id, data)
        }
        ExprNode::Cast(cast_expr) => infer_cast_expr(cast_expr, block_id, data),
        ExprNode::LuaMeta(_) => {
            // LuaMeta nodes should be processed in Lua execution phase before type inference
            // For now, return unknown type as placeholder
            unknown_type()
        }
    }
}

fn infer_heap_alloc_expr(
    heap_alloc_expr: &HeapAllocExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    // Always infer the type of the expression to be allocated
    let value_type = infer_expr(&heap_alloc_expr.expr, block_id, data, None);

    // Special case: for array expressions, use the element type directly
    // REASON: The expression would otherwise immediately decay to a pointer...
    let pointer_type = match &*heap_alloc_expr.expr {
        ExprNode::Array(_) | ExprNode::ArrayLiteral(_) => {
            value_type // Here, we already have a pointer, so we just don't do anything...
        }
        _ => {
            // Normal case: heap-allocate space for the value
            value_type.ptr_to()
        }
    };

    data.type_info_table
        .insert(heap_alloc_expr.id, pointer_type);

    pointer_type
}

fn infer_deref_expr(
    deref_expr: &DerefExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    let operand_type = infer_expr(&deref_expr.expr, block_id, data, None);

    let result_type = match operand_type {
        TypeInfo::Pointer(pointee_type) => pointee_type,
        _ => {
            let error = YuuError::builder()
                .kind(ErrorKind::NotAPointer)
                .message(format!("Cannot dereference non-pointer type '{}'", operand_type))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(
                    deref_expr.span.clone(),
                    format!("has type '{}', expected pointer", operand_type),
                )
                .help("The dereference operator (*) can only be applied to pointer types")
                .build();
            data.errors.push(error);
            error_type()
        }
    };

    data.type_info_table.insert(deref_expr.id, result_type);

    result_type
}

fn infer_address_of_expr(
    address_of_expr: &AddressOfExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    // Check that we can only take address of lvalues
    if !address_of_expr.expr.is_lvalue() {
        let error = YuuError::builder()
            .kind(ErrorKind::InvalidAddressOfExpression)
            .message("Can only take address of lvalues (variables, fields, array elements)")
            .source(data.src_code.source.clone(), data.src_code.file_name.clone())
            .span(
                (address_of_expr.span.start, address_of_expr.span.end - address_of_expr.span.start),
                "cannot take address of expression"
            )
            .help("Bind the expression first and then take the address of the identifier. For example: let temp = a + b; &temp")
            .build();
        data.errors.push(error);
        return crate::utils::type_info_table::error_type();
    }

    let operand_type = infer_expr(&address_of_expr.expr, block_id, data, None);

    let result_type = operand_type.ptr_to();

    data.type_info_table.insert(address_of_expr.id, result_type);

    result_type
}

fn infer_array_expr(
    array_expr: &crate::pass_parse::ast::ArrayExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    use crate::pass_type_inference::types::infer_type;

    // Determine element type
    let element_type = if let Some(explicit_type) = &array_expr.element_type {
        // Explicit type provided: [value:type; count] or [:type; count]
        infer_type(
            explicit_type,
            data.type_registry,
            data.errors,
            data.src_code,
        )
    } else if let Some(init_value) = &array_expr.init_expr {
        // Type inferred from init value: [value; count]
        infer_expr(init_value, block_id, data, None)
    } else {
        // This should not happen - we need either explicit type or init value
        panic!(
            "Compiler Bug: Array expression must have either explicit type or init value for type inference"
        );
    };

    // Verify size is an integer type
    let size_type = infer_expr(&array_expr.size, block_id, data, None);
    if !matches!(size_type, TypeInfo::BuiltInPrimitive(PrimitiveType::I64)) {
        data.errors.push(
            crate::pass_diagnostics::error::YuuError::builder()
                .kind(crate::pass_diagnostics::error::ErrorKind::TypeIncompatible)
                .message(format!("Array size must be 'i64', found '{}'", size_type))
                .source(
                    data.src_code.source.clone(),
                    data.src_code.file_name.clone(),
                )
                .span(array_expr.size.span(), format!("has type '{}', expected 'i64'", size_type))
                .help("Array sizes must be of type i64")
                .build(),
        );
        return error_type();
    }

    // Arrays are treated as pointers to the element type
    let result_type = element_type.ptr_to();

    data.type_info_table.insert(array_expr.id, result_type);

    result_type
}

fn infer_array_literal_expr(
    array_literal_expr: &ArrayLiteralExpr,
    block_id: usize,
    data: &mut TransientData,
) -> &'static TypeInfo {
    use crate::pass_type_inference::types::infer_type;

    debug_assert!(
        !array_literal_expr.elements.is_empty(),
        "Empty array literals should be rejected during parsing"
    );

    // Determine element type
    let element_type = if let Some(explicit_type) = &array_literal_expr.element_type {
        // Explicit type provided: [1:i64, 2, 3]
        infer_type(
            explicit_type,
            data.type_registry,
            data.errors,
            data.src_code,
        )
    } else {
        // Type inferred from first element: [1, 2, 3]
        infer_expr(&array_literal_expr.elements[0], block_id, data, None)
    };

    // Type check remaining elements against the determined element type
    for (i, element) in array_literal_expr.elements.iter().enumerate().skip(1) {
        let element_type_inferred = infer_expr(element, block_id, data, None);

        if element_type_inferred != element_type {
            data.errors.push(
                crate::pass_diagnostics::error::YuuError::builder()
                    .kind(crate::pass_diagnostics::error::ErrorKind::TypeIncompatible)
                    .message(format!(
                        "Array element {} has incompatible type '{}', expected '{}'",
                        i, element_type_inferred, element_type
                    ))
                    .source(
                        data.src_code.source.clone(),
                        data.src_code.file_name.clone(),
                    )
                    .span(element.span(), format!("has type '{}', expected '{}'", element_type_inferred, element_type))
                    .help("All array elements must have the same type")
                    .build(),
            );
        }
    }

    // Array literals are treated as pointers to the element type
    let result_type = element_type.ptr_to();

    data.type_info_table
        .insert(array_literal_expr.id, result_type);

    result_type
}
