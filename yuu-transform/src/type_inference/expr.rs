use yuu_parse::add_ids::GetId;
use yuu_shared::{
    ast::{
        AssignmentExpr, BinaryExpr, BlockExpr, BreakStmt, ExprNode, FuncCallExpr, IdentExpr,
        IfExpr, LiteralExpr, StmtNode, UnaryExpr,
    },
    binding_info::{BindingInfo, BindingInfoKind},
    block::Block,
    block::FUNC_BLOCK_NAME,
    semantic_error::SemanticError,
    type_info::{
        inactive_type, primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil,
        TypeInfo,
    },
};

const MAX_SIMILAR_NAMES: u64 = 3;
const MIN_DST_SIMILAR_NAMES: u64 = 3;

use super::{pass_type_inference::TransientData, resolve_function_overload};

fn infer_literal(lit: &LiteralExpr, data: &mut TransientData) -> &'static TypeInfo {
    let out = match lit.lit.kind {
        yuu_shared::token::TokenKind::Integer(integer) => match integer {
            yuu_shared::token::Integer::I64(_) => primitive_i64(),
        },
        yuu_shared::token::TokenKind::F32(_) => primitive_f32(),
        yuu_shared::token::TokenKind::F64(_) => primitive_f64(),
        _ => unreachable!("Compiler bug: Literal not implemented"),
    };
    data.type_info_table.types.insert(lit.id, out);
    out
}

fn infer_binary(
    binary_expr: &BinaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let lhs = infer_expr(&binary_expr.left, block, data, None)?;
    let rhs = infer_expr(&binary_expr.right, block, data, None)?;

    let op_name = binary_expr.op.static_name();
    let ty = block
        .resolve_function(op_name, data.type_info_table, &[lhs, rhs], |_, func| {
            func.ret
        })
        .map_err(|err| panic!("User bug: Function overload error: {:?}", err))?;

    data.type_info_table.types.insert(binary_expr.id, ty);
    Ok(ty)
}

fn infer_unary(
    unary_expr: &UnaryExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let ty = infer_expr(&unary_expr.operand, block, data, None)?;
    let op_name = unary_expr.op.static_name();

    let result_ty = block
        .resolve_function(op_name, data.type_info_table, &[ty], |_, func| func.ret)
        .map_err(|_err| panic!("User bug: Function overload error"))?;

    data.type_info_table.types.insert(unary_expr.id, result_ty);
    Ok(result_ty)
}

fn infer_ident(
    ident_expr: &IdentExpr,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> Result<&'static TypeInfo, SemanticError> {
    let binding = block.get_binding(&ident_expr.ident).ok_or_else(|| {
        let similar_names =
            block.get_similar_names(&ident_expr.ident, MAX_SIMILAR_NAMES, MIN_DST_SIMILAR_NAMES);

        panic!(
            "User bug: Cannot find identifier `{}`, similar names: {:?}",
            ident_expr.ident, similar_names
        );
    })?;

    let (ty, nid) = match binding {
        BindingInfoKind::Unique(var) => (
            data.type_info_table
                .types
                .get(&var.id)
                .cloned()
                .expect("Compiler Bug: Variable binding not found in type table"),
            var.id,
        ),
        BindingInfoKind::Ambiguous(funcs) => {
            let binding_info = resolve_function_overload(
                &funcs,
                data.type_info_table,
                function_args.unwrap_or_default(),
            )
            .expect("User bug: Function overload error");

            let resolved_func_type = data
                .type_info_table
                .types
                .get(&binding_info.id)
                .cloned()
                .expect("Compiler Bug: Function binding not found in type table");

            // data.type_info_table
            //     .types
            //     .insert(ident_expr.id, resolved_func_type);

            (resolved_func_type, binding_info.id)
        }
    };

    data.type_info_table.types.insert(ident_expr.id, ty);
    data.binding_table.insert(ident_expr.id, nid);
    Ok(ty)
}

pub fn infer_block_no_child_creation(
    block_expr: &BlockExpr,
    root_func_block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let mut break_ty = None;

    for stmt in &block_expr.body {
        let out = super::infer_stmt(stmt, root_func_block, data)?;
        if let super::ExitKind::Break = out {
            // Store break type but keep processing - we'll unify with last expr
            break_ty = Some(inactive_type());
        }
    }

    // Get last expression type if it exists
    let last_expr_ty = if let Some(last_expr) = &block_expr.last_expr {
        let expr = infer_expr(last_expr, root_func_block, data, None)?;
        Some(expr)
    } else {
        None
    };

    // Determine type from breaks and last expression
    let block_ty = match (break_ty, last_expr_ty) {
        (Some(brk), Some(last)) => brk.unify(last).unwrap_or(inactive_type()),
        (Some(brk), None) => brk,
        (None, Some(last)) => last,
        (None, None) => primitive_nil(),
    };

    // Unify with any existing type from breaks TO this block
    let out = data
        .type_info_table
        .unify_and_insert(block_expr.id, block_ty)
        .expect("User bug: Couldn't unify block type");
    Ok(out)
}

pub fn infer_block(
    block_expr: &BlockExpr,
    parent_block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let child_block = if let Some(label) = &block_expr.label {
        let binding = BindingInfo {
            id: block_expr.id,
            src_location: Some(block_expr.span.clone()),
            is_mut: false,
        };
        parent_block.make_child(Some((label.clone(), binding)))
    } else {
        parent_block.make_child(None)
    };

    infer_block_no_child_creation(block_expr, child_block, data)
}

fn infer_func_call(
    func_call_expr: &FuncCallExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let actual_arg_types = func_call_expr
        .args
        .iter()
        .map(|arg| infer_expr(arg, block, data, None))
        .collect::<Result<Vec<_>, _>>()?;

    let actual_func_ident = infer_expr(&func_call_expr.lhs, block, data, Some(&actual_arg_types))?;

    let resolved_ret_type = match actual_func_ident {
        TypeInfo::Function(func) => func.ret,
        TypeInfo::BuiltIn(_) => {
            panic!("User bug: Cannot call a built-in type as a function")
        }
        TypeInfo::Pointer(_) => {
            panic!("User bug: Cannot call a pointer type as a function")
        }
        TypeInfo::Inactive => {
            panic!(
                "Compiler bug: Inactive type as return type of function - this should never happen"
            )
        }
    };

    data.type_info_table
        .types
        .insert(func_call_expr.id, resolved_ret_type);

    Ok(resolved_ret_type)
}

fn requires_mut(binding: &BindingInfo) -> bool {
    binding.is_mut
}

fn infer_assignment(
    assignment_expr: &AssignmentExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    let binding = &assignment_expr.binding;
    let binding_ty = data
        .type_info_table
        .types
        .get(&binding.node_id())
        .cloned()
        .expect("Compiler bug: binding not found in type table");

    // Check mutability
    if !binding.is_mut() {
        panic!("User Error: Cannot assign to immutable binding");
    }

    let value = infer_expr(&assignment_expr.rhs, block, data, None)?;

    // Prevent binding inactive types
    if matches!(value, TypeInfo::Inactive) {
        panic!("User Error: Cannot bind a value-less expression to a variable. This happens when all paths in the expression return or break");
    }

    // First unify to check compatibility
    let unified = value
        .unify(binding_ty)
        .expect("User bug: Couldn't unify assignment type");

    // Then insert the unified type and return it
    let out = data
        .type_info_table
        .unify_and_insert(assignment_expr.id, unified)
        .expect("User bug: Couldn't unify assignment type");

    Ok(out)
}

fn infer_if_expr(
    expr: &IfExpr,
    block: &mut Block,
    data: &mut TransientData,
) -> Result<&'static TypeInfo, SemanticError> {
    // Check condition
    let cond_ty = infer_expr(&expr.if_block.condition, block, data, None)?;
    if let Err(err) = cond_ty.unify(primitive_bool()) {
        panic!(
            "User Error: Condition must be of type bool, got {}",
            err.from
        );
    }

    // This creates block A
    let then_ty = infer_block(&expr.if_block.body, block, data)?;

    // Process else-if blocks...
    let if_types = expr.else_if_blocks.iter().map(|x| {
        // First check if we have a bool - if not -> error!
        if let Err(err) = cond_ty.unify(primitive_bool()) {
            panic!(
                "User Error: Condition must be of type bool, got {}",
                err.from
            );
        }
        // This creates block B
        let ty = infer_block(&x.body, block, data)?;
        Ok::<_, SemanticError>(ty)
    });

    let out_ty = if_types.into_iter().try_fold(then_ty, |acc, ty| {
        if matches!(acc, TypeInfo::Inactive) {
            return ty;
        }

        // Unify the types and use the unified type
        let ty = ty?;
        match acc.unify(ty) {
            Ok(unified) => Ok(unified),
            Err(_) => panic!(
                "User Error: Types of if expr branches don't coerce to same type: {} and {}",
                acc, ty
            ),
        }
    })?;

    // And here is where the extra block gets created - when we call infer_block
    // on the else block even though we already have a block for the else path
    if let Some(else_body) = expr.else_block.as_ref() {
        // This creates block C - even though we already have a block for the else path!
        let ty = infer_block(else_body, block, data)?;
        if let Err(_) = out_ty.unify(ty) {
            panic!(
                "User Error: Types of if expr branches don't coerce to same type: {} and {}",
                out_ty, ty
            );
        }
    };

    // Put that thing into the type info table
    data.type_info_table.types.insert(expr.id, out_ty);
    Ok(out_ty)
}

pub fn infer_expr(
    expr: &ExprNode,
    block: &mut Block,
    data: &mut TransientData,
    function_args: Option<&[&'static TypeInfo]>,
) -> Result<&'static TypeInfo, SemanticError> {
    match expr {
        ExprNode::Literal(lit) => Ok(infer_literal(lit, data)),
        ExprNode::Binary(binary) => infer_binary(binary, block, data),
        ExprNode::Unary(unary) => infer_unary(unary, block, data),
        ExprNode::Ident(ident) => infer_ident(ident, block, data, function_args),
        ExprNode::Block(block_expr) => infer_block(block_expr, block, data),
        ExprNode::FuncCall(func_call) => infer_func_call(func_call, block, data),
        ExprNode::If(if_expr) => infer_if_expr(if_expr, block, data),
        ExprNode::Assignment(assignment) => infer_assignment(assignment, block, data),
    }
}
