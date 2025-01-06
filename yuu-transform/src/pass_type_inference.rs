use std::ops::Deref;

use hashbrown::HashMap;
use yuu_shared::{
    ast::*,
    binding_info::{BindingInfo, BindingInfoKind},
    block::{Block, FunctionOverloadError, RootBlock},
    context::Context,
    scheduler::Pass,
    type_info::{
        primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil, FunctionType,
        TypeInfo, TypeInfoTable,
    },
    Span,
};

use yuu_shared::semantic_error::SemanticError;

const MAX_SIMILAR_NAMES: u64 = 3;
const MIN_DST_SIMILAR_NAMES: u64 = 3;

pub struct BindingTable {
    pub bindings: HashMap<NodeId, BindingInfo>,
}

impl Default for BindingTable {
    fn default() -> Self {
        Self::new()
    }
}

impl BindingTable {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

pub struct TransientData<'a> {
    pub type_info_table: &'a mut TypeInfoTable,
    pub ast: &'a AST,
}

pub struct PassTypeInference {}

impl PassTypeInference {
    pub fn new() -> Self {
        Self {}
    }

    pub fn match_binding_node_to_type<'a, T>(
        binding: &BindingNode,
        ty: &'static TypeInfo,
        mut match_resolver: T,
        data: &'a mut TransientData,
    ) -> Result<(), SemanticError>
    where
        T: FnMut(
            &IdentBinding,
            &'static TypeInfo,
            &'a mut TypeInfoTable,
        ) -> Result<(), SemanticError>,
    {
        match binding {
            BindingNode::Ident(ident) => {
                match_resolver(ident, ty, &mut data.type_info_table)?;
            }
        };
        Ok(())
    }

    pub fn infer_type(
        ty: &TypeNode,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<&'static TypeInfo, SemanticError> {
        let semantic_type = match ty {
            TypeNode::BuiltIn(built_in) => match built_in.kind {
                yuu_shared::ast::BuiltInTypeKind::I64 => primitive_i64(),
                yuu_shared::ast::BuiltInTypeKind::F32 => primitive_f32(),
                yuu_shared::ast::BuiltInTypeKind::F64 => primitive_f64(),
                yuu_shared::ast::BuiltInTypeKind::Bool => primitive_bool(),
            },
            TypeNode::Ident(ident) => {
                let binding = block.get_binding(&ident.name);
                match binding {
                    Some(BindingInfoKind::Unique(var)) => data
                        .type_info_table
                        .types
                        .get(&var.id)
                        .cloned()
                        .expect("Compiler Bug: Variable binding not found in type table"),
                    Some(BindingInfoKind::Ambiguous(_)) => {
                        panic!("User bug: Ambiguous type identifier found when trying to create a type identifier");
                    }
                    None => {
                        let similar_names = block.get_similar_names(
                            &ident.name,
                            MAX_SIMILAR_NAMES,
                            MIN_DST_SIMILAR_NAMES,
                        );
                        panic!(
                            "User bug: Cannot find identifier `{}`, similar names: {:?}",
                            ident.name, similar_names
                        );
                    }
                }
            }
        };
        Ok(semantic_type)
    }

    pub fn infer_stmt(
        stmt: &StmtNode,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let ty_expr = Self::infer_expr(&let_stmt.expr, block, data, None)?;
                let mut match_resolver = move |ident_binding: &IdentBinding,
                                               ty: &'static TypeInfo,
                                               ty_info_table: &mut TypeInfoTable|
                      -> Result<(), SemanticError> {
                    block.insert_variable(
                        ident_binding.name.clone(),
                        ident_binding.id,
                        ident_binding.span.clone(),
                    );

                    ty_info_table.types.insert(ident_binding.id, ty);

                    Ok(())
                };

                Self::match_binding_node_to_type(
                    &let_stmt.binding,
                    &ty_expr,
                    match_resolver,
                    data,
                )?;
                Ok(())
            }
            StmtNode::Atomic(expr) => {
                Self::infer_expr(expr, block, data, None).map(|_| ())
                // TODO: We should probably check if the type of the expression is not nil - then we should return an error
            }
            StmtNode::Return(return_stmt) => {
                Self::infer_expr(&return_stmt.expr, block, data, None).map(|_| ())
            }
        }
    }

    pub fn infer_expr(
        expr: &ExprNode,
        block: &mut Block,
        data: &mut TransientData,
        function_args: Option<&[&'static TypeInfo]>,
    ) -> Result<&'static TypeInfo, SemanticError> {
        let out = match expr {
            ExprNode::Literal(lit) => {
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
            ExprNode::Binary(binary_expr) => {
                let lhs = Self::infer_expr(&binary_expr.left, block, data, None)?;
                let rhs = Self::infer_expr(&binary_expr.right, block, data, None)?;

                let op_name = binary_expr.op.static_name();

                let ty = block
                    .resolve_function(op_name, &data.type_info_table, &[lhs, rhs], |_, func| {
                        func.ret
                    })
                    .map_err(|err| panic!("User bug: Function overload error: {:?}", err))?;

                data.type_info_table.types.insert(binary_expr.id, ty);

                ty
            }
            ExprNode::Unary(unary_expr) => {
                let ty = Self::infer_expr(&unary_expr.operand, block, data, None)?;
                let op_name = unary_expr.op.static_name();

                let result_ty = block
                    .resolve_function(op_name, &data.type_info_table, &[ty], |_, func| func.ret)
                    .map_err(|err| panic!("User bug: Function overload error"))?;

                data.type_info_table.types.insert(unary_expr.id, result_ty);

                result_ty
            }
            ExprNode::Ident(ident_expr) => {
                let binding = block.get_binding(&ident_expr.ident).ok_or_else(|| {
                    let similar_names = block.get_similar_names(
                        &ident_expr.ident,
                        MAX_SIMILAR_NAMES,
                        MIN_DST_SIMILAR_NAMES,
                    );

                    panic!("User bug: Cannot find identifier `{}`", ident_expr.ident);
                })?;

                let out = match binding {
                    BindingInfoKind::Unique(var) => data
                        .type_info_table
                        .types
                        .get(&var.id)
                        .cloned()
                        .expect("Compiler Bug: Variable binding not found in type table"),
                    BindingInfoKind::Ambiguous(funcs) => {
                        let binding_info = resolve_function_overload(
                            &funcs,
                            &data.type_info_table,
                            function_args.unwrap_or_default(),
                        )
                        .expect("User bug: Function overload error");

                        let resolved_func_type = data
                            .type_info_table
                            .types
                            .get(&binding_info.id)
                            .cloned()
                            .expect("Compiler Bug: Function binding not found in type table");

                        data.type_info_table
                            .types
                            .insert(ident_expr.id, resolved_func_type);

                        resolved_func_type
                    }
                };

                data.type_info_table.types.insert(ident_expr.id, out);

                out
            }
            ExprNode::Block(block_expr) => {
                let block_scope = block.make_child();

                let mut last_stmt_type = primitive_nil();

                for stmt in &block_expr.body {
                    match stmt {
                        StmtNode::Return(ret) => {
                            last_stmt_type = Self::infer_expr(&ret.expr, block_scope, data, None)?;
                            break; // Early return
                        }
                        _ => {
                            Self::infer_stmt(stmt, block_scope, data)?;
                        }
                    }
                }

                data.type_info_table
                    .types
                    .insert(block_expr.id, last_stmt_type);
                last_stmt_type
            }

            ExprNode::FuncCall(func_call_expr) => {
                // Here, we have to resolve the overloaded function

                let actual_arg_types = func_call_expr
                    .args
                    .iter()
                    .map(|arg| Self::infer_expr(arg, block, data, None))
                    .collect::<Result<Vec<_>, _>>()?;

                let actual_func_ident =
                    Self::infer_expr(&func_call_expr.lhs, block, data, Some(&actual_arg_types))?;

                let resolved_ret_type = match actual_func_ident {
                    TypeInfo::Function(func) => {
                        func.ret
                    }
                    TypeInfo::BuiltIn(_) => panic!("User bug: Cannot call a built-in type as a function"), 
                    TypeInfo::Function(_) => unreachable!("Compiler Bug: Function type should be a function group - overloading is allowed."),
                    TypeInfo::Pointer(_) => panic!("User bug: Cannot call a pointer type as a function"),
                };

                data.type_info_table
                    .types
                    .insert(func_call_expr.id, resolved_ret_type);

                resolved_ret_type
            }
            ExprNode::If(if_expr) => Self::infer_if_expr(if_expr, block, data)?,
        };
        Ok(out)
    }

    pub fn infer_if_expr(
        expr: &IfExpr,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<&'static TypeInfo, SemanticError> {
        let cond_ty = Self::infer_expr(&expr.if_block.condition, block, data, None)?;
        // TODO: Check if the condition type is a boolean
        if !cond_ty.does_coerce_to_same_type(primitive_bool()) {
            panic!("User bug: Condition of if expr must be of type bool");
        }

        let then_ty = Self::infer_expr(&expr.if_block.body, block, data, None)?;
        let if_types = expr.else_if_blocks.iter().map(|x| {
            // First check if we have a bool - if not -> error!

            if !cond_ty.does_coerce_to_same_type(primitive_bool()) {
                panic!("User bug: Condition of if expr must be of type bool");
            }

            let ty = Self::infer_expr(&x.body, block, data, None)?;
            Ok::<_, SemanticError>(ty)
        });

        let out_ty = if_types.into_iter().try_fold(then_ty, |acc, ty| {
            // Check if acc and ty coerce to same type
            let ty = ty?;
            if !acc.does_coerce_to_same_type(&ty) {
                panic!("User bug: Types of if expr branches don't coerce to same type");
            }
            Ok::<_, SemanticError>(ty)
        })?;

        if let Some(else_body) = expr.else_block.as_ref() {
            let ty = Self::infer_expr(&else_body, block, data, None)?;
            if !out_ty.does_coerce_to_same_type(&ty) {
                panic!("User bug: Types of if expr branches don't coerce to same type");
            }
        };

        // Put that thing into the type info table
        data.type_info_table.types.insert(expr.id, out_ty);
        Ok(out_ty)
    }

    pub fn declare_function(
        name: &str,
        args: &[FuncArg],
        ret_ty: &Option<Box<TypeNode>>,
        id: NodeId,
        span: Span,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        block
            .declare_function(name.to_string(), id, span.clone())
            .map_err(|err| panic!("_"))?;

        let func_arg_types = args
            .iter()
            .map(|arg| {
                let semantic_arg_type = Self::infer_type(&arg.ty, block, data)?;

                Self::match_binding_node_to_type(
                    &arg.binding,
                    &semantic_arg_type,
                    |ident_binding, ty, ty_info_table| {
                        block.insert_variable(
                            ident_binding.name.clone(),
                            ident_binding.id,
                            ident_binding.span.clone(),
                        );

                        ty_info_table.types.insert(ident_binding.id, ty);

                        Ok(())
                    },
                    data,
                )?;

                Ok::<_, SemanticError>(semantic_arg_type)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let ret_type = if let Some(ty) = ret_ty {
            Self::infer_type(&ty, block, data)?
        } else {
            primitive_nil()
        };

        let func = FunctionType {
            args: func_arg_types,
            ret: ret_type,
        }
        .into();

        data.type_info_table.types.insert(id, func);

        Ok(())
    }

    pub fn infer_structural(
        structural: &StructuralNode,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match structural {
            StructuralNode::FuncDecl(decl) => Self::declare_function(
                &decl.name,
                &decl.args,
                &decl.ret_ty,
                decl.id,
                decl.span.clone(),
                block,
                data,
            ),
            StructuralNode::FuncDef(def) => {
                Self::declare_function(
                    &def.decl.name,
                    &def.decl.args,
                    &def.decl.ret_ty,
                    def.id,
                    def.span.clone(),
                    block,
                    data,
                )?;

                let func_block = block.make_child();

                for arg in &def.decl.args {
                    if let BindingNode::Ident(ident) = &arg.binding {
                        func_block.insert_variable(
                            ident.name.clone(),
                            ident.id,
                            ident.span.clone(),
                        );
                    }
                }

                // Process function body separately
                for stmt in &def.body.body {
                    Self::infer_stmt(stmt, func_block, data)?;
                }
                Ok(())
            }
        }
    }

    pub fn infer(
        ast: &Node,
        block: &mut Block,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match ast {
            Node::Structural(structural) => Self::infer_structural(structural, block, data),
            Node::Stmt(stmt) => Self::infer_stmt(stmt, block, data),
            Node::Expr(expr) => {
                let _ = Self::infer_expr(expr, block, data, None)?;
                Ok(())
            }
            Node::Binding(_) => {
                unreachable!("Compiler Bug: Cannot infer a binding without a type")
            }
            Node::Type(ty) => {
                let _ = Self::infer_type(ty, block, data)?;
                Ok(())
            }
        }
    }
}

pub fn resolve_function_overload(
    candidate_funcs: &[BindingInfo],
    type_info_table: &TypeInfoTable,
    actual_args: &[&'static TypeInfo],
) -> Result<BindingInfo, FunctionOverloadError> {
    for candidate in candidate_funcs {
        if let Some(func_type) = type_info_table.types.get(&candidate.id) {
            if let TypeInfo::Function(func) = func_type {
                // Check if argument lengths match
                if func.args.len() != actual_args.len() {
                    continue;
                }

                // Check if all arguments coerce
                let mut all_args_match = true;
                for (expected, actual) in func.args.iter().zip(actual_args.iter()) {
                    if !actual.does_coerce_to_same_type(expected) {
                        all_args_match = false;
                        break;
                    }
                }

                if all_args_match {
                    return Ok(candidate.clone());
                }
            }
        }
    }

    Err(FunctionOverloadError::NoOverloadFound)
}

// impl Pass for PassTypeInference {
//     fn run(&mut self, context: &mut yuu_shared::Context) -> bool {
//         let src_info: Rc<RefCell<SourceCodeInfo>> =
//             context.require_pass_data::<SourceCodeInfo>("TypeInference");
//         let root = context.require_pass_data::<RootBlock>("TypeInference");
//         let type_info_table = context.require_pass_data::<TypeInfoTable>("TypeInference");

//         let src_info = src_info.as_ref().borrow();
//         let src_cache = src_info.cache.clone();
//         let mut type_info_table = type_info_table.as_ref().borrow_mut();
//         let type_info_table = type_info_table.deref_mut();

//         let mut data = TransientData {
//             src_cache,
//             type_info_table,
//         };

//         let mut out;
//         let root = root.as_ref().borrow();

//         for node in &src_info.root_node.structurals {
//             out = Self::infer_structural(node, &root, &mut data);
//             if let Err(_) = out {
//                 return false;
//             }
//         }

//         return true;
//     }

//     fn install(self, pipeline: &mut yuu_shared::Pipeline) {
//         pipeline.add_pass(self);
//     }
// }

impl Pass for PassTypeInference {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let root_block = context.require_pass_data::<RootBlock>(self);
        let ast = context.require_pass_data::<AST>(self);

        let mut root_block = root_block.lock().unwrap();
        let mut type_info_table = TypeInfoTable::new();
        let ast = ast.lock().unwrap();
        let ast = &*ast;

        let mut data = TransientData {
            ast,
            type_info_table: &mut type_info_table,
        };

        for node in &ast.structurals {
            Self::infer_structural(node, root_block.root_mut(), &mut data);
        }

        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_write::<RootBlock>(&self);
        schedule.produces_resource::<TypeInfoTable>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        todo!()
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use yuu_parse::{lexer::UnprocessedCodeInfo, parser::Parser};
//     use yuu_shared::context::Context;
//     use yuu_shared::pipeline::Pipeline;

//     fn create_test_context() -> Context {
//         Context::new()
//     }
//     #[test]
//     fn test_basic() {
//         let code_info = UnprocessedCodeInfo {
//             code: "fn test() {out 1 + 2;}",
//             file_name: "test.yuu",
//         };
//         let mut parser = Parser::new(&code_info);
//         let mut ctxt = parser.parse().expect("Parser error");

//         let mut pipeline = Pipeline::new();
//         let pass_type_inference = PassTypeInference::new();
//         pass_type_inference.install(&mut pipeline);

//         pipeline.run(&mut ctxt);

//         let tit = ctxt.require_pass_data::<TypeInfoTable>("Test");
//         let tit = tit.as_ref().borrow();

//         for (idx, ty) in tit.types.iter() {
//             println!("Node with Idx={idx}: {ty}");
//         }
//     }

//     #[test]
//     fn test_fac() {
//         let code_info = UnprocessedCodeInfo {
//             code: r#"fn fac(n: i64) -> i64 {
//             out if n == 0 {
//                 out 1;
//             }
//             else {
//                 let n_out = n * fac(n - 1);
//                 out n_out;
//             };
//         }"#,
//             file_name: "test.yuu",
//         };

//         let mut parser = Parser::new(&code_info);
//         let mut ctxt = parser.parse().expect("Parser error");

//         let mut pipeline = Pipeline::new();
//         let pass_type_inference = PassTypeInference::new();
//         pass_type_inference.install(&mut pipeline);

//         pipeline.run(&mut ctxt);

//         let tit = ctxt.require_pass_data::<TypeInfoTable>("Test");
//         let tit = tit.as_ref().borrow();

//         for (idx, ty) in tit.types.iter() {
//             println!("Node with Idx={idx}: {ty}");
//         }
//     }
// }
