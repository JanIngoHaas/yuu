use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    ops::Deref,
    rc::Rc,
};

use hashbrown::HashMap;
use yuu_parse::{
    parser::{default_src_cache, SourceCodeInfo, SrcCache},
    Span,
};
use yuu_shared::{ast::*, Pass};

use crate::{
    binding_info::{BindingInfo, BindingInfoKind},
    block::{Block, FunctionOverloadError},
    semantic_error::SemanticError,
    type_info::{BuiltInType, FunctionGroupKind, FunctionType, TypeInfo, TypeInfoTable},
};

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

pub struct TransientData {
    pub src_cache: SrcCache,
    pub type_info_table: TypeInfoTable,
}

pub struct PassTypeInference {}

impl PassTypeInference {
    pub fn new() -> Self {
        Self {}
    }

    pub fn match_binding_node_to_type<'a, T>(
        binding: &BindingNode,
        ty: &Rc<TypeInfo>,
        mut match_resolver: T,
        data: &'a mut TransientData,
    ) -> Result<(), SemanticError>
    where
        T: FnMut(&IdentBinding, &Rc<TypeInfo>, &'a mut TypeInfoTable) -> Result<(), SemanticError>,
    {
        match binding {
            BindingNode::Ident(ident) => {
                match_resolver(ident, ty, &mut data.type_info_table)?;
            }
        };
        Ok(())
    }

    pub fn infer_type(
        ty: &yuu_shared::ast::TypeNode,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<Rc<TypeInfo>, SemanticError> {
        let semantic_type = match ty {
            TypeNode::BuiltIn(built_in) => match built_in.kind {
                yuu_shared::ast::BuiltInTypeKind::I64 => {
                    Rc::new(TypeInfo::BuiltIn(BuiltInType::I64))
                }
                yuu_shared::ast::BuiltInTypeKind::F32 => {
                    Rc::new(TypeInfo::BuiltIn(BuiltInType::F32))
                }
                yuu_shared::ast::BuiltInTypeKind::F64 => {
                    Rc::new(TypeInfo::BuiltIn(BuiltInType::F64))
                }
            },
            TypeNode::Ident(ident) => {
                let binding = block.as_ref().borrow().get_binding(&ident.name);
                match binding {
                    Some(BindingInfoKind::Unique(var)) => data
                        .type_info_table
                        .types
                        .get(&var.id)
                        .cloned()
                        .expect("Compiler Bug: Variable binding not found in type table"),
                    Some(BindingInfoKind::Ambiguous(funcs)) => Rc::new(TypeInfo::FunctionGroup(
                        RefCell::new(FunctionGroupKind::Unresolved(funcs.clone())),
                    )),
                    None => {
                        let similar_names = block.as_ref().borrow().get_similar_names(
                            &ident.name,
                            MAX_SIMILAR_NAMES,
                            MIN_DST_SIMILAR_NAMES,
                        );
                        panic!("User bug: Cannot find identifier `{}`", ident.name);
                        //return Err(smessages);
                    }
                }
            }
        };
        Ok(semantic_type)
    }

    pub fn infer_stmt(
        stmt: &StmtNode,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let ty_expr = Self::infer_expr(&let_stmt.expr, block, data)?;
                let match_resolver = move |ident_binding: &IdentBinding,
                                           ty: &Rc<TypeInfo>,
                                           ty_info_table: &mut TypeInfoTable|
                      -> Result<(), SemanticError> {
                    block.as_ref().borrow_mut().insert_variable(
                        ident_binding.name.clone(),
                        ident_binding.id,
                        ident_binding.span.clone(),
                    );

                    ty_info_table.types.insert(ident_binding.id, ty.clone());

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
                Self::infer_expr(expr, block, data).map(|_| ())
                // TODO: We should probably check if the type of the expression is not nil - then we should return an error
            }
            StmtNode::Return(return_stmt) => {
                Self::infer_expr(&return_stmt.expr, block, data).map(|_| ())
            }
        }
    }

    pub fn infer_expr(
        expr: &ExprNode,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<Rc<TypeInfo>, SemanticError> {
        let out = match expr {
            ExprNode::Literal(lit) => {
                let out = match lit.lit.kind {
                    yuu_shared::token::TokenKind::Integer(_) => {
                        Rc::new(TypeInfo::BuiltIn(BuiltInType::I64))
                    }
                    yuu_shared::token::TokenKind::F32(_) => {
                        Rc::new(TypeInfo::BuiltIn(BuiltInType::F32))
                    }
                    yuu_shared::token::TokenKind::F64(_) => {
                        Rc::new(TypeInfo::BuiltIn(BuiltInType::F64))
                    }
                    _ => unreachable!("Compiler bug: Literal not implemented"),
                };
                data.type_info_table.types.insert(lit.id, out.clone());
                out
            }
            ExprNode::Binary(binary_expr) => {
                let lhs = Self::infer_expr(&binary_expr.left, block, data)?;
                let rhs = Self::infer_expr(&binary_expr.right, block, data)?;

                let op_name = binary_expr.op.static_name();

                let ty = block
                    .as_ref()
                    .borrow()
                    .resolve_function(
                        op_name,
                        &data.type_info_table,
                        &[&lhs, &rhs],
                        |binding, func| func.ret.clone(),
                    )
                    .map_err(|err| panic!("User bug: Function overload error: {:?}", err))?;

                data.type_info_table
                    .types
                    .insert(binary_expr.id, ty.clone());

                ty
            }
            ExprNode::Unary(unary_expr) => {
                let ty = Self::infer_expr(&unary_expr.operand, block, data)?;
                let op_name = unary_expr.op.static_name();

                let result_ty = block
                    .as_ref()
                    .borrow()
                    .resolve_function(op_name, &data.type_info_table, &[&ty], |binding, func| {
                        func.ret.clone()
                    })
                    .map_err(|err| panic!("User bug: Function overload error"))?;

                data.type_info_table
                    .types
                    .insert(unary_expr.id, result_ty.clone());

                result_ty
            }
            ExprNode::Ident(ident_expr) => {
                let binding = block
                    .as_ref()
                    .borrow()
                    .get_binding(&ident_expr.ident)
                    .ok_or_else(|| {
                        let similar_names = block.as_ref().borrow().get_similar_names(
                            &ident_expr.ident,
                            MAX_SIMILAR_NAMES,
                            MIN_DST_SIMILAR_NAMES,
                        );

                        panic!("User bug: Cannot find identifier `{}`", ident_expr.ident);

                        // SemanticError::Generic(GenericSemanticErrorMsg::new(
                        //     self.src_cache.clone(),
                        //     format!("Cannot find identifier `{}`", ident_expr.ident),
                        //     ident_expr.span.clone(),
                        //     if !similar_names.is_empty() {
                        //         vec![Note {
                        //             message: format!("Did you mean: {}", similar_names.join(", ")),
                        //             span: None,
                        //             note_type: NoteType::Help,
                        //         }]

                        //     } else {
                        //         vec![],
                        //     },
                        //     Severity::Error,
                        // ))
                    })?;

                let out = match binding {
                    BindingInfoKind::Unique(var) => {
                        // For variables, just look up and return their type
                        data.type_info_table
                            .types
                            .get(&var.id)
                            .cloned()
                            .expect("Compiler Bug: Variable binding not found in type table")
                    }
                    BindingInfoKind::Ambiguous(funcs) => {
                        // For functions, return a function group type that can be resolved later
                        // during function call type checking
                        Rc::new(TypeInfo::FunctionGroup(RefCell::new(
                            FunctionGroupKind::Unresolved(funcs),
                        )))
                    }
                };

                // Insert the type into the type table
                data.type_info_table
                    .types
                    .insert(ident_expr.id, out.clone());

                out
            }
            ExprNode::Block(block_expr) => {
                let block_scope = Block::from_parent(block.clone());

                let mut last_stmt_type = Rc::new(TypeInfo::BuiltIn(BuiltInType::Nil));

                for stmt in &block_expr.body {
                    match stmt {
                        StmtNode::Return(ret) => {
                            last_stmt_type = Self::infer_expr(&ret.expr, &block_scope, data)?;
                            break; // Early return
                        }
                        _ => {
                            Self::infer_stmt(stmt, &block_scope, data)?;
                        }
                    }
                }

                data.type_info_table
                    .types
                    .insert(block_expr.id, last_stmt_type.clone());
                last_stmt_type
            }

            ExprNode::FuncCall(func_call_expr) => {
                // Here, we have to resolve the overloaded function

                let mut actual_func_ident = Self::infer_expr(&func_call_expr.lhs, block, data)?;

                let actual_arg_types = func_call_expr
                    .args
                    .iter()
                    .map(|arg| Self::infer_expr(arg, block, data))
                    .collect::<Result<Vec<_>, _>>()?;

                let (resolved_ret_type, resolved_func) = match actual_func_ident.as_ref() {
                    TypeInfo::FunctionGroup(func_candidates) => {
                        match &*func_candidates.borrow() {
                            FunctionGroupKind::Resolved(resolved_func_type) => {
                                // If we already resolved this function group, we can just use that type
                                if let TypeInfo::Function(func) = &**resolved_func_type {
                                    if func.args.len() != actual_arg_types.len() {
                                        panic!("User bug: Expected {} arguments, got {}", func.args.len(), actual_arg_types.len());
                                        // return Err(SemanticError::Generic(GenericSemanticErrorMsg::new(
                                        //     self.src_cache.clone(),
                                        //     format!("Expected {} arguments, got {}", func.args.len(), actual_arg_types.len()),
                                        //     func_call_expr.span.clone(),
                                        //     vec![],
                                        //     Severity::Error,
                                        // )));
                                    }

                                    for (expected, got) in func.args.iter().zip(actual_arg_types.iter()) {
                                        if !got.does_coerce_to_same_type(expected) {
                                            panic!("User bug: Argument type does not match expected type");
                                        }
                                    }

                                    Ok((func.ret.clone(), resolved_func_type.clone()))
                                } else {
                                    unreachable!("Compiler Bug: Resolved function group contains non-function type")
                                }
                            }
                            FunctionGroupKind::Unresolved(func_candidates) => {
                                let (_, resolved_generic_ty, resolved_func_type) = resolve_function_group(
                                    func_candidates,
                                    &data.type_info_table,
                                    actual_arg_types.as_slice(),
                                ).map_err(|x| panic!("User bug: Function overload error"))?;
                                Ok((resolved_func_type.ret.clone(), resolved_generic_ty))
                            }
                        }
                    }
                    TypeInfo::BuiltIn(_) => panic!("User bug: Cannot call a built-in type as a function"), 
                    // Err(SemanticError::Generic(GenericSemanticErrorMsg::new(
                    //     self.src_cache.clone(),
                    //     "Cannot call a built-in type as a function".to_string(),
                    //     func_call_expr.span.clone(),
                    //     vec![],
                    //     Severity::Error,
                    // ))),
                    TypeInfo::Function(_) => unreachable!("Compiler Bug: Function type should be a function group - overloading is allowed."),
                }?;

                data.type_info_table
                    .types
                    .insert(func_call_expr.id, resolved_ret_type.clone());

                // If we had a function group, we can now overwrite the Rc<TypeInfo> with the resolved function type
                // This way, we can later just look up the type in the type table, without having to resolve the function group again
                let _ = std::mem::replace(actual_func_ident.borrow_mut(), resolved_func);

                resolved_ret_type
            }
            ExprNode::If(if_expr) => infer_if_expr(if_expr, block, data)?,
        };
        Ok(out)
    }

    pub fn infer_if_expr(
        expr: &IfExpr,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<Rc<TypeInfo>, SemanticError> {
        let cond_ty = Self::infer_expr(&expr.if_block.condition, block, data)?;
        // TODO: Check if the condition type is a boolean

        let then_ty = Self::infer_expr(&expr.if_block.body, block, data)?;
    }

    fn declare_function(
        name: &str,
        args: &[FuncArg],
        ret_ty: Option<&TypeNode>, // TODO: This should not be an Option!
        id: NodeId,
        span: Span,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        block
            .as_ref()
            .borrow_mut()
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
                        block.as_ref().borrow_mut().insert_variable(
                            ident_binding.name.clone(),
                            ident_binding.id,
                            ident_binding.span.clone(),
                        );

                        ty_info_table.types.insert(ident_binding.id, ty.clone());

                        Ok(())
                    },
                    data,
                )?;

                Ok::<_, SemanticError>(semantic_arg_type)
            })
            .collect::<Result<Rc<[_]>, _>>()?;

        let ret_type = if let Some(ty) = ret_ty {
            Self::infer_type(ty, block, data)?
        } else {
            Rc::new(TypeInfo::BuiltIn(BuiltInType::I64))
        };

        let func = Rc::new(TypeInfo::Function(FunctionType {
            args: func_arg_types,
            ret: ret_type,
        }));

        data.type_info_table.types.insert(id, func.clone());

        Ok(())
    }

    pub fn infer_structural(
        structural: &StructuralNode,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match structural {
            StructuralNode::FuncDecl(decl) => Self::declare_function(
                &decl.name,
                &decl.args,
                decl.ret_ty.as_deref(),
                decl.id,
                decl.span.clone(),
                block,
                data,
            ),
            StructuralNode::FuncDef(def) => {
                Self::declare_function(
                    &def.decl.name,
                    &def.decl.args,
                    def.decl.ret_ty.as_deref(),
                    def.id,
                    def.span.clone(),
                    block,
                    data,
                )?;

                let func_block = Block::from_parent(block.clone());

                for arg in &def.decl.args {
                    if let BindingNode::Ident(ident) = &arg.binding {
                        func_block.as_ref().borrow_mut().insert_variable(
                            ident.name.clone(),
                            ident.id,
                            ident.span.clone(),
                        );
                    }
                }

                // Process function body separately
                for stmt in &def.body.body {
                    Self::infer_stmt(stmt, &func_block, data)?;
                }
                Ok(())
            }
        }
    }

    pub fn infer(
        ast: &Node,
        block: &Rc<RefCell<Block>>,
        data: &mut TransientData,
    ) -> Result<(), SemanticError> {
        match ast {
            Node::Structural(structural) => Self::infer_structural(structural, block, data),
            Node::Stmt(stmt) => Self::infer_stmt(stmt, block, data),
            Node::Expr(expr) => {
                let _ = Self::infer_expr(expr, block, data)?;
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

fn resolve_function_group(
    candidate_funcs: &RefCell<Vec<BindingInfo>>,
    type_info_table: &TypeInfoTable,
    actual_args: &[Rc<TypeInfo>],
) -> Result<(BindingInfo, Rc<TypeInfo>, FunctionType), FunctionOverloadError> {
    let candidates = candidate_funcs.borrow();

    for candidate in candidates.iter() {
        if let Some(func_type) = type_info_table.types.get(&candidate.id) {
            if let TypeInfo::Function(func) = &**func_type {
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
                    return Ok((candidate.clone(), func_type.clone(), func.clone()));
                }
            }
        }
    }

    Err(FunctionOverloadError::NoOverloadFound)
}

impl Pass for PassTypeInference {
    fn run(&mut self, context: &mut yuu_shared::Context) -> bool {
        let src_info = context.require_pass_data::<SourceCodeInfo>("TypeInference");

        let mut data = TransientData {
            src_cache: src_info.cache.clone(),
            type_info_table: TypeInfoTable::new(),
        };

        let root = Block::root(&mut data.type_info_table);
        let out = Self::infer(&src_info.root_node, &root, &mut data);

        // Store the type info table as pass data
        context.add_pass_data(data.type_info_table);

        out.is_ok()
    }

    fn install(self, pipeline: &mut yuu_shared::Pipeline) {
        pipeline.add_pass(self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yuu_parse::{lexer::UnprocessedCodeInfo, parser::Parser};
    use yuu_shared::{Context, Pipeline};

    fn create_test_context() -> Context {
        Context::new()
    }

    #[test]
    fn test_basic() {
        let code_info = UnprocessedCodeInfo {
            code: "fn test() {out 1 + 2;}",
            file_name: "test.yuu",
        };
        let mut parser = Parser::new(&code_info);
        let mut ctxt = parser.parse_and_create_context().expect("Parser error");

        let mut pipeline = Pipeline::new();
        let pass_type_inference = PassTypeInference::new();
        pass_type_inference.install(&mut pipeline);

        pipeline.run(&mut ctxt);

        let tit = ctxt.require_pass_data::<TypeInfoTable>("Test");

        for (idx, ty) in tit.types.iter() {
            println!("Node with Idx={idx}: {ty}");
        }
    }

    #[test]
    fn test_fac() {
        let code_info = UnprocessedCodeInfo {
            code: r#"fn fac(n: i64) -> i64 {
            out if n == 0 {
                out 1; 
            }            
            else {
                let n_out = n * fac(n - 1.0);
                out n_out; 
            };
        }"#,
            file_name: "test.yuu",
        };

        let mut parser = Parser::new(&code_info);
        let mut ctxt = parser.parse_and_create_context().expect("Parser error");

        let mut pipeline = Pipeline::new();
        let pass_type_inference = PassTypeInference::new();
        pass_type_inference.install(&mut pipeline);

        pipeline.run(&mut ctxt);

        let tit = ctxt.require_pass_data::<TypeInfoTable>("Test");
        let ast = ctxt.require_pass_data::<SourceCodeInfo>("Test");

        println!("{}", serde_json::to_string_pretty(&ast.root_node).unwrap());

        for (idx, ty) in tit.types.iter() {
            println!("Node with Idx={idx}: {ty}");
        }
    }
}
