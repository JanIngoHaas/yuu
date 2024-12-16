use core::panic;
use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    rc::Rc,
};

use hashbrown::HashMap;
use yuu_parse::{parser::SrcCache, Span};
use yuu_shared::{ast::*, Pass};

use crate::{
    built_in::{BindingInfo, BindingInfoKind},
    semantic_error::{GenericSemanticErrorMsg, Note, NoteType, SemanticErrorMsg, Severity},
    type_info::{BuiltInType, FunctionGroupKind, FunctionType, TypeInfo, TypeInfoTable},
};

const MAX_SIMILAR_NAMES: u64 = 3;
const MIN_DST_SIMILAR_NAMES: u64 = 3;

pub enum FunctionInsertError {
    FunctionRedefinition,
}

impl FunctionInsertError {
    pub fn to_semantic_error_msg(&self) -> SemanticErrorMsg {
        match self {
            FunctionInsertError::FunctionRedefinition => {
                SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                    todo!("Need src_cache here - should probably pass it in"),
                    "Cannot redefine variable as function".to_string(),
                    todo!("Need span here - should probably pass it in"),
                    vec![],
                    Severity::Error,
                ))
            }
        }
    }
}

#[derive(Clone)]
pub struct Block {
    pub bindings: HashMap<String, BindingInfoKind>, // This means: A name (identifier) refers to a definition, potentially multiple definitions, which have to be resolved during type inference
    pub parent: Option<Rc<RefCell<Block>>>,
}

pub enum FunctionOverloadError {
    NotAFunction,
    NoOverloadFound,
    BindingNotFound,
}

impl FunctionOverloadError {
    pub fn to_semantic_error_msg(&self) -> SemanticErrorMsg {
        match self {
            FunctionOverloadError::NotAFunction => {
                SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                    todo!("Need src_cache"),
                    "Cannot call non-function value".to_string(),
                    todo!("Need span"),
                    vec![],
                    Severity::Error,
                ))
            }
            FunctionOverloadError::NoOverloadFound => {
                SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                    todo!("Need src_cache"),
                    "No matching function overload found".to_string(),
                    todo!("Need span"),
                    vec![],
                    Severity::Error,
                ))
            }
            FunctionOverloadError::BindingNotFound => {
                SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                    todo!("Need src_cache"),
                    "Function not found".to_string(),
                    todo!("Need span"),
                    vec![],
                    Severity::Error,
                ))
            }
        }
    }
}

impl Block {
    pub fn root() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: None,
        }))
    }

    pub fn get_binding(&self, name: &str) -> Option<BindingInfoKind> {
        if let Some(k) = self.bindings.get(name) {
            return Some(k.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.as_ref().borrow().get_binding(name);
        }

        None
    }

    pub fn from_parent(parent: Rc<RefCell<Block>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn get_similar_names(&self, name: &str, amount: u64, min_dst: u64) -> Vec<String> {
        let mut similar_names = Vec::new();

        for key in self.bindings.keys() {
            let distance = levenshtein_distance(name, key);
            if distance > 0 && distance <= min_dst as usize {
                similar_names.push((key.clone(), distance));
            }
        }

        similar_names.sort_by_key(|x| x.1);
        let mut result: Vec<_> = similar_names
            .into_iter()
            .map(|(name, _)| name)
            .take(amount as usize)
            .collect();

        // Check parent scope if we need more suggestions
        if let Some(parent) = &self.parent {
            let remaining = amount - result.len() as u64;
            if remaining > 0 {
                let mut parent_similar = parent
                    .as_ref()
                    .borrow()
                    .get_similar_names(name, remaining, min_dst);
                result.append(&mut parent_similar);
            }
        }

        result
    }

    // pub fn amortized_find(&mut self, name: &str) -> Option<NodeId> {
    //     if let Some(id) = self.bindings.get(name) {
    //         return Some(*id);
    //     }

    //     if let Some(parent) = &self.parent {
    //         for (key, value) in parent.borrow().bindings.iter() {
    //             self.bindings.insert(key.clone(), *value);
    //         }
    //         return parent.borrow_mut().amortized_find(name);
    //     }

    //     None
    // }

    pub fn declare_function(
        &mut self,
        name: String,
        id: NodeId,
        span: Span,
    ) -> Result<(), FunctionInsertError> {
        let functions = self
            .bindings
            .entry(name)
            .or_insert(BindingInfoKind::Ambiguous(Rc::new(
                RefCell::new(Vec::new()),
            )));

        match functions {
            BindingInfoKind::Ambiguous(f) => {
                f.as_ref().borrow_mut().push(BindingInfo {
                    id,
                    src_location: Some(span),
                });
                Ok(())
            }
            BindingInfoKind::Unique(_) => {
                // This is a redefinition of a variable as a function
                // We can't have that
                Err(FunctionInsertError::FunctionRedefinition)
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, id: NodeId, span: Span) -> bool {
        let variables = self.bindings.insert(
            name,
            BindingInfoKind::Unique(BindingInfo {
                id,
                src_location: Some(span),
            }),
        );

        match variables {
            Some(BindingInfoKind::Ambiguous(_)) => {
                // This is a redefinition of a function as a variable
                // We can't have that
                false
            }
            _ => true, // This also explicitly handles shadowing by just allowing it
        }
    }

    pub fn resolve_function<'a, T>(
        &self,
        name: &str,
        type_info_table: &'a TypeInfoTable,
        args: &'a [&'a Rc<TypeInfo>],
        resolver: T,
    ) -> Result<Rc<TypeInfo>, FunctionOverloadError>
    where
        T: Fn(&BindingInfo, &FunctionType) -> Rc<TypeInfo>,
    {
        let overload = self.get_binding(name);

        overload.map(|binding| match binding {
            BindingInfoKind::Ambiguous(f) => {
                for binding in f.as_ref().borrow().iter() {
                    let func_type = type_info_table.types.get(&binding.id).expect(
                        "Compiler Bug: Block has a function binding, but can't find the associated type info",
                    );

                    match func_type.as_ref() {
                        TypeInfo::Function(func) => {
                            if func.args.len() != args.len() {
                                continue;
                            }

                            let mut found = true;
                            for (arg, expected) in args.iter().zip(func.args.iter()) {
                                if !arg.does_coerce_to_same_type(expected) {
                                    found = false;
                                    break;
                                }
                            }

                            if found {
                                return Ok(resolver(binding, func));
                            }
                        }
                        _ => unreachable!("Compiler Bug: Function binding is not a function type"),
                    }
                }
                Err(FunctionOverloadError::NoOverloadFound)
            }
            BindingInfoKind::Unique(_) => Err(FunctionOverloadError::NotAFunction),
        }).ok_or(FunctionOverloadError::BindingNotFound)?
    }
}

// This was generated by Claude Sonnet
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.chars().count();
    let len2 = s2.chars().count();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let substitution = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                .min(matrix[i + 1][j] + 1)
                .min(matrix[i][j] + substitution);
        }
    }

    matrix[len1][len2]
}

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

pub struct PassTypeInference {
    src_cache: SrcCache,
    type_info_table: TypeInfoTable,
}

impl PassTypeInference {
    pub fn new(src_cache: SrcCache) -> Self {
        Self {
            src_cache,
            type_info_table: TypeInfoTable::new(),
        }
    }

    //TODO: This should probably go into its own pass - or at least into a separate file (Not now though)
    // As the binding node and the type are basically graphs, we can descend both in lockstep checking if they match.
    // As soon as we reach a leaf node of the binding node, we know that the leaf node is of the same type as the current type node.
    // If we reach a leaf node (== identifier) of the type node => error
    // TODO: Here is the bigger problem: How do we handle binding the correct expression (i.e. later the value) to the binding node?
    pub fn match_binding_node_to_type<'a, T>(
        &'a mut self,
        binding: &BindingNode,
        ty: &Rc<TypeInfo>,
        mut match_resolver: T,
    ) -> Result<(), SemanticErrorMsg>
    where
        T: FnMut(
            &IdentBinding,
            &Rc<TypeInfo>,
            &'a mut TypeInfoTable,
        ) -> Result<(), SemanticErrorMsg>,
    {
        match binding {
            BindingNode::Ident(ident) => {
                match_resolver(ident, ty, &mut self.type_info_table)?;
            }
        };
        Ok(())
    }

    pub fn infer_type(
        &mut self,
        ty: &yuu_shared::ast::TypeNode,
        block: &Rc<RefCell<Block>>,
    ) -> Result<Rc<TypeInfo>, SemanticErrorMsg> {
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
                    Some(BindingInfoKind::Unique(var)) => self
                        .type_info_table
                        .types
                        .get(&var.id)
                        .cloned()
                        .expect("Compiler Bug: Variable binding not found in type table"),
                    Some(BindingInfoKind::Ambiguous(funcs)) => Rc::new(TypeInfo::FunctionGroup(
                        RefCell::new(FunctionGroupKind::Unresolved(funcs.clone())),
                        // TODO: I think this is how it should be done, but I'm not 100% sure -> Copilot: Check this
                    )),
                    None => {
                        let similar_names = block.as_ref().borrow().get_similar_names(
                            &ident.name,
                            MAX_SIMILAR_NAMES,
                            MIN_DST_SIMILAR_NAMES,
                        );
                        let smessages = SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                            self.src_cache.clone(),
                            format!("Cannot find identifier `{}`", ident.name),
                            ident.span.clone(),
                            if !similar_names.is_empty() {
                                vec![Note {
                                    message: format!("Did you mean: {}", similar_names.join(", ")),
                                    span: None,
                                    note_type: NoteType::Help,
                                }]
                            } else {
                                vec![]
                            },
                            Severity::Error,
                        ));
                        return Err(smessages);
                    }
                }
            }
        };
        Ok(semantic_type)
    }

    pub fn infer_stmt(
        &mut self,
        stmt: &StmtNode,
        block: &Rc<RefCell<Block>>,
    ) -> Result<(), SemanticErrorMsg> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let ty_expr = self.infer_expr(&let_stmt.expr, block)?;
                let match_resolver = move |ident_binding: &IdentBinding,
                                           ty: &Rc<TypeInfo>,
                                           ty_info_table: &mut TypeInfoTable|
                      -> Result<(), SemanticErrorMsg> {
                    block.as_ref().borrow_mut().insert_variable(
                        ident_binding.name.clone(),
                        ident_binding.id,
                        ident_binding.span.clone(),
                    );

                    ty_info_table.types.insert(ident_binding.id, ty.clone());

                    Ok(())
                };

                self.match_binding_node_to_type(&let_stmt.binding, &ty_expr, match_resolver)?;
                Ok(())
            }
            StmtNode::Atomic(expr) => {
                self.infer_expr(expr, block).map(|_| ())
                // TODO: We should probably check if the type of the expression is not nil - then we should return an error
            }
            StmtNode::Return(return_stmt) => self.infer_expr(&return_stmt.expr, block).map(|_| ()),
        }
    }

    pub fn infer_expr(
        &mut self,
        expr: &ExprNode,
        block: &Rc<RefCell<Block>>,
    ) -> Result<Rc<TypeInfo>, SemanticErrorMsg> {
        let out = match expr {
            ExprNode::Literal(lit) => match lit.lit.kind {
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
            },
            ExprNode::Binary(binary_expr) => {
                let lhs = self.infer_expr(&binary_expr.left, block)?;
                let rhs = self.infer_expr(&binary_expr.right, block)?;

                let op_name = binary_expr.op.static_name();

                let ty = block
                    .as_ref()
                    .borrow()
                    .resolve_function(
                        op_name,
                        &self.type_info_table,
                        &[&lhs, &rhs],
                        |binding, func| func.ret.clone(),
                    )
                    .map_err(|err| err.to_semantic_error_msg())?;

                self.type_info_table
                    .types
                    .insert(binary_expr.id, ty.clone());

                ty
            }
            ExprNode::Unary(unary_expr) => {
                let ty = self.infer_expr(&unary_expr.operand, block)?;
                let op_name = unary_expr.op.static_name();

                let result_ty = block
                    .as_ref()
                    .borrow()
                    .resolve_function(op_name, &self.type_info_table, &[&ty], |binding, func| {
                        func.ret.clone()
                    })
                    .map_err(|err| err.to_semantic_error_msg())?;

                self.type_info_table
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
                        SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                            self.src_cache.clone(),
                            format!("Cannot find identifier `{}`", ident_expr.ident),
                            ident_expr.span.clone(),
                            if !similar_names.is_empty() {
                                vec![Note {
                                    message: format!("Did you mean: {}", similar_names.join(", ")),
                                    span: None,
                                    note_type: NoteType::Help,
                                }]
                            } else {
                                vec![]
                            },
                            Severity::Error,
                        ))
                    })?;

                let out = match binding {
                    BindingInfoKind::Unique(var) => {
                        // For variables, just look up and return their type
                        self.type_info_table
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
                out
            }
            ExprNode::Block(block_expr) => {
                let block_scope = Block::from_parent(block.clone());

                let mut last_stmt_type = Rc::new(TypeInfo::BuiltIn(BuiltInType::Nil));

                for stmt in &block_expr.body {
                    match stmt {
                        StmtNode::Return(ret) => {
                            last_stmt_type = self.infer_expr(&ret.expr, &block_scope)?;
                            break; // Early return
                        }
                        _ => {
                            self.infer_stmt(stmt, &block_scope)?;
                        }
                    }
                }

                self.type_info_table
                    .types
                    .insert(block_expr.id, last_stmt_type.clone());
                last_stmt_type
            }

            ExprNode::FuncCall(func_call_expr) => {
                // Here, we have to resolve the overloaded function

                let mut actual_func_ident = self.infer_expr(&func_call_expr.lhs, block)?;

                let actual_arg_types = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.infer_expr(arg, block))
                    .collect::<Result<Vec<_>, _>>()?;

                let (resolved_ret_type, resolved_func) = match actual_func_ident.as_ref() {
                    TypeInfo::FunctionGroup(func_candidates) => {
                        match &*func_candidates.borrow() {
                            FunctionGroupKind::Resolved(resolved_func_type) => {
                                // If we already resolved this function group, we can just use that type
                                if let TypeInfo::Function(func) = &**resolved_func_type {
                                    if func.args.len() != actual_arg_types.len() {
                                        return Err(SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                                            self.src_cache.clone(),
                                            format!("Expected {} arguments, got {}", func.args.len(), actual_arg_types.len()),
                                            func_call_expr.span.clone(),
                                            vec![],
                                            Severity::Error,
                                        )));
                                    }

                                    for (expected, got) in func.args.iter().zip(actual_arg_types.iter()) {
                                        if !got.does_coerce_to_same_type(expected) {
                                            return Err(SemanticErrorMsg::FunctionArgTypeMismatch {
                                                cache: self.src_cache.clone(),
                                                span: func_call_expr.span.clone(),
                                                expected: format!("{}", expected),
                                                got: format!("{}", got),
                                            });
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
                                    &self.type_info_table,
                                    actual_arg_types.as_slice(),
                                ).map_err(|x| x.to_semantic_error_msg())?;
                                Ok((resolved_func_type.ret.clone(), resolved_generic_ty))
                            }
                        }
                    }
                    TypeInfo::BuiltIn(_) => Err(SemanticErrorMsg::Generic(GenericSemanticErrorMsg::new(
                        self.src_cache.clone(),
                        "Cannot call a built-in type as a function".to_string(),
                        func_call_expr.span.clone(),
                        vec![],
                        Severity::Error,
                    ))),
                    TypeInfo::Function(_) => unreachable!("Compiler Bug: Function type should be a function group - overloading is allowed."),
                }?;

                self.type_info_table
                    .types
                    .insert(func_call_expr.id, resolved_ret_type.clone());

                // If we had a function group, we can now overwrite the Rc<TypeInfo> with the resolved function type
                // This way, we can later just look up the type in the type table, without having to resolve the function group again
                let _ = std::mem::replace(actual_func_ident.borrow_mut(), resolved_func);

                resolved_ret_type
            }
        };
        Ok(out)
    }

    fn declare_function(
        &mut self,
        name: &str,
        args: &[FuncArg],
        ret_ty: Option<&TypeNode>, // Add return type parameter
        id: NodeId,
        span: Span,
        block: &Rc<RefCell<Block>>,
    ) -> Result<(), SemanticErrorMsg> {
        block
            .as_ref()
            .borrow_mut()
            .declare_function(name.to_string(), id, span.clone())
            .map_err(|err| err.to_semantic_error_msg())?;

        let func_arg_types = args
            .iter()
            .map(|arg| {
                let semantic_arg_type = self.infer_type(&arg.ty, block)?;

                self.match_binding_node_to_type(
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
                )?;

                Ok::<_, SemanticErrorMsg>(semantic_arg_type)
            })
            .collect::<Result<Rc<[_]>, _>>()?;

        let ret_type = if let Some(ty) = ret_ty {
            self.infer_type(ty, block)?
        } else {
            Rc::new(TypeInfo::BuiltIn(BuiltInType::I64))
        };

        let func = Rc::new(TypeInfo::Function(FunctionType {
            args: func_arg_types,
            ret: ret_type,
        }));

        self.type_info_table.types.insert(id, func.clone());

        Ok(())
    }

    pub fn infer_structural(
        &mut self,
        structural: &StructuralNode,
        block: &Rc<RefCell<Block>>,
    ) -> Result<(), SemanticErrorMsg> {
        match structural {
            StructuralNode::FuncDecl(decl) => self.declare_function(
                &decl.name,
                &decl.args,
                decl.ret_ty.as_deref(),
                decl.id,
                decl.span.clone(),
                block,
            ),
            StructuralNode::FuncDef(def) => {
                self.declare_function(
                    &def.decl.name,
                    &def.decl.args,
                    def.decl.ret_ty.as_deref(),
                    def.id,
                    def.span.clone(),
                    block,
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
                    self.infer_stmt(stmt, &func_block)?;
                }
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
    fn run(mut self, ast: &Node, context: &mut yuu_shared::Context) -> bool {
        let root = Block::root();

        let out = match ast {
            Node::Structural(structural) => self.infer_structural(structural, &root),
            Node::Stmt(stmt) => self.infer_stmt(stmt, &root),
            Node::Expr(expr) => self.infer_expr(expr, &root).map(|_| ()),
            Node::Binding(_) => {
                unreachable!("Compiler Bug: Cannot infer a binding without a type")
            }
            Node::Type(ty) => self.infer_type(ty, &root).map(|_| ()),
        };

        context.add_pass_data(self.type_info_table);

        if let Err(err) = out {
            err.eprint();
            false
        } else {
            true
        }
    }
}
