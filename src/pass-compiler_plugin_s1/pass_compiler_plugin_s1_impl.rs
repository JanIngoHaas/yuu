use crate::pass_parse::ast::*;
use crate::pass_diagnostics::error::{ErrorKind, YuuError};
use crate::pass_parse::add_ids::{AddId, IdGenerator};
use mlua::{Lua, Result as LuaResult, LuaSerdeExt};
use serde::de::DeserializeOwned;
use logos::Span;

pub struct PluginPass {
    lua: Lua,
}

struct TransientData<'a> {
    id_generator: &'a mut IdGenerator,
    errors: Vec<YuuError>,
    source_info: &'a SourceInfo,
}

impl PluginPass {
    pub fn new() -> LuaResult<Self> {
        let lua = Lua::new();
        Ok(Self { lua })
    }

    pub fn run(&self, ast: &mut AST, id_generator: &mut IdGenerator, source_info: &SourceInfo) -> Vec<YuuError> {
        let mut data = TransientData {
            id_generator,
            errors: Vec::new(),
            source_info,
        };

        for structural in &mut ast.structurals {
            self.visit_structural(structural, &mut data);
        }
        data.errors
    }

    fn execute_lua_transform<T>(&self, lua_code: &str, span: &Span, data: &mut TransientData) -> Option<T>
    where
        T: DeserializeOwned + AddId
    {
        // 1. Execute lua_code and get result
        let result_value = match self.lua.load(lua_code).eval() {
            Ok(result) => result,
            Err(lua_err) => {
                data.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!("Lua execution error: {}", lua_err))
                        .source(data.source_info.source.clone(), data.source_info.file_name.clone())
                        .span(span.clone(), "lua transformation failed")
                        .help("Check Lua syntax and semantics")
                        .build()
                );
                return None;
            }
        };

        // 2. Convert result to T
        let mut new_node: T = match self.lua.from_value(result_value) {
            Ok(node) => node,
            Err(err) => {
                data.errors.push(
                    YuuError::builder()
                        .kind(ErrorKind::InvalidSyntax)
                        .message(format!("Failed to convert Lua result to node: {}", err))
                        .source(data.source_info.source.clone(), data.source_info.file_name.clone())
                        .span(span.clone(), "node deserialization failed")
                        .help("Ensure Lua code returns a valid node structure TODO: Here, which one?")
                        .build()
                );
                return None;
            }
        };

        // 3. Assign new ID
        new_node.add_id(data.id_generator);

        Some(new_node)
    }

    fn replace_node<T>(&self, node: &mut T, new_node: T) {
        *node = new_node;
    }

    fn visit_structural(&self, structural: &mut StructuralNode, data: &mut TransientData) {
        match structural {
            StructuralNode::FuncDef(func_def) => {
                for arg in &mut func_def.decl.args {
                    self.visit_type(&mut arg.ty, data);
                }
                if let Some(ret_ty) = &mut func_def.decl.ret_ty {
                    self.visit_type(ret_ty, data);
                }
                self.visit_block(&mut func_def.body, data);
            }
            StructuralNode::FuncDecl(func_decl) => {
                for arg in &mut func_decl.args {
                    self.visit_type(&mut arg.ty, data);
                }
                if let Some(ret_ty) = &mut func_decl.ret_ty {
                    self.visit_type(ret_ty, data);
                }
            }
            StructuralNode::StructDecl(_) => {}
            StructuralNode::StructDef(struct_def) => {
                for field in &mut struct_def.fields {
                    self.visit_type(&mut field.ty, data);
                }
            }
            StructuralNode::EnumDef(enum_def) => {
                for variant in &mut enum_def.variants {
                    if let Some(ty) = &mut variant.data_type {
                        self.visit_type(ty, data);
                    }
                }
            }
            StructuralNode::Error(_) => {}
            StructuralNode::LuaMeta(lm) => {
                if let Some(new_node) = self.execute_lua_transform(&lm.lua_code, &lm.span, data) {
                    self.replace_node(structural, new_node);
                }
            },
        }
    }

    fn visit_stmt(&self, stmt: &mut StmtNode, data: &mut TransientData) {
        match stmt {
            StmtNode::Let(stmt) => {
                self.visit_binding(&mut stmt.binding, data);
                self.visit_expr(&mut stmt.expr, data);
            }
            StmtNode::Atomic(expr) => {
                self.visit_expr(expr, data);
            }
            StmtNode::Break(_) => {}
            StmtNode::Return(stmt) => {
                if let Some(expr) = &mut stmt.expr {
                    self.visit_expr(expr, data);
                }
            }
            StmtNode::Defer(stmt) => {
                self.visit_expr(&mut stmt.expr, data);
            }
            StmtNode::Match(stmt) => {
                self.visit_expr(&mut stmt.scrutinee, data);
                for arm in &mut stmt.arms {
                    self.visit_block(&mut arm.body, data);
                }
                if let Some(default_block) = &mut stmt.default_case {
                    self.visit_block(default_block, data);
                }
            }
            StmtNode::If(stmt) => {
                self.visit_condition_with_body(&mut stmt.if_block, data);
                for block in &mut stmt.else_if_blocks {
                    self.visit_condition_with_body(block, data);
                }
                if let Some(else_block) = &mut stmt.else_block {
                    self.visit_block(else_block, data);
                }
            }
            StmtNode::While(stmt) => {
                self.visit_condition_with_body(&mut stmt.condition_block, data);
            }
            StmtNode::Block(stmt) => {
                self.visit_block(stmt, data);
            }
            StmtNode::Decl(_) => {}
            StmtNode::Def(stmt) => {
                self.visit_expr(&mut stmt.expr, data);
            }
            StmtNode::LuaMeta(lm) => {
                if let Some(new_node) = self.execute_lua_transform(&lm.lua_code, &lm.span, data) {
                    self.replace_node(stmt, new_node);
                }
            },
            StmtNode::Error(_) => {}
        }
    }

    fn visit_block(&self, block: &mut BlockStmt, data: &mut TransientData) {
        for stmt in &mut block.body {
            self.visit_stmt(stmt, data);
        }
    }

    fn visit_condition_with_body(&self, cwb: &mut ConditionWithBody, data: &mut TransientData) {
        self.visit_expr(&mut cwb.condition, data);
        self.visit_block(&mut cwb.body, data);
    }

    fn visit_expr(&self, expr: &mut ExprNode, data: &mut TransientData) {
        // Recursive traversal
        match expr {
            ExprNode::Literal(_) => {}
            ExprNode::Ident(_) => {}
            ExprNode::LuaMeta(lm) => {
                if let Some(new_node) = self.execute_lua_transform(&lm.lua_code, &lm.span, data) {
                    self.replace_node(expr, new_node);
                }
            },
            ExprNode::Binary(e) => {
                self.visit_expr(&mut e.left, data);
                self.visit_expr(&mut e.right, data);
            }
            ExprNode::Unary(e) => {
                self.visit_expr(&mut e.expr, data);
            }
            ExprNode::FuncCall(e) => {
                self.visit_expr(&mut e.lhs, data);
                for arg in &mut e.args {
                    self.visit_expr(arg, data);
                }
            }
            ExprNode::Assignment(e) => {
                self.visit_expr(&mut e.lhs, data);
                self.visit_expr(&mut e.rhs, data);
            }
            ExprNode::StructInstantiation(e) => {
                for (_, field_expr) in &mut e.fields {
                    self.visit_expr(field_expr, data);
                }
            }
            ExprNode::EnumInstantiation(e) => {
                if let Some(expr_data) = &mut e.data {
                    self.visit_expr(expr_data, data);
                }
            }
            ExprNode::MemberAccess(e) => {
                self.visit_expr(&mut e.lhs, data);
            }
            ExprNode::Deref(e) => {
                self.visit_expr(&mut e.expr, data);
            }
            ExprNode::AddressOf(e) => {
                self.visit_expr(&mut e.expr, data);
            }
            ExprNode::PointerOp(e) => {
                self.visit_expr(&mut e.left, data);
                self.visit_expr(&mut e.right, data);
            }
            ExprNode::HeapAlloc(e) => {
                self.visit_expr(&mut e.expr, data);
            }
            ExprNode::Array(e) => {
                if let Some(init) = &mut e.init_expr {
                    self.visit_expr(init, data);
                }
                self.visit_expr(&mut e.size, data);
                if let Some(ty) = &mut e.element_type {
                    self.visit_type(ty, data);
                }
            }
            ExprNode::ArrayLiteral(e) => {
                for element in &mut e.elements {
                    self.visit_expr(element, data);
                }
                if let Some(ty) = &mut e.element_type {
                    self.visit_type(ty, data);
                }
            }
            ExprNode::Cast(e) => {
                self.visit_expr(&mut e.expr, data);
                self.visit_type(&mut e.target_type, data);
            }
        }
    }

    fn visit_type(&self, ty: &mut TypeNode, data: &mut TransientData) {
        match ty {
            TypeNode::BuiltIn(_) => {}
            TypeNode::Ident(_) => {}
            TypeNode::Pointer(p) => {
                self.visit_type(&mut p.pointee, data);
            }
            TypeNode::Array(a) => {
                self.visit_type(&mut a.element_type, data);
                self.visit_expr(&mut a.size, data);
            }
            TypeNode::LuaMeta(lm) => {
                if let Some(new_node) = self.execute_lua_transform(&lm.lua_code, &lm.span, data) {
                    self.replace_node(ty, new_node);
                }
            },
        }
    }

    fn visit_binding(&self, binding: &mut BindingNode, data: &mut TransientData) {
        match binding {
            BindingNode::Ident(_) => {
                // Ident bindings don't have children to visit
            },
            BindingNode::LuaMeta(lm) => {
                if let Some(new_node) = self.execute_lua_transform(&lm.lua_code, &lm.span, data) {
                    self.replace_node(binding, new_node);
                }
            },
        }
    }
}
