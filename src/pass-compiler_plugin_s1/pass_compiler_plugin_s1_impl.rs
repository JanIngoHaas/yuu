use crate::pass_parse::ast::*;
use mlua::{Lua, LuaSerdeExt, Result as LuaResult, Value};

pub struct PluginPass {
    lua: Lua,
}

impl PluginPass {
    pub fn new() -> LuaResult<Self> {
        let lua = Lua::new();
        // Enable generic standard library if needed
        // lua.load_from_std_lib(mlua::StdLib::ALL)?; 
        Ok(Self { lua })
    }

    pub fn run(&self, ast: &mut AST) -> LuaResult<()> {
        for structural in &mut ast.structurals {
            self.visit_structural(structural)?;
        }
        Ok(())
    }

    fn visit_structural(&self, structural: &mut StructuralNode) -> LuaResult<()> {
        if let StructuralNode::LuaMeta(meta) = structural {
            let code = meta.lua_code.as_str();
            let chunk = self.lua.load(code);
            let new_node: Node = chunk.eval()?;
            if let Node::Structural(new_structural) = new_node {
                *structural = new_structural;
                return self.visit_structural(structural);
            }
            // If it returns something else, we should probably error or handle it.
            return Ok(());
        }

        match structural {
            StructuralNode::FuncDef(func_def) => {
                for arg in &mut func_def.decl.args {
                    self.visit_type(&mut arg.ty)?;
                }
                if let Some(ret_ty) = &mut func_def.decl.ret_ty {
                    self.visit_type(ret_ty)?;
                }
                self.visit_block(&mut func_def.body)?;
            }
            StructuralNode::FuncDecl(func_decl) => {
                for arg in &mut func_decl.args {
                    self.visit_type(&mut arg.ty)?;
                }
                if let Some(ret_ty) = &mut func_decl.ret_ty {
                    self.visit_type(ret_ty)?;
                }
            }
            StructuralNode::StructDecl(_) => {}
            StructuralNode::StructDef(struct_def) => {
                for field in &mut struct_def.fields {
                    self.visit_type(&mut field.ty)?;
                }
            }
            StructuralNode::EnumDef(enum_def) => {
                for variant in &mut enum_def.variants {
                    if let Some(ty) = &mut variant.data_type {
                        self.visit_type(ty)?;
                    }
                }
            }
            StructuralNode::Error(_) => {}
            StructuralNode::LuaMeta(_) => unreachable!("Handled above"),
        }
        Ok(())
    }

    fn visit_stmt(&self, stmt: &mut StmtNode) -> LuaResult<()> {
        if let StmtNode::LuaMeta(meta) = stmt {
            let code = meta.lua_code.as_str();
            let chunk = self.lua.load(code);
            let new_node: Node = chunk.eval()?;
            if let Node::Stmt(new_stmt) = new_node {
                *stmt = new_stmt;
                return self.visit_stmt(stmt);
            }
            return Ok(());
        }

        match stmt {
            StmtNode::Let(stmt) => {
                self.visit_binding(&mut stmt.binding)?;
                self.visit_expr(&mut stmt.expr)?;
            }
            StmtNode::Atomic(expr) => {
                self.visit_expr(expr)?;
            }
            StmtNode::Break(_) => {}
            StmtNode::Return(stmt) => {
                if let Some(expr) = &mut stmt.expr {
                    self.visit_expr(expr)?;
                }
            }
            StmtNode::Defer(stmt) => {
                self.visit_expr(&mut stmt.expr)?;
            }
            StmtNode::Match(stmt) => {
                self.visit_expr(&mut stmt.scrutinee)?;
                for arm in &mut stmt.arms {
                    self.visit_block(&mut arm.body)?;
                }
                if let Some(default_block) = &mut stmt.default_case {
                    self.visit_block(default_block)?;
                }
            }
            StmtNode::If(stmt) => {
                self.visit_condition_with_body(&mut stmt.if_block)?;
                for block in &mut stmt.else_if_blocks {
                    self.visit_condition_with_body(block)?;
                }
                if let Some(else_block) = &mut stmt.else_block {
                    self.visit_block(else_block)?;
                }
            }
            StmtNode::While(stmt) => {
                self.visit_condition_with_body(&mut stmt.condition_block)?;
            }
            StmtNode::Block(stmt) => {
                self.visit_block(stmt)?;
            }
            StmtNode::Decl(_) => {}
            StmtNode::Def(stmt) => {
                self.visit_expr(&mut stmt.expr)?;
            }
            StmtNode::LuaMeta(_) => unreachable!("Handled above"),
            StmtNode::Error(_) => {}
        }
        Ok(())
    }

    fn visit_block(&self, block: &mut BlockStmt) -> LuaResult<()> {
        for stmt in &mut block.body {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    fn visit_condition_with_body(&self, cwb: &mut ConditionWithBody) -> LuaResult<()> {
        self.visit_expr(&mut cwb.condition)?;
        self.visit_block(&mut cwb.body)?;
        Ok(())
    }

    fn visit_expr(&self, expr: &mut ExprNode) -> LuaResult<()> {
        if let ExprNode::LuaMeta(meta) = expr {
            let code = meta.lua_code.as_str();
            let chunk = self.lua.load(code);
            let new_node: Node = chunk.eval()?;
            if let Node::Expr(new_expr) = new_node {
                *expr = new_expr;
                return self.visit_expr(expr);
            }
            return Ok(());
        }

        // Recursive traversal
        match expr {
            ExprNode::Literal(_) => {}
            ExprNode::Ident(_) => {}
            ExprNode::LuaMeta(_) => unreachable!("Handled above"),
            ExprNode::Binary(e) => {
                self.visit_expr(&mut e.left)?;
                self.visit_expr(&mut e.right)?;
            }
            ExprNode::Unary(e) => {
                self.visit_expr(&mut e.expr)?;
            }
            ExprNode::FuncCall(e) => {
                self.visit_expr(&mut e.lhs)?;
                for arg in &mut e.args {
                    self.visit_expr(arg)?;
                }
            }
            ExprNode::Assignment(e) => {
                self.visit_expr(&mut e.lhs)?;
                self.visit_expr(&mut e.rhs)?;
            }
            ExprNode::StructInstantiation(e) => {
                for (_, field_expr) in &mut e.fields {
                    self.visit_expr(field_expr)?;
                }
            }
            ExprNode::EnumInstantiation(e) => {
                if let Some(data) = &mut e.data {
                    self.visit_expr(data)?;
                }
            }
            ExprNode::MemberAccess(e) => {
                self.visit_expr(&mut e.lhs)?;
            }
            ExprNode::Deref(e) => {
                self.visit_expr(&mut e.expr)?;
            }
            ExprNode::AddressOf(e) => {
                self.visit_expr(&mut e.expr)?;
            }
            ExprNode::PointerOp(e) => {
                self.visit_expr(&mut e.left)?;
                self.visit_expr(&mut e.right)?;
            }
            ExprNode::HeapAlloc(e) => {
                self.visit_expr(&mut e.expr)?;
            }
            ExprNode::Array(e) => {
                if let Some(init) = &mut e.init_expr {
                    self.visit_expr(init)?;
                }
                self.visit_expr(&mut e.size)?;
                if let Some(ty) = &mut e.element_type {
                    self.visit_type(ty)?;
                }
            }
            ExprNode::ArrayLiteral(e) => {
                for element in &mut e.elements {
                    self.visit_expr(element)?;
                }
                if let Some(ty) = &mut e.element_type {
                    self.visit_type(ty)?;
                }
            }
            ExprNode::Cast(e) => {
                self.visit_expr(&mut e.expr)?;
                self.visit_type(&mut e.target_type)?;
            }
        }
        Ok(())
    }

    fn visit_type(&self, ty: &mut TypeNode) -> LuaResult<()> {
        if let TypeNode::LuaMeta(meta) = ty {
            let code = meta.lua_code.as_str();
            let chunk = self.lua.load(code);
            let new_node: Node = chunk.eval()?;
            if let Node::Type(new_type) = new_node {
                *ty = new_type;
                return self.visit_type(ty);
            }
            return Ok(());
        }

        match ty {
            TypeNode::BuiltIn(_) => {}
            TypeNode::Ident(_) => {}
            TypeNode::Pointer(p) => {
                self.visit_type(&mut p.pointee)?;
            }
            TypeNode::Array(a) => {
                self.visit_type(&mut a.element_type)?;
                self.visit_expr(&mut a.size)?;
            }
            TypeNode::LuaMeta(_) => unreachable!("Handled above"),
        }
        Ok(())
    }

    fn visit_binding(&self, binding: &mut BindingNode) -> LuaResult<()> {
        if let BindingNode::LuaMeta(meta) = binding {
            let code = meta.lua_code.as_str();
            let chunk = self.lua.load(code);
            let new_node: Node = chunk.eval()?;
            if let Node::Binding(new_binding) = new_node {
                *binding = new_binding;
                return self.visit_binding(binding);
            }
            return Ok(());
        }
        // Ident bindings don't have children to visit
        Ok(())
    }
}
