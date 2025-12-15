use crate::{
    pass_diagnostics::{YuuErrorBuilder, error::YuuError},
    pass_parse::{
        BlockStmt, ExprNode, StmtNode, StructuralNode,
        ast::{AST, SourceInfo},
    },
    utils::{TypeRegistry, collections::UstrIndexSet},
};
use miette::Result;

/// Errors from check decl def analysis
pub struct CheckDeclDefErrors(pub Vec<YuuError>);

/// Pass that checks if every declaration is defined in all branches
pub struct CheckDeclDef;

impl CheckDeclDef {
    pub fn new() -> Self {
        Self
    }

    /// Run the pass on the AST
    pub fn run(
        &self,
        ast: &AST,
        type_registry: &TypeRegistry,
        src_info: &SourceInfo,
    ) -> Result<CheckDeclDefErrors> {
        let mut analyzer = CheckDeclDefAnalyzer {
            errors: Vec::new(),
            src_info,
        };

        for structural in &ast.structurals {
            match structural.as_ref() {
                StructuralNode::FuncDef(func_def_structural) => {
                    let _ = analyzer.check_block(&func_def_structural.body);
                }
                _ => (),
            }
        }

        Ok(CheckDeclDefErrors(analyzer.errors))
    }
}

impl Default for CheckDeclDef {
    fn default() -> Self {
        Self::new()
    }
}

struct CheckDeclDefAnalyzer<'a> {
    errors: Vec<YuuError>,
    src_info: &'a SourceInfo,
}

impl CheckDeclDefAnalyzer<'_> {
    fn report_inconsistent_defs(
        &mut self,
        vars_in_first: impl Iterator<Item = impl AsRef<str>>,
        vars_in_second: impl Iterator<Item = impl AsRef<str>>,
        first_context: &str,
        second_context: &str,
    ) {
        for var in vars_in_first {
            let error = YuuErrorBuilder::new()
                .kind(crate::pass_diagnostics::error::ErrorKind::InvalidStatement)
                .message(format!(
                    "Variable '{}' is defined in {} but not in {}",
                    var.as_ref(),
                    first_context,
                    second_context
                ))
                .source(
                    self.src_info.source.clone(),
                    self.src_info.file_name.clone(),
                )
                .help("Make sure all variables are defined consistently across all branches")
                .build();
            self.errors.push(error);
        }

        for var in vars_in_second {
            let error = YuuErrorBuilder::new()
                .kind(crate::pass_diagnostics::error::ErrorKind::InvalidStatement)
                .message(format!(
                    "Variable '{}' is defined in {} but not in {}",
                    var.as_ref(),
                    second_context,
                    first_context
                ))
                .source(
                    self.src_info.source.clone(),
                    self.src_info.file_name.clone(),
                )
                .help("Make sure all variables are defined consistently across all branches")
                .build();
            self.errors.push(error);
        }
    }

    fn check_block(&mut self, block: &BlockStmt) -> UstrIndexSet {
        let stmts = &block.body;

        let mut defs = UstrIndexSet::default();
        let mut decls = UstrIndexSet::default();

        for stmt in stmts {
            let cont = self.check_stmt(&stmt, &mut defs, &mut decls);
            if !cont {
                return defs;
            }
        }

        return defs;
    }

    fn check_expr(&mut self, expr: &ExprNode, decls: &UstrIndexSet, defs: &UstrIndexSet) {
        match expr {
            ExprNode::Literal(_) => (),
            ExprNode::Binary(binary_expr) => {
                self.check_expr(&binary_expr.left, decls, defs);
                self.check_expr(&binary_expr.right, decls, defs);
            }
            ExprNode::Unary(unary_expr) => {
                self.check_expr(&unary_expr.expr, decls, defs);
            }
            ExprNode::Ident(ident_expr) => {
                // Only fail if the variable is declared but not defined
                // If it's not declared at all, it's a let statement and shouldn't fail here
                if decls.contains(&ident_expr.ident) && !defs.contains(&ident_expr.ident) {
                    let error = YuuErrorBuilder::new()
                        .kind(crate::pass_diagnostics::error::ErrorKind::InvalidExpression)
                        .message(format!(
                            "Variable '{}' is declared but not defined",
                            ident_expr.ident
                        ))
                        .source(
                            self.src_info.source.clone(),
                            self.src_info.file_name.clone(),
                        )
                        .span(
                            ident_expr.span.clone(),
                            format!("Variable '{}' used here", ident_expr.ident),
                        )
                        .help("Make sure to define the variable before using it")
                        .build();
                    self.errors.push(error);
                }
            }
            ExprNode::FuncCall(func_call_expr) => {
                for arg in &func_call_expr.args {
                    self.check_expr(arg, decls, defs);
                }
            }
            ExprNode::Assignment(assignment_expr) => {
                self.check_expr(&assignment_expr.lhs, decls, defs);
                self.check_expr(&assignment_expr.rhs, decls, defs);
            }
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                for field in &struct_instantiation_expr.fields {
                    self.check_expr(&field.1, decls, defs);
                }
            }
            ExprNode::EnumInstantiation(enum_instantiation_expr) => {
                if let Some(ref expr) = enum_instantiation_expr.data {
                    self.check_expr(expr, decls, defs);
                }
            }
            ExprNode::MemberAccess(member_access_expr) => {
                self.check_expr(&member_access_expr.lhs, decls, defs);
            }
            ExprNode::Deref(deref_expr) => {
                self.check_expr(&deref_expr.expr, decls, defs);
            }
            ExprNode::AddressOf(address_of_expr) => {
                self.check_expr(&address_of_expr.expr, decls, defs);
            }
            ExprNode::PointerOp(pointer_op_expr) => {
                self.check_expr(&pointer_op_expr.left, decls, defs);
                self.check_expr(&pointer_op_expr.right, decls, defs);
            }
            ExprNode::HeapAlloc(heap_alloc_expr) => {
                self.check_expr(&heap_alloc_expr.value, decls, defs);
            }
            ExprNode::Array(array_expr) => {
                array_expr
                    .init_value
                    .as_ref()
                    .map(|expr| self.check_expr(expr, decls, defs));
                self.check_expr(&array_expr.size, decls, defs);
            }
            ExprNode::ArrayLiteral(array_literal_expr) => {
                for element in &array_literal_expr.elements {
                    self.check_expr(element, decls, defs);
                }
            }
            ExprNode::Cast(cast_expr) => {
                self.check_expr(&cast_expr.expr, decls, defs);
            }
        }
    }

    fn check_stmt(
        &mut self,
        stmt: &StmtNode,
        defs: &mut UstrIndexSet,
        decls: &mut UstrIndexSet,
    ) -> bool {
        match stmt {
            StmtNode::If(if_stmt) => {
                self.check_expr(&if_stmt.if_block.condition, decls, defs);
                let defs_if: UstrIndexSet = self.check_block(&if_stmt.if_block.body);

                for else_if in &if_stmt.else_if_blocks {
                    self.check_expr(&else_if.condition, decls, defs);
                    let defs_elif: UstrIndexSet = self.check_block(&else_if.body);
                    let not_declared_in_elif = defs_if.difference(&defs_elif);
                    let not_declared_in_if = defs_elif.difference(&defs_if);

                    self.report_inconsistent_defs(
                        not_declared_in_elif.map(|s| s.as_str()),
                        not_declared_in_if.map(|s| s.as_str()),
                        "if branch",
                        "else-if branch",
                    );
                }

                // Now check the else block if it exists
                if let Some(else_block) = &if_stmt.else_block {
                    let defs_else: UstrIndexSet = self.check_block(&else_block);
                    let not_declared_in_else = defs_if.difference(&defs_else);
                    let not_declared_in_if = defs_else.difference(&defs_if);
                    self.report_inconsistent_defs(
                        not_declared_in_else.map(|s| s.as_str()),
                        not_declared_in_if.map(|s| s.as_str()),
                        "if branch",
                        "else branch",
                    );
                }

                // Now, add the defs from the if block (which should also be the defs from all other branches) to the defs from the current block
                defs.extend(defs_if);
                true
            }
            StmtNode::While(while_stmt) => {
                self.check_expr(&while_stmt.condition_block.condition, decls, defs);
                self.check_block(&while_stmt.condition_block.body);
                true
            }
            StmtNode::Block(block_stmt) => {
                self.check_block(&block_stmt);
                true
            }
            StmtNode::Break(b) => false,
            StmtNode::Return(r) => {
                if let Some(ref e) = r.expr {
                    self.check_expr(e, decls, defs);
                }
                false
            }
            StmtNode::Atomic(a) => {
                self.check_expr(a, decls, defs);
                true
            }
            StmtNode::Decl(d) => {
                decls.insert(d.ident.name);
                true
            }

            StmtNode::Let(let_stmt) => {
                self.check_expr(&let_stmt.expr, decls, defs);
                true
            }
            StmtNode::Defer(defer_stmt) => {
                self.check_expr(&defer_stmt.expr, decls, defs);
                true
            }
            StmtNode::Match(match_stmt) => {
                self.check_expr(&match_stmt.scrutinee, decls, defs);

                if !match_stmt.arms.is_empty() {
                    let anchor_defs = self.check_block(&match_stmt.arms[0].body);

                    for arm in match_stmt.arms.iter().skip(1) {
                        let arm_defs = self.check_block(&arm.body);
                        let not_declared_in_arm = anchor_defs.difference(&arm_defs);
                        let not_declared_in_anchor = arm_defs.difference(&anchor_defs);

                        self.report_inconsistent_defs(
                            not_declared_in_arm.map(|s| s.as_str()),
                            not_declared_in_anchor.map(|s| s.as_str()),
                            "first match arm",
                            "other match arm",
                        );
                    }

                    if let Some(default_case) = &match_stmt.default_case {
                        let default_defs = self.check_block(default_case);
                        let not_declared_in_default = anchor_defs.difference(&default_defs);
                        let not_declared_in_anchor = default_defs.difference(&anchor_defs);

                        self.report_inconsistent_defs(
                            not_declared_in_default.map(|s| s.as_str()),
                            not_declared_in_anchor.map(|s| s.as_str()),
                            "match arm",
                            "default case",
                        );
                    }

                    defs.extend(anchor_defs);
                }

                true
            }
            StmtNode::Error(_) => true,
            StmtNode::Def(def_stmt) => {
                defs.insert(def_stmt.ident.name);
                true
            }
        }
    }
}
