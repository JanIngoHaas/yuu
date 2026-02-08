use std::collections::HashSet;

use crate::{
    pass_diagnostics::error::{ErrorKind, YuuError},
    pass_parse::{
        Arg, BindingNode, BlockStmt, ExprNode, MatchArm, RefutablePatternNode, SourceInfo,
        StmtNode, StructuralMetadata, StructuralNode,
        ast::{AST, NodeId},
    },
    utils::collections::FastHashMap,
};
use miette::Result;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

/// Errors from check decl def analysis
pub struct CheckDeclDefErrors(pub Vec<YuuError>);

/// Pass that checks if every declaration is defined across all reachable control-flow paths
pub struct CheckDeclDef;

impl CheckDeclDef {
    pub fn new() -> Self {
        Self
    }

    /// Run the pass on the AST
    pub fn run(&self, ast: &AST, src_info: &SourceInfo) -> Result<CheckDeclDefErrors> {
        let errors: Vec<YuuError> = ast
            .structurals
            .par_iter()
            .flat_map(|structural| {
                let mut analyzer = CheckDeclDefAnalyzer {
                    errors: Vec::new(),
                    src_info,
                };

                if let StructuralNode::FuncDef(func_def) = structural.as_ref() {
                    analyzer.check_function(
                        func_def.decl.args.as_slice(),
                        &func_def.body,
                        &structural.metadata,
                    );
                }

                analyzer.errors
            })
            .collect();

        Ok(CheckDeclDefErrors(errors))
    }
}

impl Default for CheckDeclDef {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InitState {
    Uninit,
    MaybeInit,
    Init,
}

impl InitState {
    fn join(self, other: InitState) -> InitState {
        match (self, other) {
            (InitState::Init, InitState::Init) => InitState::Init,
            (InitState::Uninit, InitState::Uninit) => InitState::Uninit,
            _ => InitState::MaybeInit,
        }
    }
}

#[derive(Clone, Debug)]
struct FlowState {
    vars: FastHashMap<NodeId, InitState>,
    reachable: bool,
}

impl FlowState {
    fn with_function_args(args: &[Arg]) -> Self {
        let mut vars = FastHashMap::default();
        for arg in args {
            vars.insert(arg.id, InitState::Init);
        }
        Self {
            vars,
            reachable: true,
        }
    }
}

struct CheckDeclDefAnalyzer<'a> {
    errors: Vec<YuuError>,
    src_info: &'a SourceInfo,
}

impl CheckDeclDefAnalyzer<'_> {
    fn check_function(&mut self, args: &[Arg], body: &BlockStmt, metadata: &StructuralMetadata) {
        let entry = FlowState::with_function_args(args);
        let _ = self.check_block(body, entry, metadata);
    }

    fn check_block(
        &mut self,
        block: &BlockStmt,
        mut flow: FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        for stmt in &block.body {
            if !flow.reachable {
                break;
            }
            flow = self.check_stmt(stmt, flow, metadata);
        }
        flow
    }

    fn check_stmt(
        &mut self,
        stmt: &StmtNode,
        mut flow: FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        match stmt {
            StmtNode::If(if_stmt) => self.check_if_stmt(if_stmt, flow, metadata),
            StmtNode::While(while_stmt) => self.check_while_stmt(while_stmt, flow, metadata),
            StmtNode::Block(block_stmt) => self.check_block(block_stmt, flow, metadata),
            StmtNode::Match(match_stmt) => self.check_match_stmt(match_stmt, flow, metadata),
            StmtNode::Break(_) => {
                flow.reachable = false;
                flow
            }
            StmtNode::Return(ret) => {
                if let Some(expr) = &ret.expr {
                    self.check_expr(expr, &flow, metadata);
                }
                flow.reachable = false;
                flow
            }
            StmtNode::Atomic(expr) => {
                self.check_expr(expr, &flow, metadata);
                flow
            }
            StmtNode::Decl(decl_stmt) => {
                flow.vars.insert(decl_stmt.ident.id, InitState::Uninit);
                flow
            }
            StmtNode::Def(def_stmt) => {
                self.check_expr(&def_stmt.expr, &flow, metadata);
                if let Some(binding_id) = resolve_def_binding_id(def_stmt, metadata)
                    && flow.vars.contains_key(&binding_id)
                {
                    flow.vars.insert(binding_id, InitState::Init);
                }
                flow
            }
            StmtNode::Let(let_stmt) => {
                self.check_expr(&let_stmt.expr, &flow, metadata);
                if let BindingNode::Ident(ident) = let_stmt.binding.as_ref() {
                    // Rust-style rebinding: a new binding ID is always initialized.
                    flow.vars.insert(ident.id, InitState::Init);
                }
                flow
            }
            StmtNode::Defer(defer_stmt) => {
                self.check_expr(&defer_stmt.expr, &flow, metadata);
                flow
            }
            StmtNode::Error(_) | StmtNode::LuaMeta(_) => flow,
        }
    }

    fn check_if_stmt(
        &mut self,
        if_stmt: &crate::pass_parse::IfStmt,
        incoming: FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        if !incoming.reachable {
            return incoming;
        }

        self.check_expr(&if_stmt.if_block.condition, &incoming, metadata);

        let mut branch_flows = Vec::new();
        branch_flows.push(self.check_block(&if_stmt.if_block.body, incoming.clone(), metadata));

        for else_if in &if_stmt.else_if_blocks {
            self.check_expr(&else_if.condition, &incoming, metadata);
            branch_flows.push(self.check_block(&else_if.body, incoming.clone(), metadata));
        }

        if let Some(else_block) = &if_stmt.else_block {
            branch_flows.push(self.check_block(else_block, incoming.clone(), metadata));
        } else {
            // Implicit else path: straight-line continuation without running any branch body.
            branch_flows.push(incoming);
        }

        self.merge_flows(&branch_flows)
    }

    fn check_while_stmt(
        &mut self,
        while_stmt: &crate::pass_parse::WhileStmt,
        incoming: FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        if !incoming.reachable {
            return incoming;
        }

        self.check_expr(&while_stmt.condition_block.condition, &incoming, metadata);
        let body_out =
            self.check_block(&while_stmt.condition_block.body, incoming.clone(), metadata);

        // Loop may execute zero times, so include incoming as an implicit path.
        self.merge_flows(&[incoming, body_out])
    }

    fn check_match_stmt(
        &mut self,
        match_stmt: &crate::pass_parse::MatchStmt,
        incoming: FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        if !incoming.reachable {
            return incoming;
        }

        self.check_expr(&match_stmt.scrutinee, &incoming, metadata);

        let mut branch_flows = Vec::new();
        for arm in &match_stmt.arms {
            branch_flows.push(self.check_match_arm(arm, &incoming, metadata));
        }

        if let Some(default_case) = &match_stmt.default_case {
            branch_flows.push(self.check_block(default_case, incoming.clone(), metadata));
        } else {
            // Non-exhaustive case fallback path.
            branch_flows.push(incoming);
        }

        self.merge_flows(&branch_flows)
    }

    fn check_match_arm(
        &mut self,
        arm: &MatchArm,
        incoming: &FlowState,
        metadata: &StructuralMetadata,
    ) -> FlowState {
        let mut flow = incoming.clone();

        if let Some(binding) = match_arm_binding(arm.pattern.as_ref())
            && let BindingNode::Ident(ident) = binding
        {
            // Pattern binding is initialized for this arm path.
            flow.vars.insert(ident.id, InitState::Init);
        }

        self.check_block(&arm.body, flow, metadata)
    }

    fn merge_flows(&self, flows: &[FlowState]) -> FlowState {
        let reachable_flows = flows.iter().filter(|f| f.reachable).collect::<Vec<_>>();
        if reachable_flows.is_empty() {
            return FlowState {
                vars: flows.first().map(|f| f.vars.clone()).unwrap_or_default(),
                reachable: false,
            };
        }

        let mut keys = HashSet::<NodeId>::new();
        for flow in &reachable_flows {
            for id in flow.vars.keys() {
                keys.insert(*id);
            }
        }

        let mut vars = FastHashMap::default();
        for id in keys {
            let mut joined = InitState::Uninit;

            if let Some(fflow) = reachable_flows.iter().next() {
                joined = fflow.vars.get(&id).copied().unwrap_or(InitState::Uninit);
            }

            for flow in reachable_flows.iter().skip(1) {
                let state = flow.vars.get(&id).copied().unwrap_or(InitState::Uninit);
                joined = joined.join(state)
            }

            vars.insert(id, joined);
        }

        FlowState {
            vars,
            reachable: true,
        }
    }

    fn check_expr(&mut self, expr: &ExprNode, flow: &FlowState, metadata: &StructuralMetadata) {
        if !flow.reachable {
            return;
        }

        match expr {
            ExprNode::Literal(_) => {}
            ExprNode::Binary(binary_expr) => {
                self.check_expr(&binary_expr.left, flow, metadata);
                self.check_expr(&binary_expr.right, flow, metadata);
            }
            ExprNode::Unary(unary_expr) => {
                self.check_expr(&unary_expr.expr, flow, metadata);
            }
            ExprNode::Ident(ident_expr) => {
                if let Some(binding_id) = metadata.binding_table.get(&ident_expr.id).copied()
                    && let Some(state) = flow.vars.get(&binding_id)
                {
                    match state {
                        InitState::Init => {}
                        InitState::Uninit => self.report_uninitialized_use(ident_expr),
                        InitState::MaybeInit => self.report_maybe_initialized_use(ident_expr),
                    }
                }
            }
            ExprNode::FuncCall(func_call_expr) => {
                self.check_expr(&func_call_expr.lhs, flow, metadata);
                for arg in &func_call_expr.args {
                    self.check_expr(arg, flow, metadata);
                }
            }
            ExprNode::Assignment(assignment_expr) => {
                self.check_expr(&assignment_expr.lhs, flow, metadata);
                self.check_expr(&assignment_expr.rhs, flow, metadata);
            }
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                for (_field, expr) in &struct_instantiation_expr.fields {
                    self.check_expr(expr, flow, metadata);
                }
            }
            ExprNode::EnumInstantiation(enum_instantiation_expr) => {
                if let Some(expr) = enum_instantiation_expr.data.as_ref() {
                    self.check_expr(expr, flow, metadata);
                }
            }
            ExprNode::MemberAccess(member_access_expr) => {
                self.check_expr(&member_access_expr.lhs, flow, metadata);
            }
            ExprNode::Deref(deref_expr) => {
                self.check_expr(&deref_expr.expr, flow, metadata);
            }
            ExprNode::AddressOf(address_of_expr) => {
                self.check_expr(&address_of_expr.expr, flow, metadata);
            }
            ExprNode::HeapAlloc(heap_alloc_expr) => {
                self.check_expr(&heap_alloc_expr.expr, flow, metadata);
            }
            ExprNode::Array(array_expr) => {
                if let Some(expr) = array_expr.init_expr.as_ref() {
                    self.check_expr(expr, flow, metadata);
                }
                self.check_expr(&array_expr.size, flow, metadata);
            }
            ExprNode::ArrayLiteral(array_literal_expr) => {
                for element in &array_literal_expr.elements {
                    self.check_expr(element, flow, metadata);
                }
            }
            ExprNode::Cast(cast_expr) => {
                self.check_expr(&cast_expr.expr, flow, metadata);
            }
            ExprNode::LuaMeta(_) => {}
        }
    }

    fn report_uninitialized_use(&mut self, ident_expr: &crate::pass_parse::IdentExpr) {
        let error = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
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

    fn report_maybe_initialized_use(&mut self, ident_expr: &crate::pass_parse::IdentExpr) {
        let error = YuuError::builder()
            .kind(ErrorKind::InvalidExpression)
            .message(format!(
                "Variable '{}' may be undefined on some control-flow paths",
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
            .help("Define the variable on every reachable path before using it")
            .build();
        self.errors.push(error);
    }
}

fn match_arm_binding(pattern: &RefutablePatternNode) -> Option<&BindingNode> {
    match pattern {
        RefutablePatternNode::Enum(enum_pattern) => enum_pattern.binding.as_deref(),
    }
}

fn resolve_def_binding_id(
    def_stmt: &crate::pass_parse::DefStmt,
    metadata: &StructuralMetadata,
) -> Option<NodeId> {
    metadata.binding_table.get(&def_stmt.ident.id).copied()
}
