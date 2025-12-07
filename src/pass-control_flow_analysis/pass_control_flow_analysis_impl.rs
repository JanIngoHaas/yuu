use crate::{
    pass_diagnostics::error::YuuError,
    pass_parse::{
        BlockStmt, SourceInfo,
        ast::{AST, NodeId, StmtNode, StructuralNode},
    },
    pass_type_inference::{TypeInfo, TypeRegistry, primitive_nil},
};
use miette::Result;

/// Errors from control flow analysis
pub struct ControlFlowAnalysisErrors(pub Vec<YuuError>);

/// Control flow analysis pass that validates function return paths
pub struct ControlFlowAnalysis;

impl ControlFlowAnalysis {
    pub fn new() -> Self {
        Self
    }

    /// Run control flow analysis on the AST
    pub fn run(
        &self,
        ast: &AST,
        type_registry: &TypeRegistry,
        src_info: &SourceInfo,
    ) -> Result<ControlFlowAnalysisErrors> {
        let mut analyzer = ControlFlowAnalyzer {
            errors: Vec::new(),
            type_registry,
            src_info,
        };

        // Analyze all structural nodes (functions)
        for structural in &ast.structurals {
            analyzer.analyze_structural(structural);
        }

        Ok(ControlFlowAnalysisErrors(analyzer.errors))
    }
}

impl Default for ControlFlowAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

struct ControlFlowAnalyzer<'a> {
    errors: Vec<YuuError>,
    type_registry: &'a TypeRegistry,
    src_info: &'a SourceInfo,
}

/// Represents the control flow state of a code path
#[derive(Debug, Clone, Copy, PartialEq)]
enum FlowState {
    /// Path could continue (or not, but we are not here for possibilities)
    CouldContinue,
    /// Path definitely returns
    AllReturn,
}

impl FlowState {
    /// Combine two flow states (e.g., from if-else branches)
    fn combine(&mut self, other: FlowState) -> FlowState {
        match (self, other) {
            (FlowState::AllReturn, FlowState::AllReturn) => FlowState::AllReturn,
            (FlowState::CouldContinue, FlowState::CouldContinue) => FlowState::CouldContinue,
            _ => FlowState::CouldContinue,
        }
    }
}

impl<'a> ControlFlowAnalyzer<'a> {
    fn analyze_structural(&mut self, structural: &StructuralNode) {
        match structural {
            StructuralNode::FuncDef(func_def) => {
                // Get the function's declared return type
                let function_type = self
                    .type_registry
                    .type_info_table
                    .get(func_def.id)
                    .expect("Function return type needs to be known at this point.");

                // Extract the return type from the function type
                let declared_return_type = match function_type {
                    TypeInfo::Function(ft) => ft.ret,
                    _ => unreachable!("Expected function type for function definition"),
                };

                // First, check if the function actually has a return type of something other than "nil"
                if declared_return_type.is_exact_same_type(primitive_nil()) {
                    return; // No need to analyze further, implicit returns are allowed (we are returning nil)
                }

                // Analyze the function body's control flow
                let flow_state = self.analyze_block(&func_def.body);

                // Check if function can reach end without returning
                if flow_state != FlowState::AllReturn {
                    self.implicit_return_error(
                        &func_def.decl.name,
                        declared_return_type,
                        func_def.id,
                        func_def.span.clone(),
                    );
                }
            }
            _ => {
                // Other structural nodes don't need control flow analysis
            }
        }
    }

    fn analyze_block(&mut self, block: &BlockStmt) -> FlowState {
        for stmt in block.body.iter() {
            let stmt_state = self.analyze_stmt(stmt);

            // Once we hit a return, the rest is unreachable
            if stmt_state == FlowState::AllReturn {
                return FlowState::AllReturn;
            }
        }

        FlowState::CouldContinue
    }

    fn analyze_stmt(&mut self, stmt: &StmtNode) -> FlowState {
        match stmt {
            StmtNode::Return(_) => FlowState::AllReturn,

            StmtNode::If(if_stmt) => {
                // Analyze the "then" branch
                let mut then_state = self.analyze_block(&if_stmt.if_block.body);

                // If else-ifs exist, they all need to return as well, otherwise, we could have the case that we're hitting this else-if and not return anything, in the end...
                for else_if in &if_stmt.else_if_blocks {
                    let else_if_state = self.analyze_block(&else_if.body);
                    then_state.combine(else_if_state);
                }

                if let Some(else_block) = &if_stmt.else_block {
                    let else_state = self.analyze_block(else_block);
                    then_state.combine(else_state)
                } else {
                    // If no else, we could have the case that neither the if nor the else-if conditions are met, thus leading to "no return";
                    then_state.combine(FlowState::CouldContinue)
                }
            }

            StmtNode::While(while_stmt) => {
                // While loops don't guarantee execution of their body -> Flag always "No return"
                let _body_state = self.analyze_block(&while_stmt.condition_block.body);
                FlowState::CouldContinue
            }

            StmtNode::Block(block_stmt) => self.analyze_block(block_stmt),

            StmtNode::Match(match_stmt) => {
                // In match arms, we have to check that all return; just simple checking.. Exhaustiveness is already checked in type-inference.
                let mut fstate = None;
                for arm in &match_stmt.arms {
                    let arm_state = self.analyze_block(&arm.body);
                    fstate.get_or_insert(arm_state).combine(arm_state);
                }

                // In the event that we have a default match arm, we need to account for that as well
                if let Some(else_arm) = &match_stmt.default_case {
                    let else_state = self.analyze_block(else_arm);
                    fstate.get_or_insert(else_state).combine(else_state);
                }
                fstate.unwrap_or(FlowState::CouldContinue)
            }

            // Other statements don't affect control flow
            StmtNode::Let(_) | StmtNode::Atomic(_) | StmtNode::Break(_) | StmtNode::Defer(_) | StmtNode::Decl(_) | StmtNode::Def(_) | StmtNode::Error(_) => {
                FlowState::CouldContinue
            }
        }
    }

    fn implicit_return_error(
        &mut self,
        func_name: &ustr::Ustr,
        declared_return_type: &'static TypeInfo,
        _func_id: NodeId,
        func_span: logos::Span,
    ) {
        let error = YuuError::builder()
                .kind(crate::pass_diagnostics::error::ErrorKind::TypeMismatch)
                .message(format!(
                    "Function '{}' may reach end without returning, but is declared to return '{}'",
                    func_name.as_str(),
                    declared_return_type
                ))
                .source(self.src_info.source.clone(), self.src_info.file_name.clone())
                .span(
                    (func_span.start, func_span.end - func_span.start),
                    "function may implicitly return nil"
                )
                .help(
                    "Some execution paths don't return - add return statement at the end of the function or in all execution paths that may execute"
                )
                .build();

        self.errors.push(error);
    }
}
