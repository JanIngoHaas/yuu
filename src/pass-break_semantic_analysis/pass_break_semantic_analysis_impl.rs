use crate::{
    pass_diagnostics::error::YuuError,
    pass_parse::ast::{AST, StmtNode, StructuralNode, NodeId, BreakStmt},
    utils::source_info::SourceInfo,
};
use miette::Result;

/// Errors from break semantic analysis
pub struct BreakSemanticAnalysisErrors(pub Vec<YuuError>);

/// Break semantic analysis pass that validates break statements are within loops
pub struct BreakSemanticAnalysis;

impl BreakSemanticAnalysis {
    pub fn new() -> Self {
        Self
    }

    /// Run break semantic analysis on the AST
    pub fn run(&self, ast: &AST, src_info: &SourceInfo) -> Result<BreakSemanticAnalysisErrors> {
        let mut analyzer = BreakSemanticAnalyzer {
            errors: Vec::new(),
            loop_context_stack: Vec::new(),
            src_info,
        };

        // Analyze all structural nodes
        for structural in &ast.structurals {
            analyzer.analyze_structural(structural);
        }

        Ok(BreakSemanticAnalysisErrors(analyzer.errors))
    }
}

impl Default for BreakSemanticAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

struct BreakSemanticAnalyzer<'a> {
    errors: Vec<YuuError>,
    loop_context_stack: Vec<NodeId>, // Stack of loop node IDs we're currently inside
    src_info: &'a SourceInfo,
}

impl<'a> BreakSemanticAnalyzer<'a> {
    fn analyze_structural(&mut self, structural: &StructuralNode) {
        match structural {
            StructuralNode::FuncDef(func_def) => {
                // Analyze function body
                self.analyze_block(&func_def.body.body);
            }
            StructuralNode::FuncDecl(_) |
            StructuralNode::StructDecl(_) |
            StructuralNode::StructDef(_) |
            StructuralNode::EnumDef(_) |
            StructuralNode::Error(_) => {
                // These don't contain executable code with break statements
            }
        }
    }

    fn analyze_block(&mut self, stmts: &[StmtNode]) {
        for stmt in stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_stmt(&mut self, stmt: &StmtNode) {
        match stmt {
            StmtNode::Break(break_stmt) => {
                self.validate_break_statement(break_stmt);
            }
            
            StmtNode::While(while_stmt) => {
                // Enter loop context
                self.loop_context_stack.push(while_stmt.id);
                
                // Analyze the loop body
                self.analyze_block(&while_stmt.condition_block.body);
                
                // Exit loop context
                self.loop_context_stack.pop();
            }
            
            StmtNode::If(if_stmt) => {
                // Analyze then block
                self.analyze_block(&if_stmt.condition_block.body);
                
                // Analyze else block if present
                if let Some(else_block) = &if_stmt.else_block {
                    self.analyze_block(&else_block.body);
                }
            }
            
            StmtNode::Block(block_stmt) => {
                self.analyze_block(&block_stmt.body);
            }
            
            StmtNode::Match(match_stmt) => {
                // Analyze match arms
                for arm in &match_stmt.arms {
                    self.analyze_block(&arm.body);
                }
            }
            
            // Other statements don't create new scopes or contain breaks
            StmtNode::Let(_) | 
            StmtNode::Atomic(_) | 
            StmtNode::Return(_) | 
            StmtNode::Error(_) => {
                // Nothing to analyze for break statements
            }
        }
    }

    fn validate_break_statement(&mut self, break_stmt: &BreakStmt) {
        if self.loop_context_stack.is_empty() {
            // Break statement outside of any loop context
            let error = YuuError::builder()
                .kind(crate::pass_diagnostics::error::ErrorKind::InvalidStatement)
                .message("Break statement must be inside a loop".to_string())
                .source(self.src_info.source.clone(), self.src_info.file_name.clone())
                .span(
                    (break_stmt.span.start, break_stmt.span.end - break_stmt.span.start),
                    "break statement outside loop"
                )
                .help("Break statements can only be used inside while loops or other loop constructs")
                .build();
            
            self.errors.push(error);
        }
    }

    fn _current_loop_context(&self) -> Option<NodeId> {
        self.loop_context_stack.last().copied()
    }

    fn _in_loop_context(&self) -> bool {
        !self.loop_context_stack.is_empty()
    }
}