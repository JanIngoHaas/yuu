use ustr::Ustr;

use crate::pass_diagnostics::error::YuuError;
use crate::pass_type_inference::{IndexUstrMap, IndexUstrSet};


pub use self::pass_type_dependencies_impl::TypeDependencyAnalysis;

mod pass_type_dependencies_impl;

/// Type dependency graph using petgraph
pub struct TypeDependencyGraph(pub IndexUstrMap<Vec<Ustr>>);

/// Errors that can occur during type dependency analysis
#[derive(Debug, Clone)]
pub struct TypeDependencyAnalysisErrors(pub Vec<YuuError>);

impl TypeDependencyAnalysisErrors {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn has_errors(&self) -> bool {
        !self.0.is_empty()
    }

    pub fn add_error(&mut self, error: YuuError) {
        self.0.push(error);
    }
}

impl Default for TypeDependencyAnalysisErrors {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeDependencyGraph {
    fn create_topological_order_inner(&self, ordered: &mut Vec<Ustr>, node: Ustr) {
        let deps = self.0.get(&node);
        if let Some(deps) = deps {
            for &dep in deps {
                self.create_topological_order_inner(ordered, dep);
            }
        }

        ordered.push(node);
    }

    pub fn create_topological_order(&self) -> Vec<Ustr> {
        // Find the "root" nodes (i.e. nodes that depend on other nodes but have no other node that depends on them)

        let mut root_nodes = IndexUstrSet::from_iter(self.0.keys().copied());

        for (_node, deps) in &self.0 {
            for dep in deps {
                root_nodes.swap_remove(dep);    
            }
        }

        // The remaining items in "root_nodes" had no incoming edges!

        let mut ordered = Vec::new();
        while let Some(node) = root_nodes.pop() {
            self.create_topological_order_inner(&mut ordered, node);
        }

        ordered
    }
}
