use crate::ast::*;

// This trait is implemented by Claude 3.5 Sonnet
pub trait ToGraphviz {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>);
    fn node_id(&self) -> usize;
}

impl ToGraphviz for Node {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            Node::Expr(expr) => expr.to_graphviz(graph, parent_id),
            Node::Stmt(stmt) => stmt.to_graphviz(graph, parent_id),
            Node::Type(ty) => ty.to_graphviz(graph, parent_id),
            Node::Structural(s) => s.to_graphviz(graph, parent_id),
            Node::Pattern(pattern_node) => pattern_node.to_graphviz(graph, parent_id),
        }
    }

    fn node_id(&self) -> usize {
        match self {
            Node::Expr(expr) => expr.node_id(),
            Node::Stmt(stmt) => stmt.node_id(),
            Node::Type(ty) => ty.node_id(),
            Node::Structural(s) => s.node_id(),
            Node::Pattern(pattern_node) => pattern_node.node_id(),
        }
    }
}

impl ToGraphviz for ExprNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        let my_id = self.node_id();
        match self {
            ExprNode::Literal(lit) => {
                graph.push_str(&format!(
                    "    node{} [label=\"Literal({:?})\"]\n",
                    my_id, lit.lit.kind
                ));
            }
            ExprNode::Binary(bin) => {
                graph.push_str(&format!(
                    "    node{} [label=\"Binary({})\"]\n",
                    my_id, bin.op
                ));
                bin.left.to_graphviz(graph, Some(my_id));
                bin.right.to_graphviz(graph, Some(my_id));
            }
            ExprNode::Unary(un) => {
                graph.push_str(&format!("    node{} [label=\"Unary({})\"]\n", my_id, un.op));
                un.operand.to_graphviz(graph, Some(my_id));
            }
            ExprNode::Ident(id) => {
                graph.push_str(&format!(
                    "    node{} [label=\"Ident({})\"]\n",
                    my_id, id.ident
                ));
            }
            ExprNode::Block(block) => {
                graph.push_str(&format!("    node{} [label=\"Block\"]\n", my_id));
                for stmt in &block.body {
                    stmt.to_graphviz(graph, Some(my_id));
                }
            }
            ExprNode::FuncCall(func_call_expr) => {
                graph.push_str(&format!("    node{} [label=\"FuncCall\"]\n", my_id));
                func_call_expr.lhs.to_graphviz(graph, Some(my_id));
                for arg in &func_call_expr.args {
                    arg.to_graphviz(graph, Some(my_id));
                }
            }
        }
        if let Some(parent) = parent_id {
            graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
        }
    }

    fn node_id(&self) -> usize {
        match self {
            ExprNode::Literal(lit) => lit.id,
            ExprNode::Binary(bin) => bin.id,
            ExprNode::Unary(un) => un.id,
            ExprNode::Ident(id) => id.id,
            ExprNode::Block(block) => block.id,
            ExprNode::FuncCall(func_call_expr) => func_call_expr.id,
        }
    }
}

impl ToGraphviz for StmtNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            StmtNode::Let(let_stmt) => {
                let stmt_id = let_stmt.id;
                // Create the Let node
                graph.push_str(&format!("    node{} [label=\"Let\"]\n", stmt_id));
                // Add pattern and expression as children
                let_stmt.pattern.to_graphviz(graph, Some(stmt_id));
                let_stmt.expr.to_graphviz(graph, Some(stmt_id));
                // Connect to parent if it exists
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, stmt_id));
                }
            }
            StmtNode::Atomic(expr) => expr.to_graphviz(graph, parent_id),
            StmtNode::Return(ret) => {
                graph.push_str(&format!(
                    "    node{} [label=\"Return({:?})\"]\n",
                    ret.id, ret.ty
                ));
                ret.expr.to_graphviz(graph, Some(ret.id));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, ret.id));
                }
            }
        }
    }

    fn node_id(&self) -> usize {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.id,
            StmtNode::Atomic(expr) => expr.node_id(),
            StmtNode::Return(ret) => ret.id,
        }
    }
}

impl ToGraphviz for TypeNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            TypeNode::Ident(id) => {
                let my_id = self.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"Type({})\"]\n",
                    my_id, id.name
                ));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
            TypeNode::BuiltIn(built_in_type) => {
                let my_id = self.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"BuiltIn({:?})\"]\n",
                    my_id, built_in_type.kind
                ));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
        }
    }

    fn node_id(&self) -> usize {
        match self {
            TypeNode::Ident(id) => id.id,
            TypeNode::BuiltIn(built_in_type) => built_in_type.id,
        }
    }
}

impl ToGraphviz for StructuralNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            StructuralNode::FuncDecl(decl) => {
                let my_id = self.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"FuncDecl({})\"]\n",
                    my_id, decl.name
                ));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
            StructuralNode::FuncDef(def) => {
                let my_id = self.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"FuncDef({})\"]\n",
                    my_id, def.decl.name
                ));
                for stmt in &def.body.body {
                    stmt.to_graphviz(graph, Some(my_id));
                }
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
        }
    }

    fn node_id(&self) -> usize {
        match self {
            StructuralNode::FuncDecl(decl) => decl.span.start,
            StructuralNode::FuncDef(def) => def.id,
        }
    }
}

impl ToGraphviz for PatternNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            PatternNode::Ident(ident) => {
                // Use pattern's own ID, not parent's
                let my_id = self.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"Pattern({})\"]\n",
                    my_id, ident.name
                ));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
        }
    }

    fn node_id(&self) -> usize {
        match self {
            PatternNode::Ident(ident) => ident.id,
        }
    }
}

// Add to the Display impl for Node
impl Node {
    pub fn to_graphviz_string(&self) -> String {
        let mut graph = String::from("digraph AST {\n");
        self.to_graphviz(&mut graph, None);
        graph.push_str("}\n");
        graph
    }
}
