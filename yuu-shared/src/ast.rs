use logos::Span;
use serde::{Deserialize, Serialize};

use crate::token::{Integer, Token, TokenVariants};
use std::fmt::{self, Display, Formatter};

/// Binary operators for arithmetic expressions
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Subtract => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
        }
    }
}

/// Unary operators for expressions
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Pos, // Meaningless, but included for completeness
    Negate,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Pos => write!(f, "+"),
            UnaryOp::Negate => write!(f, "-"),
        }
    }
}

/// Represents a literal value in the AST
#[derive(Serialize, Deserialize, Clone)]
pub struct LiteralExpr {
    pub lit: Token,
    pub id: usize,
}

/// Represents a binary expression (e.g., a + b)
#[derive(Serialize, Deserialize, Clone)]
pub struct BinaryExpr {
    pub left: Box<ExprNode>,
    pub right: Box<ExprNode>,
    pub op: BinOp,
    pub span: Span,
    pub id: usize,
}

/// Represents a unary expression (e.g., -a, !b)
#[derive(Serialize, Deserialize, Clone)]
pub struct UnaryExpr {
    pub operand: Box<ExprNode>,
    pub op: UnaryOp,
    pub span: Span,
    pub id: usize,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IdentExpr {
    pub ident: String,
    pub span: Span,
    pub id: usize,
}

/// Represents an expression in the AST
#[derive(Serialize, Deserialize, Clone)]
pub enum ExprNode {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Ident(IdentExpr),
    Block(BlockExpr),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct LetStmt {
    pub span: Span,
    pub name: String,
    pub expr: Box<ExprNode>,
    pub ty: Option<TypeNode>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ReturnStmtType {
    ReturnFromBlock,
    ReturnFromFunction,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ReturnStmt {
    pub span: Span,
    pub expr: Box<ExprNode>,
    pub ty: ReturnStmtType,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IdentType {
    pub id: usize,
    pub span: Span,
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BuiltInTypeKind {
    I64,
    F32,
    F64,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BuiltInType {
    pub id: usize,
    pub span: Span,
    pub kind: BuiltInTypeKind,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum TypeNode {
    BuiltIn(BuiltInType),
    Ident(IdentType),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct FuncDeclStructural {
    pub id: usize,
    pub span: Span,
    pub name: String,
    pub args: Vec<TypeNode>,
    pub ret_ty: Option<Box<TypeNode>>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BlockExpr {
    pub id: usize,
    pub span: Span,
    pub body: Vec<StmtNode>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct FuncDefStructural {
    pub id: usize,
    pub decl: FuncDeclStructural,
    pub body: BlockExpr,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum StructuralNode {
    FuncDecl(FuncDeclStructural),
    FuncDef(FuncDefStructural),
}

#[derive(Serialize, Deserialize, Clone)]
pub enum StmtNode {
    Let(LetStmt),
    Atomic(ExprNode),
    Return(ReturnStmt),
}

/// Represents a node in the AST
#[derive(Serialize, Deserialize, Clone)]
pub enum Node {
    Expr(ExprNode),
    Stmt(StmtNode),
    Type(TypeNode),
    Structural(StructuralNode),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_graphviz_string())
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Node {
    fn span(&self) -> Span {
        match self {
            Node::Expr(expr) => expr.span(),
            Node::Stmt(stmt_node) => stmt_node.span(),
            Node::Type(type_node) => type_node.span(),
            Node::Structural(structural_node) => match structural_node {
                StructuralNode::FuncDecl(decl) => decl.span(),
                StructuralNode::FuncDef(def) => def.span(),
            },
        }
    }
}

impl Spanned for ExprNode {
    fn span(&self) -> Span {
        match self {
            ExprNode::Literal(lit) => lit.span(),
            ExprNode::Binary(bin) => bin.span(),
            ExprNode::Unary(un) => un.span(),
            ExprNode::Ident(id) => id.span(),
            ExprNode::Block(block_expr) => block_expr.span(),
        }
    }
}

impl Spanned for LiteralExpr {
    fn span(&self) -> Span {
        self.lit.span.clone()
    }
}

impl Spanned for BinaryExpr {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for UnaryExpr {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for IdentExpr {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for Token {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for LetStmt {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for ReturnStmt {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for IdentType {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for FuncDeclStructural {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for FuncDefStructural {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for StmtNode {
    fn span(&self) -> Span {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.span(),
            StmtNode::Atomic(expr_node) => expr_node.span(),
            StmtNode::Return(return_stmt) => return_stmt.span(),
        }
    }
}

impl Spanned for TypeNode {
    fn span(&self) -> Span {
        match self {
            TypeNode::Ident(ident_type) => ident_type.span(),
            TypeNode::BuiltIn(built_in_type) => built_in_type.span(),
        }
    }
}

impl Spanned for BlockExpr {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Spanned for StructuralNode {
    fn span(&self) -> Span {
        match self {
            StructuralNode::FuncDecl(decl) => decl.span(),
            StructuralNode::FuncDef(def) => def.span(),
        }
    }
}

impl Spanned for BuiltInType {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

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
        }
    }

    fn node_id(&self) -> usize {
        match self {
            Node::Expr(expr) => expr.node_id(),
            Node::Stmt(stmt) => stmt.node_id(),
            Node::Type(ty) => ty.node_id(),
            Node::Structural(s) => s.node_id(),
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
        }
    }
}

impl ToGraphviz for StmtNode {
    fn to_graphviz(&self, graph: &mut String, parent_id: Option<usize>) {
        match self {
            StmtNode::Let(let_stmt) => {
                let my_id = let_stmt.expr.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"Let({})\"]\n",
                    my_id, let_stmt.name
                ));
                let_stmt.expr.to_graphviz(graph, Some(my_id));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
            StmtNode::Atomic(expr) => expr.to_graphviz(graph, parent_id),
            StmtNode::Return(ret) => {
                let my_id = ret.expr.node_id();
                graph.push_str(&format!(
                    "    node{} [label=\"Return({:?})\"]\n",
                    my_id, ret.ty
                ));
                ret.expr.to_graphviz(graph, Some(my_id));
                if let Some(parent) = parent_id {
                    graph.push_str(&format!("    node{} -> node{}\n", parent, my_id));
                }
            }
        }
    }

    fn node_id(&self) -> usize {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.expr.node_id(),
            StmtNode::Atomic(expr) => expr.node_id(),
            StmtNode::Return(ret) => ret.expr.node_id(),
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

// Add to the Display impl for Node
impl Node {
    pub fn to_graphviz_string(&self) -> String {
        let mut graph = String::from("digraph AST {\n");
        self.to_graphviz(&mut graph, None);
        graph.push_str("}\n");
        graph
    }
}
