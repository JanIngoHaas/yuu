use logos::Span;

use crate::token::{Integer, Token, TokenVariants};
use std::fmt::{self, Display, Formatter};

/// Binary operators for arithmetic expressions
#[derive(Debug, Clone, Copy, PartialEq)]
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
#[derive(Debug, Clone, Copy, PartialEq)]
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
#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub lit: Token,
    pub id: usize,
}

impl LiteralExpr {
    pub fn get_type(&self) -> &'static str {
        match self.lit.kind {
            TokenVariants::Integer(Integer::I64(_)) => "i64",
            TokenVariants::F32(_) => "f32",
            TokenVariants::F64(_) => "f64",
            _ => unreachable!("literal kind has no immediate type"),
        }
    }
}

/// Represents a binary expression (e.g., a + b)
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<ExprNode>,
    pub right: Box<ExprNode>,
    pub op: BinOp,
    pub span: Span,
    pub id: usize,
}

/// Represents a unary expression (e.g., -a, !b)
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operand: Box<ExprNode>,
    pub op: UnaryOp,
    pub span: Span,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct IdentExpr {
    pub ident: String,
    pub span: Span,
    pub id: usize,
}

/// Represents an expression in the AST
#[derive(Debug, Clone)]
pub enum ExprNode {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Ident(IdentExpr),
}

/// Represents a node in the AST
#[derive(Debug, Clone)]
pub enum Node {
    Expr(ExprNode),
}

// Github Copilot generated this code
impl Node {
    fn print_tree(&self, f: &mut fmt::Formatter<'_>, prefix: &str, is_last: bool) -> fmt::Result {
        // Determine the appropriate connector
        let connector = if is_last { "└─" } else { "├─" };
        // Write the current node
        write!(f, "{}{}", prefix, connector)?;
        match self {
            Node::Expr(expr) => match expr {
                ExprNode::Literal(lit) => match &lit.lit.kind {
                    TokenVariants::Integer(Integer::I64(i)) => {
                        writeln!(f, "Literal({})", i)
                    }
                    TokenVariants::F32(x) => writeln!(f, "Literal({})", x),
                    TokenVariants::F64(x) => writeln!(f, "Literal({})", x),
                    _ => writeln!(f, "Literal({:?})", lit.lit.kind),
                },
                ExprNode::Binary(bin) => {
                    writeln!(f, "Binary({})", bin.op)?;
                    // Prepare the prefix for child nodes
                    let new_prefix = format!("{}{}", prefix, if is_last { "    " } else { "│   " });
                    // Left child is not the last when there is a right child
                    Node::Expr(*bin.left.clone()).print_tree(&mut *f, &new_prefix, false)?;
                    // Right child is the last
                    Node::Expr(*bin.right.clone()).print_tree(&mut *f, &new_prefix, true)
                }
                ExprNode::Unary(un) => {
                    writeln!(f, "Unary({})", un.op)?;
                    // Prepare the prefix for the operand
                    let new_prefix = format!("{}{}", prefix, if is_last { "    " } else { "│   " });
                    // Operand is always the last (or only) child
                    Node::Expr(*un.operand.clone()).print_tree(f, &new_prefix, true)
                }
                ExprNode::Ident(id) => {
                    writeln!(f, "Ident({})", id.ident)
                }
            },
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Start with no prefix and consider the root node as the last
        self.print_tree(f, "", true)
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Node {
    fn span(&self) -> Span {
        match self {
            Node::Expr(expr) => expr.span(),
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
