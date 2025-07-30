use logos::Span;
use serde::{Deserialize, Serialize};
use ustr::Ustr;

use crate::pass_parse::token::Token;
use std::{
    fmt::{self, Display, Formatter},
    sync::Arc,
};

/// Source code information
#[derive(Debug, Clone)]
pub struct SourceInfo {
    pub source: Arc<str>,
    pub file_name: Arc<str>,
}

/// Binary operators for arithmetic expressions
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Subtract => write!(f, "-"),
            BinOp::Multiply => write!(f, "*"),
            BinOp::Divide => write!(f, "/"),
            BinOp::Eq => write!(f, "=="),
            BinOp::NotEq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::LtEq => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::GtEq => write!(f, ">="),
        }
    }
}

impl BinOp {
    pub fn static_name(&self) -> Ustr {
        match self {
            BinOp::Add => "add",
            BinOp::Subtract => "sub",
            BinOp::Multiply => "mul",
            BinOp::Divide => "div",
            BinOp::Eq => "eq",
            BinOp::NotEq => "ne",
            BinOp::Lt => "lt",
            BinOp::LtEq => "le",
            BinOp::Gt => "gt",
            BinOp::GtEq => "ge",
        }
        .intern()
    }
}

/// Unary operators for expressions
#[derive(Serialize, Deserialize, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Pos, // Meaningless, but included for completeness
    Negate,
}

impl UnaryOp {
    pub fn static_name(&self) -> Ustr {
        match self {
            UnaryOp::Pos => "pos",
            UnaryOp::Negate => "neg",
        }
        .intern()
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Pos => write!(f, "+"),
            UnaryOp::Negate => write!(f, "-"),
        }
    }
}

/// Literal expression
#[derive(Serialize, Deserialize, Clone, PartialEq)]
pub struct LiteralExpr {
    pub lit: Token,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ConditionWithBody {
    pub condition: Box<ExprNode>,
    pub body: BlockExpr,
}

/// Represents an if expression in the AST
#[derive(Serialize, Deserialize, Clone)]
pub struct IfExpr {
    pub id: NodeId,
    pub span: Span,
    pub if_block: ConditionWithBody,
    pub else_if_blocks: Vec<ConditionWithBody>,
    pub else_block: Option<BlockExpr>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct WhileExpr {
    pub id: NodeId,
    pub span: Span,
    pub condition_block: ConditionWithBody,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Field {
    pub name: Ustr,
    pub span: Span,
}

/// Represents an instantiation of a struct
#[derive(Serialize, Deserialize, Clone)]
pub struct StructInstantiationExpr {
    pub id: NodeId,
    pub span: Span,
    pub struct_name: Ustr,
    pub fields: Vec<(Field, ExprNode)>,
}

/// Represents a binary expression (e.g., a + b)
#[derive(Serialize, Deserialize, Clone)]
pub struct BinaryExpr {
    pub left: Box<ExprNode>,
    pub right: Box<ExprNode>,
    pub op: BinOp,
    pub span: Span,
    pub id: NodeId,
}

/// Represents a unary expression (e.g., -a, !b)
#[derive(Serialize, Deserialize, Clone)]
pub struct UnaryExpr {
    pub operand: Box<ExprNode>,
    pub op: UnaryOp,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IdentExpr {
    pub ident: Ustr,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct AssignmentExpr {
    pub lhs: Box<ExprNode>, // Changed from Box<BindingNode>
    pub rhs: Box<ExprNode>,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct MemberAccessExpr {
    pub lhs: Box<ExprNode>,
    pub field: Field,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct EnumUnitPattern {
    pub enum_name: Ustr,
    pub variant_name: Ustr,
    pub span: Span,
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct EnumDataPattern {
    pub enum_name: Ustr,
    pub variant_name: Ustr,
    pub binding: Box<BindingNode>,
    pub span: Span,
    pub id: NodeId,
}

// Enum-specific pattern for variant matching
#[derive(Serialize, Deserialize, Clone)]
pub enum EnumPattern {
    Unit(EnumUnitPattern),     // enum name, variant name
    WithData(EnumDataPattern), // Blue(pattern) - enum name, variant name, inner pattern
}

/// Refutable patterns...
#[derive(Serialize, Deserialize, Clone)]
pub enum RefutablePatternNode {
    Enum(EnumPattern),
}

/// Match arm in a match expression
#[derive(Serialize, Deserialize, Clone)]
pub struct MatchArm {
    pub pattern: Box<RefutablePatternNode>,
    pub body: Box<ExprNode>,
    pub span: Span,
    pub id: NodeId,
}

/// Match expression
#[derive(Serialize, Deserialize, Clone)]
pub struct MatchExpr {
    pub id: NodeId,
    pub span: Span,
    pub scrutinee: Box<ExprNode>, // The expression being matched
    pub arms: Vec<MatchArm>,
}

/// Enum variant instantiation (Color::Red or Color::Blue(value))
#[derive(Serialize, Deserialize, Clone)]
pub struct EnumInstantiationExpr {
    pub id: NodeId,
    pub span: Span,
    pub enum_name: Ustr,
    pub variant_name: Ustr,
    pub data: Option<Box<ExprNode>>, // None for unit variants
}

/// Represents an expression in the AST
#[derive(Serialize, Deserialize, Clone)]
pub enum ExprNode {
    Literal(LiteralExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Ident(IdentExpr),
    Block(BlockExpr),
    FuncCall(FuncCallExpr),
    If(IfExpr), // TODO: Need a pass that checks if we have a if-else block - only "if" is not enough (often).
    While(WhileExpr),
    Assignment(AssignmentExpr),
    StructInstantiation(StructInstantiationExpr),
    EnumInstantiation(EnumInstantiationExpr),
    MemberAccess(MemberAccessExpr),
    Match(MatchExpr),
    //Error,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct LetStmt {
    pub span: Span,
    pub id: NodeId,
    pub expr: Box<ExprNode>,
    pub ty: Option<TypeNode>,
    pub binding: Box<BindingNode>,
}

// #[derive(Serialize, Deserialize, Clone)]
// pub struct ReturnStmt {
//     pub span: Span,
//     pub expr: Box<ExprNode>,
//     pub id: NodeId,
// }

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct IdentBinding {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
    pub is_mut: bool,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum BindingNode {
    Ident(IdentBinding),
}

impl BindingNode {
    pub fn is_mut(&self) -> bool {
        match self {
            BindingNode::Ident(ident) => ident.is_mut,
        }
    }
}

impl Display for BindingNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BindingNode::Ident(ident) => write!(f, "{}", ident.name),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct IdentType {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum BuiltInTypeKind {
    I64,
    F32,
    F64,
    Bool,
}

impl Display for BuiltInTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BuiltInTypeKind::I64 => write!(f, "i64"),
            BuiltInTypeKind::F32 => write!(f, "f32"),
            BuiltInTypeKind::F64 => write!(f, "f64"),
            BuiltInTypeKind::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BuiltInType {
    pub id: NodeId,
    pub span: Span,
    pub kind: BuiltInTypeKind,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum TypeNode {
    BuiltIn(BuiltInType),
    Ident(IdentType),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Arg {
    pub ty: TypeNode,
    pub name: Ustr,
    pub span: Span,
    pub id: NodeId,
    pub is_mut: bool,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct FuncDeclStructural {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
    pub args: Vec<Arg>,
    pub ret_ty: Option<Box<TypeNode>>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BlockExpr {
    pub id: NodeId,
    pub span: Span,
    pub body: Vec<StmtNode>,
    pub label: Option<Ustr>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct FuncCallExpr {
    pub id: NodeId,
    pub span: Span,
    pub lhs: Box<ExprNode>,
    pub args: Vec<ExprNode>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct FuncDefStructural {
    pub id: NodeId,
    pub decl: FuncDeclStructural,
    pub body: BlockExpr,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct StructDeclStructural {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct StructDefStructural {
    pub id: NodeId,
    pub span: Span,
    pub decl: StructDeclStructural,
    pub fields: Vec<Arg>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct EnumVariant {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
    pub data_type: Option<TypeNode>, // None for unit variants, Some(T) for data variants
}

#[derive(Serialize, Deserialize, Clone)]
pub struct EnumDeclStructural {
    pub id: NodeId,
    pub span: Span,
    pub name: Ustr,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct EnumDefStructural {
    pub id: NodeId,
    pub span: Span,
    pub decl: EnumDeclStructural,
    pub variants: Vec<EnumVariant>,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum StructuralNode {
    FuncDecl(FuncDeclStructural),
    FuncDef(FuncDefStructural),
    StructDecl(StructDeclStructural),
    StructDef(StructDefStructural),
    EnumDef(EnumDefStructural),
    Error(NodeId),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct BreakStmt {
    pub span: Span,
    pub expr: Box<ExprNode>,  // Optional for => ; case
    pub target: Option<Ustr>, // None for break expr, Some(label) for break label expr
    pub id: NodeId,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum StmtNode {
    Let(LetStmt),
    Atomic(ExprNode),
    //Return(ReturnStmt),
    Break(BreakStmt), // new variant
    Error(NodeId),
}

/// Represents a node in the AST
#[derive(Serialize, Deserialize, Clone)]
pub enum Node {
    Expr(ExprNode),
    Stmt(StmtNode),
    Type(TypeNode),
    Structural(StructuralNode),
    Binding(BindingNode),
}

pub struct AST {
    pub structurals: Vec<Box<StructuralNode>>,
}

// impl Display for Node {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.to_graphviz_string())
//     }
// }

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Node {
    fn span(&self) -> Span {
        match self {
            Node::Expr(expr) => expr.span(),
            Node::Stmt(stmt_node) => stmt_node.span(),
            Node::Type(type_node) => type_node.span(),
            Node::Structural(structural_node) => structural_node.span(),
            Node::Binding(pattern_node) => match pattern_node {
                BindingNode::Ident(ident) => ident.span.clone(),
            },
        }
    }
}

impl Spanned for ExprNode {
    fn span(&self) -> Span {
        match self {
            ExprNode::Literal(lit) => lit.lit.span.clone(),
            ExprNode::Binary(bin) => bin.span.clone(),
            ExprNode::Unary(un) => un.span.clone(),
            ExprNode::Ident(id) => id.span.clone(),
            ExprNode::Block(block_expr) => block_expr.span.clone(),
            ExprNode::FuncCall(func_call_expr) => func_call_expr.span.clone(),
            ExprNode::If(if_expr) => if_expr.span.clone(),
            ExprNode::Assignment(assign) => assign.span.clone(),
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                struct_instantiation_expr.span.clone()
            }
            ExprNode::EnumInstantiation(enum_instantiation_expr) => {
                enum_instantiation_expr.span.clone()
            }
            ExprNode::While(while_expr) => while_expr.span.clone(),
            ExprNode::MemberAccess(member_access_expr) => member_access_expr.span.clone(),
            ExprNode::Match(match_expr) => match_expr.span.clone(),
        }
    }
}

impl Spanned for StmtNode {
    fn span(&self) -> Span {
        match self {
            StmtNode::Let(let_stmt) => let_stmt.span.clone(),
            StmtNode::Atomic(expr_node) => expr_node.span(),
            //StmtNode::Return(return_stmt) => return_stmt.span.clone(),
            StmtNode::Break(exit_stmt) => exit_stmt.span.clone(),
            StmtNode::Error(_) => 0..0,
        }
    }
}

impl Spanned for TypeNode {
    fn span(&self) -> Span {
        match self {
            TypeNode::Ident(ident_type) => ident_type.span.clone(),
            TypeNode::BuiltIn(built_in_type) => built_in_type.span.clone(),
        }
    }
}

impl Spanned for StructuralNode {
    fn span(&self) -> Span {
        match self {
            StructuralNode::FuncDecl(decl) => decl.span.clone(),
            StructuralNode::FuncDef(def) => def.span.clone(),
            StructuralNode::Error(_) => 0..0,
            StructuralNode::StructDecl(struct_decl_structural) => {
                struct_decl_structural.span.clone()
            }
            StructuralNode::StructDef(struct_def_structural) => struct_def_structural.span.clone(),
            StructuralNode::EnumDef(enum_def_structural) => enum_def_structural.span.clone(),
        }
    }
}

impl Spanned for BindingNode {
    fn span(&self) -> Span {
        match self {
            BindingNode::Ident(ident) => ident.span.clone(),
        }
    }
}

pub type NodeId = i64;

pub trait InternUstr {
    fn intern(&self) -> Ustr;
}

impl InternUstr for str {
    fn intern(&self) -> Ustr {
        Ustr::from(self)
    }
}

impl InternUstr for String {
    fn intern(&self) -> Ustr {
        Ustr::from(self.as_str())
    }
}
