// use yuu_shared::{
//     ast::*,
//     token::{Integer, TokenKind},
// };
// use yuu_transform::{block, type_info::TypeInfoTable};

// pub struct AstToCPass;

// pub struct TransientData<'a> {
//     output: String,
//     types: &'a TypeInfoTable,
//     merge_counter: u64,
// }

// impl AstToCPass {
//     pub fn new() -> Self {
//         Self {}
//     }

//     pub fn gen_expr(expr: &ExprNode, data: &mut TransientData) {
//         let out = match &expr {
//             ExprNode::Literal(literal_expr) => Self::gen_literal_expr(literal_expr, data),
//             ExprNode::Binary(binary_expr) => gen_binary_expr(binary_expr, data),
//             ExprNode::Unary(unary_expr) => gen_unary_expr(unary_expr, data),
//             ExprNode::Ident(ident_expr) => gen_ident_expr(ident_expr, data),
//             ExprNode::Block(block_expr) => gen_block_expr(block_expr, data),
//             ExprNode::FuncCall(func_call_expr) => gen_func_call_expr(func_call_expr, data),
//             ExprNode::If(if_expr) => gen_if_expr(if_expr, data),
//         };
//     }

//     pub fn gen_literal_expr(literal_expr: &LiteralExpr, data: &mut TransientData) {
//         let out = match literal_expr.lit.kind {
//             TokenKind::Integer(Integer::I64(i)) => format!("INT64_C({})", i),
//             TokenKind::F32(f) => format!("{}f", f),
//             TokenKind::F64(f) => format!("{}d", f),
//             TokenKind::TrueKw => format!("true"),
//             TokenKind::FalseKw => format!("false"),
//             _ => unreachable!("Other literals are not supported yet"),
//         };
//         data.output.push_str(&out);
//     }

//     pub fn gen_binary_expr(binary_expr: &BinaryExpr, data: &mut TransientData) {
//         let out = format!("({}{}{})", binary_expr.lhs, binary_expr.op, binary_expr.rhs);
//         data.output.push_str(&out);
//     }

//     pub fn gen_unary_expr(unary_expr: &UnaryExpr, data: &mut TransientData) {
//         let out = format!("({}{})", unary_expr.op, unary_expr.rhs);
//         data.output.push_str(&out);
//     }

//     pub fn gen_ident_expr(ident_expr: &IdentExpr, data: &mut TransientData) {
//         let out = format!("{}", ident_expr.ident);
//         data.output.push_str(&ident_expr.ident);
//     }

//     pub fn gen_block_expr(block_expr: &BlockExpr, data: &mut TransientData) {
//         let out = format!("{{{}}}", block_expr.body);
//         data.output.push_str(&out);
//     }

//     pub fn generate(node: &Node, data: &mut TransientData) {
//         match &node {
//             Node::Expr(expr_node) => todo!(),
//             Node::Stmt(stmt_node) => todo!(),
//             Node::Type(type_node) => todo!(),
//             Node::Structural(structural_node) => todo!(),
//             Node::Binding(binding_node) => todo!(),
//         };
//     }
// }
