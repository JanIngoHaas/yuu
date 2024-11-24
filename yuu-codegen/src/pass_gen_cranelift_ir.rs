// use codegen::Context;
// use cranelift::jit::{JITBuilder, JITModule};
// use cranelift::module::{default_libcall_names, DataDescription, Linkage, Module};
// use cranelift::object::{ObjectBuilder, ObjectModule};
// use cranelift::{native, prelude::*};
// use yuu_shared::token::{Integer, TokenVariants};
// use yuu_shared::{ast::*, Pass};
// use yuu_transform::pass_type_check::{BuiltInType, TypeInfo, TypeInfoMap};

// pub struct GenCraneliftIRPass;

// struct GenWithContext<'a> {
//     pub(crate) type_info: &'a TypeInfoMap,
//     pub(crate) ctx: Context,
//     pub(crate) func_builder_ctx: FunctionBuilderContext,
// }

// impl<'a> GenWithContext<'a> {
//     pub fn new(type_info: &'a TypeInfoMap) -> Self {
//         let ctx = Context::new();
//         let func_builder_ctx = FunctionBuilderContext::new();
//         Self {
//             type_info,
//             ctx,
//             func_builder_ctx,
//         }
//     }

//     fn gen_lit_expr(lit: &LiteralExpr, func_builder: &mut FunctionBuilder) -> Value {
//         match lit.lit.kind {
//             TokenVariants::Integer(Integer::I64(i)) => func_builder.ins().iconst(types::I64, i),
//             TokenVariants::F32(x) => func_builder.ins().f32const(x),
//             TokenVariants::F64(x) => func_builder.ins().f64const(x),
//             _ => panic!("invalid literal type in codegen pass"),
//         }
//     }

//     fn gen_unary_expr(
//         unary_expr: &UnaryExpr,
//         func_builder: &mut FunctionBuilder,
//         type_info: &'a TypeInfoMap,
//     ) -> Value {
//         let operand = Self::gen_expr(&unary_expr.operand, func_builder, type_info);
//         match unary_expr.op {
//             UnaryOp::Pos => operand,
//             UnaryOp::Negate => {
//                 let ty = type_info.types.get(&unary_expr.id).unwrap();
//                 match ty {
//                     TypeInfo::BuiltIn(BuiltInType::I64) => func_builder.ins().ineg(operand),
//                     TypeInfo::BuiltIn(BuiltInType::F32 | BuiltInType::F64) => {
//                         func_builder.ins().fneg(operand)
//                     }
//                     TypeInfo::BuiltIn(BuiltInType::Error) => {
//                         panic!("error type in codegen pass => this should not happen. Error out before this point")
//                     }
//                 }
//             }
//         }
//     }

//     fn gen_expr(
//         expr: &ExprNode,
//         func_builder: &mut FunctionBuilder,
//         type_info: &'a TypeInfoMap,
//     ) -> Value {
//         match expr {
//             ExprNode::Literal(literal) => Self::gen_lit_expr(literal, func_builder),
//             ExprNode::Binary(binary) => Self::gen_bin_expr(binary, func_builder, type_info),
//             ExprNode::Unary(unary_expr) => {
//                 Self::gen_unary_expr(unary_expr, func_builder, type_info)
//             }
//             ExprNode::Ident(ident_expr) => todo!("TODO: implement codegen for ident expr"),
//         }
//     }

//     fn gen_bin_expr(
//         expr: &BinaryExpr,
//         func_builder: &mut FunctionBuilder,
//         type_info: &'a TypeInfoMap,
//     ) -> Value {
//         match expr.op {
//             BinOp::Add => {
//                 let lhs = Self::gen_expr(&expr.left, func_builder, type_info);
//                 let rhs = Self::gen_expr(&expr.right, func_builder, type_info);
//                 let ty = type_info.types.get(&expr.id).unwrap();
//                 match ty {
//                     TypeInfo::BuiltIn(BuiltInType::I64) => func_builder.ins().iadd(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::F32) => func_builder.ins().fadd(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::F64) => func_builder.ins().fadd(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::Error) => {
//                         panic!("error type in codegen pass => this should not happen. Error out before this point")
//                     }
//                 }
//             }
//             BinOp::Subtract => {
//                 let lhs = Self::gen_expr(&expr.left, func_builder, type_info);
//                 let rhs = Self::gen_expr(&expr.right, func_builder, type_info);
//                 let ty = type_info.types.get(&expr.id).unwrap();
//                 match ty {
//                     TypeInfo::BuiltIn(BuiltInType::I64) => func_builder.ins().isub(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::F32 | BuiltInType::F64) => {
//                         func_builder.ins().fsub(lhs, rhs)
//                     }
//                     TypeInfo::BuiltIn(BuiltInType::Error) => {
//                         panic!("error type in codegen pass => this should not happen. Error out before this point")
//                     }
//                 }
//             }
//             BinOp::Multiply => {
//                 let lhs = Self::gen_expr(&expr.left, func_builder, type_info);
//                 let rhs = Self::gen_expr(&expr.right, func_builder, type_info);
//                 let ty = type_info.types.get(&expr.id).unwrap();
//                 match ty {
//                     TypeInfo::BuiltIn(BuiltInType::I64) => func_builder.ins().imul(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::F32 | BuiltInType::F64) => {
//                         func_builder.ins().fmul(lhs, rhs)
//                     }
//                     TypeInfo::BuiltIn(BuiltInType::Error) => {
//                         panic!("error type in codegen pass => this should not happen. Error out before this point")
//                     }
//                 }
//             }
//             BinOp::Divide => {
//                 let lhs = Self::gen_expr(&expr.left, func_builder, type_info);
//                 let rhs = Self::gen_expr(&expr.right, func_builder, type_info);
//                 let ty = type_info.types.get(&expr.id).unwrap();
//                 match ty {
//                     TypeInfo::BuiltIn(BuiltInType::I64) => func_builder.ins().sdiv(lhs, rhs),
//                     TypeInfo::BuiltIn(BuiltInType::F32 | BuiltInType::F64) => {
//                         func_builder.ins().fdiv(lhs, rhs)
//                     }
//                     TypeInfo::BuiltIn(BuiltInType::Error) => {
//                         panic!("error type in codegen pass => this should not happen. Error out before this point")
//                     }
//                 }
//             }
//         }
//     }

//     // fn gen_func_def(&mut self, node: &Node) {
//     //     let mut func_builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.func_builder_ctx);
//     //     let entry_block = func_builder.create_block();
//     //     func_builder.append_block_params_for_function_params(entry_block);
//     //     func_builder.switch_to_block(entry_block);
//     //     func_builder.seal_block(entry_block);
//     //     let vars = func_builder.block_params(entry_block)
//     //     let type_info = self.type_info;
//     //     if let Node::Expr(expr) = node {
//     //         let val = Self::gen_expr(expr, &mut func_builder, type_info);
//     //         func_builder.ins().return_(&[val]);
//     //     } else {
//     //         panic!("codegen pass expects a node of type ExprNode (currently)")
//     //     }
//     // }
// }

// fn compile_ast_node(node: &Node, module: &Box<dyn Module>) {
//     let mut ctxt = codegen::Context::new();
//     ctxt
// }

// impl Pass for GenCraneliftIRPass {
//     fn run(&mut self, ast: &yuu_shared::ast::Node, context: &mut yuu_shared::Context) -> bool {
//         let type_info = context
//             .get_pass_data::<TypeInfoMap>()
//             .expect("expect type info pass to run before codegen pass");

//         let mut internal = GenWithContext::new(type_info);

//         // Define a dummy main function
//         let mut builder =
//             JITBuilder::new(default_libcall_names()).expect("failed to create JITBuilder object");
//         let mut module = JITModule::new(builder);
//         false
//     }
// }
