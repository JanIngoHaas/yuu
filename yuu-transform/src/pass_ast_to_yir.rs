use std::{borrow::Borrow, ops::Deref, rc::Rc};
use yuu_parse::parser::SourceCodeInfo;
use yuu_shared::{
    ast::{
        BinOp, BindingNode, BlockExpr, ExprNode, Node, NodeId, StmtNode, StructuralNode, UnaryOp,
        AST,
    },
    graphviz_output::ToGraphviz,
    type_info::{TypeInfo, TypeInfoTable},
    yir::{
        Function, InstId, Instructions, Module, Operand, RegisterCreateInfo, RegisterRef,
        YirTypeInfo,
    },
    Pass,
};

pub struct TransientData<'a> {
    pub function: Function,
    type_table: &'a TypeInfoTable,
}

impl<'a> TransientData<'a> {
    fn get_type(&self, node_id: NodeId) -> &Rc<TypeInfo> {
        self.type_table.types.get(&node_id).unwrap()
    }

    fn get_lowered_type(&self, node_id: NodeId) -> Rc<YirTypeInfo> {
        lower_type(self.get_type(node_id))
    }

    pub fn make_register_create_info(
        &mut self,
        node_id: NodeId,
        name: String,
        ty_as_ptr: bool,
    ) -> RegisterCreateInfo {
        let ty = self.get_lowered_type(node_id);
        let ty = if ty_as_ptr { ty.ptr_to() } else { ty };
        self.function
            .instructions
            .make_register_create_info(node_id, name, ty)
    }
}

pub fn lower_type(ty: &Rc<TypeInfo>) -> Rc<YirTypeInfo> {
    match ty.borrow() {
        TypeInfo::BuiltIn(prim) => Rc::new(YirTypeInfo::YuuPrimitiveType(*prim)),
        TypeInfo::Function(func) => {
            let args = func.args.iter().map(|arg| lower_type(arg)).collect();
            let ret = lower_type(&func.ret);
            Rc::new(YirTypeInfo::FunctionType(args, ret))
        }
        _ => unreachable!("Unsupported type"),
    }
}

pub struct AstToYirPass;

fn declare_function(module: &mut Module, name: &str, type_id: NodeId, tit: &TypeInfoTable) {
    let yuu_type = tit
        .types
        .get(&type_id)
        .expect("ICE: Expecting type to be present at this point");
    let yir_type = lower_type(yuu_type);
    module.declare_function(name.to_string(), yir_type);
}

impl AstToYirPass {
    pub fn new() -> Self {
        Self
    }

    fn lower_ast<'a>(&self, ast: &AST, tit: &'a TypeInfoTable) -> Module {
        let mut module = Module::new();

        // Go through all structural elements, and pre-declare them in the module:
        for node in &ast.structurals {
            match node.as_ref() {
                StructuralNode::FuncDecl(func_decl_structural) => {
                    declare_function(
                        &mut module,
                        &func_decl_structural.name,
                        func_decl_structural.id,
                        tit,
                    );
                }
                StructuralNode::FuncDef(func_def_structural) => {
                    declare_function(
                        &mut module,
                        &func_def_structural.decl.name,
                        func_def_structural.id,
                        tit,
                    );
                }
            }
        }

        for func in ast.structurals.iter().filter_map(|x| {
            // Filter out everything else than function definitions, i.e. we want the bodies!
            if let StructuralNode::FuncDef(def) = x.as_ref() {
                Some(def)
            } else {
                None
            }
        }) {
            let lowered_function = module.get_declared_function(&func.decl.name).unwrap();
            let mut data = TransientData {
                type_table: tit,
                function: lowered_function,
            };

            let (args, _ret) = match data.function.function_type.as_ref() {
                YirTypeInfo::FunctionType(args, ret) => (args.clone(), ret.clone()),
                _ => unreachable!("ICE: Expecting Function type"),
            };

            // Handle function argument bindings
            for (idx, (yir_arg, yuu_arg)) in args.iter().zip(func.decl.args.iter()).enumerate() {
                self.lower_binding(
                    &yuu_arg.binding,
                    &mut data,
                    Operand::RegisterOp(
                        RegisterCreateInfo::new(
                            format!("func_arg.{}", idx),
                            yir_arg.clone(),
                            yuu_arg.id,
                        )
                        .assume_exists(),
                    ),
                    yir_arg,
                )
            }

            self.lower_block_expr(&func.body, &mut data);

            module.define_function(data.function);
        }
        module
    }

    fn lower_block_expr(&self, block_expr: &BlockExpr, data: &mut TransientData) -> Operand {
        for stmt in &block_expr.body {
            let result = self.lower_stmt(stmt, data);
            // If result is Some, we have a block return statement
            if result.is_some() {
                return result.unwrap();
            }
        }
        // If we reach this point, the block has no return statement
        // We don't need to do anything, but we need to return something
        Operand::NoOp
    }

    fn lower_expr(&self, expr: &ExprNode, data: &mut TransientData) -> Operand {
        match expr {
            ExprNode::Literal(lit) => match &lit.lit {
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::Integer(yuu_shared::token::Integer::I64(n)),
                    ..
                } => Operand::ImmediateI64Op(*n),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::F32(f),
                    ..
                } => Operand::ImmediateF32Op(*f),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::F64(f),
                    ..
                } => Operand::ImmediateF64Op(*f),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::NilKw,
                    ..
                } => Operand::NoOp,
                _ => todo!("Other literals not implemented yet"),
            },

            ExprNode::Binary(bin_expr) => {
                let lhs = self.lower_expr(&bin_expr.left, data);
                let rhs: Operand = self.lower_expr(&bin_expr.right, data);
                let result = data.make_register_create_info(
                    bin_expr.id,
                    "bin_expr_result".to_string(),
                    false,
                );

                let register_ref = match bin_expr.op {
                    BinOp::Add => data
                        .function
                        .instructions
                        .add_bin_add(result.clone(), lhs, rhs),
                    BinOp::Subtract => {
                        data.function
                            .instructions
                            .add_bin_sub(result.clone(), lhs, rhs)
                    }
                    BinOp::Multiply => {
                        data.function
                            .instructions
                            .add_bin_mul(result.clone(), lhs, rhs)
                    }
                    BinOp::Divide => {
                        data.function
                            .instructions
                            .add_bin_div(result.clone(), lhs, rhs)
                    }
                    BinOp::Eq => data
                        .function
                        .instructions
                        .add_bin_eq(result.clone(), lhs, rhs),
                    _ => todo!("Other binary operators not implemented yet"),
                };

                Operand::RegisterOp(register_ref)
            }

            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.operand, data);
                let result =
                    data.make_register_create_info(un_expr.id, "un_expr_result".to_string(), false);

                match un_expr.op {
                    UnaryOp::Negate => {
                        let register_ref = data
                            .function
                            .instructions
                            .add_un_neg(result.clone(), operand);
                        Operand::RegisterOp(register_ref)
                    }
                    UnaryOp::Pos => operand,
                }
            }

            ExprNode::If(if_expr) => {
                let cond = self.lower_expr(&if_expr.if_block.condition, data);

                let then_label = data.function.instructions.fresh_label();
                let else_label = data.function.instructions.fresh_label();
                let merge_label = data.function.instructions.fresh_label();

                // Create temporary storage for the result
                let result =
                    data.make_register_create_info(if_expr.id, "if_expr_result".to_string(), true);
                let storage = data.function.instructions.add_alloca(result.clone());

                data.function
                    .instructions
                    .add_branch(cond, then_label, else_label);

                // Then block
                data.function
                    .instructions
                    .add_label("if-then".to_string(), then_label);
                let op = self.lower_expr(&if_expr.if_block.body, data);
                data.function.instructions.add_store(storage.clone(), op);
                data.function.instructions.add_goto(merge_label);

                // First else block or else-if chain start
                data.function
                    .instructions
                    .add_label("if-else".to_string(), else_label);

                let mut next_if_label = else_label;

                // Handle else-if blocks
                for (i, else_if) in if_expr.else_if_blocks.iter().enumerate() {
                    let cond = self.lower_expr(&else_if.condition, data);
                    let next_else_label = data.function.instructions.fresh_label();

                    data.function
                        .instructions
                        .add_branch(cond, next_if_label, next_else_label);
                    data.function
                        .instructions
                        .add_label(format!("if-elseif-{}-then", i), next_if_label);

                    let op = self.lower_expr(&else_if.body, data);
                    data.function.instructions.add_store(storage.clone(), op);
                    data.function.instructions.add_goto(merge_label);

                    next_if_label = next_else_label;
                }

                // Final else block if it exists
                if let Some(else_block) = &if_expr.else_block {
                    data.function
                        .instructions
                        .add_label("if-else-merge".to_string(), next_if_label);
                    let op = self.lower_expr(&else_block, data);
                    data.function.instructions.add_store(storage.clone(), op);
                    data.function.instructions.add_goto(merge_label);
                }

                // Merge block
                data.function
                    .instructions
                    .add_label("if_merge".to_string(), merge_label);

                // Load the result from storage
                let load_target =
                    data.make_register_create_info(if_expr.id, "if_expr_result".to_string(), false);
                let load_id = data
                    .function
                    .instructions
                    .add_load(load_target, Operand::RegisterOp(storage));

                Operand::RegisterOp(load_id)
            }

            ExprNode::Ident(ident_expr) => {
                println!("Ident: {:?}", ident_expr.ident);
                let addr_reg = data
                    .function
                    .variables
                    .get(&ident_expr.ident)
                    .expect("Variable not found in symbol table")
                    .clone();
                // Need to load from the address
                let load_target =
                    data.make_register_create_info(ident_expr.id, ident_expr.ident.clone(), false);
                let load_target = data
                    .function
                    .instructions
                    .add_load(load_target, Operand::RegisterOp(addr_reg));
                Operand::RegisterOp(load_target)
            }
            ExprNode::Block(block_expr) => self.lower_block_expr(block_expr, data),
            ExprNode::FuncCall(func_call_expr) => {
                // Get function name from the LHS expression
                let func_name = match &*func_call_expr.lhs {
                    ExprNode::Ident(ident) => ident.ident.clone(),
                    _ => panic!("Function call LHS must be an identifier"),
                };

                // Lower all arguments
                let args = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.lower_expr(arg, data))
                    .collect();

                // Create target register if the function has a return value
                let target = if data.get_type(func_call_expr.id).is_nil() {
                    None
                } else {
                    Some(data.make_register_create_info(
                        func_call_expr.id,
                        "func_call_result".to_string(),
                        false,
                    ))
                };

                // Add the call instruction
                let call_id = data
                    .function
                    .instructions
                    .add_call(target.clone(), func_name, args);

                if let Some(rref) = call_id {
                    Operand::RegisterOp(rref)
                } else {
                    Operand::NoOp
                }
            }
        }
    }

    fn lower_binding(
        &self,
        binding: &BindingNode,
        data: &mut TransientData,
        src: Operand,
        ty: &Rc<YirTypeInfo>,
    ) {
        // Now, add the variable to the symbol table

        match binding {
            yuu_shared::ast::BindingNode::Ident(ident_binding) => {
                let var = data.function.instructions.make_register_create_info(
                    ident_binding.id,
                    ident_binding.name.clone(),
                    ty.clone().ptr_to(),
                );
                // alloc_a_reg_id holds the address of the variable
                let alloc_a_reg_id = data.function.instructions.add_alloca(var);
                let name = ident_binding.name.clone();
                data.function.variables.insert(name, alloc_a_reg_id.clone());
                data.function.instructions.add_store(alloc_a_reg_id, src);
            }
        }
    }

    fn lower_stmt(&self, stmt: &StmtNode, data: &mut TransientData) -> Option<Operand> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.expr, data);
                let ty = data.get_lowered_type(let_stmt.expr.node_id());
                self.lower_binding(&let_stmt.binding, data, value, &ty);
                None
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr, data);
                None
            }
            StmtNode::Return(ret) => {
                let value = self.lower_expr(&ret.expr, data);
                match ret.kind {
                    yuu_shared::ast::ReturnStmtKind::ReturnFromBlock => Some(value),
                    yuu_shared::ast::ReturnStmtKind::ReturnFromFunction => {
                        todo!("Return from function not implemented yet - DO NOT TRY TO IMPL THIS NOW UNTIL IMPLEMENTED EVERYWHERE")
                    }
                }
            }
        }
    }
}

impl Pass for AstToYirPass {
    fn run(&mut self, context: &mut yuu_shared::Context) -> bool {
        let src_info = context.require_pass_data::<SourceCodeInfo>("AstToYir");
        let type_info_table = context.require_pass_data::<TypeInfoTable>("AstToYir");

        let sci = src_info.as_ref().borrow();
        let tit = type_info_table.as_ref().borrow();

        let pass = Self::new();

        let module = pass.lower_ast(&sci.root_node, &tit);
        context.add_pass_data(module);

        true
    }

    fn install(self, pipeline: &mut yuu_shared::Pipeline)
    where
        Self: Sized,
    {
        pipeline.add_pass(self);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pass_type_inference::PassTypeInference;
    use yuu_parse::{lexer::UnprocessedCodeInfo, parser::Parser};
    use yuu_shared::{Context, Pipeline};

    #[test]
    fn test_fac_to_yir() {
        let code_info = UnprocessedCodeInfo {
            code: r#"fn fac(n: i64) -> i64 {
                out if n == 0 {
                    out 1; 
                }            
                else {
                    let n_out = n * fac(n - 1);
                    out n_out; 
                };
            }"#,
            file_name: "test.yuu",
        };

        let mut parser = Parser::new(&code_info);
        let mut ctxt = parser.parse_and_create_context().expect("Parser error");

        let mut pipeline = Pipeline::new();

        // First run collect decls
        let pass_collect_decls = crate::pass_collect_decls::PassCollectDecls::new();
        pass_collect_decls.install(&mut pipeline);

        // Then run type inference
        let pass_type_inference = PassTypeInference::new();
        pass_type_inference.install(&mut pipeline);

        // Then run AST to YIR conversion
        let pass_ast_to_yir = AstToYirPass::new();
        pass_ast_to_yir.install(&mut pipeline);

        pipeline.run(&mut ctxt);

        // Get the module and print it

        let module = ctxt.require_pass_data::<Module>("Test");
        let module = module.as_ref().borrow();
        println!("{}", module);
    }
}
