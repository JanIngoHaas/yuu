use yuu_shared::{
    ast::{
        BinOp, BindingNode, BlockExpr, ExprNode, NodeId, StmtNode, StructuralNode, UnaryOp, AST,
    },
    scheduler::Pass,
    type_info::{TypeInfo, TypeInfoTable},
    yir::{self, BinOp as YirBinOp, Function, Module, Operand, Register, UnaryOp as YirUnaryOp},
};

pub struct TransientData<'a> {
    pub function: Function,
    type_table: &'a TypeInfoTable,
    bindings: std::collections::HashMap<String, Register>,
}

impl TransientData<'_> {
    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.type_table.types.get(&node_id).unwrap()
    }

    fn lower_binding(&mut self, binding: &BindingNode, value: Operand) {
        match binding {
            BindingNode::Ident(ident_binding) => {
                let ty = self.get_type(ident_binding.id);
                let target = self.function.make_alloca(ident_binding.name.clone(), ty);
                self.function
                    .make_store(Operand::Register(target.clone()), value);
                self.bindings.insert(ident_binding.name.clone(), target);
            }
        }
    }

    fn lower_expr(&mut self, expr: &ExprNode, block_level: u64) -> Operand {
        match expr {
            ExprNode::Literal(lit) => match &lit.lit {
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::Integer(yuu_shared::token::Integer::I64(n)),
                    ..
                } => Operand::I64Const(*n),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::F32(f),
                    ..
                } => Operand::F32Const(*f),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::F64(f),
                    ..
                } => Operand::F64Const(*f),
                yuu_shared::token::Token {
                    kind: yuu_shared::token::TokenKind::NilKw,
                    ..
                } => Operand::NoOp,
                _ => todo!("Other literals not implemented yet"),
            },

            ExprNode::Binary(bin_expr) => {
                let lhs = self.lower_expr(&bin_expr.left, block_level);
                let rhs = self.lower_expr(&bin_expr.right, block_level);
                let ty = self.get_type(bin_expr.id);

                let op = match bin_expr.op {
                    BinOp::Add => YirBinOp::Add,
                    BinOp::Subtract => YirBinOp::Sub,
                    BinOp::Multiply => YirBinOp::Mul,
                    BinOp::Divide => YirBinOp::Div,
                    BinOp::Eq => YirBinOp::Eq,
                };

                let result = self
                    .function
                    .make_binary("bin_result".to_string(), op, lhs, rhs, ty);
                Operand::Register(result)
            }

            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.operand, block_level);
                let ty = self.get_type(un_expr.id);

                match un_expr.op {
                    UnaryOp::Negate => {
                        let target = self.function.fresh_register("neg_result".to_string(), ty);
                        self.function
                            .make_unary(target.clone(), YirUnaryOp::Neg, operand);
                        Operand::Register(target)
                    }
                    UnaryOp::Pos => operand,
                }
            }

            ExprNode::If(if_expr) => {
                let cond = self.lower_expr(&if_expr.if_block.condition, block_level);
                let (then_label, else_label) = self.function.branch(cond);
                let merge_label = self.function.add_block("merge".to_string());

                // Create Omega node for the result
                let ty = self.get_type(if_expr.id);
                let result = self.function.fresh_register("if_result".to_string(), ty);
                self.function
                    .make_omega(result.clone(), vec![then_label.clone(), else_label.clone()]);

                // Then block
                self.function.set_current_block(&then_label);
                let then_value = self.lower_expr(&if_expr.if_block.body, block_level + 1);
                self.function.make_omikron(result.clone(), then_value);
                self.function
                    .set_terminator(yir::ControlFlow::Jump(merge_label.clone()));

                // TODO: Else-If blocks

                // Else block
                self.function.set_current_block(&else_label);
                let else_value = if let Some(else_block) = &if_expr.else_block {
                    self.lower_expr(&else_block, block_level + 1)
                } else {
                    Operand::NoOp
                };
                self.function.make_omikron(result.clone(), else_value);
                self.function
                    .set_terminator(yir::ControlFlow::Jump(merge_label.clone()));

                // Continue in merge block
                self.function.set_current_block(&merge_label);

                Operand::Register(result)
            }

            ExprNode::Ident(ident_expr) => {
                if let Some(reg) = self.bindings.get(&ident_expr.ident) {
                    let ty = self.get_type(ident_expr.id);
                    let result = self.function.make_load(
                        "load_result".to_string(),
                        Operand::Register(reg.clone()),
                        ty,
                    );
                    Operand::Register(result)
                } else {
                    panic!("Identifier not found in bindings: {}", ident_expr.ident);
                }
            }

            ExprNode::Block(block_expr) => self.lower_block_expr(block_expr, block_level + 1),

            ExprNode::FuncCall(func_call_expr) => {
                let func_name = match &*func_call_expr.lhs {
                    ExprNode::Ident(ident) => ident.ident.clone(),
                    _ => panic!("Function call LHS must be an identifier"),
                };

                let args: Vec<_> = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.lower_expr(arg, block_level))
                    .collect();

                let return_type = self.get_type(func_call_expr.id);

                let result = self.function.make_call(
                    "call_result".to_string(),
                    func_name,
                    args,
                    return_type,
                );

                result.map(Operand::Register).unwrap_or(Operand::NoOp)
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr, block_level: u64) -> Operand {
        for stmt in &block_expr.body {
            if let Some(result) = self.lower_stmt(stmt, block_level) {
                return result;
            }
        }
        Operand::NoOp
    }

    fn lower_stmt(&mut self, stmt: &StmtNode, block_level: u64) -> Option<Operand> {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.expr, block_level);
                self.lower_binding(&let_stmt.binding, value);
                None
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr, block_level);
                None
            }
            StmtNode::Return(ret) => {
                let value = self.lower_expr(&ret.expr, block_level);
                match ret.kind {
                    yuu_shared::ast::ReturnStmtKind::ReturnFromBlock => {
                        if block_level > 0 {
                            Some(value)
                        } else {
                            self.function
                                .set_terminator(yir::ControlFlow::Return(Some(value)));
                            None
                        }
                    }
                    yuu_shared::ast::ReturnStmtKind::ReturnFromFunction => {
                        self.function
                            .set_terminator(yir::ControlFlow::Return(Some(value)));
                        None
                    }
                }
            }
        }
    }
}

pub struct PassAstToYir;

impl Default for PassAstToYir {
    fn default() -> Self {
        Self::new()
    }
}

impl PassAstToYir {
    pub fn new() -> Self {
        Self
    }

    fn lower_ast(&self, ast: &AST, tit: &TypeInfoTable) -> Module {
        let mut module = Module::new();

        // Declare functions
        for node in &ast.structurals {
            match node.as_ref() {
                StructuralNode::FuncDecl(func_decl) => {
                    let ty = tit.types[&func_decl.id];
                    let func = Function::new(func_decl.name.clone(), ty);
                    module.declare_function(func);
                }
                StructuralNode::FuncDef(func_def) => {
                    let ty = tit.types[&func_def.id];
                    let func = Function::new(func_def.decl.name.clone(), ty);
                    module.declare_function(func);
                }
            }
        }

        // Define functions
        for func in ast.structurals.iter().filter_map(|x| match x.as_ref() {
            StructuralNode::FuncDef(def) => Some(def),
            _ => None,
        }) {
            let return_type = match tit.types[&func.id] {
                TypeInfo::Function(ft) => ft.ret,
                _ => panic!("Expected function type"),
            };

            let mut data = TransientData {
                function: Function::new(func.decl.name.clone(), return_type),
                type_table: tit,
                bindings: std::collections::HashMap::new(),
            };

            // Add parameters
            for arg in &func.decl.args {
                let (ty, name) = match &arg.binding {
                    BindingNode::Ident(id) => (data.get_type(id.id), id.name.clone()),
                };
                let param = data.function.fresh_register(name.clone(), ty);
                data.function.params.push(param.clone());
                // Use lower_binding to handle parameter storage consistently
                data.lower_binding(&arg.binding, Operand::Register(param));
            }

            data.lower_block_expr(&func.body, 0);
            module.define_function(data.function);
        }

        module
    }
}

impl Pass for PassAstToYir {
    fn run(&self, context: &mut yuu_shared::context::Context) -> anyhow::Result<()> {
        let ast = context.require_pass_data::<AST>(self);
        let ast = ast.lock().unwrap();
        let ast = &*ast;
        let type_info_table = context.require_pass_data::<TypeInfoTable>(self);
        let type_info_table = type_info_table.lock().unwrap();
        let type_info_table = &*type_info_table;
        let module = self.lower_ast(ast, type_info_table);
        context.add_pass_data(module);
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_read::<TypeInfoTable>(&self);
        schedule.produces_resource::<Module>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "AstToYir"
    }
}
