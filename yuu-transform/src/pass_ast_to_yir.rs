use std::vec;

use yuu_parse::add_ids::GetId;
use yuu_shared::{
    ast::{
        BinOp, BindingNode, BlockExpr, ExprNode, IfExpr, NodeId, StmtNode, StructuralNode, UnaryOp,
        AST,
    },
    block::{self, Block, BlockIterator, RootBlock, FUNC_BLOCK_NAME},
    scheduler::Pass,
    type_info::{TypeInfo, TypeInfoTable},
    yir::{
        self, BinOp as YirBinOp, ControlFlow, Function, Module, Operand, Register,
        UnaryOp as YirUnaryOp,
    },
};

pub struct TransientData<'a> {
    pub function: Function,
    type_table: &'a TypeInfoTable,
    block: &'a Block,
    block_iter: BlockIterator<'a>, // Add iterator
    register_bindings: hashbrown::HashMap<NodeId, Register>,
    block_labels: hashbrown::HashMap<NodeId, (yir::Label, Register)>, // Maps block ID to its label and omega register
}

impl<'a> TransientData<'a> {
    fn new(function: Function, type_table: &'a TypeInfoTable, block: &'a Block) -> Self {
        Self {
            function,
            type_table,
            block,
            block_iter: BlockIterator::new(block), // Initialize iterator
            register_bindings: hashbrown::HashMap::new(),
            block_labels: hashbrown::HashMap::new(),
        }
    }

    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.type_table
            .types
            .get(&node_id)
            .unwrap_or_else(|| panic!("No type info found for node {}", node_id))
    }

    fn allocate_and_initialize_binding(&mut self, binding: &BindingNode, value: Operand) {
        match binding {
            BindingNode::Ident(ident_binding) => {
                let ty = self.get_type(ident_binding.id);
                let target = self.function.make_alloca(ident_binding.name.clone(), ty);
                self.function
                    .make_store(Operand::Register(target.clone()), value);
                self.register_bindings.insert(ident_binding.id, target);
            }
        }
    }

    fn lower_expr(&mut self, expr: &ExprNode) -> Operand {
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
                let lhs = self.lower_expr(&bin_expr.left);
                let rhs = self.lower_expr(&bin_expr.right);
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
                let operand = self.lower_expr(&un_expr.operand);
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

            ExprNode::If(if_expr) => self.lower_if_expr(if_expr),

            ExprNode::Ident(ident_expr) => {
                if let Some(binding) = self.block.get_unique_binding_forced(&ident_expr.ident) {
                    if let Some(reg) = self.register_bindings.get(&binding.id) {
                        let ty = self.get_type(ident_expr.id);
                        let result = self.function.make_load(
                            "load_result".to_string(),
                            Operand::Register(reg.clone()),
                            ty,
                        );
                        Operand::Register(result)
                    } else {
                        Operand::NoOp
                    }
                } else {
                    Operand::NoOp
                }
            }

            ExprNode::Block(block_expr) => self.lower_block_expr(block_expr),

            ExprNode::FuncCall(func_call_expr) => {
                let func_name = match &*func_call_expr.lhs {
                    ExprNode::Ident(ident) => ident.ident.clone(),
                    _ => panic!("Function call LHS must be an identifier"),
                };

                let args: Vec<_> = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.lower_expr(arg))
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
            ExprNode::Assignment(assignment_expr) => {
                let binding = &assignment_expr.binding;
                let value = self.lower_expr(&assignment_expr.rhs);

                match binding.as_ref() {
                    BindingNode::Ident(ident_binding) => {
                        let target = self.register_bindings.get(&ident_binding.id).unwrap();
                        self.function
                            .make_store(Operand::Register(target.clone()), value);
                        Operand::Register(target.clone())
                    }
                }
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr) -> Operand {
        self.block_iter.descend();

        // For labeled blocks, create omega register
        let omega = if let Some(label) = &block_expr.label {
            let binding = self
                .block_iter
                .peek()
                .get_unique_binding_forced(label)
                .expect("Compiler bug: Block binding not found");

            let omega = self
                .function
                .fresh_register("block_return".to_string(), self.get_type(block_expr.id));
            self.function.make_omega(omega.clone());

            // Store for break targets
            self.block_labels.insert(
                binding.id,
                (
                    self.function.get_next_block_label_forced().clone(),
                    omega.clone(),
                ),
            );

            Some(omega)
        } else {
            None
        };

        // Process statements in current block
        for stmt in &block_expr.body {
            self.lower_stmt(stmt);
        }

        // Handle block completion
        if let Some(last_expr) = &block_expr.last_expr {
            let value = self.lower_expr(last_expr);

            match omega {
                // Labeled block: jump to next with value in omega
                Some(omega) => {
                    self.function
                        .make_jump_to_next_block_label(vec![(omega.clone(), value)]);
                    Operand::Register(omega)
                }
                // Unlabeled block:
                _ => {
                    // Jump to next block, pass operand back to caller
                    self.function.make_jump_to_next_block_label(vec![]);
                    value
                }
            }
        } else {
            if let Some(omega) = omega {
                self.function.make_jump_to_next_block_label(vec![]);
                Operand::Register(omega)
            } else {
                Operand::NoOp
            }
        }
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.expr);
                self.allocate_and_initialize_binding(&let_stmt.binding, value);
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr);
            }
            StmtNode::Break(exit_stmt) => {
                let value = self.lower_expr(&exit_stmt.value);

                if exit_stmt.target == FUNC_BLOCK_NAME {
                    self.function.make_return(Some(value));
                } else if let Some(target_binding) = self.block.get_name(&exit_stmt.target) {
                    // Look up the target block's label and omega register
                    if let Some((target_label, omega_reg)) =
                        self.block_labels.get(&target_binding.id)
                    {
                        // Write the break value to the block's omega register
                        self.function
                            .make_jump(target_label.clone(), vec![(omega_reg.clone(), value)]);
                    }
                }
            }
        }
    }

    fn lower_if_expr(&mut self, if_expr: &IfExpr) -> Operand {
        // 1. Evaluate condition
        let cond = self.lower_expr(&if_expr.if_block.condition);

        // 2. Create then/else blocks
        let (then_label, else_label) = self.function.make_branch(cond, vec![], vec![]);
        let merge_label = self.function.add_block("merge".to_string());

        // 3. Create result register if needed
        let omega_reg = if !self.get_type(if_expr.id).is_nil() {
            let omega = self
                .function
                .fresh_register("if_result".to_string(), self.get_type(if_expr.id));
            self.function.make_omega(omega.clone());
            Some(omega)
        } else {
            None
        };

        // 5. Lower then block
        self.function
            .set_current_block(&then_label, merge_label.clone());
        let then_value = self.lower_block_expr(&if_expr.if_block.body);
        if let Some(omega) = &omega_reg {
            self.function
                .make_jump(merge_label.clone(), vec![(omega.clone(), then_value)]);
        } else {
            self.function.make_jump(merge_label.clone(), vec![]);
        }

        // 6. Lower else block
        self.function
            .set_current_block(&else_label, merge_label.clone());
        if let Some(else_block) = &if_expr.else_block {
            let value = self.lower_expr(else_block);
            if let Some(omega) = &omega_reg {
                self.function
                    .make_jump(merge_label.clone(), vec![(omega.clone(), value)]);
            } else {
                self.function.make_jump(merge_label.clone(), vec![]);
            }
        } else {
            self.function.make_jump(merge_label.clone(), vec![]);
        };

        // 7. Continue in merge block
        self.function.set_next_as_current();
        omega_reg.map(Operand::Register).unwrap_or(Operand::NoOp)
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

    fn lower_ast(&self, ast: &AST, tit: &TypeInfoTable, root_block: &RootBlock) -> Module {
        let mut module = Module::new();

        // Define functions
        for func in ast.structurals.iter().filter_map(|x| match x.as_ref() {
            StructuralNode::FuncDef(def) => Some(def),
            _ => None,
        }) {
            let return_type = match tit.types[&func.id] {
                TypeInfo::Function(ft) => ft.ret,
                _ => panic!("Expected function type"),
            };

            let mut data = TransientData::new(
                Function::new(func.decl.name.clone(), return_type),
                tit,
                root_block.root(), // Use root block from type inference
            );

            // Add parameters
            for arg in &func.decl.args {
                let (ty, name) = match &arg.binding {
                    BindingNode::Ident(id) => (data.get_type(id.id), id.name.clone()),
                };
                let param = data.function.fresh_register(name.clone(), ty);
                data.function.params.push(param.clone());
                // Use allocate_and_initialize_binding to handle parameter storage consistently
                data.allocate_and_initialize_binding(&arg.binding, Operand::Register(param));
            }

            // Process the function body block
            data.lower_block_expr(&func.body);
            module.define_function(data.function);
        }

        module
    }
}

impl Pass for PassAstToYir {
    fn run(&self, context: &mut yuu_shared::context::Context) -> anyhow::Result<()> {
        let ast = context.require_pass_data::<AST>(self);
        let type_info_table = context.require_pass_data::<TypeInfoTable>(self);
        let root_block = context.require_pass_data::<Box<RootBlock>>(self);

        let ast = ast.lock().unwrap();
        let type_info_table = type_info_table.lock().unwrap();
        let root_block = root_block.lock().unwrap();

        let module = self.lower_ast(&ast, &type_info_table, &root_block);

        context.add_pass_data(module);
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_read::<TypeInfoTable>(&self);
        schedule.requires_resource_read::<Box<RootBlock>>(&self);
        schedule.produces_resource::<Module>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "AstToYir"
    }
}
