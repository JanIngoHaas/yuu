use std::vec;

use indexmap::IndexMap;
use ustr::Ustr;
use yuu_shared::{
    ast::{
        BinOp, BindingNode, BlockExpr, ExprNode, IfExpr, InternUstr, NodeId, StmtNode, StructuralNode, UnaryOp, AST
    },
    block::BindingTable,
    scheduler::Pass,
    type_info::{TypeInfo, TypeInfoTable},
    type_registry::{self, TypeRegistry},
    yir::{self, BinOp as YirBinOp, Function, Module, Operand, Register, UnaryOp as YirUnaryOp},
};

pub struct TransientData<'a> {
    pub function: Function,
    tr: &'a TypeRegistry,
    register_bindings: IndexMap<NodeId, Register>,
    block_labels: IndexMap<NodeId, (yir::Label, Register)>, // Maps block ID to its label and omega register
}

impl<'a> TransientData<'a> {
    fn new(
        function: Function,
        tr: &'a TypeRegistry,
    ) -> Self {
        Self {
            function,
            tr,
            register_bindings: IndexMap::new(),
            block_labels: IndexMap::new(),
        }
    }

    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.tr.type_info_table
            .types
            .get(&node_id)
            .unwrap_or_else(|| panic!("No type info found for node {}", node_id))
    }

    fn allocate_and_initialize_ident_binding(&mut self, ident: Ustr, id: NodeId, value: Operand) {
        let ty = self.get_type(id);
        let target = self.function.make_alloca(ident, ty);
        self.function
            .make_store(Operand::Register(target.clone()), value);
        self.register_bindings.insert(id, target);
    }

    fn allocate_and_initialize_binding(&mut self, binding: &BindingNode, value: Operand) {
        match binding {
            BindingNode::Ident(ident_binding) => {
                self.allocate_and_initialize_ident_binding(
                    ident_binding.name.clone(),
                    ident_binding.id,
                    value,
                );
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
                    .make_binary("bin_result".intern(), op, lhs, rhs, ty);
                Operand::Register(result)
            }
            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.operand);
                let ty = self.get_type(un_expr.id);

                match un_expr.op {
                    UnaryOp::Negate => {
                        let target = self.function.fresh_register("neg_result".intern(), ty);
                        self.function
                            .make_unary(target.clone(), YirUnaryOp::Neg, operand);
                        Operand::Register(target)
                    }
                    UnaryOp::Pos => operand,
                }
            }
            ExprNode::If(if_expr) => self.lower_if_expr(if_expr),
            ExprNode::Ident(ident_expr) => {
                debug_assert!(self.tr.bindings.contains_key(&ident_expr.id));
                debug_assert!(
                    self.register_bindings
                        .contains_key(&self.tr.bindings[&ident_expr.id])
                );
                if let Some(reg) = self
                    .tr.bindings
                    .get(&ident_expr.id)
                    .and_then(|x| self.register_bindings.get(x))
                {
                    let ty = self.get_type(ident_expr.id);
                    let result = self.function.make_load(
                        "load_result".intern(),
                        Operand::Register(reg.clone()),
                        ty,
                    );
                    Operand::Register(result)
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

                let result =
                    self.function
                        .make_call("call_result".intern(), func_name, args, return_type);

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
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                let sinfo = self.tr.resolve_struct(struct_instantiation_expr.struct_name).expect("Compiler bug: struct not found in lowering to YIR");
                // Alloc
                let struct_reg = self.function.make_alloca(
                    struct_instantiation_expr.struct_name.clone(),
                    &sinfo.ty, 
                );

                // Generate code for the struct's fields expressions:
                for (field, field_expr) in &struct_instantiation_expr.fields {
                    let field_name = field.name;
                    let field_op = self.lower_expr(field_expr);
                    let field = sinfo.fields.get(&field_name).expect("Compiler bug: field not found in lowering to YIR");
                    let field_reg = self.function.fresh_register(field_name, field.ty);
                    // Use the GetFieldPtr instruction to get the pointer to the field
                    self.function.make_get_field_ptr(
                        field_reg,
                        Operand::Register(struct_reg),
                        field_name
                    );

                    // Store the value into the field
                    self.function.make_store(Operand::Register(field_reg), field_op);
                }

                // Return the pointer to the struct
                Operand::Register(struct_reg)
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr) -> Operand {
        // For labeled blocks, create omega register
        let omega_with_label = if let Some(label_name) = block_expr.label.as_ref() {
            let omega = self
                .function
                .fresh_register("block_return".intern(), self.get_type(block_expr.id));
            self.function.make_omega(omega.clone());

            // Make a labeld_block_merge bb that we jump to...
            let block = self.function.add_block(
                /*format!("{}_merge", label.as_str()).intern()*/ *label_name,
            );
            self.block_labels
                .insert(block_expr.id, (block.clone(), omega.clone())); // That's where we jump to from deep within the callstack

            Some((omega, block))
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

            match omega_with_label {
                // Labeled block: jump to next with value in omega
                Some((omega, label)) => {
                    self.function
                        .make_jump_if_no_terminator(label.clone(), vec![(omega.clone(), value)]);
                    self.function.set_current_block(&label);
                    Operand::Register(omega)
                }
                // Unlabeled block: Just return the value, no jumping around needed
                _ => value,
            }
        // Doesnt have a last_expr, so... just jump to the next block, not writing anything
        } else if let Some((omega, label)) = omega_with_label {
            self.function
                .make_jump_if_no_terminator(label.clone(), vec![]);
            self.function.set_current_block(&label);
            Operand::Register(omega)
        } else {
            Operand::NoOp
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
                let value = self.lower_expr(&exit_stmt.expr);
                // debug_assert!(self.binding_table.contains_key(&exit_stmt.id));
                // debug_assert!(self
                //     .block_labels
                //     .contains_key(&self.binding_table[&exit_stmt.id]));
                if let Some((target_label, omega_reg)) = self
                    .tr.bindings
                    .get(&exit_stmt.id)
                    .and_then(|x| self.block_labels.get(x))
                {
                    // Write the break value to the block's omega register
                    self.function
                        .make_jump(target_label.clone(), vec![(omega_reg.clone(), value)]);
                }
            }
            StmtNode::Error(_) => unreachable!(
                "Syntax Error reached during lowering - pipeline was wrongly configured or compiler bug"
            ),
        }
    }

    fn lower_if_expr(&mut self, if_expr: &IfExpr) -> Operand {
        // Evaluate the condition
        let cond = self.lower_expr(&if_expr.if_block.condition);

        // Create all blocks first, merge will be the next block for both branches
        let (then_label, else_label) = self.function.make_branch(cond, vec![], vec![]);
        let merge_label = self.function.add_block("merge".intern());

        // Create omega register and declare it in current block
        let omega = self
            .function
            .fresh_register("if_result".intern(), self.get_type(if_expr.id));
        self.function.make_omega(omega.clone());

        // Lower the then block, setting merge as its next block
        self.function.set_current_block(&then_label);
        let then_value = self.lower_block_expr(&if_expr.if_block.body);
        self.function
            .make_jump_if_no_terminator(merge_label.clone(), vec![(omega.clone(), then_value)]);

        // Lower the else block, also setting merge as its next block
        self.function.set_current_block(&else_label);
        let else_value = if let Some(else_block) = &if_expr.else_block {
            self.lower_block_expr(else_block)
        } else {
            Operand::NoOp
        };

        self.function
            .make_jump_if_no_terminator(merge_label.clone(), vec![(omega.clone(), else_value)]);

        // Set merge block as current (no next block needed since it's the end)
        self.function.set_current_block(&merge_label);
        Operand::Register(omega)
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

    fn lower_ast(&self, ast: &AST, tr: &TypeRegistry) -> Module {
        let mut module = Module::new();

        // Define functions
        for structural in ast.structurals.iter() {
            match structural.as_ref() {
                StructuralNode::FuncDef(func) => {
                    let return_type = match tr.type_info_table.types[&func.id] {
                        TypeInfo::Function(ft) => ft.ret,
                        _ => panic!("Expected function type"),
                    };

                    let mut data = TransientData::new(
                        Function::new(func.decl.name.clone(), return_type),
                        tr,
                    );

                    // Add parameters
                    for arg in &func.decl.args {
                        let (ty, name) = (data.get_type(arg.id), arg.name.clone());
                        let param = data.function.fresh_register(name.clone(), ty);
                        data.function.params.push(param.clone());
                        // Use allocate_and_initialize_ident_binding to handle parameter storage consistently
                        data.allocate_and_initialize_ident_binding(
                            arg.name,
                            arg.id,
                            Operand::Register(param),
                        );
                    }

                    // Process the function body block
                    let final_out = data.lower_block_expr(&func.body);
                    data.function.make_return(Some(final_out));
                    module.define_function(data.function);
                }

                // Define structs
                StructuralNode::StructDef(sdefs) => {
                    module.define_struct(sdefs.decl.name);
                },
                _ => (),
            }
        }

        module
    }
}

impl Pass for PassAstToYir {
    fn run(&self, context: &mut yuu_shared::context::Context) -> anyhow::Result<()> {
        let ast = context.get_resource::<AST>(self);
        let reg = context.get_resource::<TypeRegistry>(self);
        let reg = reg.lock().unwrap();

        let ast = ast.lock().unwrap();

        let module = self.lower_ast(&ast, &reg);

        context.add_pass_data(module);
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<AST>(&self);
        schedule.requires_resource_read::<TypeRegistry>(&self);
        schedule.produces_resource::<Module>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "AstToYir"
    }
}
