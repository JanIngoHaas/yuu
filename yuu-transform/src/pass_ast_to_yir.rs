use std::vec;

use indexmap::IndexMap;
use ustr::Ustr;
use yuu_shared::{
    ast::{
        AST, BinOp, BindingNode, BlockExpr, ExprNode, IfExpr, InternUstr, NodeId, StmtNode,
        StructuralNode, UnaryOp,
    },
    block::BindingTable,
    scheduler::Pass,
    type_info::{TypeInfo, TypeInfoTable},
    type_registry::{self, TypeRegistry},
    yir::{self, BinOp as YirBinOp, Function, Module, Operand, UnaryOp as YirUnaryOp, Variable},
};

pub struct TransientData<'a> {
    pub function: Function,
    tr: &'a TypeRegistry,
    variable_bindings: IndexMap<NodeId, Variable>,
    block_labels: IndexMap<NodeId, (yir::Label, Variable)>, // Maps block ID to its label and omega variable
}

enum StmtRes {
    Proceed,
    Break(Operand),
    BreakWithLabel,
}

impl<'a> TransientData<'a> {
    fn new(function: Function, tr: &'a TypeRegistry) -> Self {
        Self {
            function,
            tr,
            variable_bindings: IndexMap::new(),
            block_labels: IndexMap::new(),
        }
    }

    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.tr
            .type_info_table
            .types
            .get(&node_id)
            .unwrap_or_else(|| panic!("No type info found for node {}", node_id))
    }

    fn allocate_and_initialize_ident_binding(&mut self, ident: Ustr, id: NodeId, value: Operand) {
        let target = self.function.make_assign(ident, value);
        self.variable_bindings.insert(id, target);
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
                    BinOp::NotEq => YirBinOp::NotEq,
                    BinOp::Lt => YirBinOp::LessThan,
                    BinOp::LtEq => YirBinOp::LessThanEq,
                    BinOp::Gt => YirBinOp::GreaterThan,
                    BinOp::GtEq => YirBinOp::GreaterThanEq,
                };

                let result = self
                    .function
                    .make_binary("bin_result".intern(), op, lhs, rhs, ty);
                Operand::Variable(result)
            }
            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.operand);
                let ty = self.get_type(un_expr.id);

                match un_expr.op {
                    UnaryOp::Negate => {
                        let target = self.function.make_unary(
                            "neg_result".intern(),
                            ty,
                            YirUnaryOp::Neg,
                            operand,
                        );
                        Operand::Variable(target)
                    }
                    UnaryOp::Pos => operand,
                }
            }
            ExprNode::If(if_expr) => self.lower_if_expr(if_expr),
            ExprNode::Ident(ident_expr) => {
                debug_assert!(self.tr.bindings.contains_key(&ident_expr.id));
                debug_assert!(
                    self.variable_bindings
                        .contains_key(&self.tr.bindings[&ident_expr.id])
                );
                if let Some(reg) = self
                    .tr
                    .bindings
                    .get(&ident_expr.id)
                    .and_then(|x| self.variable_bindings.get(x))
                {
                    Operand::Variable(*reg)
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

                result.map(Operand::Variable).unwrap_or(Operand::NoOp)
            }
            ExprNode::Assignment(assignment_expr) => {
                let lhs = self.lower_expr(&assignment_expr.lhs);
                let rhs = self.lower_expr(&assignment_expr.rhs);

                // TODO: How do I lower this?A
            }
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                let sinfo = self
                    .tr
                    .resolve_struct(struct_instantiation_expr.struct_name)
                    .expect("Compiler bug: struct not found in lowering to YIR");

                // Allocate
                let struct_reg = self.function.make_alloca(sinfo.name, sinfo.ty);

                // Generate code for the struct's fields expressions:
                for (field, field_expr) in &struct_instantiation_expr.fields {
                    let field_name = field.name;
                    let field_op = self.lower_expr(field_expr);
                    let field = sinfo
                        .fields
                        .get(&field_name)
                        .expect("Compiler bug: field not found in lowering to YIR");
                    // Use the GetFieldPtr instruction to get the pointer to the field
                    let field_reg = self.function.make_get_field_ptr(
                        field_name,
                        Operand::Variable(struct_reg),
                        field,
                    );

                    // Store the value into the field
                    self.function.make_store(field_reg, field_op);
                }

                // Return the pointer to the struct
                Operand::Variable(struct_reg)
            }
            ExprNode::While(while_expr) => {
                // Lower condition:
                let cond = self.lower_expr(&while_expr.condition_block.condition);

                // Make a branch for the loop body and the merge point
                let (loop_body_label, merge_label) =
                    self.function.make_branch(cond, vec![], vec![]);

                // Create omega variable for the loop body
                let omega = self
                    .function
                    .make_omega("while_result".intern(), self.get_type(while_expr.id));

                // Lower the loop body, set merge as its next block and loop body as current block
                self.function.set_current_block(&loop_body_label);
                let loop_body_operand = self.lower_block_expr(&while_expr.condition_block.body);
                self.function
                    .make_jump_if_no_terminator(merge_label, vec![(omega, loop_body_operand)]);

                self.function.set_current_block(&merge_label);

                Operand::Variable(omega)
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr) -> Operand {
        // For labeled blocks, create omega variable
        let omega_with_label = if let Some(label_name) = block_expr.label {
            let omega = self
                .function
                .make_omega("block_return".intern(), self.get_type(block_expr.id));

            // Make a labeld_block_merge bb that we jump to...
            let block = self.function.add_block(
                /*format!("{}_merge", label_name.as_str()).intern()*/ label_name,
            );
            self.block_labels.insert(block_expr.id, (block, omega)); // That's where we jump to from deep within the callstack (or from the block itself)

            Some((omega, block))
        } else {
            None
        };

        // Process statements in current block
        for stmt in &block_expr.body {
            let res = self.lower_stmt(stmt);

            match res {
                // Direct break, referencing the parent block
                StmtRes::Break(value) => {
                    match omega_with_label {
                        // Labeled block: jump to next with value in omega
                        Some((omega, label)) => {
                            self.function.make_jump_if_no_terminator(
                                label.clone(),
                                vec![(omega.clone(), value)],
                            );
                            self.function.set_current_block(&label);
                            return Operand::Variable(omega);
                        }
                        // Unlabeled block: Just return the value, no jumping around needed
                        _ => return value,
                    }
                }
                StmtRes::Proceed => (),
                StmtRes::BreakWithLabel => {
                    // This is a break with label, we don't need to do anything here
                    // The break was already handled in the lower_stmt function
                    // But, we don't have to proceed with the rest of the block - it's essentially dead code
                    break;
                }
            }
        }

        // no break stmt - jump to the next block, not writing anything
        if let Some((omega, label)) = omega_with_label {
            self.function
                .make_jump_if_no_terminator(label.clone(), vec![]);
            self.function.set_current_block(&label);
            Operand::Variable(omega)
        } else {
            Operand::NoOp
        }
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) -> StmtRes {
        match stmt {
            StmtNode::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.expr);
                self.allocate_and_initialize_binding(&let_stmt.binding, value);
                StmtRes::Proceed
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr);
                StmtRes::Proceed
            }
            StmtNode::Break(exit_stmt) => {
                let value = self.lower_expr(&exit_stmt.expr);
                if exit_stmt.target.is_none() {
                    return StmtRes::Break(value);
                }
                debug_assert!(self.tr.bindings.contains_key(&exit_stmt.id));
                debug_assert!(
                    self.block_labels
                        .contains_key(&self.tr.bindings[&exit_stmt.id])
                );
                if let Some((target_label, omega_var)) = self
                    .tr
                    .bindings
                    .get(&exit_stmt.id)
                    .and_then(|x| self.block_labels.get(x))
                {
                    // Write the break value to the block's omega variable
                    self.function
                        .make_jump(*target_label, vec![(*omega_var, value)]);
                }
                StmtRes::BreakWithLabel
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

        // Create omega variable and declare it in current block
        let omega = self
            .function
            .make_omega("if_result".intern(), self.get_type(if_expr.id));

        // Lower the then block, setting merge as its next block
        self.function.set_current_block(&then_label);
        let then_value = self.lower_block_expr(&if_expr.if_block.body);
        self.function
            .make_jump_if_no_terminator(merge_label, vec![(omega, then_value)]);

        // Lower the else block, also setting merge as its next block
        self.function.set_current_block(&else_label);
        let else_value = if let Some(else_block) = &if_expr.else_block {
            self.lower_block_expr(else_block)
        } else {
            Operand::NoOp
        };

        self.function
            .make_jump_if_no_terminator(merge_label, vec![(omega, else_value)]);

        // Set merge block as current (no next block needed since it's the end)
        self.function.set_current_block(&merge_label);
        Operand::Variable(omega)
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

                    let mut data =
                        TransientData::new(Function::new(func.decl.name.clone(), return_type), tr);

                    // Add parameters
                    for arg in &func.decl.args {
                        let (ty, name) = (data.get_type(arg.id), arg.name.clone());
                        let param = data.function.add_param(name, ty);
                        // variable the parameter
                        data.variable_bindings.insert(arg.id, param);
                    }

                    // Process the function body block
                    let final_out = data.lower_block_expr(&func.body);
                    data.function.make_return(Some(final_out));
                    module.define_function(data.function);
                }

                // Define structs
                StructuralNode::StructDef(sdefs) => {
                    module.define_struct(sdefs.decl.name);
                }
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
