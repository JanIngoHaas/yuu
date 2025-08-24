use crate::{
    pass_parse::{
        GetId, RefutablePatternNode,
        ast::{
            AST, BinOp, BindingNode, BlockExpr, ExprNode, IfExpr, InternUstr, NodeId, StmtNode,
            StructuralNode, UnaryOp,
        },
        token::{Integer, Token, TokenKind},
    },
    pass_type_inference::{PrimitiveType, TypeInfo, TypeRegistry},
    pass_yir_lowering::{
        Label,
        yir::{
            self, BinOp as YirBinOp, Function, Module, Operand, UnaryOp as YirUnaryOp, Variable,
        },
    },
};
use indexmap::IndexMap;

pub struct TransientData<'a> {
    pub function: Function,
    tr: &'a TypeRegistry,
    // Renamed from variable_bindings
    var_map: IndexMap<NodeId, Variable>, // Maps AST Binding NodeId -> YIR Variable
    // New map for labeled blocks: Maps AST BlockExpr NodeId -> (Merge Label, Result Variable)
    merge_block_info: IndexMap<NodeId, (yir::Label, Option<Variable>)>,
}

enum StmtRes {
    Proceed,
    Break(Operand),
    BreakWithLabel(Label, Operand),
}

impl<'a> TransientData<'a> {
    fn new(function: Function, tr: &'a TypeRegistry) -> Self {
        Self {
            function,
            tr,
            var_map: IndexMap::new(),
            merge_block_info: IndexMap::new(), // Initialize the new map
        }
    }

    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.tr
            .type_info_table
            .types
            .get(&node_id)
            .unwrap_or_else(|| panic!("No type info found for node {}", node_id))
    }

    fn lower_expr(&mut self, expr: &ExprNode) -> Operand {
        match expr {
            ExprNode::Literal(lit) => match &lit.lit {
                Token {
                    kind: TokenKind::Integer(Integer::I64(n)),
                    ..
                } => Operand::I64Const(*n),
                Token {
                    kind: TokenKind::F32(f),
                    ..
                } => Operand::F32Const(*f),
                Token {
                    kind: TokenKind::F64(f),
                    ..
                } => Operand::F64Const(*f),
                Token {
                    kind: TokenKind::NilKw,
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
                    BinOp::Modulo => YirBinOp::Mod,
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
            ExprNode::Ident(ident_expr) => {
                // Find the NodeId of the variable declaration this identifier refers to
                let binding_id = *self.tr.bindings.get(&ident_expr.id).unwrap_or_else(|| {
                    panic!(
                        "Compiler Bug: No binding found for ident expr '{}': {} -> Span: {:#?}; bindings: {:#?}",
                        ident_expr.ident, ident_expr.id, ident_expr.span, self.tr.bindings
                    )
                });

                // Find the YIR variable associated with that declaration
                let yir_var = *self.var_map.get(&binding_id).unwrap_or_else(|| {
                    panic!(
                        "Compiler Bug: No YIR variable found for binding ID {} (ident: {})",
                        binding_id, ident_expr.ident
                    )
                });

                Operand::Variable(yir_var)
            }
            ExprNode::Block(block_expr) => {
                let (entry, exit, val) = self.lower_block_expr(block_expr);

                // Connect entry with exit
                let tmp = *self.function.get_current_block_label();
                self.function.set_current_block(&entry);
                self.function.make_jump_if_no_terminator(exit);
                self.function.set_current_block(&tmp);

                val
            }
            ExprNode::FuncCall(func_call_expr) => {
                let func_name = match &*func_call_expr.lhs {
                    ExprNode::Ident(ident) => ident.ident,
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
                let rhs = self.lower_expr(&assignment_expr.rhs);
                let lhs = self.lower_expr(&assignment_expr.lhs);

                // Store rhs into lhs

                let lhs_var = match lhs {
                    Operand::Variable(var) => var,
                    _ => unreachable!("Invalid assignment target - can only assign to variables"),
                };

                self.function.make_store(lhs_var, rhs);
                lhs

                // // Compute the r-value of the assignment
                // match assignment_expr.lhs.as_ref() {
                //     ExprNode::Ident(ident_expr) => {
                //         // 1. Find the YIR Variable for this identifier
                //         let binding_id =
                //             *self.tr.bindings.get(&ident_expr.id).unwrap_or_else(|| {
                //                 panic!(
                //                     "Compiler Bug: No binding found for ident expr {}",
                //                     ident_expr.id
                //                 )
                //             });
                //         let target_var = *self.var_map.get(&binding_id).unwrap_or_else(|| {
                //             panic!(
                //                 "Compiler Bug: No YIR variable found for binding ID {} (ident: {})",
                //                 binding_id, ident_expr.ident
                //             )
                //         });

                //         // 2. Use make_assign for direct variable assignment
                //         self.function.make_assign(target_var, rhs);
                //         Operand::Variable(target_var)
                //     }

                //     _ => unreachable!("Invalid assignment target - can only assign to variables"),
                // }
            }
            ExprNode::StructInstantiation(struct_instantiation_expr) => {
                let sinfo = self
                    .tr
                    .resolve_struct(struct_instantiation_expr.struct_name)
                    .expect("Compiler bug: struct not found in lowering to YIR");

                // Allocate
                let struct_var = self.function.make_alloca(sinfo.name, sinfo.ty, None);

                // Generate code for the struct's fields expressions:
                for (field, field_expr) in &struct_instantiation_expr.fields {
                    let field_name = field.name;
                    let field_op = self.lower_expr(field_expr);

                    let field = sinfo
                        .fields
                        .get(&field_name)
                        .expect("Compiler bug: field not found in lowering to YIR");
                    // Use the GetFieldPtr instruction to get the pointer to the field
                    let field_var = self.function.make_get_field_ptr(
                        field_name,
                        Operand::Variable(struct_var),
                        field,
                    );

                    // Store the value into the field
                    self.function.make_store(field_var, field_op);
                }
                // Return the struct variable pointer (already in pointer context from declare_var)
                Operand::Variable(struct_var)
            }

            ExprNode::While(while_expr) => {
                // Create labels
                let cond_label = self.function.add_block("while_cond".intern());

                // Jump into the condition check
                self.function.make_jump(cond_label); // Condition block
                self.function.set_current_block(&cond_label);
                let cond = self.lower_expr(&while_expr.condition_block.condition);

                let (entry, exit, result) = self.lower_block_expr(&while_expr.condition_block.body);

                // Branch to body or exit
                self.function.make_branch_to_existing(cond, entry, exit);

                // From entry, jump back to condition.
                self.function.set_current_block(&entry);
                self.function.make_jump_if_no_terminator(cond_label);
                self.function.set_current_block(&exit);
                result
            }
            ExprNode::MemberAccess(member_access_expr) => {
                // Lower the left-hand side expression
                let lhs = self.lower_expr(&member_access_expr.lhs);
                // Get the field name
                let field_name = member_access_expr.field.name;

                // Get the type of the left-hand side expression
                let lhs_type = self.get_type(member_access_expr.lhs.node_id());

                let struct_type = match lhs_type {
                    TypeInfo::Struct(sinfo) => sinfo,
                    _ => unreachable!("Member access on non-struct type: {:#?}", lhs_type),
                };

                let field_info = self
                    .tr
                    .resolve_struct(struct_type.name)
                    .expect("Compiler bug: struct not found in lowering to YIR");

                // Use make_get_field_ptr to get the pointer to the field
                let field_ptr = self.function.make_get_field_ptr(
                    "field_ptr".intern(),
                    lhs,
                    &field_info.fields[&field_name],
                );

                Operand::Variable(field_ptr)
            }
            ExprNode::EnumInstantiation(ei) => {
                let enum_info = self
                    .tr
                    .resolve_enum(ei.enum_name)
                    .expect("Compiler bug: enum not found in lowering to YIR");

                let variant_info = enum_info
                    .variants
                    .get(&ei.variant_name)
                    .expect("Compiler bug: enum variant not found in lowering to YIR");

                let variant_index = variant_info.variant_idx;

                // Handle associated data if present
                let data_operand = if let Some(data_expr) = &ei.data {
                    Some(self.lower_expr(data_expr))
                } else {
                    None
                };

                // Create the enum using the new MakeEnum instruction
                let enum_var = self.function.make_enum(
                    "enum_result".intern(),
                    enum_info.ty,
                    ei.variant_name,
                    variant_index,
                    data_operand,
                );

                Operand::Variable(enum_var)
            }
            ExprNode::Match(m) => {
                // Lower the scrutinee expression
                let scrutinee = self.lower_expr(&m.scrutinee);

                // Create a target variable for the match result
                let target_var = self.function.make_alloca_if_valid_type(
                    "match_result".intern(),
                    self.get_type(m.id),
                    None,
                );

                let merge_label = self.function.add_block("match_merge".intern());

                let mut variant_labels = IndexMap::new();
                let mut enum_name = None;

                for arm in m.arms.iter() {
                    match &*arm.pattern {
                        RefutablePatternNode::Enum(enum_pattern) => {
                            // Store enum name for jump table (should be same for all arms)
                            if enum_name.is_none() {
                                enum_name = Some(enum_pattern.enum_name);
                            }

                            // Create a label for this case
                            let case_label = self.function.add_block("match_case".intern());

                            // We are in the case block now
                            self.function.set_current_block(&case_label);

                            // Handle the binding for enum data patterns
                            if let Some(binding) = &enum_pattern.binding {
                                match binding.as_ref() {
                                    BindingNode::Ident(ident_binding) => {
                                        let binding_id = ident_binding.id;
                                        let name_hint = ident_binding.name;
                                        let var_type = self.get_type(binding_id);

                                        // Create a variable to hold the extracted enum data
                                        // This will be populated when the enum variant matches
                                        let yir_var = self.function.make_alloca(
                                            name_hint, var_type,
                                            None, // No initial value - will be set during match
                                        );

                                        // Map the AST binding ID to the new YIR variable
                                        self.var_map.insert(binding_id, yir_var);
                                    }
                                }
                            }

                            // Map variant name to case label
                            variant_labels.insert(enum_pattern.variant_name, case_label);
                            let arm_body_lowered = self.lower_expr(&arm.body);

                            // Store the result of the arm body into the target variable if it exists
                            self.function
                                .make_store_if_exists(target_var, arm_body_lowered);

                            // Jump to the merge block after executing the arm
                            self.function.make_jump_if_no_terminator(merge_label);
                        }
                    }
                }

                // Now we have all the case labels, we can create the semantic jump table
                self.function.make_jump_table_enum(
                    scrutinee,
                    enum_name.expect("No enum patterns found in match"),
                    variant_labels,
                    None, // TODO: For now: No default case, need to do this later though!
                );

                target_var.map(Operand::Variable).unwrap_or(Operand::NoOp)
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr) -> (Label, Label, Operand) {
        // Create the "merge" part

        let curr_block_tmp = *self.function.get_current_block_label();

        let block_result = self.function.make_alloca_if_valid_type(
            "block_result".intern(),
            self.get_type(block_expr.id),
            None,
        );

        let exit_block_label = self.function.add_block("merge_block".intern());

        if let Some(result_var) = block_result {
            self.merge_block_info
                .insert(block_expr.id, (exit_block_label, Some(result_var)));
        } else {
            self.merge_block_info
                .insert(block_expr.id, (exit_block_label, None));
        }

        // Create the "execution" part
        let entry_block_name = block_expr
            .label
            .iter()
            .copied()
            .next()
            .unwrap_or("anon_block".intern());

        let entry_block_label = self.function.add_block(entry_block_name);
        self.function.set_current_block(&entry_block_label);

        // Process statements in current block
        for stmt in &block_expr.body {
            let res = self.lower_stmt(stmt);

            match res {
                // Direct break, referencing the parent block
                StmtRes::Break(value) => {
                    // Assign the value to the result variable if it exists
                    self.function.make_store_if_exists(block_result, value);
                    self.function.make_jump_if_no_terminator(exit_block_label);
                    self.function.set_current_block(&curr_block_tmp);
                    println!(
                        "In Break: ({:?}, {:?})",
                        entry_block_label.name(),
                        exit_block_label.name(),
                    );
                    return (
                        entry_block_label,
                        exit_block_label,
                        block_result.map(Operand::Variable).unwrap_or(Operand::NoOp),
                    );
                }
                StmtRes::Proceed => (),
                StmtRes::BreakWithLabel(jump_to, write_to_value) => {
                    println!("Jumping to: {:?}", jump_to.name());
                    self.function.make_jump(jump_to);
                    self.function
                        .make_store_if_exists(block_result, write_to_value);

                    self.function.set_current_block(&curr_block_tmp);
                    println!(
                        "In BreakWithLabel: ({:?}, {:?}, {:?})",
                        entry_block_label.name(),
                        exit_block_label.name(),
                        block_result.map(Operand::Variable).unwrap_or(Operand::NoOp),
                    );
                    return (
                        entry_block_label,
                        exit_block_label,
                        block_result.map(Operand::Variable).unwrap_or(Operand::NoOp),
                    );
                }
            }
        }

        self.function.set_current_block(&curr_block_tmp); // Restore the previous current block
        println!(
            "At end of block: ({:?}, {:?}, {:?})",
            entry_block_label.name(),
            exit_block_label.name(),
            block_result.map(Operand::Variable).unwrap_or(Operand::NoOp),
        );
        (
            entry_block_label,
            exit_block_label,
            block_result.map(Operand::Variable).unwrap_or(Operand::NoOp),
        )
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) -> StmtRes {
        match stmt {
            StmtNode::Let(let_stmt) => {
                // Lower the initializer expression first
                let init_value_operand = self.lower_expr(&let_stmt.expr);

                // Handle the binding pattern
                match let_stmt.binding.as_ref() {
                    BindingNode::Ident(ident_binding) => {
                        let binding_id = ident_binding.id;
                        let name_hint = ident_binding.name;
                        let var_type = self.get_type(binding_id);
                        let yir_var = self.function.make_alloca(
                            name_hint,
                            var_type,
                            Some(init_value_operand), // Pass the lowered initializer
                        );

                        // Map the AST binding ID to the new YIR variable (already in pointer context)
                        self.var_map.insert(binding_id, yir_var);
                    } // If other BindingNode variants existed, they would be handled here
                      // e.g., BindingNode::StructPattern(...) => { /* Destructuring logic */ }
                }

                StmtRes::Proceed
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr); // Lower expression for side effects
                StmtRes::Proceed
            }
            StmtNode::Break(exit_stmt) => {
                let value = self.lower_expr(&exit_stmt.expr);
                if exit_stmt.target.is_none() {
                    return StmtRes::Break(value);
                }
                debug_assert!(self.tr.bindings.contains_key(&exit_stmt.id));
                debug_assert!(
                    self.merge_block_info
                        .contains_key(&self.tr.bindings[&exit_stmt.id])
                );
                let (target_label, result_var) = self
                    .tr
                    .bindings
                    .get(&exit_stmt.id)
                    .and_then(|x| self.merge_block_info.get(x))
                    .unwrap();

                StmtRes::BreakWithLabel(
                    *target_label,
                    result_var.map(Operand::Variable).unwrap_or(Operand::NoOp),
                )
            }
            StmtNode::Error(_) => unreachable!(
                "Syntax Error reached during lowering - pipeline was wrongly configured or compiler bug"
            ),
        }
    }

    fn lower_if_expr(&mut self, if_expr: &IfExpr) -> Operand {
        // For all branches (exit nodes), we finally jump to the merge block
        let merge_final = self.function.add_block("if_merge".intern());
        let merge_final_var = self.function.make_alloca_if_valid_type(
            "if_merge_result".intern(),
            self.get_type(if_expr.id),
            None,
        );

        let cond_block = self.function.add_block("if_cond".intern());
        self.function.make_jump_if_no_terminator(cond_block);
        self.function.set_current_block(&cond_block);

        let mut prev_cond = self.lower_expr(&if_expr.if_block.condition);

        // ==== Lower 'then' block ====
        let (entry_if, exit_if, value_if) = self.lower_block_expr(&if_expr.if_block.body);

        self.function.set_current_block(&exit_if);
        self.function.make_jump_if_no_terminator(merge_final);

        self.function.set_current_block(&entry_if);
        self.function
            .make_store_if_exists(merge_final_var, value_if);
        self.function.make_jump_if_no_terminator(merge_final);

        let mut prev_body = entry_if;

        // Lower the else-if blocks
        for else_if_block in &if_expr.else_if_blocks {
            let (entry_elif, exit_elif, value_elif) = self.lower_block_expr(&else_if_block.body);

            let cond_block = self.function.add_block("elif_cond".intern());

            // Cond jump to either prev_body or to this cond block
            self.function
                .make_branch_to_existing(prev_cond, prev_body, cond_block);

            // Now, remember to connect the exit elif block to the final merge block first
            self.function.set_current_block(&exit_elif);
            self.function.make_jump_if_no_terminator(merge_final);

            self.function.set_current_block(&entry_elif);
            self.function
                .make_store_if_exists(merge_final_var, value_elif);
            self.function.make_jump_if_no_terminator(merge_final);

            self.function.set_current_block(&cond_block);
            prev_cond = self.lower_expr(&else_if_block.condition);

            prev_body = entry_elif;
        }

        // Lower the else block ( if it exists )

        if let Some(else_block) = &if_expr.else_block {
            let (entry_else, exit_else, value_else) = self.lower_block_expr(else_block);

            // Cond jump to either prev_body or to this else entry block (because there is no condition!)
            self.function
                .make_branch_to_existing(prev_cond, prev_body, entry_else);

            // Now, connect the exit/entry to else block to the final merge block
            self.function.set_current_block(&exit_else);
            self.function.make_jump_if_no_terminator(merge_final);

            self.function.set_current_block(&entry_else);
            self.function
                .make_store_if_exists(merge_final_var, value_else);
            self.function.make_jump_if_no_terminator(merge_final);
        } else {
            // No else block - just jump to merge from the last condition's "false" branch
            self.function
                .make_branch_to_existing(prev_cond, prev_body, merge_final);
        }

        // Finally, we need to ensure the merge block is connected properly
        self.function.set_current_block(&merge_final);
        merge_final_var
            .map(Operand::Variable)
            .unwrap_or(Operand::NoOp)
    }
}

pub struct YirLowering;

impl Default for YirLowering {
    fn default() -> Self {
        Self::new()
    }
}

impl YirLowering {
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
                        TransientData::new(Function::new(func.decl.name, return_type), tr);

                    for arg in &func.decl.args {
                        let (ty, name) = (data.get_type(arg.id), arg.name);
                        let param = data.function.add_param("stack_param_mem".intern(), ty);
                        let param_ptr = data.function.make_take_address(name, param);
                        data.var_map.insert(arg.id, param_ptr);
                    }

                    let (entry, exit, final_out) = data.lower_block_expr(&func.body);

                    data.function.make_jump(entry);

                    data.function.set_current_block(&entry);
                    data.function.make_jump_if_no_terminator(exit);
                    data.function.set_current_block(&exit);
                    data.function.make_return(Some(final_out));

                    data.function.sort_blocks_by_id();
                    module.define_function(data.function);
                    println!("End function: {}\n", func.decl.name);
                }

                // Define structs
                StructuralNode::StructDef(sdefs) => {
                    module.define_struct(sdefs.decl.name);
                }

                // Define enums
                StructuralNode::EnumDef(edefs) => {
                    module.define_enum(edefs.decl.name);
                }
                _ => (),
            }
        }

        module
    }
}

impl YirLowering {
    pub fn run(&self, ast: &AST, type_registry: &TypeRegistry) -> miette::Result<Module> {
        let module = self.lower_ast(ast, type_registry);
        Ok(module)
    }
}
