use std::intrinsics::offset;

use crate::{
    pass_parse::{
        EnumPattern, GetId, RefutablePatternNode,
        ast::{
            AST, BinOp, BindingNode, BlockExpr, ExprNode, IfExpr, InternUstr, NodeId, StmtNode,
            StructuralNode, UnaryOp,
        },
        token::{Integer, Token, TokenKind},
    },
    pass_type_inference::{FieldsMap, PrimitiveType, TypeInfo, TypeRegistry},
    pass_yir_lowering::yir::{
        self, BinOp as YirBinOp, Function, Module, Operand, UnaryOp as YirUnaryOp, Variable,
    },
};
use indexmap::IndexMap;

pub struct TransientData<'a> {
    pub function: Function,
    tr: &'a TypeRegistry,
    // Renamed from variable_bindings
    var_map: IndexMap<NodeId, Variable>, // Maps AST Binding NodeId -> YIR Variable
    // New map for labeled blocks: Maps AST BlockExpr NodeId -> (Merge Label, Result Variable)
    labeled_block_info: IndexMap<NodeId, (yir::Label, Variable)>,
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
            var_map: IndexMap::new(),
            labeled_block_info: IndexMap::new(), // Initialize the new map
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
                // Find the NodeId of the variable declaration this identifier refers to
                let binding_id = *self.tr.bindings.get(&ident_expr.id).unwrap_or_else(|| {
                    panic!(
                        "Compiler Bug: No binding found for ident expr {}",
                        ident_expr.id
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
            ExprNode::Block(block_expr) => self.lower_block_expr(block_expr),
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
                let body_label = self.function.add_block("while_body".intern());
                let exit_label = self.function.add_block("while_exit".intern());
                // Jump into the condition check
                self.function.make_jump(cond_label); // Condition block
                self.function.set_current_block(&cond_label);
                let cond = self.lower_expr(&while_expr.condition_block.condition);

                // Branch to body or exit
                self.function
                    .make_branch_to_existing(cond, body_label, exit_label);
                // Body block
                self.function.set_current_block(&body_label);
                let body_val = self.lower_block_expr(&while_expr.condition_block.body);
                // After body (if not broken out), loop back to condition
                self.function.make_jump_if_no_terminator(cond_label);
                // Exit block
                self.function.set_current_block(&exit_label);
                // Loop yields the last body value (or NoOp)
                body_val
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
            ExprNode::EnumInstantiation(_) => {
                // TODO: Implement enum instantiation in YIR lowering
                todo!("Enum instantiation YIR lowering not yet implemented")
            }
            ExprNode::Match(m) => {
                // Lower the scrutinee expression
                let scrutinee = self.lower_expr(&m.scrutinee);

                // Create a target variable for the match result
                let target_var =
                    self.function
                        .make_alloca("match_result".intern(), self.get_type(m.id), None);

                let merge_label = self.function.add_block("match_merge".intern());

                let mut offset_labels = IndexMap::new();

                for arm in m.arms.iter() {
                    match &*arm.pattern {
                        RefutablePatternNode::Enum(enum_pattern) => {
                            // Create a label for this case
                            let case_label = self.function.add_block("match_case".intern());

                            // We are in the case block now
                            self.function.set_current_block(&case_label);

                            match enum_pattern {
                                EnumPattern::Unit(enum_unit_pattern) => {
                                    let enum_info = self
                                        .tr
                                        .resolve_enum(enum_unit_pattern.enum_name)
                                        .expect("Compiler bug: enum not found in lowering to YIR");

                                    let current_variant = enum_info.variants.get(&enum_unit_pattern.variant_name).expect("Compiler bug: enum variant not found in lowering to YIR");
                                    let variant_idx = current_variant.variant_idx;
                                    offset_labels.insert(variant_idx, case_label);
                                }
                                EnumPattern::WithData(enum_data_pattern) => {
                                    // TODO: Claude: lower binding here -> refactor corresponding functions
                                    // TODO: Claude: Here, we have some redundancy, i.e. enum_data_pattern and enum_unit_pattern could have a common struct and then just an Option<> for the inner refutable pattern... This would make things easier for us. Try to refactor this here and everywhere else.
                                    let binding = "hi".intern();
                                    let enum_info = self
                                        .tr
                                        .resolve_enum(enum_data_pattern.enum_name)
                                        .expect("Compiler bug: enum not found in lowering to YIR");

                                    let current_variant = enum_info.variants.get(&enum_data_pattern.variant_name).expect("Compiler bug: enum variant not found in lowering to YIR");
                                    let variant_idx = current_variant.variant_idx;
                                    offset_labels.insert(variant_idx, case_label);
                                }
                            }
                            let arm_body_lowered = self.lower_expr(&arm.body);

                            // Store the result of the arm body into the target variable
                            self.function.make_store(target_var, arm_body_lowered);

                            // Jump to the merge block after executing the arm
                            self.function.make_jump_if_no_terminator(merge_label);
                        }
                    }
                }

                // Now we have all the case labels, we can create the jump table
                self.function
                    .make_jump_table(scrutinee, offset_labels, None); // TODO: For now: No default case, need to do this later though!

                Operand::Variable(target_var)
            }
        }
    }

    fn lower_block_expr(&mut self, block_expr: &BlockExpr) -> Operand {
        // For labeled blocks, create result variable
        let result_with_label = if let Some(label_name) = block_expr.label {
            let result = self.function.make_alloca(
                "block_return".intern(),
                self.get_type(block_expr.id),
                None,
            );

            // Make a labeld_block_merge bb that we jump to...
            let block = self.function.add_block(
                /*format!("{}_merge", label_name.as_str()).intern()*/ label_name,
            );
            self.labeled_block_info
                .insert(block_expr.id, (block, result)); // That's where we jump to from deep within the callstack (or from the block itself)

            Some((result, block))
        } else {
            None
        };

        // Process statements in current block
        for stmt in &block_expr.body {
            let res = self.lower_stmt(stmt);

            match res {
                // Direct break, referencing the parent block
                StmtRes::Break(value) => {
                    match result_with_label {
                        // Labeled block: jump to next with value in result
                        Some((result, label)) => {
                            // Assign the value to the result variable
                            self.function.make_store(result, value);
                            self.function.make_jump_if_no_terminator(label);
                            self.function.set_current_block(&label);
                            return Operand::Variable(result);
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
        if let Some((result, label)) = result_with_label {
            self.function.make_jump_if_no_terminator(label);
            self.function.set_current_block(&label);
            Operand::Variable(result)
        } else {
            Operand::NoOp
        }
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
                    self.labeled_block_info
                        .contains_key(&self.tr.bindings[&exit_stmt.id])
                );
                if let Some((target_label, result_var)) = self
                    .tr
                    .bindings
                    .get(&exit_stmt.id)
                    .and_then(|x| self.labeled_block_info.get(x))
                {
                    self.function.make_store(*result_var, value);
                    self.function.make_jump(*target_label);
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

        // Get the result type for the if expression
        let result_type = self.get_type(if_expr.id);

        // Declare a variable to hold the result of the if-expression, if it has a result type

        let if_result_var = if !matches!(
            result_type,
            TypeInfo::Inactive | TypeInfo::BuiltInPrimitive(PrimitiveType::Nil)
        ) {
            Some(
                self.function
                    .make_alloca("if_result".intern(), result_type, None),
            )
        } else {
            None // If the result type is inactive, we don't need a variable
        };

        // Create all blocks first, merge will be the next block for both branches
        let (then_label, else_label) = self.function.make_branch(cond);
        let merge_label = self.function.add_block("merge".intern());

        // Lower the then block, setting merge as its next block
        self.function.set_current_block(&then_label);
        let then_value = self.lower_block_expr(&if_expr.if_block.body);
        if let Some(if_result_var) = if_result_var {
            self.function.make_store(if_result_var, then_value);
        } // Assign then_value to if_result_var
        self.function.make_jump_if_no_terminator(merge_label);

        // Lower the else block, also setting merge as its next block
        self.function.set_current_block(&else_label);
        let else_value = if let Some(else_block) = &if_expr.else_block {
            self.lower_block_expr(else_block)
        } else {
            Operand::NoOp // If there's no else block, the value is NoOp
        };

        if let Some(if_result_var) = if_result_var {
            self.function.make_store(if_result_var, else_value); // Assign else_value to if_result_var
        }

        self.function.make_jump_if_no_terminator(merge_label);

        // Set merge block as current (no next block needed since it's the end)
        self.function.set_current_block(&merge_label);
        if_result_var.map_or(Operand::NoOp, Operand::Variable)
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

                    let final_out = data.lower_block_expr(&func.body);
                    data.function.make_return(Some(final_out));
                    data.function.sort_blocks_by_id();
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

impl YirLowering {
    pub fn run(&self, ast: &AST, type_registry: &TypeRegistry) -> miette::Result<Module> {
        let module = self.lower_ast(ast, type_registry);
        Ok(module)
    }
}
