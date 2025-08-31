use crate::{
    pass_parse::{
        BlockStmt, GetId,
        ast::{
            AST, BinOp, BindingNode, ExprNode, InternUstr, NodeId, StmtNode, StructuralNode,
            UnaryOp,
        },
        token::{Integer, Token, TokenKind},
    },
    pass_type_inference::{TypeInfo, TypeRegistry},
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
    // Loop context stack to track current loop merge blocks for break statements
    loop_context: Vec<yir::Label>,
}

enum StmtRes {
    Proceed,
    Break,
}

impl<'a> TransientData<'a> {
    fn new(function: Function, tr: &'a TypeRegistry) -> Self {
        Self {
            function,
            tr,
            var_map: IndexMap::new(),
            loop_context: Vec::new(), // Initialize the loop context stack
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
                let data_operand = ei.data.as_ref().map(|data_expr| self.lower_expr(data_expr));

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
        }
    }

    fn lower_block_body(&mut self, block_stmt: &BlockStmt) {
        // Process statements in current block
        for stmt in &block_stmt.body {
            let res = self.lower_stmt(stmt);

            match res {
                // Break - everything below is unreachable, we omit further lowering
                StmtRes::Break => {
                    return;
                }
                StmtRes::Proceed => (),
            }
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
                let _ = self.lower_expr(expr);
                StmtRes::Proceed
            }
            StmtNode::Break(_exit_stmt) => {
                // Jump to the current loop's merge block
                if let Some(loop_merge) = self.loop_context.last() {
                    self.function.make_jump(*loop_merge);
                } else {
                    // TODO: This should be caught by a separate semantic analysis pass
                    panic!(
                        "Break statement outside of loop context - this should be caught by semantic analysis"
                    );
                }
                StmtRes::Break
            }
            StmtNode::Error(_) => unreachable!(
                "Syntax Error reached during lowering - pipeline was wrongly configured or compiler bug"
            ),
            StmtNode::Return(return_stmt) => {
                let ret_value = return_stmt.expr.as_ref().map(|e| self.lower_expr(e));
                self.function.make_return(ret_value);
                StmtRes::Break
            }
            StmtNode::Match(match_stmt) => {
                let match_header = self.function.add_block("match_header".intern());
                let match_merge_block = self.function.add_block("match_merge".intern());

                self.function.make_jump_if_no_terminator(match_header);
                self.function.set_current_block(&match_header);

                let lowered_match_expr = self.lower_expr(&match_stmt.scrutinee);

                let ty = self.get_type(match_stmt.scrutinee.node_id());

                match ty {
                    TypeInfo::Enum(_) => {
                        let mut jump_targets = IndexMap::new();
                        for (arm_idx, arm) in match_stmt.arms.iter().enumerate() {
                            let match_arm_block = self.function.add_block("match_arm".intern());
                            self.function.set_current_block(&match_arm_block);
                            self.lower_block_body(&arm.body);
                            self.function.make_jump_if_no_terminator(match_merge_block);
                            jump_targets.insert(arm_idx as i64, match_arm_block);
                        }

                        self.function.set_current_block(&match_header);

                        // Create default case if present in AST
                        let default_block = if let Some(default_case) = &match_stmt.default_case {
                            let default_block = self.function.add_block("match_default".intern());
                            self.function.set_current_block(&default_block);
                            self.lower_block_body(default_case);
                            self.function.make_jump_if_no_terminator(match_merge_block);
                            Some(default_block)
                        } else {
                            None
                        };

                        self.function.set_current_block(&match_header);
                        self.function.make_jump_table(
                            lowered_match_expr,
                            jump_targets,
                            default_block,
                        );
                    }
                    _ => unreachable!("Match expression must be (currently) of enum type"),
                }

                self.function.set_current_block(&match_merge_block);
                StmtRes::Proceed
            }
            StmtNode::If(if_stmt) => {
                // For every branch, we have a header and body -> header: condition, body: statements; we finally have a merge block where we join the control flow

                let if_header = self.function.add_block("if_header".intern());
                let if_body = self.function.add_block("if_body".intern());
                let merge_block = self.function.add_block("if_merge".intern());

                self.function.make_jump_if_no_terminator(if_header);
                self.function.set_current_block(&if_header);

                let lowered_if_condition = self.lower_expr(&if_stmt.if_block.condition);

                self.function.set_current_block(&if_body);
                self.lower_block_body(&if_stmt.if_block.body);
                self.function.make_jump_if_no_terminator(merge_block);
                self.function.set_current_block(&if_header);

                let mut prev_body = if_body;
                let mut prev_condition = lowered_if_condition;

                for else_if_block in &if_stmt.else_if_blocks {
                    let else_if_header = self.function.add_block("else_if_header".intern());
                    let else_if_body = self.function.add_block("else_if_body".intern());

                    // Conditional jump
                    self.function.make_branch_to_existing(
                        prev_condition,
                        prev_body,
                        else_if_header,
                    );

                    self.function.set_current_block(&else_if_header);
                    let lowered_else_if_cond = self.lower_expr(&else_if_block.condition);

                    self.function.set_current_block(&else_if_body);
                    self.lower_block_body(&else_if_block.body);
                    self.function.make_jump_if_no_terminator(merge_block);
                    self.function.set_current_block(&else_if_header);

                    prev_body = else_if_body;
                    prev_condition = lowered_else_if_cond;
                }

                // Handle else block if it exists
                if let Some(else_block) = &if_stmt.else_block {
                    let else_body = self.function.add_block("else_body".intern());

                    self.function
                        .make_branch_to_existing(prev_condition, prev_body, else_body);

                    self.function.set_current_block(&else_body);
                    self.lower_block_body(else_block);
                    self.function.make_jump_if_no_terminator(merge_block);
                }

                self.function.set_current_block(&merge_block);
                StmtRes::Proceed
            }
            StmtNode::While(while_stmt) => {
                let while_header = self.function.add_block("while_header".intern());
                let while_body = self.function.add_block("while_body".intern());
                let while_merge = self.function.add_block("while_merge".intern());

                self.function.make_jump_if_no_terminator(while_header);
                self.function.set_current_block(&while_header);

                let lowered_while_condition =
                    self.lower_expr(&while_stmt.condition_block.condition);
                self.function.make_branch_to_existing(
                    lowered_while_condition,
                    while_body,
                    while_merge,
                );

                self.function.set_current_block(&while_body);

                // Push loop merge block for break statements
                self.loop_context.push(while_merge);
                self.lower_block_body(&while_stmt.condition_block.body);
                // Pop loop context when exiting loop body
                self.loop_context.pop();

                self.function.make_jump_if_no_terminator(while_header);

                self.function.set_current_block(&while_merge);
                StmtRes::Proceed
            }
            StmtNode::Block(block_stmt) => {
                let block_body = self.function.add_block("block_body".intern());
                self.function.make_jump_if_no_terminator(block_body);
                self.lower_block_body(block_stmt);
                StmtRes::Proceed
            }
        }
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

                    data.lower_block_body(&func.body);

                    // TODO: Implement a helper make_return_if_no_terminator
                    if data
                        .function
                        .get_current_block()
                        .map(|x| &x.terminator)
                        .is_none()
                    {
                        data.function.make_return(None);
                    }

                    data.function.sort_blocks_by_id();
                    module.define_function(data.function);
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
