use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::{
    pass_lexing::{Integer, Token, TokenKind},
    pass_parse::{
        BlockStmt, GetId, IdentExpr, LValueKind, RefutablePatternNode,
        ast::{
            AST, BinOp, BindingNode, ExprNode, InternUstr, NodeId, StmtNode, StructuralNode,
            UnaryOp,
        },
    },
    pass_yir_lowering::yir::{
        self, BinOp as YirBinOp, Function, Module, Operand, UnaryOp as YirUnaryOp, Variable,
    },
    utils::{
        TypeRegistry, calculate_struct_layout, calculate_type_layout,
        collections::{FastHashMap, IndexMap},
        type_info_table::{PrimitiveType, TypeInfo, primitive_u64},
    },
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContextKind {
    // Return the actual value (load from memory if needed)
    AsIs,

    // Return a pointer to where the value is stored (don't load the value)
    // Essential for chained field access like "a.b.c" where intermediate steps
    // need pointers to field locations, not the field values themselves
    StorageLocation,
}

struct Scope {
    vars: Vec<Variable>,
    defers: Vec<ExprNode>,
}

pub struct TransientData<'a> {
    pub function: Function,
    tr: &'a TypeRegistry,
    type_info_table: &'a crate::utils::type_info_table::TypeInfoTable,
    bindings: &'a crate::utils::BindingTable,
    var_map: FastHashMap<NodeId, Variable>, // Maps AST Binding NodeId -> YIR Variable
    loop_context: Vec<yir::Label>,
    scope_stack: Vec<Scope>,
    current_temporaries: Vec<Variable>, // Temporaries created in the current statement
    emit_kill_sets: bool,               // Flag to control KillSet emission
}

enum StmtRes {
    Proceed,
    Break,
}

impl<'a> TransientData<'a> {
    fn new(
        function: Function,
        tr: &'a TypeRegistry,
        type_info_table: &'a crate::utils::type_info_table::TypeInfoTable,
        bindings: &'a crate::utils::BindingTable,
        emit_kill_sets: bool,
    ) -> Self {
        Self {
            function,
            tr,
            type_info_table,
            bindings,
            var_map: FastHashMap::default(),
            loop_context: Vec::new(),
            scope_stack: Vec::new(),
            current_temporaries: Vec::new(),
            emit_kill_sets,
        }
    }

    fn get_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.type_info_table
            .get(node_id)
            .unwrap_or_else(|| panic!("No type info found for node {}", node_id))
    }

    /// Lower built-in primitive arithmetic operations (i64 + i64, f32 * f32, etc.)
    fn lower_builtin_binary_op(
        &mut self,
        bin_expr: &crate::pass_parse::ast::BinaryExpr,
        lhs_operand: Operand,
        rhs_operand: Operand,
        result_ty: &'static TypeInfo,
        context: ContextKind,
    ) -> Operand {
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

        let result_var = self.function.make_binary(
            "bin_result".intern(),
            op,
            lhs_operand,
            rhs_operand,
            result_ty,
        );
        self.add_temporary(result_var);
        let result_operand = Operand::Variable(result_var);

        match context {
            ContextKind::AsIs => result_operand,
            ContextKind::StorageLocation => {
                let storage_var = self.function.make_alloca_single(
                    "bin_result_storage".intern(),
                    result_ty,
                    None,
                );
                self.add_temporary(storage_var);
                self.function.make_store(storage_var, result_operand);
                Operand::Variable(storage_var)
            }
        }
    }

    /// Lower pointer arithmetic operations (ptr + int, ptr - int, ptr - ptr)
    /// Returns Some(operand) if this is a valid pointer arithmetic operation, None otherwise
    fn try_lower_pointer_arithmetic(
        &mut self,
        bin_expr: &crate::pass_parse::ast::BinaryExpr,
        lhs_operand: Operand,
        rhs_operand: Operand,
        lhs_type: &'static TypeInfo,
        rhs_type: &'static TypeInfo,
        result_ty: &'static TypeInfo,
        context: ContextKind,
    ) -> Option<Operand> {
        use PrimitiveType::*;
        use TypeInfo::*;

        let result_operand = match (bin_expr.op, lhs_type, rhs_type) {
            // pointer + integer = pointer
            (BinOp::Add, Pointer(_), BuiltInPrimitive(I64 | U64)) => {
                let element_ty = lhs_type.deref_ptr();
                // LLVM-style GEP: [0, offset] where 0 = no base pointer offset, offset = element index
                let result_var = self.function.make_get_element_ptr(
                    "ptr_offset".intern(),
                    lhs_operand,
                    vec![Operand::U64Const(0), rhs_operand],  // [0, n] for ptr[n]
                    element_ty,
                );
                self.add_temporary(result_var);
                Operand::Variable(result_var)
            }

            // pointer - integer = pointer
            (BinOp::Subtract, Pointer(_), BuiltInPrimitive(I64 | U64)) => {
                // Negate the index for subtraction
                let negated_var = self.function.make_unary(
                    "negated_index".intern(),
                    rhs_type,
                    YirUnaryOp::Neg,
                    rhs_operand,
                );
                self.add_temporary(negated_var);

                let element_ty = lhs_type.deref_ptr();
                // LLVM-style GEP: [0, -offset] where 0 = no base pointer offset, -offset = negative element index
                let result_var = self.function.make_get_element_ptr(
                    "ptr_offset".intern(),
                    lhs_operand,
                    vec![Operand::U64Const(0), Operand::Variable(negated_var)],  // [0, -n] for ptr[-n]
                    element_ty,
                );
                self.add_temporary(result_var);
                Operand::Variable(result_var)
            }

            // pointer - pointer = integer (distance calculation)
            (BinOp::Subtract, Pointer(_), Pointer(_)) => {
                // Emit a binary operation for pointer subtraction
                // This should calculate the distance between pointers
                let result_var = self.function.make_binary(
                    "ptr_distance".intern(),
                    YirBinOp::Sub,
                    lhs_operand,
                    rhs_operand,
                    result_ty,
                );
                self.add_temporary(result_var);
                Operand::Variable(result_var)
            }

            // Not a pointer arithmetic operation
            _ => return None,
        };

        Some(match context {
            ContextKind::AsIs => result_operand,
            ContextKind::StorageLocation => {
                let storage_var =
                    self.function
                        .make_alloca_single("ptr_storage".intern(), result_ty, None);
                self.add_temporary(storage_var);
                self.function.make_store(storage_var, result_operand);
                Operand::Variable(storage_var)
            }
        })
    }

    /// Placeholder for lowering user-defined operator function calls
    fn lower_user_defined_binary_op(
        &mut self,
        _bin_expr: &crate::pass_parse::ast::BinaryExpr,
        _lhs_operand: Operand,
        _rhs_operand: Operand,
        _result_ty: &'static TypeInfo,
        _context: ContextKind,
    ) -> Operand {
        // TODO: Implement user-defined operator function calls
        // This will involve:
        // 1. Looking up the binding ID from the bindings table
        // 2. Emitting a function call instruction with the operands as arguments
        // 3. Handling the return value properly based on context
        todo!("User-defined operator function calls not yet implemented in YIR lowering")
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(Scope {
            vars: Vec::new(),
            defers: Vec::new(),
        });
    }

    fn pop_scope_and_generate_killset(&mut self) {
        if let Some(scope) = self.scope_stack.pop() {
            // Execute deferred expressions in reverse order
            for defer_expr in scope.defers.into_iter().rev() {
                let _ = self.lower_expr(&defer_expr, ContextKind::AsIs);
            }

            // Generate killset for variables only if flag is set
            if self.emit_kill_sets && !scope.vars.is_empty() {
                self.function.make_kill_set(scope.vars);
            }
        }
    }

    fn add_variable_to_current_scope(&mut self, var: Variable) {
        if self.emit_kill_sets
            && let Some(current_scope) = self.scope_stack.last_mut()
        {
            current_scope.vars.push(var);
        }
    }

    fn add_deferred_expression(&mut self, expr: &ExprNode) {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.defers.push(expr.clone());
        }
    }

    fn add_temporary(&mut self, var: Variable) {
        if self.emit_kill_sets {
            self.current_temporaries.push(var);
        }
    }

    fn kill_current_temporaries(&mut self) {
        if self.emit_kill_sets && !self.current_temporaries.is_empty() {
            self.function
                .make_kill_set(std::mem::take(&mut self.current_temporaries));
        }
    }

    fn lower_ident_assignment(&mut self, ident_expr: &IdentExpr, context: ContextKind) -> Variable {
        let binding_id = *self.bindings.get(&ident_expr.id).unwrap_or_else(|| {
            panic!(
                "Compiler bug: No binding found for ident expr '{}': {} -> Span: {:#?}",
                ident_expr.ident, ident_expr.id, ident_expr.span
            )
        });

        let yir_var = *self.var_map.get(&binding_id).unwrap_or_else(|| {
            panic!(
                "Compiler bug: No YIR variable found for binding ID {} (ident: {})",
                binding_id, ident_expr.ident
            )
        });

        match context {
            ContextKind::StorageLocation => yir_var,
            ContextKind::AsIs => {
                let temp = self
                    .function
                    .make_load("var_value".intern(), Operand::Variable(yir_var));
                self.add_temporary(temp);
                temp
            }
        }
    }

    fn lower_expr(&mut self, expr: &ExprNode, context: ContextKind) -> Operand {
        match expr {
            ExprNode::Literal(lit) => {
                debug_assert!(
                    context != ContextKind::StorageLocation,
                    "Compiler bug: Cannot get storage location of literal - literals are values, not storage"
                );

                match &lit.lit {
                    Token {
                        kind: TokenKind::Integer(Integer::I64(n)),
                        ..
                    } => Operand::I64Const(*n),
                    Token {
                        kind: TokenKind::Integer(Integer::U64(n)),
                        ..
                    } => Operand::U64Const(*n),
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
                }
            }
            ExprNode::Binary(bin_expr) => {
                // Lower operands first
                let lhs_operand = self.lower_expr(&bin_expr.left, ContextKind::AsIs);
                let rhs_operand = self.lower_expr(&bin_expr.right, ContextKind::AsIs);

                // Get operand types for dispatch
                let lhs_type = self.get_type(bin_expr.left.node_id());
                let rhs_type = self.get_type(bin_expr.right.node_id());
                let result_ty = self.get_type(bin_expr.id);

                // Type-based dispatch following the same pattern as type inference

                // Case 1: Built-in primitive arithmetic (i64 + i64, f32 * f32, etc.)
                if let (TypeInfo::BuiltInPrimitive(a), TypeInfo::BuiltInPrimitive(b)) =
                    (lhs_type, rhs_type)
                    && a == b
                {
                    return self.lower_builtin_binary_op(
                        bin_expr,
                        lhs_operand,
                        rhs_operand,
                        result_ty,
                        context,
                    );
                }

                // Case 2: Try pointer arithmetic operations
                if let Some(result) = self.try_lower_pointer_arithmetic(
                    bin_expr,
                    lhs_operand,
                    rhs_operand,
                    lhs_type,
                    rhs_type,
                    result_ty,
                    context,
                ) {
                    return result;
                }

                // Case 3: User-defined operations - emit function call
                self.lower_user_defined_binary_op(
                    bin_expr,
                    lhs_operand,
                    rhs_operand,
                    result_ty,
                    context,
                )
            }
            ExprNode::Unary(un_expr) => {
                let operand = self.lower_expr(&un_expr.expr, ContextKind::AsIs);
                let ty = self.get_type(un_expr.id);

                let result_operand = match un_expr.op {
                    UnaryOp::Negate => {
                        let temp = self.function.make_unary(
                            "neg_result".intern(),
                            ty,
                            YirUnaryOp::Neg,
                            operand,
                        );
                        self.add_temporary(temp);
                        Operand::Variable(temp)
                    }
                    UnaryOp::Pos => {
                        // Nothing really happens here, so we just return the variable itself or if
                        // not a variable, we just create a temp one and return that...
                        if let Operand::Variable(var) = operand {
                            Operand::Variable(var)
                        } else {
                            let temp = self.function.make_alloca_single(
                                "pos_temp".intern(),
                                ty,
                                Some(operand),
                            );
                            self.add_temporary(temp);
                            Operand::Variable(temp)
                        }
                    }
                    UnaryOp::Free => {
                        self.function.make_heap_free(operand);
                        Operand::NoOp
                    }
                };

                match context {
                    ContextKind::AsIs => result_operand,
                    ContextKind::StorageLocation => {
                        if result_operand == Operand::NoOp {
                            unreachable!(
                                "Compiler bug: Cannot get storage location for NoOp operand from unary expression"
                            );
                        }
                        let storage = self.function.make_alloca_single(
                            "unary_storage".intern(),
                            ty,
                            Some(result_operand),
                        );
                        self.add_temporary(storage);
                        Operand::Variable(storage)
                    }
                }
            }
            ExprNode::Ident(ident_expr) => {
                Operand::Variable(self.lower_ident_assignment(ident_expr, context))
            }
            ExprNode::FuncCall(func_call_expr) => {
                let func_name = match &*func_call_expr.lhs {
                    ExprNode::Ident(ident) => ident.ident,
                    _ => unreachable!("Function call LHS must be an identifier"),
                };

                let args: Vec<_> = func_call_expr
                    .args
                    .iter()
                    .map(|arg| self.lower_expr(arg, ContextKind::AsIs))
                    .collect();

                let return_type = self.get_type(func_call_expr.id);
                let result =
                    self.function
                        .make_call("call_result".intern(), func_name, args, return_type);

                match result {
                    Some(result_var) => {
                        self.add_temporary(result_var);
                        match context {
                            ContextKind::AsIs => Operand::Variable(result_var),
                            ContextKind::StorageLocation => {
                                let storage = self.function.make_alloca_single(
                                    "call_storage".intern(),
                                    return_type,
                                    Some(Operand::Variable(result_var)),
                                );
                                self.add_temporary(storage);
                                Operand::Variable(storage)
                            }
                        }
                    }
                    None => {
                        debug_assert!(
                            context != ContextKind::StorageLocation,
                            "Compiler bug: Cannot get storage location of void function call"
                        );
                        Operand::NoOp
                    }
                }
            }

            ExprNode::Assignment(assignment_expr) => {
                let rhs = self.lower_expr(&assignment_expr.rhs, ContextKind::AsIs);
                let target_var = match assignment_expr.lvalue_kind {
                    LValueKind::Variable => match assignment_expr.lhs.as_ref() {
                        ExprNode::Ident(ident_expr) => {
                            self.lower_ident_assignment(ident_expr, ContextKind::StorageLocation)
                        }
                        _ => unreachable!("Compiler bug: Variable LValueKind must have Ident LHS"),
                    },
                    LValueKind::FieldAccess => {
                        let lhs =
                            self.lower_expr(&assignment_expr.lhs, ContextKind::StorageLocation);
                        match lhs {
                            Operand::Variable(var) => var,
                            _ => unreachable!(
                                "Compiler bug: FieldAccess LHS must evaluate to a variable (pointer)"
                            ),
                        }
                    }
                    LValueKind::Dereference => {
                        let lhs =
                            self.lower_expr(&assignment_expr.lhs, ContextKind::StorageLocation);
                        match lhs {
                            Operand::Variable(var) => var,
                            _ => unreachable!(
                                "Compiler bug: Dereference LHS must evaluate to a variable (pointer)"
                            ),
                        }
                    }
                };

                self.function.make_store(target_var, rhs);

                Operand::NoOp

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

                let struct_var = self.function.make_alloca_single(sinfo.name, sinfo.ty, None);
                self.add_temporary(struct_var);

                for (field, field_expr) in &struct_instantiation_expr.fields {
                    let field_name = field.name;
                    let field_op = self.lower_expr(field_expr, ContextKind::AsIs);

                    let (field_idx, field_ty) = sinfo
                        .fields
                        .iter()
                        .enumerate()
                        .find(|(_, (n, _))| **n == field_name)
                        .map(|(idx, (_, info))| (idx, info.ty))
                        .expect("Compiler bug: field not found in lowering to YIR");

                    // LLVM-style GEP: [0, field_idx] for struct.field access
                    let field_var = self.function.make_get_element_ptr(
                        field_name,
                        Operand::Variable(struct_var),
                        vec![Operand::U64Const(0), Operand::U64Const(field_idx as u64)],
                        field_ty,
                    );
                    self.add_temporary(field_var);

                    self.function.make_store(field_var, field_op);
                }
                match context {
                    ContextKind::StorageLocation => Operand::Variable(struct_var),
                    ContextKind::AsIs => {
                        let loaded_value = self
                            .function
                            .make_load("struct_value".intern(), Operand::Variable(struct_var));
                        self.add_temporary(loaded_value);
                        Operand::Variable(loaded_value)
                    }
                }
            }

            ExprNode::MemberAccess(member_access_expr) => {
                let lhs_type = self.get_type(member_access_expr.lhs.node_id());

                // Determine the appropriate context and extract struct type info
                let (lhs, struct_type) = match lhs_type {
                    TypeInfo::Struct(sinfo) => {
                        // Direct struct access: we need the storage location to get a pointer to the struct
                        let lhs =
                            self.lower_expr(&member_access_expr.lhs, ContextKind::StorageLocation);
                        (lhs, sinfo)
                    }
                    TypeInfo::Pointer(inner_type) => {
                        match inner_type {
                            TypeInfo::Struct(sinfo) => {
                                // Pointer to struct access: we need the pointer value itself
                                let lhs =
                                    self.lower_expr(&member_access_expr.lhs, ContextKind::AsIs);
                                (lhs, sinfo)
                            }
                            _ => unreachable!(
                                "Member access on pointer to non-struct type: {:#?}",
                                inner_type
                            ),
                        }
                    }
                    _ => unreachable!("Member access on non-struct type: {:#?}", lhs_type),
                };

                let field_name = member_access_expr.field.name;

                let field_info = self
                    .tr
                    .resolve_struct(struct_type.name)
                    .expect("Compiler bug: struct not found in lowering to YIR");

                let (field_idx, field_ty) = field_info
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, (n, _))| **n == field_name)
                    .map(|(idx, (_, info))| (idx, info.ty))
                    .expect("Compiler bug: field not found in lowering to YIR");

                // LLVM-style GEP: [0, field_idx] where 0 = no base pointer offset, field_idx = struct field index
                let field_ptr = self.function.make_get_element_ptr(
                    "field_ptr".intern(),
                    lhs,
                    vec![Operand::U64Const(0), Operand::U64Const(field_idx as u64)],
                    field_ty,
                );
                self.add_temporary(field_ptr);

                match context {
                    ContextKind::StorageLocation => Operand::Variable(field_ptr),
                    ContextKind::AsIs => {
                        let loaded_value = self
                            .function
                            .make_load("field_value".intern(), Operand::Variable(field_ptr));
                        self.add_temporary(loaded_value);
                        Operand::Variable(loaded_value)
                    }
                }
            }
            ExprNode::EnumInstantiation(ei) => {
                let enum_info = self
                    .tr
                    .resolve_enum(ei.enum_name)
                    .expect("Compiler bug: enum not found in lowering to YIR");

                let variant_info = enum_info
                    .variants_info
                    .fields
                    .get(&ei.variant_name)
                    .expect("Compiler bug: enum variant not found in lowering to YIR");

                let data_operand = ei
                    .data
                    .as_ref()
                    .map(|data_expr| self.lower_expr(data_expr, ContextKind::AsIs));

                let enum_var = self.function.make_alloca_single(
                    "enum_result".intern(),
                    enum_info.wrapper_info.ty,
                    None,
                );
                self.add_temporary(enum_var);

                // Write tag field (field 0 in wrapper struct)
                let tag_ptr = self.function.make_get_element_ptr(
                    "enum_tag_ptr".intern(),
                    Operand::Variable(enum_var),
                    vec![Operand::U64Const(0), Operand::U64Const(0)], // [0, 0] for struct.tag_field
                    primitive_u64(),
                );
                self.add_temporary(tag_ptr);
                self.function
                    .make_store(tag_ptr, Operand::U64Const(variant_info.field_idx));

                if let Some(data_operand) = data_operand {
                    // Write payload field (field 1 in wrapper struct)
                    let payload_ptr = self.function.make_get_element_ptr(
                        "enum_payload_ptr".intern(),
                        Operand::Variable(enum_var),
                        vec![Operand::U64Const(0), Operand::U64Const(1)], // [0, 1] for struct.payload_field
                        variant_info.ty,
                    );
                    self.add_temporary(payload_ptr);
                    self.function.make_store(payload_ptr, data_operand);
                }

                match context {
                    ContextKind::StorageLocation => Operand::Variable(enum_var),
                    ContextKind::AsIs => {
                        let loaded_value = self
                            .function
                            .make_load("enum_value".intern(), Operand::Variable(enum_var));
                        self.add_temporary(loaded_value);
                        Operand::Variable(loaded_value)
                    }
                }
            }

            ExprNode::Deref(deref_expr) => {
                let ptr_var = self.lower_expr(&deref_expr.expr, ContextKind::AsIs);
                match context {
                    ContextKind::AsIs => {
                        let loaded = self.function.make_load("deref_val".intern(), ptr_var);
                        self.add_temporary(loaded);
                        Operand::Variable(loaded)
                    }
                    ContextKind::StorageLocation => ptr_var,
                }
            }
            ExprNode::AddressOf(address_of_expr) => {
                debug_assert_eq!(context, ContextKind::AsIs);
                self.lower_expr(&address_of_expr.expr, ContextKind::StorageLocation)
            }

            ExprNode::HeapAlloc(heap_alloc_expr) => {
                debug_assert_eq!(context, ContextKind::AsIs);

                let expr_type = self.get_type(heap_alloc_expr.id);

                // Check if we can initialize directly on heap
                match &*heap_alloc_expr.expr {
                    ExprNode::Array(array_expr) => {
                        // Get the size operand (can be dynamic for heap allocation)
                        let count_operand = self.lower_expr(&array_expr.size, ContextKind::AsIs);

                        // Get element type
                        let element_type = expr_type.deref_ptr(); // Arrays are stored as pointers
                        let element_layout = calculate_type_layout(element_type, self.tr);

                        // Calculate total size: element_size * count
                        let total_size_var = self.function.make_binary(
                            "array_total_size".intern(),
                            crate::pass_yir_lowering::yir::BinOp::Mul,
                            Operand::U64Const(element_layout.size as u64),
                            count_operand,
                            primitive_u64(),
                        );
                        self.add_temporary(total_size_var);

                        // Create ArrayInit based on init_value
                        let init = if let Some(init_value) = &array_expr.init_expr {
                            let init_operand = self.lower_expr(init_value, ContextKind::AsIs);
                            Some(crate::pass_yir_lowering::yir::ArrayInit::Splat(
                                init_operand,
                            ))
                        } else {
                            None
                        };

                        let ptr_var = self.function.make_heap_alloc(
                            "heap_array".intern(),
                            element_type,
                            count_operand,
                            init,
                        );
                        self.add_temporary(ptr_var);
                        Operand::Variable(ptr_var)
                    }
                    ExprNode::ArrayLiteral(array_literal_expr) => {
                        debug_assert!(
                            !array_literal_expr.elements.is_empty(),
                            "Empty array literals should be rejected during parsing"
                        );

                        // Lower all elements to operands
                        let element_operands: Vec<Operand> = array_literal_expr
                            .elements
                            .iter()
                            .map(|elem| self.lower_expr(elem, ContextKind::AsIs))
                            .collect();

                        // Get element type
                        let element_type = expr_type.deref_ptr(); // Arrays decay to pointers
                        // IMMEDIATELY

                        let ptr_var = self.function.make_heap_alloc_with_values(
                            "heap_array_literal".intern(),
                            element_type,
                            element_operands,
                        );
                        self.add_temporary(ptr_var);
                        Operand::Variable(ptr_var)
                    }
                    ExprNode::StructInstantiation(struct_instantiation_expr) => {
                        let value_type = self.get_type(struct_instantiation_expr.id);

                        // Allocate memory on heap first
                        let ptr_var = self.function.make_heap_alloc_single(
                            "heap_ptr".intern(),
                            value_type,
                            None,
                        );
                        self.add_temporary(ptr_var);

                        // Initialize struct fields directly in heap memory
                        let sinfo = self
                            .tr
                            .resolve_struct(struct_instantiation_expr.struct_name)
                            .expect("Compiler bug: struct not found in lowering to YIR");

                        for (field, field_expr) in &struct_instantiation_expr.fields {
                            let field_name = field.name;
                            let field_op = self.lower_expr(field_expr, ContextKind::AsIs);

                            let (field_idx, field_ty) = sinfo
                                .fields
                                .iter()
                                .enumerate()
                                .find(|(_, (n, _))| **n == field_name)
                                .map(|(idx, (_, info))| (idx, info.ty))
                                .expect("Compiler bug: field not found in lowering to YIR");
                            // LLVM-style GEP: [0, field_idx] for heap_ptr->field access
                            let field_var = self.function.make_get_element_ptr(
                                field_name,
                                Operand::Variable(ptr_var),
                                vec![Operand::U64Const(0), Operand::U64Const(field_idx as u64)],
                                field_ty,
                            );
                            self.add_temporary(field_var);

                            self.function.make_store(field_var, field_op);
                        }
                        Operand::Variable(ptr_var)
                    }
                    _ => {
                        let value_type = self.get_type(heap_alloc_expr.expr.node_id());

                        // For non-struct/array literals, fall back to stack-then-copy
                        let ptr_var = self.function.make_heap_alloc_single(
                            "heap_ptr".intern(),
                            value_type,
                            None,
                        );
                        self.add_temporary(ptr_var);

                        let value_operand =
                            self.lower_expr(&heap_alloc_expr.expr, ContextKind::AsIs);
                        self.function.make_store(ptr_var, value_operand);
                        Operand::Variable(ptr_var)
                    }
                }
            }
            ExprNode::Array(array_expr) => {
                // Get the size operand
                let size_operand = self.lower_expr(&array_expr.size, ContextKind::AsIs);
                let count = match size_operand {
                    Operand::U64Const(n) => n,
                    Operand::I64Const(n) if n >= 0 => n as u64,
                    _ => {
                        unreachable!(
                            "Array size must be a compile-time constant - type checker should have ensured this"
                        );
                    }
                };

                // Get element type from type registry (already inferred during type checking)
                let array_type = self.get_type(array_expr.id);
                let element_type = array_type.deref_ptr(); // Arrays are stored as pointers

                let ptr_var = if let Some(init_value) = &array_expr.init_expr {
                    // Pattern: [value; count] - initialized with repeated value
                    let init_operand = self.lower_expr(init_value, ContextKind::AsIs);
                    let init = Some(crate::pass_yir_lowering::yir::ArrayInit::Splat(
                        init_operand,
                    ));
                    self.function
                        .make_alloca("array".intern(), element_type, count, init)
                } else {
                    // Pattern: [:type; count] - uninitialized
                    self.function
                        .make_alloca("array".intern(), element_type, count, None)
                };

                self.add_temporary(ptr_var);
                Operand::Variable(ptr_var)
            }
            ExprNode::ArrayLiteral(array_literal_expr) => {
                debug_assert!(
                    !array_literal_expr.elements.is_empty(),
                    "Empty array literals should be rejected during parsing"
                );

                // Lower all elements to operands
                let element_operands: Vec<Operand> = array_literal_expr
                    .elements
                    .iter()
                    .map(|elem| self.lower_expr(elem, ContextKind::AsIs))
                    .collect();

                // Get element type from type registry (already inferred)
                let array_type = self.get_type(array_literal_expr.id);
                let element_type = array_type.deref_ptr(); // Arrays are stored as pointers

                // Create array with explicit element values
                let init = crate::pass_yir_lowering::yir::ArrayInit::Elements(element_operands);
                let ptr_var = self.function.make_alloca(
                    "array_literal".intern(),
                    element_type,
                    array_literal_expr.elements.len() as u64,
                    Some(init),
                );

                self.add_temporary(ptr_var);
                Operand::Variable(ptr_var)
            }
            ExprNode::Cast(cast_expr) => {
                debug_assert_eq!(context, ContextKind::AsIs);

                let expr_operand = self.lower_expr(&cast_expr.expr, ContextKind::AsIs);
                let target_type = self.get_type(cast_expr.id);

                // For now, only support int to pointer casting
                if target_type.is_ptr() {
                    let result_var = self.function.make_int_to_ptr(
                        "cast_result".intern(),
                        target_type,
                        expr_operand,
                    );
                    self.add_temporary(result_var);
                    Operand::Variable(result_var)
                } else {
                    panic!(
                        "Cast expressions other than int-to-pointer not yet implemented in YIR lowering"
                    )
                }
            }
            ExprNode::LuaMeta(_) => {
                panic!("Compiler bug: LuaMeta nodes should be replaced before YIR lowering")
            }
        }
    }

    fn lower_block_body(&mut self, block_stmt: &BlockStmt) {
        // Push a new scope for this block
        self.push_scope();

        for stmt in &block_stmt.body {
            let res = self.lower_stmt(stmt);

            match res {
                StmtRes::Break => {
                    // Generate KillSet before breaking
                    self.pop_scope_and_generate_killset();
                    return;
                }
                StmtRes::Proceed => (),
            }
        }

        // Pop scope and generate KillSet for variables going out of scope
        self.pop_scope_and_generate_killset();
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) -> StmtRes {
        let result = match stmt {
            StmtNode::Let(let_stmt) => {
                let init_value_operand = self.lower_expr(&let_stmt.expr, ContextKind::AsIs);
                self.lower_binding(let_stmt.binding.as_ref(), init_value_operand);
                StmtRes::Proceed
            }
            StmtNode::Atomic(expr) => {
                let _ = self.lower_expr(expr, ContextKind::AsIs);
                StmtRes::Proceed
            }
            StmtNode::Break(_exit_stmt) => {
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
            StmtNode::Defer(defer_stmt) => {
                // Add deferred expression to current scope
                self.add_deferred_expression(&defer_stmt.expr);
                StmtRes::Proceed
            }
            StmtNode::Error(_) => unreachable!(
                "Syntax Error reached during lowering - pipeline was wrongly configured or compiler bug"
            ),
            StmtNode::Return(return_stmt) => {
                let ret_value = return_stmt
                    .expr
                    .as_ref()
                    .map(|e| self.lower_expr(e, ContextKind::AsIs));

                // Pull all higher-level scopes into the current scope for KILLSETting
                if !self.scope_stack.is_empty() {
                    // First collect variables from all higher scopes
                    let mut vars_to_merge = Vec::new();
                    for scope in &self.scope_stack[..self.scope_stack.len() - 1] {
                        vars_to_merge.extend_from_slice(&scope.vars);
                    }

                    vars_to_merge.extend_from_slice(&self.current_temporaries);
                    self.current_temporaries.clear();

                    if let Some(current_scope) = self.scope_stack.last_mut() {
                        current_scope.vars.extend_from_slice(&vars_to_merge);
                    }
                }

                self.function.make_return(ret_value);
                StmtRes::Break
            }
            StmtNode::Match(match_stmt) => {
                let match_header = self.function.add_block("match_header".intern());
                let match_merge_block = self.function.add_block("match_merge".intern());

                self.function.make_jump_if_no_terminator(match_header);
                self.function.set_current_block(&match_header);

                let lowered_match_expr = self.lower_expr(&match_stmt.scrutinee, ContextKind::AsIs);

                let ty = self.get_type(match_stmt.scrutinee.node_id());

                // Handle enum types (now represented as structs)
                let enum_info = match ty {
                    TypeInfo::Struct(struct_type) => {
                        self.tr.resolve_enum(struct_type.name).unwrap()
                    }
                    _ => panic!("Match statements currently only support enum types"),
                };

                let _struct_layout = calculate_struct_layout(enum_info.wrapper_info, self.tr);

                // Ensure we have a pointer to the scrutinee value
                let scrutinee_ptr = match lowered_match_expr {
                    Operand::Variable(v) if v.ty().is_ptr() => Operand::Variable(v),
                    Operand::Variable(v) => {
                        let tmp = self.function.make_alloca_single(
                            "match_scrutinee".intern(),
                            v.ty(),
                            None,
                        );
                        self.add_temporary(tmp);
                        self.function.make_store(tmp, Operand::Variable(v));
                        Operand::Variable(tmp)
                    }
                    _ => {
                        let tmp =
                            self.function
                                .make_alloca_single("match_scrutinee".intern(), ty, None);
                        self.add_temporary(tmp);
                        self.function.make_store(tmp, lowered_match_expr);
                        Operand::Variable(tmp)
                    }
                };

                // Load the tag (discriminant) once for jump table dispatch
                let tag_ptr = self.function.make_get_element_ptr(
                    "match_tag_ptr".intern(),
                    scrutinee_ptr,
                    vec![Operand::U64Const(0), Operand::U64Const(0)], // [0, 0] for struct.tag_field
                    primitive_u64(),
                );
                self.add_temporary(tag_ptr);
                let tag_val = self
                    .function
                    .make_load("match_tag".intern(), Operand::Variable(tag_ptr));
                self.add_temporary(tag_val);
                let tag_operand = Operand::Variable(tag_val);

                let mut jump_targets = IndexMap::default();
                for arm in match_stmt.arms.iter() {
                    let RefutablePatternNode::Enum(enum_pattern) = arm.pattern.as_ref();
                    let variant_info = enum_info
                        .variants_info
                        .fields
                        .get(&enum_pattern.variant_name)
                        .unwrap();
                    let match_arm_block = self.function.add_block("match_arm".intern());
                    self.function.set_current_block(&match_arm_block);

                    if let Some(variant_data_binding) = enum_pattern.binding.as_ref() {
                        let payload_ptr = self.function.make_get_element_ptr(
                            "variant_payload".intern(),
                            scrutinee_ptr,
                            vec![Operand::U64Const(0), Operand::U64Const(1)], // [0, 1] for struct.payload_field
                            variant_info.ty,
                        );
                        self.add_temporary(payload_ptr);
                        let variant_value = self
                            .function
                            .make_load("variant_value".intern(), Operand::Variable(payload_ptr));
                        self.add_temporary(variant_value);
                        self.lower_binding(variant_data_binding, Operand::Variable(variant_value));
                    }

                    self.lower_block_body(&arm.body);
                    self.function.make_jump_if_no_terminator(match_merge_block);
                    jump_targets.insert(variant_info.field_idx, match_arm_block);
                }

                self.function.set_current_block(&match_header);

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
                self.function
                    .make_jump_table(tag_operand, jump_targets, default_block);

                self.function.set_current_block(&match_merge_block);
                StmtRes::Proceed
            }
            StmtNode::If(if_stmt) => {
                let if_header = self.function.add_block("if_header".intern());
                let if_body = self.function.add_block("if_body".intern());
                let merge_block = self.function.add_block("if_merge".intern());

                self.function.make_jump_if_no_terminator(if_header);
                self.function.set_current_block(&if_header);

                let lowered_if_condition =
                    self.lower_expr(&if_stmt.if_block.condition, ContextKind::AsIs);
                self.kill_current_temporaries();

                self.function.set_current_block(&if_body);
                self.lower_block_body(&if_stmt.if_block.body);
                self.function.make_jump_if_no_terminator(merge_block);
                self.function.set_current_block(&if_header);

                let mut prev_body = if_body;
                let mut prev_condition = lowered_if_condition;

                for else_if_block in &if_stmt.else_if_blocks {
                    let else_if_header = self.function.add_block("else_if_header".intern());
                    let else_if_body = self.function.add_block("else_if_body".intern());

                    self.function.make_branch_to_existing(
                        prev_condition,
                        prev_body,
                        else_if_header,
                    );

                    self.function.set_current_block(&else_if_header);
                    let lowered_else_if_cond =
                        self.lower_expr(&else_if_block.condition, ContextKind::AsIs);
                    self.kill_current_temporaries();

                    self.function.set_current_block(&else_if_body);
                    self.lower_block_body(&else_if_block.body);
                    self.function.make_jump_if_no_terminator(merge_block);
                    self.function.set_current_block(&else_if_header);

                    prev_body = else_if_body;
                    prev_condition = lowered_else_if_cond;
                }

                if let Some(else_block) = &if_stmt.else_block {
                    let else_body = self.function.add_block("else_body".intern());

                    self.function
                        .make_branch_to_existing(prev_condition, prev_body, else_body);

                    self.function.set_current_block(&else_body);
                    self.lower_block_body(else_block);
                    self.function.make_jump_if_no_terminator(merge_block);
                } else {
                    self.function
                        .make_branch_to_existing(prev_condition, prev_body, merge_block);
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
                    self.lower_expr(&while_stmt.condition_block.condition, ContextKind::AsIs);
                self.kill_current_temporaries();
                self.function.make_branch_to_existing(
                    lowered_while_condition,
                    while_body,
                    while_merge,
                );

                self.function.set_current_block(&while_body);

                self.loop_context.push(while_merge);
                self.lower_block_body(&while_stmt.condition_block.body);
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
            StmtNode::Decl(decl_stmt) => {
                let binding_id = decl_stmt.ident.id;
                let name_hint = decl_stmt.ident.name;
                let var_type = self.get_type(decl_stmt.ident.id);
                let yir_var = self.function.make_alloca(name_hint, var_type, 1, None);
                self.var_map.insert(binding_id, yir_var);
                self.add_variable_to_current_scope(yir_var);
                StmtRes::Proceed
            }
            StmtNode::Def(def_stmt) => {
                let rhs_value = self.lower_expr(&def_stmt.expr, ContextKind::AsIs);

                // Find the YIR variable that was created during the decl
                let binding_id = *self.bindings.get(&def_stmt.ident.id).unwrap_or_else(|| {
                    panic!(
                        "Compiler bug: No binding found for def ident '{}': {} -> Span: {:#?}",
                        def_stmt.ident.name, def_stmt.ident.id, def_stmt.ident.span
                    )
                });
                let yir_var = *self.var_map.get(&binding_id).unwrap_or_else(|| {
                    panic!(
                        "Compiler bug: No YIR variable found for def binding ID {} (ident: {})",
                        binding_id, def_stmt.ident.name
                    )
                });

                // Store the RHS value into the already-allocated variable
                self.function.make_store(yir_var, rhs_value);
                StmtRes::Proceed
            }
            StmtNode::LuaMeta(_) => {
                panic!("Compiler bug: LuaMeta nodes should be replaced before YIR lowering")
            }
        };

        self.kill_current_temporaries();
        result
    }

    fn lower_binding(&mut self, binding: &BindingNode, init_value_operand: Operand) {
        match binding {
            BindingNode::Ident(ident_binding) => {
                let binding_id = ident_binding.id;
                let name_hint = ident_binding.name;
                let var_type = self.get_type(binding_id);
                let yir_var =
                    self.function
                        .make_alloca_single(name_hint, var_type, Some(init_value_operand));
                self.var_map.insert(binding_id, yir_var);

                self.add_variable_to_current_scope(yir_var);
            }
            BindingNode::LuaMeta(_) => {
                panic!("Compiler bug: LuaMeta nodes should be replaced before YIR lowering")
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

    fn lower_ast(&self, ast: &AST, tr: &TypeRegistry, emit_kill_sets: bool) -> Module {
        let functions: Vec<Function> = ast
            .structurals
            .par_iter()
            .filter_map(|structural| {
                if let StructuralNode::FuncDef(func) = structural.as_ref() {
                    let return_type = match tr.get_structural_type(func.id) {
                        TypeInfo::Function(ft) => ft.ret,
                        _ => unreachable!("Expected function type"),
                    };

                    // Extract metadata from StructuralElement wrapper
                    let metadata = &structural.metadata;

                    let mut data = TransientData::new(
                        Function::new(func.decl.name, structural.node.node_id(), return_type),
                        tr,
                        &metadata.type_info_table,
                        &metadata.binding_table,
                        emit_kill_sets,
                    );

                    // Push function-level scope for parameters
                    data.push_scope();

                    for arg in &func.decl.args {
                        let (ty, name) = (data.get_type(arg.id), arg.name);
                        let param = data.function.add_param("stack_param_mem".intern(), ty);
                        let param_ptr = data.function.make_take_address(name, param);
                        data.var_map.insert(arg.id, param_ptr);

                        // Add parameter to function scope
                        data.add_variable_to_current_scope(param_ptr);
                    }

                    data.lower_block_body(&func.body);

                    // Generate KillSet for function parameters at end of function
                    data.pop_scope_and_generate_killset();

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
                    Some(data.function)
                } else {
                    None
                }
            })
            .collect();

        let mut module = Module::new();

        for func in functions {
            module.define_function(func);
        }

        for structural in &ast.structurals {
            match structural.as_ref() {
                StructuralNode::StructDef(sdefs) => {
                    module.define_struct(sdefs.decl.name);
                }
                StructuralNode::EnumDef(edefs) => {
                    module.define_enum(edefs.decl.name);
                }
                StructuralNode::LuaMeta(_) => {
                    panic!("Compiler bug: LuaMeta nodes should be replaced before YIR lowering")
                }
                _ => (),
            }
        }

        module
    }
}

impl YirLowering {
    pub fn run(
        &self,
        ast: &AST,
        type_registry: &TypeRegistry,
        emit_kill_sets: bool,
    ) -> miette::Result<Module> {
        let module = self.lower_ast(ast, type_registry, emit_kill_sets);
        Ok(module)
    }
}
