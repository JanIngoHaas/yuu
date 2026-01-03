use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module as LlvmModule;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::types::{AnyTypeEnum, BasicTypeEnum, BasicMetadataTypeEnum, BasicType};
use inkwell::targets::TargetData;
use inkwell::IntPredicate;

use crate::pass_yir_lowering::yir as yir;
use crate::utils::TypeRegistry;
use crate::utils::collections::FastHashMap;
use crate::utils::type_info_table::{PrimitiveType, TypeInfo};

// Transient lowering state kept while lowering a module/function.
pub struct TransientData<'ctx> {
    pub context: &'ctx Context,
    pub module: LlvmModule<'ctx>,
    pub builder: Builder<'ctx>,
    pub type_registry: &'ctx TypeRegistry,
    pub target_data: TargetData,
    /// Current function being lowered
    pub function: Option<FunctionValue<'ctx>>,
    /// Map YIR Variables -> LLVM pointer values (allocated stack slots / locals)
    pub var_map: FastHashMap<yir::Variable, BasicValueEnum<'ctx>>,
    /// Optional mapping from labels to LLVM basic blocks created during lowering
    pub block_map: FastHashMap<i64, inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> TransientData<'ctx> {
    pub fn new(context: &'ctx Context, name: &str, type_registry: &'ctx TypeRegistry) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let data_layout = module.get_data_layout();
        let data_layout_str = data_layout.as_str().to_str().unwrap();
        println!("LLVM Module Data Layout: {}", data_layout_str);
        let target_data = TargetData::create(data_layout_str);
        drop(data_layout);
        Self {
            context,
            module,
            builder,
            type_registry,
            target_data,
            function: None,
            var_map: FastHashMap::default(),
            block_map: FastHashMap::default(),
        }
    }
}

pub struct LLVMLowerer;

impl LLVMLowerer {

    pub fn lower_type_basic<'ctx>(&self, type_registry: &TypeRegistry, ty: &'static TypeInfo, context: &'ctx Context) -> BasicTypeEnum<'ctx>
    {
        match ty {
            TypeInfo::BuiltInPrimitive(prim) => {
                match prim {
                    PrimitiveType::I8 => BasicTypeEnum::IntType(context.i8_type()),
                    PrimitiveType::U8 => BasicTypeEnum::IntType(context.i8_type()),
                    PrimitiveType::I64 => BasicTypeEnum::IntType(context.i64_type()),
                    PrimitiveType::U64 => BasicTypeEnum::IntType(context.i64_type()),
                    PrimitiveType::F32 => BasicTypeEnum::FloatType(context.f32_type()),
                    PrimitiveType::F64 => BasicTypeEnum::FloatType(context.f64_type()),
                    PrimitiveType::Bool => BasicTypeEnum::IntType(context.bool_type()),
                    PrimitiveType::Nil => panic!("Nil type does not have a BasicTypeEnum representation"),
                }
            }
            TypeInfo::Pointer(_) => {
                BasicTypeEnum::PointerType(context.ptr_type(inkwell::AddressSpace::default()))
            }
            TypeInfo::Struct(s) => {
                let resolved = type_registry.resolve_struct(s.name).expect("Compiler Bug: Unknown struct type during LLVM lowering");
                let fields = resolved.fields.iter().map(|f| {
                    let llvm_field_ty = self.lower_type_basic(type_registry, f.1.ty, context);
                    llvm_field_ty
                }).collect::<Vec<BasicTypeEnum>>();
                BasicTypeEnum::StructType(context.struct_type(&fields, false))
            }
            _ => panic!("Unsupported type for LLVM lowering to BasicTypeEnum"),
        }
    }

    pub fn lower_type<'ctx>(&self, type_registry: &TypeRegistry, ty: &'static TypeInfo, context: &'ctx Context) -> AnyTypeEnum<'ctx>
    {
        match ty {
            TypeInfo::BuiltInPrimitive(prim) => {
                match prim {
                    PrimitiveType::I8 => AnyTypeEnum::IntType(context.i8_type()),
                    PrimitiveType::U8 => AnyTypeEnum::IntType(context.i8_type()),
                    PrimitiveType::I64 => AnyTypeEnum::IntType(context.i64_type()),
                    PrimitiveType::U64 => AnyTypeEnum::IntType(context.i64_type()),
                    PrimitiveType::F32 => AnyTypeEnum::FloatType(context.f32_type()),
                    PrimitiveType::F64 => AnyTypeEnum::FloatType(context.f64_type()),
                    PrimitiveType::Bool => AnyTypeEnum::IntType(context.bool_type()),
                    PrimitiveType::Nil => AnyTypeEnum::VoidType(context.void_type()),
                }
            }
            TypeInfo::Pointer(_) => {
                AnyTypeEnum::PointerType(context.ptr_type(inkwell::AddressSpace::default()))
            }
            TypeInfo::Struct(s) => {
                let resolved = type_registry.resolve_struct(s.name).expect("Compiler Bug: Unknown struct type during LLVM lowering");
                let fields = resolved.fields.iter().map(|f| {
                    let llvm_field_ty = self.lower_type_basic(type_registry, f.1.ty, context);
                    llvm_field_ty
                }).collect::<Vec<BasicTypeEnum>>();
                AnyTypeEnum::StructType(context.struct_type(&fields, false))
            }
            _ => panic!("Unsupported type for LLVM lowering"),
        }
    }

    /// Top-level lowering entry point for a YIR module.
    /// This will iterate all defined functions and lower them.
    pub fn lower_module(&self, module: &yir::Module, type_registry: &TypeRegistry) {
        let context = Context::create();
        let mut td = TransientData::new(&context, "yuu_module", type_registry);

        // Generate helper functions once at module level
        self.generate_template_fill_helper(&mut td);

        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => todo!(),
                yir::FunctionDeclarationState::Defined(function) => {
                    self.lower_function(function, &mut td);
                },
            }
        }
        
        
        
    }

    /// Generate (or retrieve) a helper function for filling arrays with a template value using memcpy
    /// Equivalent to C's __yuu_template_fill: void __yuu_template_fill(void* dest, const void* template_val, size_t element_size, size_t count, u32 dest_align, u32 src_align)
    #[allow(dead_code)]
    fn generate_template_fill_helper<'ctx>(&'ctx self, td: &'ctx mut TransientData) -> FunctionValue<'ctx> {
        let name = "__yuu_template_fill";
        if let Some(existing) = td.module.get_function(name) {
            return existing;
        }

        // Use i64 for sizes - we only target 64-bit systems
        let i8_ptr_ty = td.context.ptr_type(inkwell::AddressSpace::default());
        let i64_ty = td.context.i64_type();
        let u32_ty = td.context.i32_type();

        // Define: void __yuu_template_fill(i8* dest, i8* template_val, i64 element_size, i64 count, u32 dest_align, u32 src_align)
        let fn_type = td
            .context
            .void_type()
            .fn_type(&[
                i8_ptr_ty.into(),
                i8_ptr_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
                u32_ty.into(),    // dest_align
                u32_ty.into(),    // src_align
            ], false);
        let function = td.module.add_function(name, fn_type, None);

        let entry_bb = td.context.append_basic_block(function, "entry");
        let loop_bb = td.context.append_basic_block(function, "loop");
        let after_bb = td.context.append_basic_block(function, "after");

        // Preserve previous insertion point so we don't disturb outer lowering
        let prev_block = td.builder.get_insert_block();

        td.builder.position_at_end(entry_bb);

        let dest_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let template_ptr = function.get_nth_param(1).unwrap().into_pointer_value();
        let elem_size = function.get_nth_param(2).unwrap().into_int_value();
        let count = function.get_nth_param(3).unwrap().into_int_value();
        let dest_align = function.get_nth_param(4).unwrap().into_int_value();
        let src_align = function.get_nth_param(5).unwrap().into_int_value();

        let zero = i64_ty.const_zero();
        let is_zero = td
            .builder
            .build_int_compare(IntPredicate::EQ, count, zero, "count_is_zero")
            .unwrap();
        td.builder
            .build_conditional_branch(is_zero, after_bb, loop_bb)
            .unwrap();

        td.builder.position_at_end(loop_bb);
        let phi_idx = td.builder.build_phi(i64_ty, "idx").unwrap();
        phi_idx.add_incoming(&[(&zero, entry_bb)]);
        let idx_val = phi_idx.as_basic_value().into_int_value();

        let offset = td
            .builder
            .build_int_mul(idx_val, elem_size, "byte_offset")
            .unwrap();
        let dest_byte_ptr = unsafe {
            td.builder
                .build_gep(td.context.i8_type(), dest_ptr, &[offset], "dest_byte_ptr")
                .unwrap()
        };

        // Copy one element from template_ptr into dest_byte_ptr using runtime alignment values
        td.builder
            .build_memcpy(
                dest_byte_ptr,
                dest_align.get_zero_extended_constant().unwrap_or(1) as u32,
                template_ptr,
                src_align.get_zero_extended_constant().unwrap_or(1) as u32,
                elem_size
            )
            .unwrap();

        let one = i64_ty.const_int(1, false);
        let next_idx = td
            .builder
            .build_int_add(idx_val, one, "next_idx")
            .unwrap();
        let continue_cond = td
            .builder
            .build_int_compare(IntPredicate::ULT, next_idx, count, "continue")
            .unwrap();
        td.builder
            .build_conditional_branch(continue_cond, loop_bb, after_bb)
            .unwrap();
        phi_idx.add_incoming(&[(&next_idx.as_basic_value_enum(), loop_bb)]);

        td.builder.position_at_end(after_bb);
        td.builder.build_return(None).unwrap();

        if let Some(bb) = prev_block {
            td.builder.position_at_end(bb);
        }

        function
    }

    pub fn lower_function(&self, func: &yir::Function, td: &mut TransientData) {
        
        // Pragmatic C-like ABI approach:
        // - Primitives (int, float, bool) and pointers: pass directly
        // - Structs: pass by pointer (caller allocates, passes pointer)
        // - Return values: primitives/pointers directly, structs via hidden sret parameter
        // This avoids complex platform-specific ABI rules until we can use LLVM's ABI library.
        
        let llvm_ret_type = self.lower_type(td.type_registry, func.return_type, td.context);
        
        // For now, just pass all parameters directly (we'll refine this to pass structs by pointer later)
        let llvm_param_types: Vec<_> = func.params.iter().map(|param| {
            self.lower_type_basic(td.type_registry, param.ty(), td.context)
        }).collect();
        
        let llvm_param_metadata_types: Vec<BasicMetadataTypeEnum> = llvm_param_types.iter().map(|ty| (*ty).into()).collect();

        // Create the function type and add it to the module
        let fn_type = match llvm_ret_type {
            AnyTypeEnum::VoidType(void_ty) => void_ty.fn_type(&llvm_param_metadata_types, false),
            AnyTypeEnum::IntType(int_ty) => int_ty.fn_type(&llvm_param_metadata_types, false),
            AnyTypeEnum::FloatType(float_ty) => float_ty.fn_type(&llvm_param_metadata_types, false),
            AnyTypeEnum::PointerType(ptr_ty) => ptr_ty.fn_type(&llvm_param_metadata_types, false),
            AnyTypeEnum::StructType(struct_ty) => {
                // For struct returns, we should use sret, but for now just return directly
                struct_ty.fn_type(&llvm_param_metadata_types, false)
            }
            _ => panic!("Unsupported return type for LLVM function"),
        };
        
        let llvm_fn = td.module.add_function(&func.name, fn_type, None);
        td.function = Some(llvm_fn);

        // Create LLVM basic blocks for each YIR block and record in block_map
        for (id, yir_block) in func.blocks.iter() {
            let name = format!("bb_{}", yir_block.label.name());
            let llvm_bb = td.context.append_basic_block(llvm_fn, &name);
            td.block_map.insert(*id, llvm_bb);
        }
        
        // Create allocas for function parameters and map them to var_map
        let entry_llvm_bb = *td.block_map.get(&func.entry_block).unwrap();
        td.builder.position_at_end(entry_llvm_bb);
        
        let llvm_params = llvm_fn.get_param_iter();
        for ((yir_param, param_ty), param_value) in func.params.iter().zip(llvm_param_types.iter()).zip(llvm_params) {
            let alloca = td.builder.build_alloca(*param_ty, yir_param.name()).unwrap();
            let _ = td.builder.build_store(alloca, param_value).unwrap();
            td.var_map.insert(*yir_param, alloca.into());
        }
        
        // Iterate blocks in order and lower each
        for (id, yir_block) in func.blocks.iter() {
            // Position builder at end of corresponding LLVM block
            // Copy the basic block value out of the map so we don't hold an immutable borrow
            let llvm_bb = *td.block_map.get(id).unwrap();
            td.builder.position_at_end(llvm_bb);

            // Lower all instructions
            for instr in &yir_block.instructions {
                self.lower_instruction(instr, td);
            }

            // Lower terminator
            self.lower_terminator(&yir_block.terminator, llvm_bb, td);
        }
    }

    /// Dispatch lowering for a single instruction. These are stubs that should be
    /// filled in with proper lowering logic.
    fn lower_instruction<'ctx>(&self, instr: &yir::Instruction, td: &mut TransientData<'ctx>) {
        match instr {
            yir::Instruction::Alloca(cmd) => self.lower_alloca(cmd, td),
            yir::Instruction::StoreImmediate(cmd) => self.lower_store_immediate(cmd, td),
            yir::Instruction::TakeAddress(cmd) => self.lower_take_address(cmd, td),
            yir::Instruction::Load(cmd) => self.lower_load(cmd, td),
            yir::Instruction::Store(cmd) => self.lower_store(cmd, td),
            yir::Instruction::Binary(cmd) => self.lower_binary(cmd, td),
            yir::Instruction::Unary(cmd) => self.lower_unary(cmd, td),
            yir::Instruction::Call(cmd) => self.lower_call(cmd, td),
            yir::Instruction::IntToPtr(cmd) => self.lower_int_to_ptr(cmd, td),
            yir::Instruction::HeapAlloc(cmd) => self.lower_heap_alloc(cmd, td),
            yir::Instruction::HeapFree(cmd) => self.lower_heap_free(cmd, td),
            yir::Instruction::MemCpy(cmd) => self.lower_memcpy(cmd, td),
            yir::Instruction::MemSet(cmd) => self.lower_memset(cmd, td),
            yir::Instruction::GetElementPtr(cmd) => self.lower_get_element_ptr(cmd, td),
            yir::Instruction::KillSet(cmd) => self.lower_kill_set(cmd, td),
            yir::Instruction::Reinterp(cmd) => self.lower_reinterp(cmd, td),
        }
    }

    fn lower_terminator<'ctx>(&self, terminator: &yir::ControlFlow, _llvm_bb: inkwell::basic_block::BasicBlock<'ctx>, td: &mut TransientData<'ctx>) {
        match terminator {
            yir::ControlFlow::Jump { target } => {
                // Emit unconditional branch to the target block
                if let Some(target_bb) = td.block_map.get(&target.id()) {
                    let _ = td.builder.build_unconditional_branch(*target_bb);
                }
            }
            yir::ControlFlow::Branch { condition: _, if_true, if_false } => {
                // Evaluate branch condition (stub: currently not translated)
                // TODO: Lower `condition` to an LLVM i1 value
                // For now, use a constant true which goes to `if_true`.
                if let Some(true_bb) = td.block_map.get(&if_true.id()) {
                    if let Some(false_bb) = td.block_map.get(&if_false.id()) {
                        let i1_true = td.context.bool_type().const_int(1, false);
                        let _ = td.builder.build_conditional_branch(i1_true, *true_bb, *false_bb);
                    }
                }
            }
            yir::ControlFlow::JumpTable { scrutinee: _scrutinee, jump_targets: _, default } => {
                // TODO: implement jump table lowering using switch instruction
                // For now, lower to unconditional branch to default if present
                if let Some(default_label) = default {
                    if let Some(default_bb) = td.block_map.get(&default_label.id()) {
                        let _ = td.builder.build_unconditional_branch(*default_bb);
                    }
                }
            }
            yir::ControlFlow::Return(opt) => {
                // TODO: Lower return value properly according to function return type
                match opt {
                    Some(_op) => {
                        // Placeholder: return a zero i64
                        let zero = td.context.i64_type().const_int(0, false);
                        let _ = td.builder.build_return(Some(&zero));
                    }
                    None => {
                        let _ = td.builder.build_return(None);
                    }
                }
            }
            yir::ControlFlow::Unterminated => {
                // Should not happen in final IR - this is an ICE
                panic!("ICE: Encountered unterminated block during LLVM lowering");
            }
        }
    }

    // --- Instruction lowering stubs ---
    fn lower_alloca<'ctx>(&self, cmd: &yir::AllocaCmd, td: &mut TransientData<'ctx>) {
        let lty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);
        let allocd_stack_ptr = if cmd.count == 1 {
            let alloced = td.builder.build_alloca(lty, cmd.target.name()).unwrap();
            alloced
        } else {
            let alloced = td.builder.build_array_alloca(lty, td.context.i64_type().const_int(cmd.count as u64, false), cmd.target.name()).unwrap();
            alloced
        };

        td.var_map.insert(cmd.target, allocd_stack_ptr.into());

        if let Some(init) = &cmd.init {
            match init {
                yir::ArrayInit::Zero => {
                    let zero = td.context.i8_type().const_int(0, false);
                    // Get the size of one element
                    let elem_size = lty.size_of().expect("Type must be sized for alloca");
                    // Calculate total size = elem_size * count
                    let total_size = if cmd.count == 1 {
                        elem_size
                    } else {
                        let count_val = td.context.i64_type().const_int(cmd.count as u64, false);
                        td.builder.build_int_mul(elem_size, count_val, "total_size").unwrap()
                    };
                    // Alignment: use the specified alignment or get LLVM's ABI alignment
                    let alignment = cmd.align.unwrap_or_else(|| {
                        td.target_data.get_abi_alignment(&lty) as u64
                    }) as u32;
                    td.builder.build_memset(allocd_stack_ptr, alignment, zero, total_size).unwrap();
                },
                yir::ArrayInit::Splat(operand) => {
                    // Fill all elements with the same value
                    let val = self.lower_operand(operand, td);
                    const SMALL_ARRAY_THRESHOLD: u64 = 8;

                    if cmd.count <= SMALL_ARRAY_THRESHOLD {
                        // Small arrays: unroll for better performance
                        for i in 0..cmd.count {
                            let index = td.context.i64_type().const_int(i, false);
                            let elem_ptr = if cmd.count == 1 {
                                allocd_stack_ptr
                            } else {
                                unsafe {
                                    td.builder.build_gep(lty, allocd_stack_ptr, &[index], "elem_ptr").unwrap()
                                }
                            };
                            td.builder.build_store(elem_ptr, val).unwrap();
                        }
                    } else {
                        // Large arrays: use helper function to avoid compile-time explosion
                        let elem_size = lty.size_of().expect("Type must be sized for alloca");
                        let count_val = td.context.i64_type().const_int(cmd.count as u64, false);
                        let elem_align = td.target_data.get_abi_alignment(&lty);

                        // Create a temporary stack slot containing one template element
                        let template_slot = td.builder.build_alloca(lty, "splat_template").unwrap();
                        td.builder.build_store(template_slot, val).unwrap();

                        // Get the pre-generated helper function
                        let helper_fn = td.module.get_function("__yuu_template_fill").unwrap();

                        // Cast pointers to i8* expected by the helper
                        let i8_ptr_ty = td.context.ptr_type(inkwell::AddressSpace::default());
                        let dest_ptr = td
                            .builder
                            .build_bit_cast(allocd_stack_ptr, i8_ptr_ty, "dest_i8_ptr")
                            .unwrap()
                            .into_pointer_value();
                        let template_ptr = td
                            .builder
                            .build_bit_cast(template_slot, i8_ptr_ty, "template_i8_ptr")
                            .unwrap()
                            .into_pointer_value();

                        // Call the helper function with alignment arguments
                        let dest_align_val = td.context.i32_type().const_int(elem_align as u64, false);
                        let src_align_val = td.context.i32_type().const_int(elem_align as u64, false);
                        let args = vec![
                            dest_ptr.into(),
                            template_ptr.into(),
                            elem_size.into(),
                            count_val.into(),
                            dest_align_val.into(),
                            src_align_val.into(),
                        ];
                        td.builder.build_call(helper_fn, &args, "template_fill_call").unwrap();
                    }
                },
                yir::ArrayInit::Elements(operands) => {
                    // Store each element at its index
                    for (i, operand) in operands.iter().enumerate() {
                        let val = self.lower_operand(operand, td);
                        let index = td.context.i64_type().const_int(i as u64, false);
                        // SAFETY: GEP on array allocation
                        let elem_ptr = unsafe {
                            td.builder.build_gep(lty, allocd_stack_ptr, &[index], "elem_ptr").unwrap()
                        };
                        td.builder.build_store(elem_ptr, val).unwrap();
                    }
                },
            }
        }

    }

    fn lower_store_immediate<'ctx>(&self, _cmd: &yir::StoreImmediateCmd, _td: &mut TransientData<'ctx>) {
        // TODO: store immediate constant into the given stack slot
    }

    fn lower_take_address<'ctx>(&self, _cmd: &yir::TakeAddressCmd, _td: &mut TransientData<'ctx>) {
        // TODO: take address of variable (pointer to stack slot)
    }

    fn lower_load<'ctx>(&self, _cmd: &yir::LoadCmd, _td: &mut TransientData<'ctx>) {
        // TODO: load through pointer
    }

    fn lower_store<'ctx>(&self, _cmd: &yir::StoreCmd, _td: &mut TransientData<'ctx>) {
        // TODO: store through pointer
    }

    fn lower_binary<'ctx>(&self, cmd: &yir::BinaryCmd, td: &mut TransientData<'ctx>) {
        use inkwell::FloatPredicate;

        // Evaluate operands
        let lhs_val = self.lower_operand(&cmd.lhs, td);
        let rhs_val = self.lower_operand(&cmd.rhs, td);

        // Resulting value after applying the binary op
        let result = match cmd.op {
            yir::BinOp::Add => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td.builder.build_int_add(l, r, "addtmp").unwrap().as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td.builder.build_float_add(l, r, "addtmp").unwrap().as_basic_value_enum(),
                _ => panic!("Unsupported types for Add"),
            },
            yir::BinOp::Sub => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td.builder.build_int_sub(l, r, "subtmp").unwrap().as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td.builder.build_float_sub(l, r, "subtmp").unwrap().as_basic_value_enum(),
                _ => panic!("Unsupported types for Sub"),
            },
            yir::BinOp::Mul => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td.builder.build_int_mul(l, r, "multmp").unwrap().as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td.builder.build_float_mul(l, r, "multmp").unwrap().as_basic_value_enum(),
                _ => panic!("Unsupported types for Mul"),
            },
            yir::BinOp::Div => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                    // Use signed or unsigned division depending on operand type
                    match cmd.lhs.ty() {
                        TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => td.builder.build_int_signed_div(l, r, "divtmp").unwrap().as_basic_value_enum(),
                        TypeInfo::BuiltInPrimitive(PrimitiveType::U64) | TypeInfo::BuiltInPrimitive(PrimitiveType::U8) => td.builder.build_int_unsigned_div(l, r, "divtmp").unwrap().as_basic_value_enum(),
                        _ => panic!("Unsupported integer type for Div"),
                    }
                }
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td.builder.build_float_div(l, r, "divtmp").unwrap().as_basic_value_enum(),
                _ => panic!("Unsupported types for Div"),
            },
            yir::BinOp::Mod => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                    match cmd.lhs.ty() {
                        TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => td.builder.build_int_signed_rem(l, r, "modtmp").unwrap().as_basic_value_enum(),
                        TypeInfo::BuiltInPrimitive(PrimitiveType::U64) | TypeInfo::BuiltInPrimitive(PrimitiveType::U8) => td.builder.build_int_unsigned_rem(l, r, "modtmp").unwrap().as_basic_value_enum(),
                        _ => panic!("Unsupported integer type for Mod"),
                    }
                }
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td.builder.build_float_rem(l, r, "modtmp").unwrap().as_basic_value_enum(),
                _ => panic!("Unsupported types for Mod"),
            },
            // Comparisons -> produce a bool (i1)
            yir::BinOp::Eq | yir::BinOp::NotEq | yir::BinOp::LessThan | yir::BinOp::LessThanEq | yir::BinOp::GreaterThan | yir::BinOp::GreaterThanEq => {
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        let pred = match cmd.op {
                            yir::BinOp::Eq => IntPredicate::EQ,
                            yir::BinOp::NotEq => IntPredicate::NE,
                            yir::BinOp::LessThan => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => IntPredicate::SLT,
                                _ => IntPredicate::ULT,
                            },
                            yir::BinOp::LessThanEq => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => IntPredicate::SLE,
                                _ => IntPredicate::ULE,
                            },
                            yir::BinOp::GreaterThan => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => IntPredicate::SGT,
                                _ => IntPredicate::UGT,
                            },
                            yir::BinOp::GreaterThanEq => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64) | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => IntPredicate::SGE,
                                _ => IntPredicate::UGE,
                            },
                            _ => unreachable!(),
                        };
                        td.builder.build_int_compare(pred, l, r, "cmp_tmp").unwrap().as_basic_value_enum()
                    }
                    (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
                        let pred = match cmd.op {
                            yir::BinOp::Eq => FloatPredicate::OEQ,
                            yir::BinOp::NotEq => FloatPredicate::ONE,
                            yir::BinOp::LessThan => FloatPredicate::OLT,
                            yir::BinOp::LessThanEq => FloatPredicate::OLE,
                            yir::BinOp::GreaterThan => FloatPredicate::OGT,
                            yir::BinOp::GreaterThanEq => FloatPredicate::OGE,
                            _ => unreachable!(),
                        };
                        td.builder.build_float_compare(pred, l, r, "fcmp_tmp").unwrap().as_basic_value_enum()
                    }
                    (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                        // For pointer comparisons, cast to integer then compare
                        let li = td.builder.build_ptr_to_int(lp, td.context.i64_type(), "ptr_to_int_l").unwrap();
                        let ri = td.builder.build_ptr_to_int(rp, td.context.i64_type(), "ptr_to_int_r").unwrap();
                        let pred = match cmd.op {
                            yir::BinOp::Eq => IntPredicate::EQ,
                            yir::BinOp::NotEq => IntPredicate::NE,
                            _ => panic!("Unsupported pointer comparison"),
                        };
                        td.builder.build_int_compare(pred, li, ri, "ptr_cmp").unwrap().as_basic_value_enum()
                    }
                    _ => panic!("Unsupported operand types for comparison"),
                }
            }
        };

        td.var_map.insert(cmd.target, result);
    }

    fn lower_unary<'ctx>(&self, cmd: &yir::UnaryCmd, td: &mut TransientData<'ctx>) {
        // TODO: lower unary ops
    }

    fn lower_call<'ctx>(&self, _cmd: &yir::CallCmd, _td: &mut TransientData<'ctx>) {
        // TODO: lower function call, handle arguments and return value
    }

    fn lower_int_to_ptr<'ctx>(&self, cmd: &yir::IntToPtrCmd, td: &mut TransientData<'ctx>) {
        let int_val = self.lower_operand(&cmd.source, td).into_int_value();
        let int_as_ptr = td.builder.build_int_to_ptr(int_val, td.context.ptr_type(inkwell::AddressSpace::default()), "int_to_ptr").unwrap();
        td.var_map.insert(cmd.target, int_as_ptr.into());
    }

    fn lower_heap_alloc<'ctx>(&self, cmd: &yir::HeapAllocCmd, td: &mut TransientData<'ctx>) {

        let ty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);
        let malloced_ptr = match cmd.count {
            yir::MallocCount::Scalar => {
                let malloced_ptr = td.builder.build_malloc(ty, "heap_alloc").unwrap();
                malloced_ptr
            },
            yir::MallocCount::Multiple(operand) => {
                let count_val = self.lower_operand(&operand, td);
                let malloced_ptr = td.builder.build_array_malloc(ty, count_val.into_int_value(), "heap_alloc").unwrap();
                malloced_ptr
            }
        };

        td.var_map.insert(cmd.target, malloced_ptr.into());     

    }

    fn lower_heap_free<'ctx>(&self, cmd: &yir::HeapFreeCmd, td: &mut TransientData<'ctx>) {
        let ptr_val = self.lower_operand(&cmd.ptr, td);
        let ptr = ptr_val.into_pointer_value();
        td.builder.build_free(ptr).unwrap();
    }

    fn lower_memcpy<'ctx>(&self, cmd: &yir::MemCpyCmd, td: &mut TransientData<'ctx>) {
        let ll_dest = self.lower_operand(&cmd.dest, td);
        let ll_src = self.lower_operand(&cmd.src, td);
        let size_val = self.lower_operand(&cmd.count, td);

        let dest_ptr = ll_dest.into_pointer_value();
        let src_ptr = ll_src.into_pointer_value();
        let size_int = size_val.into_int_value();

        let src_alignment = td.target_data.get_abi_alignment(&ll_src.get_type()) as u32;
        let dest_alignment = td.target_data.get_abi_alignment(&ll_dest.get_type()) as u32;

        td.builder.build_memcpy(dest_ptr, dest_alignment, src_ptr, src_alignment, size_int).unwrap();
    }

    fn lower_memset<'ctx>(&self, cmd: &yir::MemSetCmd, td: &mut TransientData<'ctx>) {
        let ll_dest = self.lower_operand(&cmd.dest, td);
        let ll_value = self.lower_operand(&cmd.value, td);
        let size_val = self.lower_operand(&cmd.count, td);

        let dest_ptr = ll_dest.into_pointer_value();
        let value_int = ll_value.into_int_value();
        let size_int = size_val.into_int_value();

        let dest_alignment = td.target_data.get_abi_alignment(&ll_dest.get_type()) as u32;

        td.builder.build_memset(dest_ptr, dest_alignment, value_int, size_int).unwrap();
    }

    fn lower_get_element_ptr<'ctx>(&self, cmd: &yir::GetElementPtrCmd, td: &mut TransientData<'ctx>) {
        let base_ptr_val = self.lower_operand(&cmd.base, td);
        let base_ptr = base_ptr_val.into_pointer_value();

        // Get the base pointer's pointee type - this is what we're indexing into
        let base_pointee_ty = cmd.base.ty().deref_ptr();
        let base_llvm_type = self.lower_type_basic(td.type_registry, base_pointee_ty, td.context);

        // Lower all indices to LLVM integer values
        let mut llvm_indices = Vec::new();
        for index_op in &cmd.indices {
            let index_val = self.lower_operand(index_op, td);
            let index_int = match index_val {
                BasicValueEnum::IntValue(i) => i,
                _ => panic!("GetElementPtr index must be an integer"),
            };
            llvm_indices.push(index_int);
        }

        // SAFETY: GEP on valid pointer with valid indices
        // Use the base pointer's pointee type for proper LLVM GEP semantics
        let gep_ptr = unsafe {
            td.builder.build_gep(base_llvm_type, base_ptr, &llvm_indices, "gep_tmp").unwrap()
        };

        td.var_map.insert(cmd.target, gep_ptr.into());
    }

    fn lower_kill_set<'ctx>(&self, _cmd: &yir::KillSetCmd, _td: &mut TransientData<'ctx>) {
        unimplemented!("KillSet lowering is not yet implemented");
    }

    fn lower_reinterp<'ctx>(&self, cmd: &yir::ReinterpCmd, td: &mut TransientData<'ctx>) {
        let source_val = self.lower_operand(&cmd.source, td);
        let target_ty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);

        let reinterpreted_ptr = td.builder.build_bit_cast(source_val, target_ty, "reinterp_ptr").unwrap();
        td.var_map.insert(cmd.target, reinterpreted_ptr);
    }

    /// Convert a YIR Operand to an LLVM BasicValueEnum
    fn lower_operand<'ctx>(&self, operand: &yir::Operand, td: &mut TransientData<'ctx>) -> BasicValueEnum<'ctx> {
        match operand {
            yir::Operand::I64Const(v) => BasicValueEnum::IntValue(td.context.i64_type().const_int(*v as u64, true)),
            yir::Operand::U64Const(v) => BasicValueEnum::IntValue(td.context.i64_type().const_int(*v, false)),
            yir::Operand::F32Const(v) => BasicValueEnum::FloatValue(td.context.f32_type().const_float(*v as f64)),
            yir::Operand::F64Const(v) => BasicValueEnum::FloatValue(td.context.f64_type().const_float(*v)),
            yir::Operand::BoolConst(v) => BasicValueEnum::IntValue(td.context.bool_type().const_int(if *v { 1 } else { 0 }, false)),
            yir::Operand::Variable(var) => {
                // Variable is a pointer to a stack slot OR it is already a value by LLVM
                let ptr = td.var_map.get(var)
                    .expect("ICE: Variable not found in var_map during operand lowering");
                *ptr
            }
            yir::Operand::NoOp => panic!("ICE: Cannot lower NoOp operand to LLVM value"),
        }
    }
}
