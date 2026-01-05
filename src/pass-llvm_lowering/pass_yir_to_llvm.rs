use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::TargetData;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
};
use std::path::Path;

use crate::pass_yir_lowering::yir;
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
    pub target_machine: TargetMachine,
    /// Current function being lowered
    pub function: Option<FunctionValue<'ctx>>,
    /// Map YIR Variables -> LLVM pointer values (allocated stack slots / locals)
    pub var_map: FastHashMap<yir::Variable, BasicValueEnum<'ctx>>,
    /// Optional mapping from labels to LLVM basic blocks created during lowering
    pub block_map: FastHashMap<i64, inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> TransientData<'ctx> {
    pub fn new(context: &'ctx Context, name: &str, type_registry: &'ctx TypeRegistry) -> Self {
        // Initialize native target to ensure we get correct layout
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");

        let module = context.create_module(name);

        // Create target machine to get correct data layout
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        let target_data = target_machine.get_target_data();
        module.set_data_layout(&target_data.get_data_layout());

        let data_layout_str = target_data
            .get_data_layout()
            .as_str()
            .to_str()
            .unwrap()
            .to_string();
        println!("LLVM Module Data Layout: {}", data_layout_str);

        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            type_registry,
            target_data,
            target_machine,
            function: None,
            var_map: FastHashMap::default(),
            block_map: FastHashMap::default(),
        }
    }
}

pub struct LLVMLowerer;

impl LLVMLowerer {
    pub fn new() -> Self {
        Self
    }

    pub fn lower_type_basic<'ctx>(
        &self,
        type_registry: &TypeRegistry,
        ty: &'static TypeInfo,
        context: &'ctx Context,
    ) -> BasicTypeEnum<'ctx> {
        match ty {
            TypeInfo::BuiltInPrimitive(prim) => match prim {
                PrimitiveType::I8 => BasicTypeEnum::IntType(context.i8_type()),
                PrimitiveType::U8 => BasicTypeEnum::IntType(context.i8_type()),
                PrimitiveType::I64 => BasicTypeEnum::IntType(context.i64_type()),
                PrimitiveType::U64 => BasicTypeEnum::IntType(context.i64_type()),
                PrimitiveType::F32 => BasicTypeEnum::FloatType(context.f32_type()),
                PrimitiveType::F64 => BasicTypeEnum::FloatType(context.f64_type()),
                PrimitiveType::Bool => BasicTypeEnum::IntType(context.bool_type()),
                PrimitiveType::Nil => {
                    panic!("Nil type does not have a BasicTypeEnum representation")
                }
            },
            TypeInfo::Pointer(_) => {
                BasicTypeEnum::PointerType(context.ptr_type(inkwell::AddressSpace::default()))
            }
            TypeInfo::Struct(s) => {
                let resolved = type_registry
                    .resolve_struct(s.name)
                    .expect("Compiler Bug: Unknown struct type during LLVM lowering");
                let fields = resolved
                    .fields
                    .iter()
                    .map(|f| {
                        let llvm_field_ty = self.lower_type_basic(type_registry, f.1.ty, context);
                        llvm_field_ty
                    })
                    .collect::<Vec<BasicTypeEnum>>();
                BasicTypeEnum::StructType(context.struct_type(&fields, false))
            }
            _ => panic!("Unsupported type for LLVM lowering to BasicTypeEnum"),
        }
    }

    pub fn lower_type<'ctx>(
        &self,
        type_registry: &TypeRegistry,
        ty: &'static TypeInfo,
        context: &'ctx Context,
    ) -> AnyTypeEnum<'ctx> {
        match ty {
            TypeInfo::BuiltInPrimitive(prim) => match prim {
                PrimitiveType::I8 => AnyTypeEnum::IntType(context.i8_type()),
                PrimitiveType::U8 => AnyTypeEnum::IntType(context.i8_type()),
                PrimitiveType::I64 => AnyTypeEnum::IntType(context.i64_type()),
                PrimitiveType::U64 => AnyTypeEnum::IntType(context.i64_type()),
                PrimitiveType::F32 => AnyTypeEnum::FloatType(context.f32_type()),
                PrimitiveType::F64 => AnyTypeEnum::FloatType(context.f64_type()),
                PrimitiveType::Bool => AnyTypeEnum::IntType(context.bool_type()),
                PrimitiveType::Nil => AnyTypeEnum::VoidType(context.void_type()),
            },
            TypeInfo::Pointer(_) => {
                AnyTypeEnum::PointerType(context.ptr_type(inkwell::AddressSpace::default()))
            }
            TypeInfo::Struct(s) => {
                let resolved = type_registry
                    .resolve_struct(s.name)
                    .expect("Compiler Bug: Unknown struct type during LLVM lowering");
                let fields = resolved
                    .fields
                    .iter()
                    .map(|f| {
                        let llvm_field_ty = self.lower_type_basic(type_registry, f.1.ty, context);
                        llvm_field_ty
                    })
                    .collect::<Vec<BasicTypeEnum>>();
                AnyTypeEnum::StructType(context.struct_type(&fields, false))
            }
            _ => panic!("Unsupported type for LLVM lowering"),
        }
    }

    /// Generate LLVM IR from YIR module and return as string
    pub fn lower_module_to_ir(
        &self,
        module: &yir::Module,
        type_registry: &TypeRegistry,
    ) -> Result<String, String> {
        let context = Context::create();
        let mut td = TransientData::new(&context, "yuu_module", type_registry);

        // Generate helper functions once at module level
        self.generate_template_fill_helper(&mut td);

        // Two-pass approach to support recursive and mutually recursive functions:
        // Pass 1: Declare all functions (create function signatures)
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                    // They should be resolved when we encounter the definition
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.declare_function(function, &mut td);
                }
            }
        }

        // Pass 2: Lower function bodies
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.lower_function_body(function, &mut td);
                }
            }
        }

        // Verify the module
        if let Err(msg) = td.module.verify() {
            return Err(format!("LLVM module verification failed: {}", msg));
        }

        Ok(td.module.print_to_string().to_string())
    }

    /// JIT compile and execute a function
    pub fn jit_execute_main(
        &self,
        module: &yir::Module,
        type_registry: &TypeRegistry,
    ) -> Result<i32, String> {
        let context = Context::create();
        let mut td = TransientData::new(&context, "yuu_module", type_registry);

        // Generate helper functions once at module level
        self.generate_template_fill_helper(&mut td);

        // Two-pass approach to support recursive and mutually recursive functions:
        // Pass 1: Declare all functions (create function signatures)
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.declare_function(function, &mut td);
                }
            }
        }

        // Pass 2: Lower function bodies
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.lower_function_body(function, &mut td);
                }
            }
        }

        // Verify the module
        if let Err(msg) = td.module.verify() {
            return Err(format!("LLVM module verification failed: {}", msg));
        }

        // Create JIT execution engine
        let execution_engine = td
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| format!("Failed to create JIT execution engine: {}", e))?;

        // Check that main function exists
        let _main_fn = td
            .module
            .get_function("main")
            .ok_or_else(|| "No main function found for JIT execution".to_string())?;

        // Get the JIT compiled function
        unsafe {
            let main_jit: JitFunction<unsafe extern "C" fn() -> i64> = execution_engine
                .get_function("main")
                .map_err(|e| format!("Failed to get JIT function: {}", e))?;

            // Execute the function
            let result = main_jit.call();
            Ok(result as i32)
        }
    }

    /// AOT compile to object file
    pub fn aot_compile(
        &self,
        module: &yir::Module,
        type_registry: &TypeRegistry,
        output_path: &Path,
    ) -> Result<(), String> {
        let context = Context::create();
        let mut td = TransientData::new(&context, "yuu_module", type_registry);

        // Generate helper functions once at module level
        self.generate_template_fill_helper(&mut td);

        // Two-pass approach to support recursive and mutually recursive functions:
        // Pass 1: Declare all functions (create function signatures)
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.declare_function(function, &mut td);
                }
            }
        }

        // Pass 2: Lower function bodies
        for (_name, fds) in &module.functions {
            match fds {
                yir::FunctionDeclarationState::Declared(_function) => {
                    // For now, we don't need to handle forward declarations
                }
                yir::FunctionDeclarationState::Defined(function) => {
                    self.lower_function_body(function, &mut td);
                }
            }
        }

        // Verify the module
        if let Err(msg) = td.module.verify() {
            return Err(format!("LLVM module verification failed: {}", msg));
        }

        // Initialize LLVM targets
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to initialize native target: {}", e))?;

        // Get the target triple for the current host
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("Failed to get target from triple: {}", e))?;

        // Create target machine
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| "Failed to create target machine".to_string())?;

        // Compile to object file
        target_machine
            .write_to_file(&td.module, FileType::Object, output_path)
            .map_err(|e| format!("Failed to write object file: {}", e))?;

        println!(
            "Successfully compiled to object file: {}",
            output_path.display()
        );
        Ok(())
    }

    /// Generate (or retrieve) a helper function for filling arrays with a template value using memcpy
    /// Equivalent to C's __yuu_template_fill: void __yuu_template_fill(void* dest, const void* template_val, size_t element_size, size_t count, u32 dest_align, u32 src_align)
    fn generate_template_fill_helper<'ctx>(
        &'ctx self,
        td: &'ctx mut TransientData,
    ) -> FunctionValue<'ctx> {
        let name = "__yuu_template_fill";
        if let Some(existing) = td.module.get_function(name) {
            return existing;
        }

        // Use i64 for sizes - we only target 64-bit systems
        let i8_ptr_ty = td.context.ptr_type(inkwell::AddressSpace::default());
        let i64_ty = td.context.i64_type();
        let u32_ty = td.context.i32_type();

        // Define: void __yuu_template_fill(i8* dest, i8* template_val, i64 element_size, i64 count, u32 dest_align, u32 src_align)
        let fn_type = td.context.void_type().fn_type(
            &[
                i8_ptr_ty.into(),
                i8_ptr_ty.into(),
                i64_ty.into(),
                i64_ty.into(),
                u32_ty.into(), // dest_align
                u32_ty.into(), // src_align
            ],
            false,
        );
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
                elem_size,
            )
            .unwrap();

        let one = i64_ty.const_int(1, false);
        let next_idx = td.builder.build_int_add(idx_val, one, "next_idx").unwrap();
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

    /// Declare a function (create its signature in the LLVM module)
    /// This must be called before lowering function bodies to support recursive calls
    pub fn declare_function(&self, func: &yir::Function, td: &mut TransientData) {
        // Pragmatic C-like ABI approach:
        // - Primitives (int, float, bool) and pointers: pass directly
        // - Structs: pass by pointer (caller allocates, passes pointer)
        // - Return values: primitives/pointers directly, structs via hidden sret parameter
        // This avoids complex platform-specific ABI rules until we can use LLVM's ABI library.

        let llvm_ret_type = self.lower_type(td.type_registry, func.return_type, td.context);

        let llvm_param_types: Vec<_> = func
            .params
            .iter()
            .map(|param| {
                // YIR parameters are always pointer types (representing stack storage)
                // Extract the underlying value type for the LLVM function signature
                self.lower_type_basic(td.type_registry, param.ty().deref_ptr(), td.context)
            })
            .collect();

        let llvm_param_metadata_types: Vec<BasicMetadataTypeEnum> =
            llvm_param_types.iter().map(|ty| (*ty).into()).collect();

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

        let _llvm_fn = td.module.add_function(&func.name, fn_type, None);
    }

    /// Lower the body of a function (assumes the function has already been declared)
    pub fn lower_function_body(&self, func: &yir::Function, td: &mut TransientData) {
        // Get the function that was already declared
        let llvm_fn = td
            .module
            .get_function(&func.name)
            .expect("ICE: Function should have been declared before lowering body");

        td.function = Some(llvm_fn);

        // Get the parameter types again (we need them for allocas)
        let llvm_param_types: Vec<_> = func
            .params
            .iter()
            .map(|param| {
                self.lower_type_basic(td.type_registry, param.ty().deref_ptr(), td.context)
            })
            .collect();

        // Create LLVM basic blocks for each YIR block and record in block_map
        for (id, yir_block) in func.blocks.iter() {
            let name = format!("bb_{}", yir_block.label.name());
            let llvm_bb = td.context.append_basic_block(llvm_fn, &name);
            td.block_map.insert(*id, llvm_bb);
        }

        // Create allocas for function parameters and map them to var_map
        // YIR parameters are value types, but we store them in stack allocas
        // and map the YIR variable to the alloca pointer
        let entry_llvm_bb = *td.block_map.get(&func.entry_block).unwrap();
        td.builder.position_at_end(entry_llvm_bb);

        let llvm_params = llvm_fn.get_param_iter();
        for ((yir_param, param_ty), param_value) in func
            .params
            .iter()
            .zip(llvm_param_types.iter())
            .zip(llvm_params)
        {
            // Create alloca for the parameter value
            let alloca = td
                .builder
                .build_alloca(*param_ty, yir_param.name())
                .unwrap();
            // Store the incoming parameter value into the alloca
            let _ = td.builder.build_store(alloca, param_value).unwrap();
            // Map the YIR parameter variable to the alloca pointer
            // This means when we reference this parameter in YIR, we'll load from this alloca
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
            self.lower_terminator(&yir_block.terminator, td);
        }
    }

    /// Dispatch lowering for a single instruction. These are stubs that should be
    /// filled in with proper lowering logic.
    fn lower_instruction<'ctx>(&self, instr: &yir::Instruction, td: &mut TransientData<'ctx>) {
        match instr {
            yir::Instruction::Alloca(cmd) => self.lower_alloca(cmd, td),
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

    fn lower_terminator<'ctx>(&self, terminator: &yir::ControlFlow, td: &mut TransientData<'ctx>) {
        match terminator {
            yir::ControlFlow::Jump { target } => {
                // Emit unconditional branch to the target block
                if let Some(target_bb) = td.block_map.get(&target.id()) {
                    let _ = td.builder.build_unconditional_branch(*target_bb);
                }
            }
            yir::ControlFlow::Branch {
                condition,
                if_true,
                if_false,
            } => {
                // Evaluate the branch condition
                let condition_value = self.lower_operand(condition, td);
                let condition_i1 = match condition_value {
                    BasicValueEnum::IntValue(int_val) => {
                        // Convert to i1 if it's not already
                        if int_val.get_type().get_bit_width() == 1 {
                            int_val
                        } else {
                            // Compare with zero to get i1
                            let zero = int_val.get_type().const_zero();
                            td.builder
                                .build_int_compare(IntPredicate::NE, int_val, zero, "tobool")
                                .unwrap()
                        }
                    }
                    _ => panic!("Branch condition must be an integer value"),
                };

                if let Some(true_bb) = td.block_map.get(&if_true.id()) {
                    if let Some(false_bb) = td.block_map.get(&if_false.id()) {
                        let _ =
                            td.builder
                                .build_conditional_branch(condition_i1, *true_bb, *false_bb);
                    }
                }
            }
            yir::ControlFlow::JumpTable {
                scrutinee,
                jump_targets,
                default,
            } => {
                // Lower the scrutinee (switch value)
                let scrutinee_value = self.lower_operand(scrutinee, td).into_int_value();

                // Get the default block
                let default_bb = if let Some(default_label) = default {
                    *td.block_map
                        .get(&default_label.id())
                        .expect("ICE: Default block not found in block_map")
                } else {
                    // If no default provided, create an unreachable block
                    let unreachable_bb = td
                        .context
                        .append_basic_block(td.function.unwrap(), "unreachable");
                    unreachable_bb
                };

                // Build the switch instruction cases
                let mut switch_cases = Vec::new();
                for (variant_index, target_label) in jump_targets {
                    let case_value = td.context.i64_type().const_int(*variant_index, false);
                    let target_bb = *td
                        .block_map
                        .get(&target_label.id())
                        .expect("ICE: Jump target block not found in block_map");
                    switch_cases.push((case_value, target_bb));
                }

                let _switch_instr = td
                    .builder
                    .build_switch(scrutinee_value, default_bb, &switch_cases)
                    .unwrap();

                // If we created an unreachable block, fill it
                if default.is_none() {
                    let current_bb = td.builder.get_insert_block().unwrap();
                    td.builder.position_at_end(default_bb);
                    td.builder.build_unreachable().unwrap();
                    td.builder.position_at_end(current_bb);
                }
            }
            yir::ControlFlow::Return(opt) => {
                // TODO: Lower return value properly according to function return type
                match opt {
                    Some(return_operand) => {
                        // Lower the return value and return it
                        let return_value = self.lower_operand(return_operand, td);
                        let _ = td.builder.build_return(Some(&return_value));
                    }
                    None => {
                        // Void return
                        let _ = td.builder.build_return(None);
                    }
                }
            }
            yir::ControlFlow::Unterminated => {
                td.builder.build_unreachable().unwrap();
            }
        }
    }

    // --- Instruction lowering stubs ---
    fn lower_alloca<'ctx>(&self, cmd: &yir::AllocaCmd, td: &mut TransientData<'ctx>) {
        let elem_ty = cmd.target.ty().deref_ptr();
        let lty = self.lower_type_basic(td.type_registry, elem_ty, td.context);
        let allocd_stack_ptr = if cmd.count == 1 {
            let alloced = td.builder.build_alloca(lty, cmd.target.name()).unwrap();
            alloced
        } else {
            let alloced = td
                .builder
                .build_array_alloca(
                    lty,
                    td.context.i64_type().const_int(cmd.count as u64, false),
                    cmd.target.name(),
                )
                .unwrap();
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
                        td.builder
                            .build_int_mul(elem_size, count_val, "total_size")
                            .unwrap()
                    };
                    // Alignment: use the specified alignment or get LLVM's ABI alignment
                    let alignment = cmd
                        .align
                        .unwrap_or_else(|| td.target_data.get_abi_alignment(&lty) as u64)
                        as u32;
                    td.builder
                        .build_memset(allocd_stack_ptr, alignment, zero, total_size)
                        .unwrap();
                }
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
                                    td.builder
                                        .build_gep(
                                            lty,
                                            allocd_stack_ptr,
                                            &[td.context.i64_type().const_zero(), index],
                                            "elem_ptr",
                                        )
                                        .unwrap()
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
                        let dest_align_val =
                            td.context.i32_type().const_int(elem_align as u64, false);
                        let src_align_val =
                            td.context.i32_type().const_int(elem_align as u64, false);
                        let args = vec![
                            dest_ptr.into(),
                            template_ptr.into(),
                            elem_size.into(),
                            count_val.into(),
                            dest_align_val.into(),
                            src_align_val.into(),
                        ];
                        td.builder
                            .build_call(helper_fn, &args, "template_fill_call")
                            .unwrap();
                    }
                }

                yir::ArrayInit::Elements(operands) => {
                    // Store each element at its index
                    for (i, operand) in operands.iter().enumerate() {
                        let val = self.lower_operand(operand, td);
                        let index = td.context.i64_type().const_int(i as u64, false);
                        // SAFETY: GEP on array allocation
                        let elem_ptr = unsafe {
                            td.builder
                                .build_gep(
                                    lty,
                                    allocd_stack_ptr,
                                    &[td.context.i64_type().const_zero(), index],
                                    "elem_ptr",
                                )
                                .unwrap()
                        };
                        td.builder.build_store(elem_ptr, val).unwrap();
                    }
                }
            }
        }
    }

    fn lower_load<'ctx>(&self, cmd: &yir::LoadCmd, td: &mut TransientData<'ctx>) {
        let ptr_value = self.lower_operand(&cmd.source, td);
        let ptr = ptr_value.into_pointer_value();

        // Get the pointee type for the load
        let pointee_ty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);

        let loaded_value = td
            .builder
            .build_load(pointee_ty, ptr, &format!("{}_loaded", cmd.target.name()))
            .unwrap();
        td.var_map.insert(cmd.target, loaded_value);
    }

    fn lower_store<'ctx>(&self, cmd: &yir::StoreCmd, td: &mut TransientData<'ctx>) {
        let dest_ptr = self.lower_operand(&cmd.dest, td).into_pointer_value();
        let value = self.lower_operand(&cmd.value, td);

        td.builder.build_store(dest_ptr, value).unwrap();
        // Note: No var_map update - this is a side-effect only instruction
    }

    fn lower_binary<'ctx>(&self, cmd: &yir::BinaryCmd, td: &mut TransientData<'ctx>) {
        use inkwell::FloatPredicate;

        // Evaluate operands
        let lhs_val = self.lower_operand(&cmd.lhs, td);
        let rhs_val = self.lower_operand(&cmd.rhs, td);

        // Resulting value after applying the binary op
        let result = match cmd.op {
            yir::BinOp::Add => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td
                    .builder
                    .build_int_add(l, r, "addtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td
                    .builder
                    .build_float_add(l, r, "addtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!("Unsupported types for Add"),
            },
            yir::BinOp::Sub => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td
                    .builder
                    .build_int_sub(l, r, "subtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td
                    .builder
                    .build_float_sub(l, r, "subtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!("Unsupported types for Sub"),
            },
            yir::BinOp::Mul => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => td
                    .builder
                    .build_int_mul(l, r, "multmp")
                    .unwrap()
                    .as_basic_value_enum(),
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td
                    .builder
                    .build_float_mul(l, r, "multmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!("Unsupported types for Mul"),
            },
            yir::BinOp::Div => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                    // Use signed or unsigned division depending on operand type
                    match cmd.lhs.ty() {
                        TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                        | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => td
                            .builder
                            .build_int_signed_div(l, r, "divtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        TypeInfo::BuiltInPrimitive(PrimitiveType::U64)
                        | TypeInfo::BuiltInPrimitive(PrimitiveType::U8) => td
                            .builder
                            .build_int_unsigned_div(l, r, "divtmp")
                            .unwrap()
                            .as_basic_value_enum(),
                        _ => panic!("Unsupported integer type for Div"),
                    }
                }
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td
                    .builder
                    .build_float_div(l, r, "divtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!("Unsupported types for Div"),
            },
            yir::BinOp::Mod => match (lhs_val, rhs_val) {
                (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match cmd.lhs.ty() {
                    TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                    | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => td
                        .builder
                        .build_int_signed_rem(l, r, "modtmp")
                        .unwrap()
                        .as_basic_value_enum(),
                    TypeInfo::BuiltInPrimitive(PrimitiveType::U64)
                    | TypeInfo::BuiltInPrimitive(PrimitiveType::U8) => td
                        .builder
                        .build_int_unsigned_rem(l, r, "modtmp")
                        .unwrap()
                        .as_basic_value_enum(),
                    _ => panic!("Unsupported integer type for Mod"),
                },
                (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => td
                    .builder
                    .build_float_rem(l, r, "modtmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!("Unsupported types for Mod"),
            },
            // Comparisons -> produce a bool (i1)
            yir::BinOp::Eq
            | yir::BinOp::NotEq
            | yir::BinOp::LessThan
            | yir::BinOp::LessThanEq
            | yir::BinOp::GreaterThan
            | yir::BinOp::GreaterThanEq => {
                match (lhs_val, rhs_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
                        let pred = match cmd.op {
                            yir::BinOp::Eq => IntPredicate::EQ,
                            yir::BinOp::NotEq => IntPredicate::NE,
                            yir::BinOp::LessThan => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                                | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => {
                                    IntPredicate::SLT
                                }
                                _ => IntPredicate::ULT,
                            },
                            yir::BinOp::LessThanEq => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                                | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => {
                                    IntPredicate::SLE
                                }
                                _ => IntPredicate::ULE,
                            },
                            yir::BinOp::GreaterThan => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                                | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => {
                                    IntPredicate::SGT
                                }
                                _ => IntPredicate::UGT,
                            },
                            yir::BinOp::GreaterThanEq => match cmd.lhs.ty() {
                                TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                                | TypeInfo::BuiltInPrimitive(PrimitiveType::I8) => {
                                    IntPredicate::SGE
                                }
                                _ => IntPredicate::UGE,
                            },
                            _ => unreachable!(),
                        };
                        td.builder
                            .build_int_compare(pred, l, r, "cmp_tmp")
                            .unwrap()
                            .as_basic_value_enum()
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
                        td.builder
                            .build_float_compare(pred, l, r, "fcmp_tmp")
                            .unwrap()
                            .as_basic_value_enum()
                    }
                    (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                        // For pointer comparisons, cast to integer then compare
                        let li = td
                            .builder
                            .build_ptr_to_int(lp, td.context.i64_type(), "ptr_to_int_l")
                            .unwrap();
                        let ri = td
                            .builder
                            .build_ptr_to_int(rp, td.context.i64_type(), "ptr_to_int_r")
                            .unwrap();
                        let pred = match cmd.op {
                            yir::BinOp::Eq => IntPredicate::EQ,
                            yir::BinOp::NotEq => IntPredicate::NE,
                            _ => panic!("Unsupported pointer comparison"),
                        };
                        td.builder
                            .build_int_compare(pred, li, ri, "ptr_cmp")
                            .unwrap()
                            .as_basic_value_enum()
                    }
                    _ => panic!("Unsupported operand types for comparison"),
                }
            }
        };

        td.var_map.insert(cmd.target, result);
    }

    fn lower_unary<'ctx>(&self, cmd: &yir::UnaryCmd, td: &mut TransientData<'ctx>) {
        let operand_value = self.lower_operand(&cmd.operand, td);

        let result = match cmd.op {
            yir::UnaryOp::Neg => match operand_value {
                BasicValueEnum::IntValue(int_val) => td
                    .builder
                    .build_int_neg(int_val, "neg_tmp")
                    .unwrap()
                    .as_basic_value_enum(),
                BasicValueEnum::FloatValue(float_val) => td
                    .builder
                    .build_float_neg(float_val, "neg_tmp")
                    .unwrap()
                    .as_basic_value_enum(),
                _ => panic!(
                    "Unsupported type for negation: {:?}",
                    operand_value.get_type()
                ),
            },
        };

        td.var_map.insert(cmd.target, result);
    }

    fn lower_call<'ctx>(&self, cmd: &yir::CallCmd, td: &mut TransientData<'ctx>) {
        // Get the function from the module
        let callee_fn = td.module.get_function(&cmd.name).unwrap_or_else(|| {
            panic!(
                "ICE: Function '{}' not found in module during call lowering",
                cmd.name
            )
        });

        // Lower all arguments
        let args: Vec<BasicMetadataValueEnum> = cmd
            .args
            .iter()
            .map(|arg| self.lower_operand(arg, td).into())
            .collect();

        // Build the call instruction
        let call_result = td
            .builder
            .build_call(callee_fn, &args, &format!("{}_call", cmd.name))
            .unwrap();

        // If there's a target variable, store the return value
        if let Some(target) = cmd.target {
            let return_value = call_result.try_as_basic_value().expect_basic("ehm wud?");
            td.var_map.insert(target, return_value);
        }
        // If target is None, this is a void function call (no return value to store)
    }

    fn lower_int_to_ptr<'ctx>(&self, cmd: &yir::IntToPtrCmd, td: &mut TransientData<'ctx>) {
        let int_val = self.lower_operand(&cmd.source, td).into_int_value();
        let int_as_ptr = td
            .builder
            .build_int_to_ptr(
                int_val,
                td.context.ptr_type(inkwell::AddressSpace::default()),
                "int_to_ptr",
            )
            .unwrap();
        td.var_map.insert(cmd.target, int_as_ptr.into());
    }

    fn lower_heap_alloc<'ctx>(&self, cmd: &yir::HeapAllocCmd, td: &mut TransientData<'ctx>) {
        let ty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);
        let malloced_ptr = match cmd.count {
            yir::MallocCount::Scalar => {
                let malloced_ptr = td.builder.build_malloc(ty, "heap_alloc").unwrap();
                malloced_ptr
            }
            yir::MallocCount::Multiple(operand) => {
                let count_val = self.lower_operand(&operand, td);
                let malloced_ptr = td
                    .builder
                    .build_array_malloc(ty, count_val.into_int_value(), "heap_alloc")
                    .unwrap();
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

        td.builder
            .build_memcpy(dest_ptr, dest_alignment, src_ptr, src_alignment, size_int)
            .unwrap();
    }

    fn lower_memset<'ctx>(&self, cmd: &yir::MemSetCmd, td: &mut TransientData<'ctx>) {
        let ll_dest = self.lower_operand(&cmd.dest, td);
        let ll_value = self.lower_operand(&cmd.value, td);
        let size_val = self.lower_operand(&cmd.count, td);

        let dest_ptr = ll_dest.into_pointer_value();
        let value_int = ll_value.into_int_value();
        let size_int = size_val.into_int_value();

        let dest_alignment = td.target_data.get_abi_alignment(&ll_dest.get_type()) as u32;

        td.builder
            .build_memset(dest_ptr, dest_alignment, value_int, size_int)
            .unwrap();
    }

    fn lower_get_element_ptr<'ctx>(
        &self,
        cmd: &yir::GetElementPtrCmd,
        td: &mut TransientData<'ctx>,
    ) {
        let base_ptr_val = self.lower_operand(&cmd.base, td);
        let base_ptr = base_ptr_val.into_pointer_value();

        // Get the base pointer's pointee type - this is what we're indexing into
        let base_pointee_ty = cmd.base.ty().deref_ptr();
        let base_llvm_type = self.lower_type_basic(td.type_registry, base_pointee_ty, td.context);

        // Lower all indices to LLVM integer values
        let mut llvm_indices = Vec::new();
        for index_op in &cmd.indices {
            let index_int = match index_op {
                yir::GEPIndex::StructIndex(i) => td.context.i32_type().const_int(*i as u64, false),
                yir::GEPIndex::ArrayIndex(op) => {
                    let val = self.lower_operand(op, td);
                    match val {
                        BasicValueEnum::IntValue(i) => i,
                        _ => panic!("GetElementPtr array index must be an integer"),
                    }
                }
            };
            llvm_indices.push(index_int);
        }

        // SAFETY: GEP on valid pointer with valid indices
        // Use the base pointer's pointee type for proper LLVM GEP semantics
        let gep_ptr = unsafe {
            td.builder
                .build_gep(base_llvm_type, base_ptr, &llvm_indices, "gep_tmp")
                .unwrap()
        };

        td.var_map.insert(cmd.target, gep_ptr.into());
    }

    fn lower_kill_set<'ctx>(&self, _cmd: &yir::KillSetCmd, _td: &mut TransientData<'ctx>) {
        unimplemented!("KillSet lowering is not yet implemented");
    }

    fn lower_reinterp<'ctx>(&self, cmd: &yir::ReinterpCmd, td: &mut TransientData<'ctx>) {
        let source_val = self.lower_operand(&cmd.source, td);
        let target_ty = self.lower_type_basic(td.type_registry, cmd.target.ty(), td.context);

        let reinterpreted_ptr = td
            .builder
            .build_bit_cast(source_val, target_ty, "reinterp_ptr")
            .unwrap();
        td.var_map.insert(cmd.target, reinterpreted_ptr);
    }

    /// Convert a YIR Operand to an LLVM BasicValueEnum
    fn lower_operand<'ctx>(
        &self,
        operand: &yir::Operand,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match operand {
            yir::Operand::I64Const(v) => {
                BasicValueEnum::IntValue(td.context.i64_type().const_int(*v as u64, true))
            }
            yir::Operand::U64Const(v) => {
                BasicValueEnum::IntValue(td.context.i64_type().const_int(*v, false))
            }
            yir::Operand::F32Const(v) => {
                BasicValueEnum::FloatValue(td.context.f32_type().const_float(*v as f64))
            }
            yir::Operand::F64Const(v) => {
                BasicValueEnum::FloatValue(td.context.f64_type().const_float(*v))
            }
            yir::Operand::BoolConst(v) => BasicValueEnum::IntValue(
                td.context
                    .bool_type()
                    .const_int(if *v { 1 } else { 0 }, false),
            ),
            yir::Operand::Variable(var) => {
                // Variable is a pointer to a stack slot OR it is already a value by LLVM
                let ptr = td
                    .var_map
                    .get(var)
                    .expect("ICE: Variable not found in var_map during operand lowering");
                *ptr
            }
            yir::Operand::NoOp => panic!("ICE: Cannot lower NoOp operand to LLVM value"),
        }
    }
}
