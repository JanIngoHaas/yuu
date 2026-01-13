// use cranelift_codegen::ir::{self as ir, InstBuilder, Signature, StackSlotData, StackSlotKind, UserFuncName};
// use cranelift_codegen::settings::Configurable;
// use cranelift_codegen::{isa, settings};
// use cranelift_module::{Module};
// use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
// use target_lexicon::Triple;
// use ustr::Ustr;

// use crate::pass_yir_lowering::{Module as YirModule, Function, BasicBlock, Instruction, ControlFlow, Operand, Variable, Label, UnaryOp,
//     AllocaCmd, StoreImmediateCmd, TakeAddressCmd, GetFieldPtrCmd, LoadCmd, StoreCmd, BinaryCmd, UnaryCmd, CallCmd,
//     GetVariantDataPtrCmd, LoadActiveVariantIdxCmd, StoreActiveVariantIdxCmd, IntToPtrCmd, HeapAllocCmd, HeapFreeCmd,
//     MemCpyCmd, MemSetCmd, GetElementPtrCmd, KillSetCmd};
// use crate::utils::type_info_table::TypeInfo;
// use crate::utils::collections::{FastHashMap, IndexMap};
// use crate::utils::c_packing::{calculate_type_layout, calculate_struct_layout};
// use crate::utils::{TypeRegistry, calculate_enum_layout};

// struct TransientData<'a> {
//     builder: FunctionBuilder<'a>,
//     isa: &'a dyn cranelift_codegen::isa::TargetIsa,
//     values: FastHashMap<Variable, cranelift_codegen::ir::Value>,
//     type_registry: &'a TypeRegistry,
//     blocks: IndexMap<Label, cranelift_codegen::ir::Block>,
//     malloc_ref: ir::FuncRef,
//     free_ref: ir::FuncRef,
//     module: &'a mut Module,
//     func_refs: FastHashMap<Ustr, ir::FuncRef>,
//     func: &'a Function,
// }

// pub struct CraneliftLowering;

// impl Default for CraneliftLowering {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl CraneliftLowering {
//     pub fn new() -> Self {
//         Self
//     }

//     pub fn run(&self, module: &YirModule, type_registry: &TypeRegistry) {
//         todo!()
//     }

//     fn lower_function(&self, func: &Function, ctxt: &mut FunctionBuilderContext, type_registry: &TypeRegistry) -> Result<(), String> {
        
//         // Create native ISA
//         let builder = settings::builder();
//         let triple = Triple::host();
//         //builder.set("opt_level", "speed").map_err(|e| format!("Settings error: {}", e))?;
//         let isa = isa::lookup(triple)
//             .map_err(|e| format!("ISA lookup failed: {}", e))?
//             .finish(settings::Flags::new(builder))
//             .map_err(|e| format!("ISA creation failed: {}", e))?;

//         // Choose calling conventions
//         let cc = isa.default_call_conv();  // System ABI for extern C
//         let sig = Signature::new(cc);

//         let (ns, idx) = func.split_ast_id();
//         let mut clfunc = CraneliftFunction::with_name_signature(UserFuncName::user(ns, idx), sig);

//         // Create a JIT module and declare imports for malloc/free
//         let mut jit_builder = JITBuilder::new(cranelift_module::default_libcall_names());
//         let mut module = JITModule::new(jit_builder);

//         // malloc signature: (size_t) -> void*
//         let mut malloc_sig = module.make_signature();
//         malloc_sig.params.push(ir::AbiParam::new(module.target_config().pointer_type()));
//         malloc_sig.returns.push(ir::AbiParam::new(module.target_config().pointer_type()));
//         let malloc_id = module.declare_function("malloc", Linkage::Import, &malloc_sig)
//             .map_err(|e| format!("Declaring malloc failed: {}", e))?;

//         // free signature: (void*) -> void
//         let mut free_sig = module.make_signature();
//         free_sig.params.push(ir::AbiParam::new(module.target_config().pointer_type()));
//         let free_id = module.declare_function("free", Linkage::Import, &free_sig)
//             .map_err(|e| format!("Declaring free failed: {}", e))?;

//         // Import the function declarations into this cranelift function
//         let malloc_ref = module.declare_func_in_func(malloc_id, &mut clfunc);
//         let free_ref = module.declare_func_in_func(free_id, &mut clfunc);

//         let builder = FunctionBuilder::new(&mut clfunc, ctxt);

//         // Now pre-declare the blocks
//         let blocks = func.blocks.iter().map(|(_idx, _b)| {
//             builder.create_block()
//         }).collect::<Vec<_>>();

//         let mut data = TransientData {
//             builder,
//             isa: isa.as_ref(),
//             values: FastHashMap::default(),
//             type_registry,
//             blocks,
//             malloc_ref,
//             free_ref,
//             module: &mut module,
//             func_refs: FastHashMap::default(),
//             func,
//         };

//         // Pre-declare vars

//         let mut cl_vars = FastHashMap::default();
//         for var in func.calculate_var_decls() {
//             let ty = self.type_to_cranelift(&var.ty(), &data);
//             if let Some(ty) = ty {
//                 let cl_var = data.builder.declare_var(ty);
//                 cl_vars.insert(*var, cl_var);
//             }
//         }

//         // Variables will be populated as we process instructions

//         // Handle entry block first
//         let cl_entry_block = data.blocks[func.entry_block as usize];
//         data.builder.append_block_params_for_function_params(cl_entry_block);
//         data.builder.switch_to_block(cl_entry_block);
//         self.lower_block(&func.blocks[&func.entry_block], &mut data);
//         data.builder.seal_block(cl_entry_block);

//         // Then handle all other blocks
//         for (idx, block) in &func.blocks {
//             if *idx != func.entry_block {
//                 let cl_block = data.blocks[*idx as usize];
//                 data.builder.switch_to_block(cl_block);
//                 self.lower_block(block, &mut data);
//                 data.builder.seal_block(cl_block);
//             }
//         }

//         data.builder.finalize();
//         Ok(())
//     }

//     fn lower_block(&self, block: &BasicBlock, data: &mut TransientData) {
//         for instruction in &block.instructions {
//             self.lower_instruction(instruction, data);
//         }

//         self.lower_terminator(&block.terminator, data);
//     }

//     fn lower_instruction(&self, instruction: &Instruction, data: &mut TransientData) {
//         match instruction {
//             Instruction::Alloca(cmd) => {
//                 self.lower_alloca(cmd, data)
//             }
//             Instruction::StoreImmediate(cmd) => {
//                 self.lower_store_immediate(cmd, data)
//             }
//             Instruction::TakeAddress(cmd) => {
//                 self.lower_take_address(cmd, data)
//             }
//             Instruction::GetFieldPtr(cmd) => {
//                 self.lower_get_field_ptr(cmd, data)
//             }
//             Instruction::Load(cmd) => {
//                 self.lower_load(cmd, data)
//             }
//             Instruction::Store(cmd) => {
//                 self.lower_store(cmd, data)
//             }
//             Instruction::Binary(cmd) => {
//                 self.lower_binary(cmd, data)
//             }
//             Instruction::Unary(cmd) => {
//                 self.lower_unary(cmd, data)
//             }
//             Instruction::Call(cmd) => {
//                 self.lower_call(cmd, data)
//             }
//             Instruction::GetVariantDataPtr(cmd) => {
//                 self.lower_get_variant_data_ptr(cmd, data)
//             }
//             Instruction::LoadActiveVariantIdx(cmd) => {
//                 self.lower_load_active_variant_idx(cmd, data)
//             }
//             Instruction::StoreActiveVariantIdx(cmd) => {
//                 self.lower_store_active_variant_idx(cmd, data)
//             }
//             Instruction::IntToPtr(cmd) => {
//                 self.lower_int_to_ptr(cmd, data)
//             }
//             Instruction::HeapAlloc(cmd) => {
//                 self.lower_heap_alloc(cmd, data)
//             }
//             Instruction::HeapFree(cmd) => {
//                 self.lower_heap_free(cmd, data)
//             }
//             Instruction::MemCpy(cmd) => {
//                 self.lower_memcpy(cmd, data)
//             }
//             Instruction::MemSet(cmd) => {
//                 self.lower_memset(cmd, data)
//             }
//             Instruction::GetElementPtr(cmd) => {
//                 self.lower_get_element_ptr(cmd, data)
//             }
//             Instruction::KillSet(cmd) => {
//                 self.lower_kill_set(cmd, data)
//             }
//         }
//     }

//     fn lower_alloca(&self, cmd: &AllocaCmd, data: &mut TransientData) {
//         // Calculate the size needed for the allocation
//         let target_ty = cmd.target.ty();
//         let element_ty = if let TypeInfo::Pointer(inner) = target_ty {
//             inner
//         } else {
//             panic!("Alloca target must be a pointer type, got {:?}", target_ty);
//         };

//         let layout = calculate_type_layout(element_ty, data.type_registry);
//         let total_size = layout.size * cmd.count as usize;
//         let align_shift = (layout.alignment as f64).log2() as u8;

//         // Create a stack slot
//         let slot = data.builder.create_sized_stack_slot(StackSlotData::new(
//             StackSlotKind::ExplicitSlot,
//             total_size as u32,
//             align_shift,
//         ));

//         // Get the address of the stack slot and store it
//         let addr = data.builder.ins().stack_addr(data.isa.pointer_type(), slot, 0);
//         data.values.insert(cmd.target, addr);
//     }

//     fn operand_to_value(&self, operand: &Operand, data: &mut TransientData) -> ir::Value {
//         match operand {
//             Operand::I64Const(val) => data.builder.ins().iconst(ir::types::I64, *val),
//             Operand::U64Const(val) => data.builder.ins().iconst(ir::types::I64, *val as i64),
//             Operand::F32Const(val) => data.builder.ins().f32const(*val),
//             Operand::F64Const(val) => data.builder.ins().f64const(*val),
//             Operand::BoolConst(val) => data.builder.ins().iconst(ir::types::I8, if *val { 1 } else { 0 }),
//             Operand::Variable(var) => data.values[var],
//             Operand::NoOp => panic!("NoOp operand should not be converted to value"),
//         }
//     }

//     fn lower_store_immediate(&self, cmd: &StoreImmediateCmd, data: &mut TransientData) {
//         let target_addr = data.values[&cmd.target];
//         let value = self.operand_to_value(&cmd.value, data);
//         data.builder.ins().store(ir::MemFlags::trusted(), value, target_addr, 0);
//     }

//     fn lower_take_address(&self, cmd: &TakeAddressCmd, data: &mut TransientData) {
//         // Since we only allow taking address of identifiers (variables from Alloca),
//         // the source should already be a stack address ==> we just return it directly
//         let addr = data.values[&cmd.source];
//         data.values.insert(cmd.target, addr);
//     }

//     fn lower_get_field_ptr(&self, cmd: &GetFieldPtrCmd, data: &mut TransientData) {
//         let base_addr = self.operand_to_value(&cmd.base, data);

//         // Get struct type from the base operand type
//         let base_struct_ty = cmd.base.ty().deref_ptr();
//         let struct_name = if let TypeInfo::Struct(name) = base_struct_ty {
//             name
//         } else {
//             panic!("GetFieldPtr requires base to point to struct type, got {:?}", base_struct_ty);
//         };

//         // Get struct info and layout
//         let struct_info = data.type_registry.resolve_struct(struct_name.name)
//             .expect("Struct not found in type registry");
//         let struct_layout = calculate_struct_layout(struct_info, data.type_registry);

//         // Find the field index to get its offset
//         let field_info = struct_info.fields.get_index_of(&cmd.field)
//             .expect("Field not found in struct");

//         let field_offset = struct_layout.fields[field_info].offset;

//         // Calculate field address (base + field_offset)
//         let offset_const = data.builder.ins().iconst(ir::types::I64, field_offset as i64);
//         let field_addr = data.builder.ins().iadd(base_addr, offset_const);

//         data.values.insert(cmd.target, field_addr);
//     }

//     fn lower_load(&self, cmd: &LoadCmd, data: &mut TransientData) {
//         let source_addr = self.operand_to_value(&cmd.source, data);

//         // Convert YIR type to Cranelift type for the load
//         let target_type = self.type_to_cranelift(cmd.target.ty(), data)
//             .expect("Target type must be convertible to Cranelift type");

//         let value = data.builder.ins().load(target_type, ir::MemFlags::trusted(), source_addr, 0);
//         data.values.insert(cmd.target, value);
//     }

//     fn lower_store(&self, cmd: &StoreCmd, data: &mut TransientData) {
//         let dest_addr = self.operand_to_value(&cmd.dest, data);
//         let value = self.operand_to_value(&cmd.value, data);
//         data.builder.ins().store(ir::MemFlags::trusted(), value, dest_addr, 0);
//     }

//     fn lower_binary(&self, cmd: &BinaryCmd, data: &mut TransientData) {
//         let lhs_val = self.operand_to_value(&cmd.lhs, data);
//         let rhs_val = self.operand_to_value(&cmd.rhs, data);

//         use crate::pass_yir_lowering::BinOp;
//         let result = match cmd.op {
//             BinOp::Add => data.builder.ins().iadd(lhs_val, rhs_val),
//             BinOp::Sub => data.builder.ins().isub(lhs_val, rhs_val),
//             BinOp::Mul => data.builder.ins().imul(lhs_val, rhs_val),
//             BinOp::Div => data.builder.ins().sdiv(lhs_val, rhs_val),
//             BinOp::Eq => data.builder.ins().icmp(ir::condcodes::IntCC::Equal, lhs_val, rhs_val),
//             BinOp::NotEq => data.builder.ins().icmp(ir::condcodes::IntCC::NotEqual, lhs_val, rhs_val),
//             BinOp::LessThan => data.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, lhs_val, rhs_val),
//             BinOp::LessThanEq => data.builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, lhs_val, rhs_val),
//             BinOp::GreaterThan => data.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, lhs_val, rhs_val),
//             BinOp::GreaterThanEq => data.builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, lhs_val, rhs_val),
//             _ => todo!("Implement other binary operations"),
//         };

//         data.values.insert(cmd.target, result);
//     }

//     fn lower_unary(&self, cmd: &UnaryCmd, data: &mut TransientData) {
//         let operand_val = self.operand_to_value(&cmd.operand, data);

//         let result = match cmd.op {
//             UnaryOp::Neg => data.builder.ins().ineg(operand_val),
//         };

//         data.values.insert(cmd.target, result);
//     }

//     fn lower_call(&self, cmd: &CallCmd, data: &mut TransientData) {
//         // Convert arguments to Cranelift values
//         let args: Vec<ir::Value> = cmd.args.iter()
//             .map(|arg| self.operand_to_value(arg, data))
//             .collect();

//         // Build (and cache) the callee signature
//         let call_conv = data.isa.default_call_conv();
//         let mut sig = data.module.make_signature();
//         sig.call_conv = call_conv;

//         for arg in &cmd.args {
//             if let Some(cl_ty) = self.type_to_cranelift(arg.ty(), data) {
//                 sig.params.push(ir::AbiParam::new(cl_ty));
//             }
//         }

//         if let Some(target) = &cmd.target {
//             if let Some(ret_ty) = self.type_to_cranelift(target.ty(), data) {
//                 sig.returns.push(ir::AbiParam::new(ret_ty));
//             }
//         }

//         let func_ref = if let Some(func_ref) = data.func_refs.get(&cmd.name).copied() {
//             func_ref
//         } else {
//             let func_id = data.module
//                 .declare_function(cmd.name.as_str(), Linkage::Import, &sig)
//                 .expect("Declaring function for call failed");
//             let func_ref = data.module.declare_func_in_func(func_id, &mut data.builder.func);
//             data.func_refs.insert(cmd.name, func_ref);
//             func_ref
//         };

//         // Make the call
//         let call_inst = data.builder.ins().call(func_ref, &args);

//         // Store result if target exists
//         if let Some(target) = &cmd.target {
//             let results = data.builder.inst_results(call_inst);
//             if !results.is_empty() {
//                 data.values.insert(*target, results[0]);
//             }
//         }
//     }

//     fn lower_get_variant_data_ptr(&self, cmd: &GetVariantDataPtrCmd, data: &mut TransientData) {
//         let base_addr = self.operand_to_value(&cmd.base, data);

//         // Get enum type from the base operand type
//         let base_enum_ty = cmd.base.ty().deref_ptr();
//         let enum_name = if let TypeInfo::Enum(enum_ty) = base_enum_ty {
//             enum_ty
//         } else {
//             panic!("GetVariantDataPtr requires base to point to enum type, got {:?}", base_enum_ty);
//         };

//         // Get enum layout to find payload offset
//         let enum_info = data.type_registry.resolve_enum(enum_name.name)
//             .expect("Enum not found in type registry");
//         let enum_layout = calculate_enum_layout(enum_info, data.type_registry);

//         // Calculate address of variant data (base + payload_offset)
//         let offset_const = data.builder.ins().iconst(ir::types::I64, enum_layout.payload_offset as i64);
//         let data_addr = data.builder.ins().iadd(base_addr, offset_const);

//         data.values.insert(cmd.target, data_addr);
//     }

//     fn lower_load_active_variant_idx(&self, cmd: &LoadActiveVariantIdxCmd, data: &mut TransientData) {
//         let source_addr = self.operand_to_value(&cmd.source, data);

//         // Load the discriminant from offset 0 (the tag is at the beginning of the enum)
//         let discriminant_value = data.builder.ins().load(ir::types::I64, ir::MemFlags::trusted(), source_addr, 0);

//         data.values.insert(cmd.target, discriminant_value);
//     }

//     fn lower_store_active_variant_idx(&self, cmd: &StoreActiveVariantIdxCmd, data: &mut TransientData) {
//         let dest_addr = data.values[&cmd.dest];
//         let value = self.operand_to_value(&cmd.value, data);

//         // Store the discriminant at offset 0 (the tag is at the beginning of the enum)
//         data.builder.ins().store(ir::MemFlags::trusted(), value, dest_addr, 0);
//     }

//     fn lower_int_to_ptr(&self, cmd: &IntToPtrCmd, data: &mut TransientData) {
//         let int_value = self.operand_to_value(&cmd.source, data);

//         // On most platforms, pointers and u64 are the same size, so this is just a reinterpret
//         // If they're different sizes, we'd need to extend or truncate accordingly
//         data.values.insert(cmd.target, int_value);
//     }

//     fn lower_heap_alloc(&self, cmd: &HeapAllocCmd, data: &mut TransientData) {
//         // `cmd.count` is the total number of bytes to allocate (frontend computes element_size * count)
//         let mut size_val = self.operand_to_value(&cmd.count, data);

//         // Ensure the size is pointer-sized
//         let ptr_ty = data.isa.pointer_type();
//         let size_ty = data.builder.func.dfg.value_type(size_val);
//         if size_ty != ptr_ty {
//             size_val = data.builder.ins().uextend(ptr_ty, size_val);
//         }

//         // Call malloc(size)
//         let call_inst = data.builder.ins().call(data.malloc_ref, &[size_val]);
//         let results = data.builder.inst_results(call_inst);
//         if results.is_empty() {
//             panic!("malloc returned no value");
//         }
//         let ptr_val = results[0];
//         data.values.insert(cmd.target, ptr_val);

//         // If initialization is requested, use `cmd.init` to memcpy/memset from constants (TODO)
//         if let Some(_init) = &cmd.init {
//             // TODO: implement array initialization after allocation
//         }
//     }

//     fn lower_heap_free(&self, cmd: &HeapFreeCmd, data: &mut TransientData) {
//         let ptr_val = self.operand_to_value(&cmd.ptr, data);
//         data.builder.ins().call(data.free_ref, &[ptr_val]);
//     }

//     fn lower_memcpy(&self, cmd: &MemCpyCmd, data: &mut TransientData) {
//         let dest_addr = self.operand_to_value(&cmd.dest, data);
//         let src_addr = self.operand_to_value(&cmd.src, data);
//         let count = self.operand_to_value(&cmd.count, data);

//         data.builder.call_memcpy(data.isa.frontend_config(), dest_addr, src_addr, count);
//     }

//     fn lower_memset(&self, cmd: &MemSetCmd, data: &mut TransientData) {
//         let dest_addr = self.operand_to_value(&cmd.dest, data);
//         let value = self.operand_to_value(&cmd.value, data);
//         let count = self.operand_to_value(&cmd.count, data);

//         data.builder.call_memset(data.isa.frontend_config(), dest_addr, value, count);
//     }

//     fn lower_get_element_ptr(&self, cmd: &GetElementPtrCmd, data: &mut TransientData) {
//         let base_addr = self.operand_to_value(&cmd.base, data);
//         let index = self.operand_to_value(&cmd.index, data);

//         // Get the element type that this pointer points to
//         let target_ty = cmd.target.ty();
//         let element_ty = target_ty.deref();

//         // Calculate element size for proper scaling
//         let layout = calculate_type_layout(element_ty, data.type_registry);
//         let element_size = layout.size as i64;

//         // Scale the index by element size if needed
//         let scaled_index = if element_size == 1 {
//             index
//         } else {
//             let size_const = data.builder.ins().iconst(ir::types::I64, element_size);
//             data.builder.ins().imul(index, size_const)
//         };

//         // Add scaled index to base address
//         let result_addr = data.builder.ins().iadd(base_addr, scaled_index);
//         data.values.insert(cmd.target, result_addr);
//     }

//     fn lower_kill_set(&self, _cmd: &KillSetCmd, _data: &mut TransientData) {
//         // No-op for Cranelift - stack variable lifetimes are managed automatically
//         // The variables will be cleaned up when the stack frame is unwound
//     }

//     fn lower_terminator(&self, terminator: &ControlFlow, data: &mut TransientData)  {
//         match terminator {
//             ControlFlow::Jump { target } => {
//                 self.lower_jump(target, data)
//             }
//             ControlFlow::Branch { condition, if_true, if_false } => {
//                 self.lower_branch(condition, if_true, if_false, data)
//             }
//             ControlFlow::JumpTable { scrutinee, jump_targets, default } => {
//                 self.lower_jump_table(scrutinee, jump_targets, default.as_ref(), data)
//             }
//             ControlFlow::Return(value) => {
//                 self.lower_return(value.as_ref(), data)
//             }
//             ControlFlow::Unterminated => {
//                 panic!("Unterminated block found - this should have been resolved during YIR generation")
//             }
//         }
//     }

//     fn lower_jump(&self, target: &Label, data: &mut TransientData) {
//         let target_block = data.blocks[&target];
//         data.builder.ins().jump(target_block, &[]);
//     }

//     fn lower_branch(&self, condition: &Operand, if_true: &Label, if_false: &Label, data: &mut TransientData) {
//         let condition_value = self.operand_to_value(condition, data);
//         let true_block = data.blocks[*if_true as usize];
//         let false_block = data.blocks[*if_false as usize];

//         data.builder.ins().brif(condition_value, true_block, &[], false_block, &[]);
//     }

//     fn lower_jump_table(&self, scrutinee: &Operand, jump_targets: &IndexMap<u64, Label>, default: Option<&Label>, data: &mut TransientData) {
//         let scrutinee_value = self.operand_to_value(scrutinee, data);

//         // Create jump table data
//         let mut jt_data = cranelift_codegen::ir::JumpTableData::new();

//         // Add all target blocks to jump table
//         for (_value, label) in jump_targets {
//             let target_block = data.blocks[*label as usize];
//             jt_data.push_entry(target_block);

//         }

//         // Create the jump table
//         let jump_table = data.builder.create_jump_table(jt_data);

//         // Handle default case
//         if let Some(default_label) = default {
//             let default_block = data.blocks[*default_label as usize];
//             data.builder.ins().br_table(scrutinee_value, default_block, jump_table);
//         }
//     }

//     fn lower_return(&self, value: Option<&Operand>, data: &mut TransientData) {
//         match value {
//             Some(operand) => {
//                 let return_value = self.operand_to_value(operand, data);
//                 data.builder.ins().return_(&[return_value]);
//             }
//             None => {
//                 data.builder.ins().return_(&[]);
//             }
//         }
//     }

//     fn lower_operand(&self, operand: &Operand, data: &mut TransientData)  {
//         todo!()
//     }

//     fn type_to_cranelift(&self, ty: &TypeInfo, data: &TransientData) -> Option<ir::Type> {
//         use crate::utils::type_info_table::PrimitiveType;
//         use ir::types;

//         match ty {
//             TypeInfo::BuiltInPrimitive(prim) => {
//                 match prim {
//                     PrimitiveType::I64 => Some(types::I64),
//                     PrimitiveType::U64 => Some(types::I64),  // Cranelift treats as signed
//                     PrimitiveType::F32 => Some(types::F32),
//                     PrimitiveType::F64 => Some(types::F64),
//                     PrimitiveType::Bool => Some(types::I8),  // 1 byte for bool
//                     PrimitiveType::Nil => None,  // Void type - no cranelift equivalent
//                 }
//             }
//             TypeInfo::Pointer(_) => Some(data.isa.pointer_type()),  // Platform-dependent pointer size
//             TypeInfo::Function(_) => Some(data.isa.pointer_type()), // Function pointers same as data pointers
//             TypeInfo::Struct(_) => Some(data.isa.pointer_type()),  // Structs as pointers to memory
//             TypeInfo::Enum(_) => Some(data.isa.pointer_type()),    // Enums as pointers to memory
//             TypeInfo::Error => panic!("Compiler bug: Cannot convert Error type to Cranelift type - type inference should have caught this"),
//             TypeInfo::Unknown => panic!("Compiler bug: Cannot convert Unknown type to Cranelift type - type inference should have caught this"),
//         }
//     }
// }