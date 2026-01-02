// use std::collections::HashMap;

// use inkwell::context::Context;
// use inkwell::builder::Builder;
// use inkwell::module::Module as LlvmModule;
// use inkwell::values::{BasicValueEnum, PointerValue, FunctionValue};
// use inkwell::types::BasicTypeEnum;
// use inkwell::AddressSpace;

// use crate::pass_yir_lowering::yir as yir;
// use crate::utils::TypeRegistry;

// // Transient lowering state kept while lowering a module/function.
// pub struct TransientData<'ctx> {
//     pub context: &'ctx Context,
//     pub module: LlvmModule<'ctx>,
//     pub builder: Builder<'ctx>,
//     /// Current function being lowered
//     pub function: Option<FunctionValue<'ctx>>,
//     /// Map YIR Variables -> LLVM pointer values (allocated stack slots / locals)
//     pub var_map: HashMap<yir::Variable, PointerValue<'ctx>>,
//     /// Optional mapping from labels to LLVM basic blocks created during lowering
//     pub block_map: HashMap<i64, inkwell::basic_block::BasicBlock<'ctx>>,
// }

// impl<'ctx> TransientData<'ctx> {
//     pub fn new(context: &'ctx Context, name: &str) -> Self {
//         let module = context.create_module(name);
//         let builder = context.create_builder();
//         Self {
//             context,
//             module,
// use crate::utils::TypeRegistry;
//             function: None,
//             var_map: HashMap::new(),
//             block_map: HashMap::new(),
//         }
//     }
// }

// pub struct Lowerer<'ctx> {
//     pub td: TransientData<'ctx>,
//     pub type_registry: &'ctx TypeRegistry,
// }

// impl<'ctx> Lowerer<'ctx> {
//     pub fn new(context: &'ctx Context, module_name: &str, type_registry: &'ctx TypeRegistry) -> Self {
//         Self {
//             td: TransientData::new(context, module_name),
//             type_registry,
//         }
//     }

//     /// Top-level lowering entry point for a YIR module.
//     /// This will iterate all defined functions and lower them.
//     pub fn lower_module(&mut self, module: &yir::Module) {
//         // Create LLVM declarations for functions first (TODO: use accurate types)
//         for (_name, func_state) in &module.functions {
//             if let yir::FunctionDeclarationState::Defined(func) = func_state {
//                 // For now, create a placeholder LLVM function with i64 return and no params.
//                 let i64_ty = self.td.context.i64_type();
//                 let fn_type = i64_ty.fn_type(&[], false);
//                 let llvm_fn = self
//                     .td
//                     .module
//                     .add_function(&func.name.to_string(), fn_type, None);
//                 // Store function in transient state while lowering
//                 self.td.function = Some(llvm_fn);

//                 // Lower function body
//                 self.lower_function(func)?;

//                 // Clear current function
//                 self.td.function = None;
//             }
//         }
//     }

//     /// Lowers a single YIR function to LLVM IR. This is a skeleton implementation:
//     /// - Creates one LLVM basic block per YIR block
//     /// - Visits instructions and terminators and dispatches to lowering stubs
//     pub fn lower_function(&mut self, func: &yir::Function) {
//         let llvm_fn = match self.td.function {
//             Some(f) => f,
//             None => return Ok(()),
//         };

//         // Create LLVM basic blocks for each YIR block and record in block_map
//         for (id, yir_block) in &func.blocks {
//             let name = format!("bb_{}_{}", yir_block.label.name(), id);
//             let llvm_bb = self.td.context.append_basic_block(llvm_fn, &name);
//             self.td.block_map.insert(*id, llvm_bb);
//         }

//         // Iterate blocks in order and lower each
//         for (id, yir_block) in &func.blocks {
//             // Position builder at end of corresponding LLVM block
//             let llvm_bb = self.td.block_map.get(id).unwrap();
//             self.td.builder.position_at_end(*llvm_bb);

//             // Lower all instructions
//             for instr in &yir_block.instructions {
//                 self.lower_instruction(instr, *llvm_bb);
//             }

//             // Lower terminator
//             self.lower_terminator(&yir_block.terminator, *llvm_bb);
//         }
//     }

//     /// Dispatch lowering for a single instruction. These are stubs that should be
//     /// filled in with proper lowering logic.
//     fn lower_instruction(&mut self, instr: &yir::Instruction, _llvm_bb: inkwell::basic_block::BasicBlock<'ctx>) {
//         match instr {
//             yir::Instruction::Alloca(cmd) => self.lower_alloca(cmd),
//             yir::Instruction::StoreImmediate(cmd) => self.lower_store_immediate(cmd),
//             yir::Instruction::TakeAddress(cmd) => self.lower_take_address(cmd),
//             yir::Instruction::Load(cmd) => self.lower_load(cmd),
//             yir::Instruction::Store(cmd) => self.lower_store(cmd),
//             yir::Instruction::Binary(cmd) => self.lower_binary(cmd),
//             yir::Instruction::Unary(cmd) => self.lower_unary(cmd),
//             yir::Instruction::Call(cmd) => self.lower_call(cmd),
//             yir::Instruction::IntToPtr(cmd) => self.lower_int_to_ptr(cmd),
//             yir::Instruction::HeapAlloc(cmd) => self.lower_heap_alloc(cmd),
//             yir::Instruction::HeapFree(cmd) => self.lower_heap_free(cmd),
//             yir::Instruction::MemCpy(cmd) => self.lower_memcpy(cmd),
//             yir::Instruction::MemSet(cmd) => self.lower_memset(cmd),
//             yir::Instruction::GetElementPtr(cmd) => self.lower_get_element_ptr(cmd),
//             yir::Instruction::KillSet(cmd) => self.lower_kill_set(cmd),
//         }
//     }

//     fn lower_terminator(&mut self, terminator: &yir::ControlFlow, _llvm_bb: inkwell::basic_block::BasicBlock<'ctx>) {
//         match terminator {
//             yir::ControlFlow::Jump { target } => {
//                 // Emit unconditional branch to the target block
//                 if let Some(target_bb) = self.td.block_map.get(&target.id()) {
//                     self.td.builder.build_unconditional_branch(*target_bb);
//                 }
//             }
//             yir::ControlFlow::Branch { condition, if_true, if_false } => {
//                 // Evaluate branch condition (stub: currently not translated)
//                 // TODO: Lower `condition` to an LLVM i1 value
//                 // For now, create a dummy condition which always goes to `if_true`
//                 if let Some(true_bb) = self.td.block_map.get(&if_true.id()) {
//                     if let Some(false_bb) = self.td.block_map.get(&if_false.id()) {
//                         // Build a select using a constant i1 true
//                         let i1_true = self.td.context.bool_type().const_int(1, false);
//                         self.td.builder.build_conditional_branch(i1_true, *true_bb, *false_bb);
//                     }
//                 }
//             }
//             yir::ControlFlow::JumpTable { scrutinee: _scrutinee, jump_targets, default } => {
//                 // TODO: implement jump table lowering using switch instruction
//                 // For now, lower to unconditional branch to default if present
//                 if let Some(default_label) = default {
//                     if let Some(default_bb) = self.td.block_map.get(&default_label.id()) {
//                         self.td.builder.build_unconditional_branch(*default_bb);
//                     }
//                 }
//             }
//             yir::ControlFlow::Return(opt) => {
//                 // TODO: Lower return value properly according to function return type
//                 match opt {
//                     Some(_op) => {
//                         // Placeholder: return a zero i64
//                         let zero = self.td.context.i64_type().const_int(0, false);
//                         self.td.builder.build_return(Some(&zero));
//                     }
//                     None => {
//                         self.td.builder.build_return(None);
//                     }
//                 }
//             }
//             yir::ControlFlow::Unterminated => {
//                 // Should not happen in final IR - this is an ICE
//                 panic!("ICE: Encountered unterminated block during LLVM lowering");
//             }
//         }
//     }

//     // --- Instruction lowering stubs ---
//     fn lower_alloca(&mut self, _cmd: &yir::AllocaCmd) {
//         // TODO: implement stack allocation, respect element type and init
//         todo!("lower_alloca not implemented yet")
//     }

//     fn lower_store_immediate(&mut self, _cmd: &yir::StoreImmediateCmd) {
//         // TODO: store immediate constant into the given stack slot
//         todo!("lower_store_immediate not implemented yet")
//     }

//     fn lower_take_address(&mut self, _cmd: &yir::TakeAddressCmd) {
//         // TODO: take address of variable (pointer to stack slot)
//         todo!("lower_take_address not implemented yet")
//     }

//     fn lower_get_field_ptr(&mut self, _cmd: &yir::GetFieldPtrCmd) {
//         // TODO: compute GEP to field within struct
//         todo!("lower_get_field_ptr not implemented yet")
//     }

//     fn lower_load(&mut self, _cmd: &yir::LoadCmd) {
//         // TODO: load through pointer
//         todo!("lower_load not implemented yet")
//     }

//     fn lower_store(&mut self, _cmd: &yir::StoreCmd) {
//         // TODO: store through pointer
//         todo!("lower_store not implemented yet")
//     }

//     fn lower_binary(&mut self, _cmd: &yir::BinaryCmd) {
//         // TODO: lower binary ops (add/sub/eq/...)
//         todo!("lower_binary not implemented yet")
//     }

//     fn lower_unary(&mut self, _cmd: &yir::UnaryCmd) {
//         // TODO: lower unary ops
//         todo!("lower_unary not implemented yet")
//     }

//     fn lower_call(&mut self, _cmd: &yir::CallCmd) {
//         // TODO: lower function call, handle arguments and return value
//         todo!("lower_call not implemented yet")
//     }

//     fn lower_get_variant_data_ptr(&mut self, _cmd: &yir::GetVariantDataPtrCmd) {
//         // TODO: compute pointer to variant data inside enum
//         todo!("lower_get_variant_data_ptr not implemented yet")
//     }

//     fn lower_load_active_variant_idx(&mut self, _cmd: &yir::LoadActiveVariantIdxCmd) {
//         // TODO: read active variant index from enum representation
//         todo!("lower_load_active_variant_idx not implemented yet")
//     }

//     fn lower_store_active_variant_idx(&mut self, _cmd: &yir::StoreActiveVariantIdxCmd) {
//         // TODO: write active variant index into enum representation
//         todo!("lower_store_active_variant_idx not implemented yet")
//     }

//     fn lower_int_to_ptr(&mut self, _cmd: &yir::IntToPtrCmd) {
//         // TODO: cast integer to pointer
//         todo!("lower_int_to_ptr not implemented yet")
//     }

//     fn lower_heap_alloc(&mut self, _cmd: &yir::HeapAllocCmd) {
//         // TODO: emit call to allocator and return pointer
//         todo!("lower_heap_alloc not implemented yet")
//     }

//     fn lower_heap_free(&mut self, _cmd: &yir::HeapFreeCmd) {
//         // TODO: emit call to deallocator
//         todo!("lower_heap_free not implemented yet")
//     }

//     fn lower_memcpy(&mut self, _cmd: &yir::MemCpyCmd) {
//         // TODO: call llvm.memcpy intrinsic or implement loop
//         todo!("lower_memcpy not implemented yet")
//     }

//     fn lower_memset(&mut self, _cmd: &yir::MemSetCmd) {
//         // TODO: call llvm.memset intrinsic or implement loop
//         todo!("lower_memset not implemented yet")
//     }

//     fn lower_get_element_ptr(&mut self, _cmd: &yir::GetElementPtrCmd) {
//         // TODO: compute pointer arithmetic for array indexing
//         todo!("lower_get_element_ptr not implemented yet")
//     }

//     fn lower_kill_set(&mut self, _cmd: &yir::KillSetCmd) {
//         // TODO: handle variable lifetime end (debug/stack unwind hints)
//         todo!("lower_kill_set not implemented yet")
//     }
// }
