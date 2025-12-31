use cranelift_codegen::ir::{Signature, UserFuncName};
use cranelift_codegen::settings::Configurable;
use cranelift_codegen::{isa, settings};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use target_lexicon::Triple;
use cranelift_codegen::ir::{Function as CraneliftFunction};

use crate::pass_yir_lowering::{Module, Function, BasicBlock, Instruction, ControlFlow, Operand, Variable, Label, BinOp, UnaryOp, ArrayInit};
use crate::utils::type_info_table::TypeInfo;
use std::collections::HashMap;

struct TransientData<'a> {
    builder: FunctionBuilder<'a>,
    isa: &'a dyn cranelift_codegen::isa::TargetIsa,
}

pub struct CraneliftLowering;

impl Default for CraneliftLowering {
    fn default() -> Self {
        Self::new()
    }
}

impl CraneliftLowering {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, module: &Module) -> Result<(), String> {

        todo!()
    }

    fn lower_function(&self, func: &Function, ctxt: &mut FunctionBuilderContext) -> Result<(), String> {
        
        // Create native ISA
        let builder = settings::builder();
        let triple = Triple::host();
        //builder.set("opt_level", "speed").map_err(|e| format!("Settings error: {}", e))?;
        let isa = isa::lookup(triple)
            .map_err(|e| format!("ISA lookup failed: {}", e))?
            .finish(settings::Flags::new(builder))
            .map_err(|e| format!("ISA creation failed: {}", e))?;

        // Choose calling conventions
        let cc = isa.default_call_conv();  // System ABI for extern C
        let sig = Signature::new(cc);

        let (ns, idx) = func.split_ast_id();
        let mut clfunc = CraneliftFunction::with_name_signature(UserFuncName::user(ns, idx), sig);
        let builder = FunctionBuilder::new(&mut clfunc, ctxt);

        let mut data = TransientData {
            builder,
            isa: isa.as_ref(),
        };

        // Now pre-declare the blocks

        let blocks = func.blocks.iter().map(|(idx, b)| {
            data.builder.create_block()
        }).collect::<Vec<_>>();

        // Pre-declare vars

        for var in func.calculate_var_decls() {
            let mut s = String::new();
            var.write_unique_name(&mut s);
        }

        Ok(())
    }

    fn lower_block(&self, block: &BasicBlock, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_instruction(&self, instruction: &Instruction, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_alloca(&self, target: &Variable, count: u64, align: Option<u64>, init: Option<&ArrayInit>, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_store_immediate(&self, target: &Variable, value: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_take_address(&self, target: &Variable, source: &Variable, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_get_field_ptr(&self, target: &Variable, base: &Operand, field: &str, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_load(&self, target: &Variable, source: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_store(&self, dest: &Operand, value: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_binary(&self, target: &Variable, op: &BinOp, lhs: &Operand, rhs: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_unary(&self, target: &Variable, op: &UnaryOp, operand: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_call(&self, target: Option<&Variable>, name: &str, args: &[Operand], data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_get_variant_data_ptr(&self, target: &Variable, base: &Operand, variant: &str, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_load_active_variant_idx(&self, target: &Variable, source: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_store_active_variant_idx(&self, dest: &Variable, value: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_int_to_ptr(&self, target: &Variable, source: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_heap_alloc(&self, target: &Variable, count: &Operand, align: Option<u64>, init: Option<&ArrayInit>, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_heap_free(&self, ptr: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_memcpy(&self, dest: &Operand, src: &Operand, count: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_memset(&self, dest: &Operand, value: &Operand, count: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_get_element_ptr(&self, target: &Variable, base: &Operand, index: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_kill_set(&self, vars: &[Variable], data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_terminator(&self, terminator: &ControlFlow, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_jump(&self, target: &Label, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_branch(&self, condition: &Operand, if_true: &Label, if_false: &Label, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_jump_table(&self, scrutinee: &Operand, jump_targets: &HashMap<u64, Label>, default: Option<&Label>, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_return(&self, value: Option<&Operand>, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn lower_operand(&self, operand: &Operand, data: &mut TransientData) -> Result<(), String> {
        todo!()
    }

    fn type_to_cranelift(&self, ty: &TypeInfo, data: &TransientData) -> Option<cranelift_codegen::ir::Type> {
        use crate::utils::type_info_table::PrimitiveType;
        use cranelift_codegen::ir::types;

        match ty {
            TypeInfo::BuiltInPrimitive(prim) => {
                match prim {
                    PrimitiveType::I64 => Some(types::I64),
                    PrimitiveType::U64 => Some(types::I64),  // Cranelift treats as signed
                    PrimitiveType::F32 => Some(types::F32),
                    PrimitiveType::F64 => Some(types::F64),
                    PrimitiveType::Bool => Some(types::I8),  // 1 byte for bool
                    PrimitiveType::Nil => None,  // Void type - no cranelift equivalent
                }
            }
            TypeInfo::Pointer(_) => Some(data.isa.pointer_type()),  // Platform-dependent pointer size
            TypeInfo::Function(_) => Some(data.isa.pointer_type()), // Function pointers same as data pointers
            TypeInfo::Struct(_) => Some(data.isa.pointer_type()),  // Structs as pointers to memory
            TypeInfo::Enum(_) => Some(data.isa.pointer_type()),    // Enums as pointers to memory
            TypeInfo::Error => panic!("Compiler bug: Cannot convert Error type to Cranelift type - type inference should have caught this"),
            TypeInfo::Unknown => panic!("Compiler bug: Cannot convert Unknown type to Cranelift type - type inference should have caught this"),
        }
    }
}