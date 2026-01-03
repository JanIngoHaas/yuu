// use crate::pass_type_dependency_analysis::TypeDependencyGraph;
// // YIR to C lowering pass - transforms the YIR intermediate representation to C code
// use crate::pass_yir_lowering::{
//     BasicBlock, BinOp, ControlFlow, Function, FunctionDeclarationState, Instruction, Label, Module,
//     Operand, UnaryOp, Variable,
// };
// use crate::utils::{
//     ComposableTypeInfo, StructInfo, TypeRegistry, UnionInfo, calculate_struct_layout, calculate_type_layout,
//     type_info_table::{PrimitiveType, TypeInfo},
// };
// use miette::IntoDiagnostic;
// use std::fmt::Write;

// const PREFIX_LABEL: &str = "lbl_";
// const PREFIX_FUNCTION: &str = "fn_";

// struct TransientData<'a> {
//     module: &'a Module,
//     tr: &'a TypeRegistry,
//     output: String,
//     type_dependency_order: &'a TypeDependencyGraph,
// }

// pub struct CLowerer;

// impl Default for CLowerer {
//     fn default() -> Self {
//         Self
//     }
// }

// impl CLowerer {
//     pub fn new() -> Self {
//         Self
//     }
// }

// pub struct CSourceCode(pub String);

// impl CLowerer {
//     fn write_var_name(var: &Variable, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
//         write!(f, "{}_{}", var.name(), var.id())
//     }

//     fn write_label(label: &Label, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
//         write!(f, "{}{}_{}", PREFIX_LABEL, label.name(), label.id())
//     }

//     fn write_function_name(
//         name: &str,
//         f: &mut impl std::fmt::Write,
//     ) -> Result<(), std::fmt::Error> {
//         // Always prefix generated function names to avoid emitting a raw C `main`
//         // implementation that returns large integers via the process exit code.
//         write!(f, "{}{}", PREFIX_FUNCTION, name)
//     }

//     fn gen_type(data: &mut TransientData, ty: &'static TypeInfo) -> Result<(), std::fmt::Error> {
//         match ty {
//             TypeInfo::BuiltInPrimitive(primitive_type) => match primitive_type {
//                 PrimitiveType::I8 => write!(data.output, "int8_t"),
//                 PrimitiveType::U8 => write!(data.output, "uint8_t"),
//                 PrimitiveType::Bool => write!(data.output, "bool"),
//                 PrimitiveType::F64 => write!(data.output, "double"),
//                 PrimitiveType::F32 => write!(data.output, "float"),
//                 PrimitiveType::I64 => write!(data.output, "int64_t"),
//                 PrimitiveType::U64 => write!(data.output, "uint64_t"),
//                 PrimitiveType::Nil => write!(data.output, "void"),
//             },
//             TypeInfo::Function(_function_type) => {
//                 write!(data.output, "void*")
//             }
//             TypeInfo::Pointer(type_info) => {
//                 Self::gen_type(data, type_info)?;
//                 write!(data.output, "*")
//             }
//             TypeInfo::Error => panic!(
//                 "Compiler bug: Attempted to generate C type for TypeInfo::Error which represents a type error"
//             ),
//             TypeInfo::Unknown => panic!(
//                 "Compiler bug: Attempted to generate C type for TypeInfo::Unknown - type inference not complete"
//             ),
//             TypeInfo::Struct(struct_type) => {
//                 write!(data.output, "struct {}", struct_type.name)
//             }
//             TypeInfo::Union(union_type) => {
//                 write!(data.output, "union {}", union_type.name)
//             }
//         }
//     }

//     fn gen_operand(
//         &self,
//         data: &mut TransientData,
//         operand: &Operand,
//     ) -> Result<(), std::fmt::Error> {
//         match operand {
//             Operand::I64Const(c) => {
//                 if *c >= 0 {
//                     write!(data.output, "INT64_C({})", c)
//                 } else {
//                     write!(data.output, "INT64_C(({}))", c)
//                 }
//             }
//             Operand::U64Const(c) => write!(data.output, "UINT64_C({})", c),
//             Operand::F32Const(c) => {
//                 if c.fract() == 0.0 {
//                     write!(data.output, "{:.1}f", c)
//                 } else {
//                     write!(data.output, "{}f", c)
//                 }
//             }
//             Operand::F64Const(c) => {
//                 if c.fract() == 0.0 {
//                     write!(data.output, "{:.1}", c)
//                 } else {
//                     write!(data.output, "{}", c)
//                 }
//             }
//             Operand::BoolConst(c) => write!(data.output, "{}", if *c { "1" } else { "0" }),
//             Operand::Variable(variable) => Self::write_var_name(variable, &mut data.output),
//             Operand::NoOp => write!(data.output, "((void)0)"),
//         }
//     }

//     fn gen_variable_decl(
//         &self,
//         data: &mut TransientData,
//         var: &Variable,
//     ) -> Result<(), std::fmt::Error> {
//         self.gen_variable_def(data, var, 1, None)
//     }

//     fn gen_variable_def(
//         &self,
//         data: &mut TransientData,
//         var: &Variable,
//         count: u64,
//         init: Option<&crate::pass_yir_lowering::yir::ArrayInit>,
//     ) -> Result<(), std::fmt::Error> {
//         let ty = var.ty();
//         Self::gen_type(data, ty.deref_ptr())?;
//         let mem_location = format!(" mem_{}_{}", var.name(), var.id());
//         write!(data.output, "{mem_location} [{count}]")?;

//         // Handle initialization on the memory storage
//         if let Some(init) = init {
//             match init {
//                 crate::pass_yir_lowering::yir::ArrayInit::Zero => {
//                     write!(data.output, "={{0}}")?;
//                 }
//                 crate::pass_yir_lowering::yir::ArrayInit::Splat(operand) => {
//                     write!(data.output, "={{")?;
//                     for i in 0..count {
//                         if i > 0 {
//                             write!(data.output, ",")?;
//                         }
//                         self.gen_operand(data, operand)?;
//                     }
//                     write!(data.output, "}}")?;
//                 }
//                 crate::pass_yir_lowering::yir::ArrayInit::Elements(elements) => {
//                     write!(data.output, "={{")?;
//                     for (i, element) in elements.iter().enumerate() {
//                         if i > 0 {
//                             write!(data.output, ",")?;
//                         }
//                         self.gen_operand(data, element)?;
//                     }
//                     write!(data.output, "}}")?;
//                 }
//             }
//         }

//         write!(data.output, ";")?;

//         Self::gen_type(data, ty)?;
//         write!(data.output, " ")?;
//         Self::write_var_name(var, &mut data.output)?;
//         write!(data.output, "={mem_location};")
//     }

//     fn gen_simple_var_decl(
//         &self,
//         data: &mut TransientData,
//         var: &Variable,
//     ) -> Result<(), std::fmt::Error> {
//         Self::gen_type(data, var.ty())?;
//         write!(data.output, " ")?;
//         Self::write_var_name(var, &mut data.output)
//     }

//     fn gen_instruction(
//         &self,
//         instruction: &Instruction,
//         data: &mut TransientData,
//     ) -> Result<(), std::fmt::Error> {
//         match instruction {
//             Instruction::Alloca(cmd) => {
//                 if let Some(align) = cmd.align {
//                     write!(data.output, "alignas({align}) ")?;
//                 }

//                 self.gen_variable_def(data, &cmd.target, cmd.count, cmd.init.as_ref())?;
//             }
//             Instruction::StoreImmediate(cmd) => {
//                 write!(data.output, "*")?;
//                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                 write!(data.output, "=")?;
//                 self.gen_operand(data, &cmd.value)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::TakeAddress(cmd) => {
//                 Self::gen_type(data, cmd.target.ty())?;
//                 write!(data.output, " ")?;
//                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                 write!(data.output, "=&")?;
//                 Self::write_var_name(&cmd.source, &mut data.output)?;
//                 write!(data.output, ";")?;
//             }

//             // TODO: This works, but in SOME (probably these can never appear due to the semantics
//             // of Yuu, but need to be aware of it) circumstances, this current code segfaults when
//             // it shouldn't:
//             // Imagine the memory is not reserved, then the deref, i.e. a->b will segfault!
//             // This aligns with current Yuu semantics, but the semantics of YIR are different here
//             // because we are just asking for a pointer, not to deref the pointer first.
//             Instruction::Load(cmd) => {
//                 self.gen_simple_var_decl(data, &cmd.target)?;
//                 write!(data.output, "=*")?;
//                 self.gen_operand(data, &cmd.source)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::Store(cmd) => {
//                 write!(data.output, "*")?;
//                 self.gen_operand(data, &cmd.dest)?;
//                 write!(data.output, "=")?;
//                 self.gen_operand(data, &cmd.value)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::Binary(cmd) => {
//                 self.gen_simple_var_decl(data, &cmd.target)?;
//                 write!(data.output, "=")?;
//                 self.gen_operand(data, &cmd.lhs)?;
//                 match cmd.op {
//                     BinOp::Add => write!(data.output, "+"),
//                     BinOp::Sub => write!(data.output, "-"),
//                     BinOp::Mul => write!(data.output, "*"),
//                     BinOp::Div => write!(data.output, "/"),
//                     BinOp::Mod => write!(data.output, "%"),
//                     BinOp::Eq => write!(data.output, "=="),
//                     BinOp::NotEq => write!(data.output, "!="),
//                     BinOp::LessThan => write!(data.output, "<"),
//                     BinOp::LessThanEq => write!(data.output, "<="),
//                     BinOp::GreaterThan => write!(data.output, ">"),
//                     BinOp::GreaterThanEq => write!(data.output, ">="),
//                 }?;
//                 self.gen_operand(data, &cmd.rhs)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::Unary(cmd) => {
//                 self.gen_simple_var_decl(data, &cmd.target)?;
//                 write!(data.output, "=")?;
//                 match cmd.op {
//                     UnaryOp::Neg => write!(data.output, "-"),
//                 }?;
//                 self.gen_operand(data, &cmd.operand)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::Call(cmd) => {
//                 if let Some(target) = &cmd.target {
//                     self.gen_simple_var_decl(data, target)?;
//                     write!(data.output, "=")?;
//                 }
//                 Self::write_function_name(&cmd.name, &mut data.output)?;
//                 write!(data.output, "(")?;
//                 for (i, arg) in cmd.args.iter().enumerate() {
//                     if i > 0 {
//                         write!(data.output, ",")?;
//                     }
//                     self.gen_operand(data, arg)?;
//                 }
//                 write!(data.output, ");")?;
//             }
//             Instruction::IntToPtr(cmd) => {
//                 self.gen_variable_decl(data, &cmd.target)?;
//                 write!(data.output, ";")?;
//                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                 write!(data.output, "=(")?;
//                 Self::gen_type(data, cmd.target.ty())?;
//                 write!(data.output, ")")?;
//                 self.gen_operand(data, &cmd.source)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::HeapAlloc(cmd) => {
//                 // Generate heap allocation
//                 let element_type = cmd.target.ty().deref_ptr();
//                 Self::gen_type(data, cmd.target.ty())?;
//                 write!(data.output, " ")?;
//                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                 write!(data.output, " = ")?;

//                 // Choose allocation strategy based on initialization
//                 match &cmd.init {
//                     Some(crate::pass_yir_lowering::yir::ArrayInit::Zero) => {
//                         // Use calloc for zero initialization
//                         if let Some(align_val) = cmd.align {
//                             write!(data.output, "aligned_alloc({}, (", align_val)?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ") * sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, ")); memset(")?;
//                             Self::write_var_name(&cmd.target, &mut data.output)?;
//                             write!(data.output, ", 0, (")?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ") * sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, "));")?;
//                         } else {
//                             write!(data.output, "calloc(")?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ", sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, "));")?;
//                         }
//                     }
//                     _ => {
//                         // Regular malloc for non-zero or no initialization
//                         if let Some(align_val) = cmd.align {
//                             write!(data.output, "aligned_alloc({}, (", align_val)?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ") * sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, "));")?;
//                         } else {
//                             write!(data.output, "malloc((")?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ") * sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, "));")?;
//                         }
//                     }
//                 }

//                 // Handle non-zero array initialization after allocation
//                 if let Some(array_init) = &cmd.init {
//                     match array_init {
//                         crate::pass_yir_lowering::yir::ArrayInit::Zero => {
//                             // Already handled above
//                         }
//                         crate::pass_yir_lowering::yir::ArrayInit::Splat(operand) => {
//                             // Use template_fill helper function
//                             write!(data.output, " ")?;
//                             Self::gen_type(data, element_type)?; // Element type
//                             write!(data.output, " __template = ")?;
//                             self.gen_operand(data, operand)?;
//                             write!(data.output, "; __yuu_template_fill(")?;
//                             Self::write_var_name(&cmd.target, &mut data.output)?;
//                             write!(data.output, ", &__template, sizeof(")?;
//                             Self::gen_type(data, element_type)?;
//                             write!(data.output, "), ")?;
//                             self.gen_operand(data, &cmd.count)?;
//                             write!(data.output, ");")?;
//                         }
//                         crate::pass_yir_lowering::yir::ArrayInit::Elements(elements) => {
//                             // Generate individual stores
//                             for (i, element) in elements.iter().enumerate() {
//                                 write!(data.output, " ")?;
//                                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                                 write!(data.output, "[{}] = ", i)?;
//                                 self.gen_operand(data, element)?;
//                                 write!(data.output, ";")?;
//                             }
//                         }
//                     }
//                 }
//             }
//             Instruction::HeapFree(cmd) => {
//                 write!(data.output, "free(")?;
//                 self.gen_operand(data, &cmd.ptr)?;
//                 write!(data.output, ");")?;
//             }
//             Instruction::GetElementPtr(cmd) => {
//                 self.gen_variable_decl(data, &cmd.target)?;
//                 write!(data.output, ";")?;
//                 Self::write_var_name(&cmd.target, &mut data.output)?;
//                 write!(data.output, "=(")?;
//                 Self::gen_type(data, cmd.target.ty())?;
//                 write!(data.output, ")((uint8_t *)")?;
//                 self.gen_operand(data, &cmd.base)?;

//                 // Convert LLVM-style multi-index GEP to byte offsets for C
//                 // Walk through indices and current type to calculate byte offsets
//                 if !cmd.indices.is_empty() {
//                     let total_offset = self.calculate_gep_byte_offset(data, &cmd.base, &cmd.indices)?;
//                     if total_offset != 0 {
//                         write!(data.output, " + {}", total_offset)?;
//                     }
//                 }

//                 write!(data.output, ");")?;
//             }
//             Instruction::MemCpy(cmd) => {
//                 write!(data.output, "memcpy(")?;
//                 self.gen_operand(data, &cmd.dest)?;
//                 write!(data.output, ", ")?;
//                 self.gen_operand(data, &cmd.src)?;
//                 write!(data.output, ", ")?;
//                 self.gen_operand(data, &cmd.count)?;
//                 write!(data.output, ");")?;
//             }
//             Instruction::MemSet(cmd) => {
//                 write!(data.output, "memset(")?;
//                 self.gen_operand(data, &cmd.dest)?;
//                 write!(data.output, ", ")?;
//                 self.gen_operand(data, &cmd.value)?;
//                 write!(data.output, ", ")?;
//                 self.gen_operand(data, &cmd.count)?;
//                 write!(data.output, ");")?;
//             }
//             Instruction::Reinterp(cmd) => {
//                 self.gen_simple_var_decl(data, &cmd.target)?;
//                 write!(data.output, " = (")?;
//                 Self::gen_type(data, cmd.target_type)?;
//                 write!(data.output, ")")?;
//                 self.gen_operand(data, &cmd.source)?;
//                 write!(data.output, ";")?;
//             }
//             Instruction::KillSet(_) => {
//                 // No code generation needed for stack variable kills in C
//             }
//         }
//         Ok(())
//     }

//     fn gen_block(
//         &self,
//         block: &BasicBlock,
//         data: &mut TransientData,
//     ) -> Result<(), std::fmt::Error> {
//         Self::write_label(&block.label, &mut data.output)?;
//         write!(data.output, ":;")?;

//         for instruction in &block.instructions {
//             self.gen_instruction(instruction, data)?;
//         }

//         match &block.terminator {
//             ControlFlow::Jump { target } => {
//                 write!(data.output, "goto ")?;
//                 Self::write_label(target, &mut data.output)?;
//                 write!(data.output, ";")?;
//             }
//             ControlFlow::Branch {
//                 condition,
//                 if_true,
//                 if_false,
//             } => {
//                 write!(data.output, "if(")?;
//                 self.gen_operand(data, condition)?;
//                 write!(data.output, "){{goto ")?;
//                 Self::write_label(if_true, &mut data.output)?;
//                 write!(data.output, ";}}else{{goto ")?;
//                 Self::write_label(if_false, &mut data.output)?;
//                 write!(data.output, ";}}")?;
//             }
//             ControlFlow::Return(value) => {
//                 write!(data.output, "return")?;
//                 if let Some(val) = value {
//                     write!(data.output, " ")?;
//                     self.gen_operand(data, val)?;
//                 }
//                 write!(data.output, ";")?;
//             }
//             ControlFlow::Unterminated => {
//                 write!(data.output, "/*UNTERMINATED*/")?;
//             }
//             ControlFlow::JumpTable {
//                 scrutinee,
//                 jump_targets,
//                 default,
//             } => {
//                 write!(data.output, "switch(")?;
//                 self.gen_operand(data, scrutinee)?;
//                 write!(data.output, "){{")?;

//                 for (variant_name, label) in jump_targets {
//                     write!(data.output, "case {}:goto ", variant_name)?;
//                     Self::write_label(label, &mut data.output)?;
//                     write!(data.output, ";")?;
//                 }

//                 if let Some(default_label) = default {
//                     write!(data.output, "default:goto ")?;
//                     Self::write_label(default_label, &mut data.output)?;
//                     write!(data.output, ";")?;
//                 }

//                 write!(data.output, "}}")?;
//             }
//         }
//         Ok(())
//     }

//     fn gen_function(
//         &self,
//         func: &Function,
//         data: &mut TransientData,
//     ) -> Result<(), std::fmt::Error> {
//         for block in func.blocks.values() {
//             self.gen_block(block, data)?;
//         }

//         Ok(())
//     }

//     fn gen_func_decl(
//         &self,
//         func: &Function,
//         data: &mut TransientData,
//     ) -> Result<(), std::fmt::Error> {
//         Self::gen_type(data, func.return_type)?;
//         write!(data.output, " ")?;
//         Self::write_function_name(&func.name, &mut data.output)?;
//         write!(data.output, "(")?;
//         for (i, param) in func.params.iter().enumerate() {
//             if i > 0 {
//                 write!(data.output, ",")?;
//             }
//             Self::gen_type(data, param.ty())?;
//             write!(data.output, " ")?;
//             Self::write_var_name(param, &mut data.output)?;
//         }
//         write!(data.output, ")")
//     }

//     fn def_struct(
//         &self,
//         data: &mut TransientData,
//         sinfo: &StructInfo,
//     ) -> Result<(), std::fmt::Error> {
//         write!(data.output, "struct {}{{", sinfo.name)?;
//         for (fname, finfo) in &sinfo.fields {
//             Self::gen_type(data, finfo.ty)?;
//             write!(data.output, " {};", fname)?;
//         }
//         write!(data.output, "}};")?;
//         Ok(())
//     }

//     /// Calculate the total byte offset for LLVM-style multi-index GEP
//     /// Converts [0, field_idx] style indices to byte offsets for C pointer arithmetic
//     fn calculate_gep_byte_offset(
//         &self,
//         data: &TransientData,
//         base: &Operand,
//         indices: &[Operand],
//     ) -> Result<u64, std::fmt::Error> {
//         let mut current_type = base.ty().deref_ptr(); // Start with what the base pointer points to
//         let mut total_offset = 0u64;

//         for (index_pos, index_operand) in indices.iter().enumerate() {
//             // Get the index value (must be constant for C backend)
//             let index_val = match index_operand {
//                 Operand::U64Const(n) => *n,
//                 Operand::I64Const(n) if *n >= 0 => *n as u64,
//                 _ => {
//                     // For now, only support constant indices in C backend
//                     // Dynamic indices would require runtime calculation
//                     panic!("C backend currently only supports constant GEP indices, got non-constant index at position {}", index_pos);
//                 }
//             };

//             // Apply the index based on the current type
//             match current_type {
//                 TypeInfo::Struct(struct_info) => {
//                     // Struct field access: index selects field
//                     let resolved_struct = data.tr.resolve_struct(struct_info.name)
//                         .expect("Struct not found during C GEP lowering");
//                     let layout = calculate_struct_layout(resolved_struct, data.tr);

//                     if (index_val as usize) >= layout.fields.len() {
//                         panic!("Struct field index {} out of bounds for struct with {} fields",
//                                index_val, layout.fields.len());
//                     }

//                     total_offset += layout.fields[index_val as usize].offset as u64;
//                     // Update current type to the field's type for next iteration
//                     current_type = resolved_struct.fields[index_val as usize].ty;
//                 }
//                 TypeInfo::BuiltInPrimitive(_) | TypeInfo::Pointer(_) => {
//                     // Element access: index * sizeof(element)
//                     let element_layout = calculate_type_layout(current_type, data.tr);
//                     total_offset += index_val * element_layout.size as u64;
//                     // Type stays the same for array-like access
//                 }
//                 _ => {
//                     panic!("Unsupported type for GEP indexing in C backend: {:?}", current_type);
//                 }
//             }
//         }

//         Ok(total_offset)
//     }


//     fn def_union(
//         &self,
//         data: &mut TransientData,
//         uinfo: &UnionInfo,
//     ) -> Result<(), std::fmt::Error> {
//         write!(data.output, "union {}{{", uinfo.name)?;
//         for (fname, finfo) in &uinfo.fields {
//             // Skip Nil-typed fields: Nil has no representation in C and corresponds
//             // to an enum variant with no payload, so no union member should be emitted.
//             if let TypeInfo::BuiltInPrimitive(PrimitiveType::Nil) = finfo.ty {
//                 continue;
//             }
//             Self::gen_type(data, finfo.ty)?;
//             write!(data.output, " {};", fname)?;
//         }
//         write!(data.output, "}};");
//         Ok(())
//     }

//     fn gen_module(&self, data: &mut TransientData) -> Result<(), std::fmt::Error> {
//         write!(
//             data.output,
//             "#include<stdint.h>\n#include<stdbool.h>\n#include<stdio.h>\n#include<stdlib.h>\n#include<string.h>\n#include<stdalign.h>\n"
//         )?;

//         // Generate helper functions for array initialization
//         write!(
//             data.output,
//             r#"static inline void __yuu_template_fill(void* dest, const void* template_val, size_t element_size, size_t count) {{
//   char* d = (char*)dest;
//   for (size_t i = 0; i < count; i++) {{
//     memcpy(d + i * element_size, template_val, element_size);
//   }}
// }}
// "#
//         )?;

//         for name in data.type_dependency_order.create_topological_order() {
//             let soe = data
//                 .tr
//                 .resolve_composable_type(name)
//                 .expect("Compiler Bug: Type not found, but should be present at this point");
//             match soe {
//                 ComposableTypeInfo::Struct(si) => self.def_struct(data, si)?,
//                 ComposableTypeInfo::Union(ui) => self.def_union(data, ui)?,
//             }
//         }

//         for function_state in data.module.functions.values() {
//             match function_state {
//                 FunctionDeclarationState::Declared(func)
//                 | FunctionDeclarationState::Defined(func) => {
//                     self.gen_func_decl(func, data)?;
//                     write!(data.output, ";")?;
//                 }
//             }
//         }

//         for function_state in data.module.functions.values() {
//             match function_state {
//                 FunctionDeclarationState::Defined(func) => {
//                     self.gen_func_decl(func, data)?;
//                     write!(data.output, "{{")?;
//                     self.gen_function(func, data)?;
//                     write!(data.output, "}}")?;
//                 }
//                 FunctionDeclarationState::Declared(_) => {}
//             }
//         }

//         // If the module defines a function named "main" in the source language,
//         // the lowering above emitted it as `fn_main` (prefixed). Emit a small C
//         // wrapper `main` that calls the generated function, prints its result to
//         // stdout and returns 0. This avoids relying on the process exit code to
//         // transport arbitrary integers (which get truncated to 8 bits on Unix).
//         if data.module.functions.keys().any(|k| k.as_str() == "main") {
//             // C wrapper: call the prefixed function and print as long long
//             write!(
//                 data.output,
//                 "int main(int argc, char** argv){{int64_t __r = {}main(); printf(\"%lld\\n\", (long long)__r); return 0;}}",
//                 PREFIX_FUNCTION
//             )?;
//         }
//         Ok(())
//     }
// }

// impl CLowerer {
//     pub fn run(
//         &self,
//         module: &Module,
//         type_registry: &TypeRegistry,
//         type_dependency_order: &TypeDependencyGraph,
//     ) -> miette::Result<CSourceCode> {
//         let mut data = TransientData {
//             module,
//             output: String::new(),
//             tr: type_registry,
//             type_dependency_order,
//         };

//         self.gen_module(&mut data).into_diagnostic()?;

//         Ok(CSourceCode(data.output))
//     }
// }
