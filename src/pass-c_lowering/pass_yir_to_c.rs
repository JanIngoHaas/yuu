use crate::pass_type_dependencies::TypeDependencyGraph;
// YIR to C lowering pass - transforms the YIR intermediate representation to C code
use crate::pass_type_inference::{
    EnumInfo, PrimitiveType, StructInfo, StructOrEnumInfo, TypeInfo, TypeRegistry,
};
use crate::pass_yir_lowering::{
    BasicBlock, BinOp, ControlFlow, Function, FunctionDeclarationState, Instruction, Label, Module,
    Operand, UnaryOp, Variable,
};
use miette::IntoDiagnostic;
use std::fmt::Write;

const PREFIX_LABEL: &str = "lbl_";
const PREFIX_FUNCTION: &str = "fn_";

struct TransientData<'a> {
    module: &'a Module,
    tr: &'a TypeRegistry,
    output: String,
    type_dependency_order: &'a TypeDependencyGraph,
}

pub struct CLowering;

impl Default for CLowering {
    fn default() -> Self {
        Self
    }
}

impl CLowering {
    pub fn new() -> Self {
        Self
    }
}

pub struct CSourceCode(pub String);

impl CLowering {
    fn write_var_name(var: &Variable, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        write!(f, "{}_{}", var.name(), var.id())
    }

    fn write_label(label: &Label, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}_{}", PREFIX_LABEL, label.name(), label.id())
    }

    fn write_function_name(
        name: &str,
        f: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        if name == "main" {
            write!(f, "main")
        } else {
            write!(f, "{}{}", PREFIX_FUNCTION, name)
        }
    }

    fn gen_type(data: &mut TransientData, ty: &'static TypeInfo) -> Result<(), std::fmt::Error> {
        match ty {
            TypeInfo::BuiltInPrimitive(primitive_type) => match primitive_type {
                PrimitiveType::Bool => write!(data.output, "bool"),
                PrimitiveType::F64 => write!(data.output, "double"),
                PrimitiveType::F32 => write!(data.output, "float"),
                PrimitiveType::I64 => write!(data.output, "int64_t"),
                PrimitiveType::U64 => write!(data.output, "uint64_t"),
                PrimitiveType::Nil => write!(data.output, "void"),
            },
            TypeInfo::Function(_function_type) => {
                write!(data.output, "void*")
            }
            TypeInfo::Pointer(type_info) => {
                Self::gen_type(data, type_info)?;
                write!(data.output, "*")
            }
            TypeInfo::Inactive => panic!(
                "Compiler bug: Attempted to generate C type for TypeInfo::Inactive which represents no value"
            ),
            TypeInfo::Error => panic!(
                "Compiler bug: Attempted to generate C type for TypeInfo::Error which represents a type error"
            ),
            TypeInfo::Struct(struct_type) => {
                write!(data.output, "struct {}", struct_type.name)
            }
            TypeInfo::Enum(enum_type) => {
                // Reference the struct definition
                write!(data.output, "struct {}", enum_type.name)
            }
        }
    }

    fn gen_operand(
        &self,
        data: &mut TransientData,
        operand: &Operand,
    ) -> Result<(), std::fmt::Error> {
        match operand {
            Operand::I64Const(c) => {
                if *c >= 0 {
                    write!(data.output, "INT64_C({})", c)
                } else {
                    write!(data.output, "INT64_C(({}))", c)
                }
            }
            Operand::U64Const(c) => write!(data.output, "UINT64_C({})", c),
            Operand::F32Const(c) => {
                if c.fract() == 0.0 {
                    write!(data.output, "{:.1}f", c)
                } else {
                    write!(data.output, "{}f", c)
                }
            }
            Operand::F64Const(c) => {
                if c.fract() == 0.0 {
                    write!(data.output, "{:.1}", c)
                } else {
                    write!(data.output, "{}", c)
                }
            }
            Operand::BoolConst(c) => write!(data.output, "{}", if *c { "1" } else { "0" }),
            Operand::Variable(variable) => Self::write_var_name(variable, &mut data.output),
            Operand::NoOp => write!(data.output, "((void)0)"),
        }
    }

    fn gen_variable_decl(
        &self,
        data: &mut TransientData,
        var: &Variable,
    ) -> Result<(), std::fmt::Error> {
        let ty = var.ty();
        Self::gen_type(data, ty.deref_ptr())?;
        write!(data.output, " mem_{}_{};", var.name(), var.id())?;
        Self::gen_type(data, ty)?;
        write!(data.output, " ")?;
        Self::write_var_name(var, &mut data.output)?;
        write!(data.output, "=&mem_{}_{}; ", var.name(), var.id())
    }

    fn gen_simple_var_decl(
        &self,
        data: &mut TransientData,
        var: &Variable,
    ) -> Result<(), std::fmt::Error> {
        Self::gen_type(data, var.ty())?;
        write!(data.output, " ")?;
        Self::write_var_name(var, &mut data.output)
    }

    fn gen_instruction(
        &self,
        instruction: &Instruction,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        match instruction {
            Instruction::Alloca { target } => {
                self.gen_variable_decl(data, target)?;
                write!(data.output, ";")?;
            }
            Instruction::StoreImmediate { target, value } => {
                write!(data.output, "*")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, "=")?;
                self.gen_operand(data, value)?;
                write!(data.output, ";")?;
            }
            Instruction::TakeAddress { target, source } => {
                Self::gen_type(data, target.ty())?;
                write!(data.output, " ")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, "=&")?;
                Self::write_var_name(source, &mut data.output)?;
                write!(data.output, ";")?;
            }
            Instruction::GetFieldPtr {
                target,
                base,
                field,
            } => {
                self.gen_variable_decl(data, target)?;
                write!(data.output, ";")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, "=&(")?;
                self.gen_operand(data, base)?;
                write!(data.output, "->{});", field)?;
            }
            Instruction::Load { target, source } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, "=*")?;
                self.gen_operand(data, source)?;
                write!(data.output, ";")?;
            }
            Instruction::Store { dest, value } => {
                write!(data.output, "*")?;
                self.gen_operand(data, dest)?;
                write!(data.output, "=*")?;
                self.gen_operand(data, value)?;
                write!(data.output, ";")?;
            }
            Instruction::Binary {
                target,
                op,
                lhs,
                rhs,
            } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, "=")?;
                self.gen_operand(data, lhs)?;
                match op {
                    BinOp::Add => write!(data.output, "+"),
                    BinOp::Sub => write!(data.output, "-"),
                    BinOp::Mul => write!(data.output, "*"),
                    BinOp::Div => write!(data.output, "/"),
                    BinOp::Mod => write!(data.output, "%"),
                    BinOp::Eq => write!(data.output, "=="),
                    BinOp::NotEq => write!(data.output, "!="),
                    BinOp::LessThan => write!(data.output, "<"),
                    BinOp::LessThanEq => write!(data.output, "<="),
                    BinOp::GreaterThan => write!(data.output, ">"),
                    BinOp::GreaterThanEq => write!(data.output, ">="),
                }?;
                self.gen_operand(data, rhs)?;
                write!(data.output, ";")?;
            }
            Instruction::Unary {
                target,
                op,
                operand,
            } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, "=")?;
                match op {
                    UnaryOp::Neg => write!(data.output, "-"),
                }?;
                self.gen_operand(data, operand)?;
                write!(data.output, ";")?;
            }
            Instruction::Call { target, name, args } => {
                if let Some(target) = target {
                    self.gen_simple_var_decl(data, target)?;
                    write!(data.output, "=")?;
                }
                Self::write_function_name(name, &mut data.output)?;
                write!(data.output, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(data.output, ",")?;
                    }
                    self.gen_operand(data, arg)?;
                }
                write!(data.output, ");")?;
            }
            Instruction::StoreActiveVariantIdx { dest, value } => {
                Self::write_var_name(dest, &mut data.output)?;
                write!(data.output, "->tag=")?;
                self.gen_operand(data, value)?;
                write!(data.output, ";")?;
            }
            Instruction::LoadActiveVariantIdx { target, source } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, "=")?;
                self.gen_operand(data, source)?;
                write!(data.output, "->tag;")?;
            }
            Instruction::GetVariantDataPtr {
                target,
                base,
                variant,
            } => {
                self.gen_variable_decl(data, target)?;
                write!(data.output, ";")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, "=&(")?;
                self.gen_operand(data, base)?;
                write!(data.output, "->data.{}_data);", variant)?;
            }
        }
        Ok(())
    }

    fn gen_block(
        &self,
        block: &BasicBlock,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        Self::write_label(&block.label, &mut data.output)?;
        write!(data.output, ":;")?;

        for instruction in &block.instructions {
            self.gen_instruction(instruction, data)?;
        }

        match &block.terminator {
            ControlFlow::Jump { target } => {
                write!(data.output, "goto ")?;
                Self::write_label(target, &mut data.output)?;
                write!(data.output, ";")?;
            }
            ControlFlow::Branch {
                condition,
                if_true,
                if_false,
            } => {
                write!(data.output, "if(")?;
                self.gen_operand(data, condition)?;
                write!(data.output, "){{goto ")?;
                Self::write_label(if_true, &mut data.output)?;
                write!(data.output, ";}}else{{goto ")?;
                Self::write_label(if_false, &mut data.output)?;
                write!(data.output, ";}}")?;
            }
            ControlFlow::Return(value) => {
                write!(data.output, "return")?;
                if let Some(val) = value {
                    write!(data.output, " ")?;
                    self.gen_operand(data, val)?;
                }
                write!(data.output, ";")?;
            }
            ControlFlow::Unterminated => {
                write!(data.output, "/*UNTERMINATED*/")?;
            }
            ControlFlow::JumpTable {
                scrutinee,
                jump_targets,
                default,
            } => {
                write!(data.output, "switch(")?;
                self.gen_operand(data, scrutinee)?;
                write!(data.output, "){{")?;

                for (variant_name, label) in jump_targets {
                    write!(data.output, "case {}:goto ", variant_name)?;
                    Self::write_label(label, &mut data.output)?;
                    write!(data.output, ";")?;
                }

                if let Some(default_label) = default {
                    write!(data.output, "default:goto ")?;
                    Self::write_label(default_label, &mut data.output)?;
                    write!(data.output, ";")?;
                }

                write!(data.output, "}}")?;
            }
        }
        Ok(())
    }

    fn gen_function(
        &self,
        func: &Function,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        for block in func.blocks.values() {
            self.gen_block(block, data)?;
        }

        Ok(())
    }

    fn gen_func_decl(
        &self,
        func: &Function,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        Self::gen_type(data, func.return_type)?;
        write!(data.output, " ")?;
        Self::write_function_name(&func.name, &mut data.output)?;
        write!(data.output, "(")?;
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                write!(data.output, ",")?;
            }
            Self::gen_type(data, param.ty())?;
            write!(data.output, " ")?;
            Self::write_var_name(param, &mut data.output)?;
        }
        write!(data.output, ")")
    }

    fn def_struct(
        &self,
        data: &mut TransientData,
        sinfo: &StructInfo,
    ) -> Result<(), std::fmt::Error> {
        write!(data.output, "struct {}{{", sinfo.name)?;
        for (fname, finfo) in &sinfo.fields {
            Self::gen_type(data, finfo.ty)?;
            write!(data.output, " {};", fname)?;
        }
        write!(data.output, "}};")?;
        Ok(())
    }

    fn def_enum(&self, data: &mut TransientData, einfo: &EnumInfo) -> Result<(), std::fmt::Error> {
        write!(data.output, "struct {}{{enum{{", einfo.name)?;
        let mut first = true;
        for (variant_name, variant_info) in &einfo.variants {
            if !first {
                write!(data.output, ",")?;
            }
            write!(data.output, "{}={}", variant_name, variant_info.variant_idx)?;
            first = false;
        }
        write!(data.output, "}}tag;")?;

        let has_data_variants = einfo.variants.values().any(|v| v.variant.is_some());
        if has_data_variants {
            write!(data.output, "union{{")?;
            for (variant_name, variant_info) in &einfo.variants {
                if let Some(variant_type) = variant_info.variant {
                    Self::gen_type(data, variant_type)?;
                    write!(data.output, " {}_data;", variant_name)?;
                }
            }
            write!(data.output, "}}data;")?;
        }

        write!(data.output, "}};")?;
        Ok(())
    }

    fn gen_module(&self, data: &mut TransientData) -> Result<(), std::fmt::Error> {
        write!(data.output, "#include<stdint.h>\n#include<stdbool.h>\n")?;

        for name in data.type_dependency_order.create_topological_order() {
            let soe = data
                .tr
                .resolve_struct_or_enum(name)
                .expect("Compiler Bug: Type not found, but should be present at this point");
            match soe {
                StructOrEnumInfo::Enum(ei) => self.def_enum(data, ei)?,
                StructOrEnumInfo::Struct(si) => self.def_struct(data, si)?,
            }
        }

        for function_state in data.module.functions.values() {
            match function_state {
                FunctionDeclarationState::Declared(func)
                | FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(func, data)?;
                    write!(data.output, ";")?;
                }
            }
        }

        for function_state in data.module.functions.values() {
            match function_state {
                FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(func, data)?;
                    write!(data.output, "{{")?;
                    self.gen_function(func, data)?;
                    write!(data.output, "}}")?;
                }
                FunctionDeclarationState::Declared(_) => {}
            }
        }
        Ok(())
    }
}

impl CLowering {
    pub fn run(
        &self,
        module: &Module,
        type_registry: &TypeRegistry,
        type_dependency_order: &TypeDependencyGraph,
    ) -> miette::Result<CSourceCode> {
        let mut data = TransientData {
            module,
            output: String::new(),
            tr: type_registry,
            type_dependency_order,
        };

        self.gen_module(&mut data).into_diagnostic()?;

        Ok(CSourceCode(data.output))
    }
}
