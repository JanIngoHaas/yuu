// YIR to C lowering pass - transforms the YIR intermediate representation to C code
use crate::pass_type_inference::{PrimitiveType, TypeInfo, TypeRegistry};
use crate::pass_yir_lowering::{
    BasicBlock, BinOp, ControlFlow, Function, FunctionDeclarationState, Instruction, Module,
    Operand, UnaryOp, Variable,
};
use miette::IntoDiagnostic;
use std::fmt::Write;
use ustr::Ustr;

const PREFIX_LABEL: &str = "lbl_";
const PREFIX_FUNCTION: &str = "fn_";

struct TransientData<'a> {
    module: &'a Module,
    tr: &'a TypeRegistry,
    output: String,
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

    fn write_label_name(label: &str, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}", PREFIX_LABEL, label)
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

    fn gen_type(
        &self,
        data: &mut TransientData,
        ty: &'static TypeInfo,
    ) -> Result<(), std::fmt::Error> {
        match ty {
            TypeInfo::BuiltInPrimitive(primitive_type) => match primitive_type {
                PrimitiveType::Bool => write!(data.output, "bool"),
                PrimitiveType::F64 => write!(data.output, "double"),
                PrimitiveType::F32 => write!(data.output, "float"),
                PrimitiveType::I64 => write!(data.output, "int64_t"),
                PrimitiveType::Nil => write!(data.output, "void"),
            },
            TypeInfo::Function(_function_type) => {
                write!(data.output, "void*")
            }
            TypeInfo::Pointer(type_info) => {
                self.gen_type(data, type_info)?;
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
                // Generate tagged union structure for enum
                if let Some(enum_info) = data.tr.resolve_enum(enum_type.name) {
                    write!(data.output, "struct {} {{\n", enum_type.name)?;
                    write!(data.output, "    int tag;\n")?;
                    write!(data.output, "    union {{\n")?;
                    
                    for (variant_name, variant_info) in &enum_info.variants {
                        if let Some(variant_type) = variant_info.variant {
                            write!(data.output, "        struct {{ ")?;
                            self.gen_type(data, variant_type)?;
                            write!(data.output, " data; }} {};\n", variant_name)?;
                        }
                        // Unit variants don't need any struct - just the tag discriminates them
                    }
                    
                    write!(data.output, "    }} data;\n")?;
                    write!(data.output, "}}")
                } else {
                    // Fallback if enum not found in registry
                    write!(data.output, "struct {}", enum_type.name)
                }
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
        self.gen_type(data, ty.deref_ptr())?;
        write!(data.output, " mem_{}_{}; ", var.name(), var.id())?;
        self.gen_type(data, ty)?;
        write!(data.output, " ")?;
        Self::write_var_name(var, &mut data.output)?;
        write!(data.output, " = &mem_{}_{}; ", var.name(), var.id())
    }

    fn gen_simple_var_decl(
        &self,
        data: &mut TransientData,
        var: &Variable,
    ) -> Result<(), std::fmt::Error> {
        self.gen_type(data, var.ty())?;
        write!(data.output, " ")?;
        Self::write_var_name(var, &mut data.output)
    }

    fn gen_instruction(
        &self,
        instruction: &Instruction,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        write!(data.output, "    ")?;

        match instruction {
            Instruction::Alloca { target } => {
                self.gen_variable_decl(data, target)?;
                writeln!(data.output, ";")?;
            }
            Instruction::StoreImmediate { target, value } => {
                write!(data.output, "*")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, " = ")?;
                self.gen_operand(data, value)?;
                writeln!(data.output, ";")?;
            }
            Instruction::TakeAddress { target, source } => {
                self.gen_type(data, target.ty())?;
                write!(data.output, " ")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, " = &")?;
                Self::write_var_name(source, &mut data.output)?;
                writeln!(data.output, ";")?;
            }
            Instruction::GetFieldPtr {
                target,
                base,
                field,
            } => {
                self.gen_variable_decl(data, target)?;
                writeln!(data.output, ";")?;
                write!(data.output, "    ")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, " = &(")?;
                self.gen_operand(data, base)?;
                write!(data.output, "->{}", field)?;
                writeln!(data.output, ");")?;
            }
            Instruction::Load { target, source } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, " = *")?;
                self.gen_operand(data, source)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Store { dest, value } => {
                write!(data.output, "*")?;
                self.gen_operand(data, dest)?;
                write!(data.output, " = *")?;
                self.gen_operand(data, value)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Binary {
                target,
                op,
                lhs,
                rhs,
            } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, " = ")?;
                self.gen_operand(data, lhs)?;
                match op {
                    BinOp::Add => write!(data.output, " + "),
                    BinOp::Sub => write!(data.output, " - "),
                    BinOp::Mul => write!(data.output, " * "),
                    BinOp::Div => write!(data.output, " / "),
                    BinOp::Eq => write!(data.output, " == "),
                    BinOp::NotEq => write!(data.output, " != "),
                    BinOp::LessThan => write!(data.output, " < "),
                    BinOp::LessThanEq => write!(data.output, " <= "),
                    BinOp::GreaterThan => write!(data.output, " > "),
                    BinOp::GreaterThanEq => write!(data.output, " >= "),
                }?;
                self.gen_operand(data, rhs)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Unary {
                target,
                op,
                operand,
            } => {
                self.gen_simple_var_decl(data, target)?;
                write!(data.output, " = ")?;
                match op {
                    UnaryOp::Neg => write!(data.output, "-"),
                }?;
                self.gen_operand(data, operand)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Call { target, name, args } => {
                if let Some(target) = target {
                    self.gen_simple_var_decl(data, target)?;
                    write!(data.output, " = ")?;
                }
                Self::write_function_name(name, &mut data.output)?;
                write!(data.output, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(data.output, ", ")?;
                    }
                    self.gen_operand(data, arg)?;
                }
                write!(data.output, ")")?;
                writeln!(data.output, ";")?;
            }
        }
        Ok(())
    }

    fn gen_block(
        &self,
        block: &BasicBlock,
        data: &mut TransientData,
    ) -> Result<(), std::fmt::Error> {
        Self::write_label_name(block.label.name(), &mut data.output)?;
        writeln!(data.output, ":;")?;

        for instruction in &block.instructions {
            self.gen_instruction(instruction, data)?;
        }

        match &block.terminator {
            ControlFlow::Jump { target } => {
                write!(data.output, "    goto ")?;
                Self::write_label_name(target.name(), &mut data.output)?;
                writeln!(data.output, ";")?;
            }
            ControlFlow::Branch {
                condition,
                if_true,
                if_false,
            } => {
                write!(data.output, "    if (")?;
                self.gen_operand(data, condition)?;
                writeln!(data.output, ") {{")?;
                write!(data.output, "        goto ")?;
                Self::write_label_name(if_true.name(), &mut data.output)?;
                writeln!(data.output, ";")?;
                writeln!(data.output, "    }} else {{")?;
                write!(data.output, "        goto ")?;
                Self::write_label_name(if_false.name(), &mut data.output)?;
                writeln!(data.output, ";")?;
                writeln!(data.output, "    }}")?;
            }
            ControlFlow::Return(value) => {
                write!(data.output, "    return")?;
                if let Some(val) = value {
                    write!(data.output, " ")?;
                    self.gen_operand(data, val)?;
                }
                writeln!(data.output, ";")?;
            }
            ControlFlow::Unterminated => {
                writeln!(data.output, "    // UNTERMINATED BLOCK")?;
            }
        }
        writeln!(data.output)?;
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
        self.gen_type(data, func.return_type)?;
        write!(data.output, " ")?;
        Self::write_function_name(&func.name, &mut data.output)?;
        write!(data.output, "(")?;
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                write!(data.output, ", ")?;
            }
            self.gen_type(data, param.ty())?;
            write!(data.output, " ")?;
            Self::write_var_name(param, &mut data.output)?;
        }
        write!(data.output, ")")
    }

    fn def_struct(
        &self,
        data: &mut TransientData,
        struct_name: Ustr,
    ) -> Result<(), std::fmt::Error> {
        let sinfo = data
            .tr
            .resolve_struct(struct_name)
            .expect("Compiler Bug: Struct not found when lowering to C");

        writeln!(data.output, "struct {} {{", sinfo.name)?;
        for (fname, finfo) in &sinfo.fields {
            write!(data.output, "    ")?;
            self.gen_type(data, finfo.ty)?;
            writeln!(data.output, " {};", fname)?;
        }
        writeln!(data.output, "}};")?;
        Ok(())
    }

    fn gen_module(&self, data: &mut TransientData) -> Result<(), std::fmt::Error> {
        writeln!(data.output, "#include <stdint.h>")?;
        writeln!(data.output, "#include <stdbool.h>")?;
        writeln!(data.output)?;
        for sname in &data.module.structs {
            self.def_struct(data, *sname)?;
        }
        if !data.module.structs.is_empty() {
            writeln!(data.output)?;
        }

        for function_state in data.module.functions.values() {
            match function_state {
                FunctionDeclarationState::Declared(func)
                | FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(func, data)?;
                    writeln!(data.output, ";")?;
                }
            }
        }
        writeln!(data.output)?;

        for function_state in data.module.functions.values() {
            match function_state {
                FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(func, data)?;
                    writeln!(data.output, " {{")?;
                    self.gen_function(func, data)?;
                    writeln!(data.output, "}}")?;
                    writeln!(data.output)?;
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
    ) -> miette::Result<CSourceCode> {
        let mut data = TransientData {
            module,
            output: String::new(),
            tr: type_registry,
        };

        self.gen_module(&mut data).into_diagnostic()?;

        Ok(CSourceCode(data.output))
    }
}
