// YIR to C lowering pass - transforms the YIR intermediate representation to C code
use crate::pass_type_inference::{PrimitiveType, TypeInfo, TypeRegistry};
use crate::pass_yir_lowering::{
    BasicBlock, BinOp, ControlFlow, Function, FunctionDeclarationState, Instruction, Module,
    Operand, UnaryOp, Variable,
};
use crate::utils::{
    context::Context,
    scheduler::{Pass, ResourceId, ResourceName},
};
use std::fmt::Write;
use ustr::Ustr;

const PREFIX_VAR: &str = "reg_";
const PREFIX_LABEL: &str = "lbl_";
const PREFIX_FUNCTION: &str = "fn_";

struct TransientData<'a> {
    module: &'a Module,
    tr: &'a TypeRegistry,
    output: String,
}

pub struct PassYirToC;

impl Default for PassYirToC {
    fn default() -> Self {
        Self
    }
}

pub struct CSourceCode(pub String);

impl ResourceId for CSourceCode {
    fn resource_name() -> ResourceName {
        "CSourceCode"
    }
}

impl PassYirToC {
    fn write_var_name(var: &Variable, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}_{}", PREFIX_VAR, var.name(), var.id())
    }

    fn write_label_name(label: &str, f: &mut impl std::fmt::Write) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}", PREFIX_LABEL, label)
    }

    fn write_function_name(
        name: &str,
        f: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}", PREFIX_FUNCTION, name)
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
                // Function pointers not implemented yet
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
            },
            Operand::F32Const(c) => write!(data.output, "{}f", c),
            Operand::F64Const(c) => write!(data.output, "{}", c),
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
        self.gen_type(data, var.ty())?;
        write!(data.output, " ")?;
        Self::write_var_name(var, &mut data.output)
    }

    fn gen_instruction(
        &self,
        instruction: &Instruction,
        data: &mut TransientData,
    ) -> anyhow::Result<()> {
        write!(data.output, "    ")?; // Indentation

        match instruction {
            Instruction::Alloca { target } => {
                // Declare the variable on the stack
                self.gen_variable_decl(data, target)?;
                writeln!(data.output, ";")?;
            }
            Instruction::StoreImmediate { target, value } => {
                // Store immediate value directly to variable
                write!(data.output, "*")?;
                Self::write_var_name(target, &mut data.output)?;
                write!(data.output, " = ")?;
                self.gen_operand(data, value)?;
                writeln!(data.output, ";")?;
            }
            Instruction::TakeAddress { target, source } => {
                // Get address of a variable
                self.gen_variable_decl(data, target)?;
                write!(data.output, " = &")?;
                Self::write_var_name(source, &mut data.output)?;
                writeln!(data.output, ";")?;
            }
            Instruction::GetFieldPtr {
                target,
                base,
                field,
            } => {
                // Get pointer to a field in a struct
                self.gen_variable_decl(data, target)?;
                write!(data.output, " = &(")?;
                self.gen_operand(data, base)?;
                write!(data.output, "->{}", field)?;
                writeln!(data.output, ");")?;
            }
            Instruction::Load { target, source } => {
                // Load value through a pointer
                self.gen_variable_decl(data, target)?;
                write!(data.output, " = *")?;
                self.gen_operand(data, source)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Store { dest, value } => {
                // Store value through a pointer
                write!(data.output, "*")?;
                self.gen_operand(data, dest)?;
                write!(data.output, " = ")?;
                self.gen_operand(data, value)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Binary {
                target,
                op,
                lhs,
                rhs,
            } => {
                // Binary operation
                self.gen_variable_decl(data, target)?;
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
                // Unary operation
                self.gen_variable_decl(data, target)?;
                write!(data.output, " = ")?;
                match op {
                    UnaryOp::Neg => write!(data.output, "-"),
                }?;
                self.gen_operand(data, operand)?;
                writeln!(data.output, ";")?;
            }
            Instruction::Call { target, name, args } => {
                // Function call
                if let Some(target) = target {
                    self.gen_variable_decl(data, target)?;
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

    fn gen_block(&self, block: &BasicBlock, data: &mut TransientData) -> anyhow::Result<()> {
        // Label
        Self::write_label_name(block.label.name(), &mut data.output)?;
        writeln!(data.output, ":;")?;

        // Generate instructions
        for instruction in &block.instructions {
            self.gen_instruction(instruction, data)?;
        }

        // Generate terminator
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
                // Should not happen in final IR
                writeln!(data.output, "    // UNTERMINATED BLOCK")?;
            }
        }
        writeln!(data.output)?; // Extra newline between blocks
        Ok(())
    }

    fn gen_function(&self, func: &Function, data: &mut TransientData) -> anyhow::Result<()> {
        // Generate variable declarations for all variables in the function
        // We need to collect all variables used in the function
        let mut variables = std::collections::HashSet::new();
        for block in func.blocks.values() {
            for instruction in &block.instructions {
                self.collect_variables_from_instruction(instruction, &mut variables);
            }
            self.collect_variables_from_terminator(&block.terminator, &mut variables);
        }

        // Add parameter variables
        for param in &func.params {
            variables.insert(*param);
        }

        // Declare all variables at the top of the function
        for var in &variables {
            write!(data.output, "    ")?;
            self.gen_variable_decl(data, var)?;
            writeln!(data.output, ";")?;
        }
        writeln!(data.output)?;

        // Generate blocks in order
        let mut blocks: Vec<&BasicBlock> = func.blocks.values().collect();
        blocks.sort_by_key(|block| block.label.id());

        for block in blocks {
            self.gen_block(block, data)?;
        }
        Ok(())
    }

    fn collect_variables_from_instruction(
        &self,
        instruction: &Instruction,
        variables: &mut std::collections::HashSet<Variable>,
    ) {
        match instruction {
            Instruction::Alloca { target } => {
                variables.insert(*target);
            }
            Instruction::StoreImmediate { target, value } => {
                variables.insert(*target);
                if let Operand::Variable(var) = value {
                    variables.insert(*var);
                }
            }
            Instruction::TakeAddress { target, source } => {
                variables.insert(*target);
                variables.insert(*source);
            }
            Instruction::GetFieldPtr { target, base, .. } => {
                variables.insert(*target);
                if let Operand::Variable(var) = base {
                    variables.insert(*var);
                }
            }
            Instruction::Load { target, source } => {
                variables.insert(*target);
                if let Operand::Variable(var) = source {
                    variables.insert(*var);
                }
            }
            Instruction::Store { dest, value } => {
                if let Operand::Variable(var) = dest {
                    variables.insert(*var);
                }
                if let Operand::Variable(var) = value {
                    variables.insert(*var);
                }
            }
            Instruction::Binary {
                target, lhs, rhs, ..
            } => {
                variables.insert(*target);
                if let Operand::Variable(var) = lhs {
                    variables.insert(*var);
                }
                if let Operand::Variable(var) = rhs {
                    variables.insert(*var);
                }
            }
            Instruction::Unary {
                target, operand, ..
            } => {
                variables.insert(*target);
                if let Operand::Variable(var) = operand {
                    variables.insert(*var);
                }
            }
            Instruction::Call { target, args, .. } => {
                if let Some(target) = target {
                    variables.insert(*target);
                }
                for arg in args {
                    if let Operand::Variable(var) = arg {
                        variables.insert(*var);
                    }
                }
            }
        }
    }

    fn collect_variables_from_terminator(
        &self,
        terminator: &ControlFlow,
        variables: &mut std::collections::HashSet<Variable>,
    ) {
        match terminator {
            ControlFlow::Jump { .. } => {}
            ControlFlow::Branch { condition, .. } => {
                if let Operand::Variable(var) = condition {
                    variables.insert(*var);
                }
            }
            ControlFlow::Return(value) => {
                if let Some(Operand::Variable(var)) = value {
                    variables.insert(*var);
                }
            }
            ControlFlow::Unterminated => {}
        }
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

    fn gen_module(&self, data: &mut TransientData) -> anyhow::Result<()> {
        // Add standard includes
        writeln!(data.output, "#include <stdint.h>")?;
        writeln!(data.output, "#include <stdbool.h>")?;
        writeln!(data.output)?;

        // Generate struct definitions
        for sname in &data.module.structs {
            self.def_struct(data, *sname)?;
        }
        if !data.module.structs.is_empty() {
            writeln!(data.output)?;
        }

        // Forward declare all functions
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

        // Generate function definitions
        for function_state in data.module.functions.values() {
            match function_state {
                FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(func, data)?;
                    writeln!(data.output, " {{")?;
                    self.gen_function(func, data)?;
                    writeln!(data.output, "}}")?;
                    writeln!(data.output)?;
                }
                FunctionDeclarationState::Declared(_) => {
                    // Just declared, no definition
                }
            }
        }
        Ok(())
    }
}

impl Pass for PassYirToC {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let module = context.get_resource::<Module>(self);
        let module = module.lock().unwrap();

        let tr = context.get_resource::<TypeRegistry>(self);
        let tr = tr.lock().unwrap();

        let mut data = TransientData {
            module: &module,
            output: String::new(),
            tr: &tr,
        };

        // Generate C code
        self.gen_module(&mut data)?;

        // Add the generated code to the context
        context.add_pass_data(CSourceCode(data.output));
        Ok(())
    }

    fn install(self, schedule: &mut crate::utils::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<Module>(&self);
        schedule.requires_resource_read::<TypeRegistry>(&self);
        schedule.produces_resource::<CSourceCode>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "YIRToC"
    }
}

