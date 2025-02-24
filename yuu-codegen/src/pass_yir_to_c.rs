use yuu_shared::{
    context::Context,
    scheduler::{Pass, ResourceId},
    type_info::{PrimitiveType, TypeInfo},
    yir::{
        self, BasicBlock, ControlFlow, Function, FunctionDeclarationState, Instruction, Module,
        Operand, Register,
    },
};

use std::fmt::Write;

const PREFIX_REGISTER: &str = "reg_";
const PREFIX_MEMORY: &str = "mem_";
const PREFIX_LABEL: &str = "lbl_";
const PREFIX_FUNCTION: &str = "fn_";

struct TransientData<'a> {
    module: &'a Module,
    output: String,
}

impl PassYirToC {
    fn write_register_name(
        reg: &Register,
        f: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}_{}", PREFIX_REGISTER, reg.name(), reg.id())
    }

    fn write_memory_name(
        reg: &Register,
        f: &mut impl std::fmt::Write,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}{}_{}", PREFIX_MEMORY, reg.name(), reg.id())
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
            TypeInfo::BuiltIn(primitive_type) => match primitive_type {
                PrimitiveType::Bool => write!(data.output, "bool"),
                PrimitiveType::F64 => write!(data.output, "double"),
                PrimitiveType::F32 => write!(data.output, "float"),
                PrimitiveType::I64 => write!(data.output, "int64_t"),
                PrimitiveType::Nil => write!(data.output, "void"),
            },
            TypeInfo::Function(_function_type) => {
                todo!()
            }
            TypeInfo::Pointer(type_info) => {
                self.gen_type(data, type_info)?;
                write!(data.output, "*")
            }
            TypeInfo::Inactive => panic!("Compiler bug: Attempted to generate C type for TypeInfo::Inactive which represents no value"),
        }
    }

    fn gen_operand(
        &self,
        data: &mut TransientData,
        operand: &Operand,
    ) -> Result<(), std::fmt::Error> {
        match operand {
            Operand::I64Const(c) => write!(data.output, "((int64_t)INT64_C({}))", c),
            Operand::F32Const(c) => write!(data.output, "({}f)", c),
            Operand::F64Const(c) => write!(data.output, "({}d)", c),
            Operand::BoolConst(c) => write!(data.output, "((bool){})", c),
            Operand::Register(register) => Self::write_register_name(register, &mut data.output),
            Operand::NoOp => Ok(()),
        }
    }

    fn gen_register(
        &self,
        data: &mut TransientData,
        reg: &Register,
    ) -> Result<(), std::fmt::Error> {
        self.gen_type(data, reg.ty())?;
        write!(data.output, " ")?;
        Self::write_register_name(reg, &mut data.output)
    }

    fn gen_instruction(
        &self,
        instruction: &Instruction,
        data: &mut TransientData,
    ) -> anyhow::Result<()> {
        match instruction {
            Instruction::Assign { target, value } => {
                self.gen_register(data, target)?;
                write!(data.output, "=")?;
                self.gen_operand(data, value)?;
            }
            Instruction::Binary {
                target,
                op,
                lhs,
                rhs,
            } => {
                self.gen_register(data, target)?;
                write!(data.output, "=")?;
                self.gen_operand(data, lhs)?;
                match op {
                    yuu_shared::yir::BinOp::Add => write!(data.output, "+"),
                    yuu_shared::yir::BinOp::Sub => write!(data.output, "-"),
                    yuu_shared::yir::BinOp::Mul => write!(data.output, "*"),
                    yuu_shared::yir::BinOp::Div => write!(data.output, "/"),
                    yuu_shared::yir::BinOp::Eq => write!(data.output, "=="),
                }?;
                self.gen_operand(data, rhs)?;
            }
            Instruction::Unary {
                target,
                op,
                operand,
            } => {
                self.gen_register(data, target)?;
                write!(data.output, "=")?;
                match op {
                    yuu_shared::yir::UnaryOp::Neg => write!(data.output, "-"),
                }?;
                self.gen_operand(data, operand)?;
            }
            Instruction::Load { target, address } => {
                self.gen_register(data, target)?;
                write!(data.output, "=*")?;
                self.gen_operand(data, address)?;
            }
            Instruction::Store { address, value } => {
                write!(data.output, "*")?;
                self.gen_operand(data, address)?;
                write!(data.output, "=")?;
                self.gen_operand(data, value)?;
            }
            Instruction::Alloca { target } => {
                self.gen_type(data, target.ty().deref_ptr())?;
                write!(data.output, " ")?;
                Self::write_memory_name(target, &mut data.output)?;
                write!(data.output, ";")?;
                self.gen_register(data, target)?;
                write!(data.output, "=&")?;
                Self::write_memory_name(target, &mut data.output)?;
            }
            Instruction::Call { target, name, args } => {
                if let Some(target) = target {
                    self.gen_type(data, target.ty())?;
                    write!(data.output, " ")?;
                    Self::write_register_name(target, &mut data.output)?;
                    write!(data.output, "=")?;
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
            }
            Instruction::Omega { target, .. } => {
                self.gen_register(data, target)?;
            }
        }
        write!(data.output, ";")?;
        Ok(())
    }

    fn gen_block(&self, block: &BasicBlock, data: &mut TransientData) -> anyhow::Result<()> {
        Self::write_label_name(block.label.name(), &mut data.output)?;
        writeln!(data.output, ":;")?;

        // Generate instructions
        for instruction in block.instructions.iter() {
            self.gen_instruction(instruction, data)?;
            writeln!(data.output)?;
        }

        // Generate terminator
        match &block.terminator {
            ControlFlow::Jump { target, writes } => {
                // Generate Omikron writes
                for (reg, value) in writes {
                    write!(data.output, "    ")?;
                    Self::write_register_name(reg, &mut data.output)?;
                    write!(data.output, "=")?;
                    self.gen_operand(data, value)?;
                    writeln!(data.output, ";")?;
                }
                write!(data.output, "    goto ")?;
                Self::write_label_name(target.name(), &mut data.output)?;
                writeln!(data.output, ";")?;
            }
            ControlFlow::Branch {
                condition,
                if_true: (true_label, true_writes),
                if_false: (false_label, false_writes),
            } => {
                write!(data.output, "    if (")?;
                self.gen_operand(data, condition)?;
                writeln!(data.output, ") {{")?;

                // Generate true branch writes
                for (reg, value) in true_writes {
                    write!(data.output, "    ")?;
                    Self::write_register_name(reg, &mut data.output)?;
                    write!(data.output, "=")?;
                    self.gen_operand(data, value)?;
                    writeln!(data.output, ";")?;
                }
                write!(data.output, "    goto ")?;
                Self::write_label_name(true_label.name(), &mut data.output)?;
                writeln!(data.output, "}}")?;

                writeln!(data.output, "else{{")?;
                // Generate false branch writes
                for (reg, value) in false_writes {
                    write!(data.output, "    ")?;
                    Self::write_register_name(reg, &mut data.output)?;
                    write!(data.output, "=")?;
                    self.gen_operand(data, value)?;
                    writeln!(data.output, ";")?;
                }
                write!(data.output, "    goto ")?;
                Self::write_label_name(false_label.name(), &mut data.output)?;
                writeln!(data.output, "}}")?;
            }
            ControlFlow::Return(value) => {
                write!(data.output, "    return ")?;
                if let Some(val) = value {
                    self.gen_operand(data, val)?;
                }
                writeln!(data.output, ";")?;
            }
        }
        Ok(())
    }

    fn gen_function(&self, func: &Function, data: &mut TransientData) -> anyhow::Result<()> {
        self.gen_block(&func.blocks[&func.entry_block], data)?;

        for (_, block) in &func.blocks {
            if block.label.name() != "entry" {
                self.gen_block(block, data)?;
            }
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
        for (i, reg) in func.params.iter().enumerate() {
            if i > 0 {
                write!(data.output, ", ")?;
            }
            self.gen_register(data, reg)?;
        }
        write!(data.output, ")")?;
        Ok(())
    }

    fn gen_module(&self, data: &mut TransientData) -> anyhow::Result<()> {
        // Add standard includes
        writeln!(data.output, "#include <stdint.h>\n")?;
        writeln!(data.output, "#include <stdbool.h>\n")?;

        // Generate function declarations
        for function_state in data.module.functions.values() {
            match function_state {
                // Predeclare all functions
                FunctionDeclarationState::Declared(func) => {
                    self.gen_func_decl(&func, data)?;
                    write!(data.output, ";")?;
                }
                FunctionDeclarationState::Defined(func) => {
                    self.gen_func_decl(&func, data)?;
                    write!(data.output, "{{")?;
                    self.gen_function(&func, data)?;
                    write!(data.output, "}}")?;
                }
            }
        }
        Ok(())
    }
}

pub struct PassYirToC;

impl Default for PassYirToC {
    fn default() -> Self {
        Self
    }
}

pub struct CSourceCode(pub String);

impl ResourceId for CSourceCode {
    fn resource_name() -> &'static str {
        "CSourceCode"
    }
}

impl Pass for PassYirToC {
    fn run(&self, context: &mut Context) -> anyhow::Result<()> {
        let module = context.require_pass_data::<Module>(self);
        let module = module.lock().unwrap();
        let mut data = TransientData {
            module: &module,
            output: String::new(),
        };

        // Generate code for each function
        self.gen_module(&mut data)?;

        // Add the generated code to the context
        context.add_pass_data(CSourceCode(data.output));
        Ok(())
    }

    fn install(self, schedule: &mut yuu_shared::scheduler::Schedule)
    where
        Self: Sized,
    {
        schedule.requires_resource_read::<Module>(&self);
        schedule.produces_resource::<CSourceCode>(&self);
        schedule.add_pass(self);
    }

    fn get_name(&self) -> &'static str {
        "YIRToC"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use yuu_parse::{lexer::UnprocessedCodeInfo, pass_parse::ParsePass};
    use yuu_shared::scheduler::{Schedule, Scheduler};
    use yuu_transform::{
        pass_ast_to_yir::PassAstToYir, pass_collect_decls::PassCollectDecls,
        type_inference::PassTypeInference,
    };

    #[test]
    fn test_fac_to_c() {
        // Create the factorial function code
        let code_info = UnprocessedCodeInfo {
            code: Arc::from(
                r#"fn fac(n: i64) -> i64 {
                    out if n == 0 {
                        out 1;
                    }
                    else {
                        let n_out = n * fac(n - 1);
                        out n_out;
                    };
                }"#,
            ),
            file_name: Arc::from("test.yuu"),
        };

        // Create a new context and add the code info
        let mut context = Context::new();
        context.add_pass_data(code_info);

        // Create and configure the schedule
        let mut schedule = Schedule::new();

        // Add passes in the correct order
        ParsePass.install(&mut schedule);
        PassCollectDecls::new().install(&mut schedule);
        PassTypeInference::new().install(&mut schedule);
        PassAstToYir.install(&mut schedule);
        PassYirToC.install(&mut schedule);

        // Run the schedule
        let scheduler = Scheduler::new();
        let context = scheduler
            .run(schedule, context)
            .expect("Failed to run schedule");

        // Get and print the generated C code
        let c_code = context.require_pass_data::<CSourceCode>(&PassYirToC);
        let c_code = c_code.lock().unwrap();
        println!("Generated C code:\n\n```C\n{}\n```", c_code.0);
    }
}
