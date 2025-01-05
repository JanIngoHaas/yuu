use std::fmt::Display;

use crate::yir::{
    Branch, Function, FunctionDeclarationState, Goto, Inst, InstKind, Instructions, Label, Load,
    Module, Operand, RegisterCreateInfo, Store,
};

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_op(self))
    }
}

impl Display for RegisterCreateInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}.{}@{}", self.name(), self.node_id(), self.ty())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "module {{")?;
        for (name, state) in &self.functions {
            match state {
                FunctionDeclarationState::Declared(ty) => {
                    writeln!(f, "  declare {} : {}", name, ty)?;
                }
                FunctionDeclarationState::Defined(func) => {
                    write!(f, "{}", func)?;
                }
            }
        }
        writeln!(f, "}}")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  fn {} {{", self.name)?;
        write!(f, "{}", self.instructions)?;
        writeln!(f, "  }}")
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for inst in &self.instructions {
            if let InstKind::LabelKind(_) = inst.kind {
                writeln!(f, "{}", format_inst(inst, self))?;
            } else {
                writeln!(f, "   {}", format_inst(inst, self))?;
            }
        }
        Ok(())
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.name, self.id)
    }
}

fn format_inst(inst: &Inst, instrs: &Instructions) -> String {
    match &inst.kind {
        InstKind::AddKind(triple) => {
            format!("{}:={} + {}", triple.register, triple.op1, triple.op2)
        }
        InstKind::SubKind(triple) => {
            format!("{}:={} - {}", triple.register, triple.op1, triple.op2)
        }
        InstKind::MulKind(triple) => {
            format!("{}:={} * {}", triple.register, triple.op1, triple.op2)
        }
        InstKind::DivKind(triple) => {
            format!("{}:={} / {}", triple.register, triple.op1, triple.op2)
        }
        InstKind::NegKind(pair) => format!("{}:=neg {}", pair.register, pair.op),
        InstKind::LabelKind(name) => format!("{}", name),
        InstKind::GotoKind(goto) => format!("goto :{}", {
            let label_name = instrs.goto_label(goto.label);
            let label = instrs.get_as_label(label_name.id).unwrap();
            label
        }),
        InstKind::BranchKind(Branch {
            cond,
            if_true,
            if_false,
        }) => {
            let true_label_name = instrs.goto_label(*if_true);
            let true_label = instrs.get_as_label(true_label_name.id).unwrap();

            let false_label_name = instrs.goto_label(*if_false);
            let false_label = instrs.get_as_label(false_label_name.id).unwrap();

            format!("if {} goto :{} elsegoto :{}", cond, true_label, false_label)
        }
        InstKind::AllocaKind(reg) => format!("{}:=alloca", reg.register),
        InstKind::StoreKind(Store { target, value }) => {
            format!("{} -> {}", value, target.register_create_info)
        }
        InstKind::LoadKind(Load { target, source }) => {
            format!("{} <- {}", target, source)
        }
        InstKind::CallInstKind(call) => {
            let target_str = call
                .target
                .as_ref()
                .map(|info| format!("{}:= ", info))
                .unwrap_or_default();
            let args = call
                .args
                .iter()
                .map(format_op)
                .collect::<Vec<_>>()
                .join(",");
            format!("{}call {}({})", target_str, call.func_name, args)
        }
        InstKind::EqKind(inst_triple) => format!(
            "{}:={} == {}",
            inst_triple.register, inst_triple.op1, inst_triple.op2
        ),
    }
}

fn format_op(op: &Operand) -> String {
    match op {
        Operand::RegisterOp(id) => format!("{}", id.register_create_info),
        Operand::ImmediateI64Op(val) => format!("{}@i64", val),
        Operand::ImmediateF32Op(val) => format!("{}@f32", val),
        Operand::ImmediateF64Op(val) => format!("{}@f64", val),
        Operand::ImmediateBoolOp(val) => format!("{}@bool", val),
        Operand::NoOp => "nop".to_string(),
    }
}
