use std::rc::Rc;
use crate::{ast::NodeId, type_info::{self, TypeInfo}};
pub type VariableId = i64;
pub type LabelId = i64;

#[derive(Clone)]
pub enum YirTypeInfo {
    YuuPrimitiveType(type_info::PrimitiveType),
}

#[derive(Clone)]
pub struct Register {
    pub name: Option<String>,
    pub id: VariableId,
    pub ty: Rc<YirTypeInfo>,
}

pub enum Operand {
    RegisterOp(Register),
    ImmediateI64Op(i64),
    ImmediateF32Op(f32),
    ImmediateF64Op(f64),
}

pub struct InstTriple {
    pub register: Register,
    pub op1: Operand,
    pub op2: Operand,
}

pub struct InstPair {
    pub mem: Register,
    pub op: Operand,
}

pub struct MergeNode {
    pub op: Operand,
    pub label: LabelId,
}

pub struct SlotDataFlow {
    pub written_by: Vec<InstId>,
    pub read_by: Vec<InstId>,
}

pub type InstId = i64;

pub struct Inst {
    pub kind: InstKind,
    pub id: InstId,
}

pub enum InstKind {
    Add(InstTriple),
    Sub(InstTriple),
    Mul(InstTriple),
    Div(InstTriple),
    Neg(InstPair),

    // Labels & Branches
    Label(LabelId, Option<String>), 
    Goto(LabelId),
    Branch(Operand, LabelId, LabelId),

    // Alloc 
    Alloca(Register),

    // Load & Store
    Store(Register, Operand),
    Load(Register, Operand),

    // Create a Slot - can be written to multiple times and read from only once
    // We also need to keep track of the data flow that goes through the slot
    Slot(Register, SlotDataFlow),
}

impl Default for SlotDataFlow {
    fn default() -> Self {
        Self {
            written_by: Vec::new(),
            read_by: Vec::new(),
        }
    }
}

pub struct InstructionList {
    pub instructions: Vec<Option<Inst>>, 
}

