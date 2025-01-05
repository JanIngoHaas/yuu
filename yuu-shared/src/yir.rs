use crate::{
    ast::NodeId,
    type_info::{self, TypeInfo},
};
use hashbrown::HashMap;
use std::fmt::{Display, Formatter, Result};
use std::{hash::Hash, rc::Rc};

pub type RegisterId = i64;

#[derive(Clone)]
pub enum YirTypeInfo {
    YuuPrimitiveType(type_info::PrimitiveType),
    FunctionType(Rc<[Rc<YirTypeInfo>]>, Rc<YirTypeInfo>),
    Pointer(Rc<YirTypeInfo>),
}

impl YirTypeInfo {
    pub fn ptr_to(self: Rc<Self>) -> Rc<Self> {
        Rc::new(YirTypeInfo::Pointer(self))
    }
}

impl Display for YirTypeInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            YirTypeInfo::YuuPrimitiveType(prim) => write!(f, "{}", prim),
            YirTypeInfo::FunctionType(args, ret) => {
                write!(f, "(")?;
                let mut first = true;
                for arg in args.iter() {
                    if !first {
                        write!(f, ",")?;
                    }
                    first = false;
                    write!(f, "{}", arg)?;
                }
                write!(f, ")->{}", ret)
            }
            YirTypeInfo::Pointer(inner) => write!(f, "ptr {}", inner),
        }
    }
}

#[derive(Clone)]
pub struct RegisterCreateInfo {
    inner: Rc<RegisterCreateInfoInner>,
}

impl RegisterCreateInfo {
    pub fn assume_exists(&self) -> RegisterRef {
        RegisterRef {
            referenced_id: -1,
            register_create_info: self.clone(),
        }
    }
}

struct RegisterCreateInfoInner {
    pub name: String,
    pub ty: Rc<YirTypeInfo>,
    pub node_id: NodeId,
}

impl RegisterCreateInfo {
    pub fn new(name: String, ty: Rc<YirTypeInfo>, node_id: NodeId) -> Self {
        Self {
            inner: Rc::new(RegisterCreateInfoInner { name, ty, node_id }),
        }
    }

    pub fn name(&self) -> &str {
        &self.inner.name
    }

    pub fn ty(&self) -> &Rc<YirTypeInfo> {
        &self.inner.ty
    }

    pub fn node_id(&self) -> NodeId {
        self.inner.node_id
    }
}

pub struct Function {
    pub name: String,
    pub function_type: Rc<YirTypeInfo>,
    pub instructions: Instructions,
    pub variables: HashMap<String, RegisterRef>,
}

impl Function {
    pub fn new(name: String, function_type: Rc<YirTypeInfo>) -> Self {
        Self {
            name,
            function_type,
            instructions: Instructions::new(),
            variables: HashMap::new(),
        }
    }

    pub fn new_main() -> Self {
        Self {
            name: "main".to_string(),
            function_type: Rc::new(YirTypeInfo::FunctionType(
                Rc::new([]),
                Rc::new(YirTypeInfo::YuuPrimitiveType(type_info::PrimitiveType::I64)),
            )),
            instructions: Instructions::new(),
            variables: HashMap::new(),
        }
    }
}

pub enum FunctionDeclarationState {
    Declared(Rc<YirTypeInfo>),
    Defined(Rc<Function>),
}

pub struct Module {
    pub functions: HashMap<String, FunctionDeclarationState>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn declare_function(&mut self, name: String, func_type: Rc<YirTypeInfo>) {
        self.functions
            .insert(name, FunctionDeclarationState::Declared(func_type));
    }

    pub fn define_function(&mut self, func: Function) {
        //TODO: Check if the function was previously declared
        self.functions.insert(
            func.name.clone(),
            FunctionDeclarationState::Defined(Rc::new(func)),
        );
    }

    pub fn get_defined_function(&self, name: &str) -> Option<Rc<Function>> {
        self.functions.get(name).and_then(|state| match state {
            FunctionDeclarationState::Defined(func) => Some(func.clone()),
            _ => None,
        })
    }

    pub fn get_declared_function(&self, name: &str) -> Option<Function> {
        self.functions.get(name).and_then(|state| match state {
            FunctionDeclarationState::Declared(ty) => {
                Some(Function::new(name.to_string(), ty.clone()))
            }
            _ => None,
        })
    }

    pub fn is_declared(&self, name: &str) -> bool {
        self.functions.get(name).is_some()
    }

    pub fn is_defined(&self, name: &str) -> bool {
        self.functions.get(name).map_or(false, |state| match state {
            FunctionDeclarationState::Defined(_) => true,
            _ => false,
        })
    }
}

pub enum Operand {
    RegisterOp(RegisterRef), // Takes the value from the register at the given InstId
    ImmediateI64Op(i64),
    ImmediateF32Op(f32),
    ImmediateF64Op(f64),
    ImmediateBoolOp(bool),
    NoOp,
}

#[derive(Clone)]
pub struct RegisterRef {
    pub referenced_id: InstId,
    pub register_create_info: RegisterCreateInfo,
}

pub struct CallInst {
    pub target: Option<RegisterCreateInfo>,
    pub func_name: String,
    pub args: Vec<Operand>,
}

pub struct InstTriple {
    pub register: RegisterCreateInfo,
    pub op1: Operand,
    pub op2: Operand,
}

pub struct InstPair {
    pub register: RegisterCreateInfo,
    pub op: Operand,
}

pub struct SlotDataFlow {
    pub written_by: Vec<InstId>,
    pub read_by: Vec<InstId>,
}

pub type InstId = i64;
pub type LabelId = i64;

pub struct Inst {
    pub kind: InstKind,
    pub id: InstId,
    pub next: Option<InstId>,
    pub prev: Option<InstId>,
}

pub struct Label {
    pub name: String,
    pub id: LabelId,
}

pub struct Goto {
    pub label: LabelId,
}

pub struct Branch {
    pub cond: Operand,
    pub if_true: LabelId,
    pub if_false: LabelId,
}

pub struct Alloca {
    pub register: RegisterCreateInfo,
}

pub struct Store {
    pub target: RegisterRef,
    pub value: Operand,
}

pub struct Load {
    pub target: RegisterCreateInfo,
    pub source: Operand,
}

pub enum InstKind {
    AddKind(InstTriple),
    SubKind(InstTriple),
    MulKind(InstTriple),
    DivKind(InstTriple),
    EqKind(InstTriple),
    NegKind(InstPair),

    // Labels & Branches
    LabelKind(Label),
    GotoKind(Goto),
    BranchKind(Branch),

    // Alloc
    AllocaKind(Alloca),

    // Load & Store
    StoreKind(Store),
    LoadKind(Load),

    // Function calls
    CallInstKind(CallInst),
}

impl Default for SlotDataFlow {
    fn default() -> Self {
        Self {
            written_by: Vec::new(),
            read_by: Vec::new(),
        }
    }
}

pub struct Instructions {
    pub instructions: Vec<Inst>,
    pub next_label: LabelId,
    pub label_map: HashMap<LabelId, InstId>,
    pub inst_indices: HashMap<InstId, usize>, // Maps InstId to index in instructions Vec
}

impl Instructions {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            next_label: 0,
            label_map: HashMap::new(),
            inst_indices: HashMap::new(),
        }
    }

    pub fn fresh_label(&mut self) -> LabelId {
        let label = self.next_label;
        self.next_label += 1;
        label
    }

    pub fn make_register_create_info(
        &mut self,
        node_id: NodeId,
        name: String,
        ty: Rc<YirTypeInfo>,
    ) -> RegisterCreateInfo {
        RegisterCreateInfo::new(name, ty, node_id)
    }

    // Helper method to create RegisterRef
    fn create_ref(&mut self, kind: InstKind, info: RegisterCreateInfo) -> RegisterRef {
        let id = self.add(kind);
        RegisterRef {
            referenced_id: id,
            register_create_info: info,
        }
    }

    pub fn add_label(&mut self, name: String, label_id: LabelId) -> InstId {
        let kind = InstKind::LabelKind(Label { name, id: label_id });
        let id = self.add(kind);
        self.label_map.insert(label_id, id);
        id
    }

    pub fn goto_label(&self, label_id: LabelId) -> &Inst {
        self.label_map
            .get(&label_id)
            .copied()
            .and_then(|x| self.get(x))
            .unwrap()
    }

    pub fn get_as_goto(&self, id: InstId) -> Option<&Goto> {
        self.get(id).and_then(|inst| match &inst.kind {
            InstKind::GotoKind(goto) => Some(goto),
            _ => None,
        })
    }

    pub fn get_as_branch(&self, id: InstId) -> Option<&Branch> {
        self.get(id).and_then(|inst| match &inst.kind {
            InstKind::BranchKind(branch) => Some(branch),
            _ => None,
        })
    }

    pub fn get_as_call_inst(&self, id: InstId) -> Option<&CallInst> {
        self.get(id).and_then(|inst| match &inst.kind {
            InstKind::CallInstKind(call) => Some(call),
            _ => None,
        })
    }

    pub fn get_as_label(&self, id: InstId) -> Option<&Label> {
        self.get(id).and_then(|inst| match &inst.kind {
            InstKind::LabelKind(label) => Some(label),
            _ => None,
        })
    }

    pub fn add_goto(&mut self, label: LabelId) -> InstId {
        let kind = InstKind::GotoKind(Goto { label });
        self.add(kind)
    }

    pub fn add_branch(&mut self, cond: Operand, if_true: LabelId, if_false: LabelId) -> InstId {
        let kind = InstKind::BranchKind(Branch {
            cond,
            if_true,
            if_false,
        });
        self.add(kind)
    }

    pub fn add_alloca(&mut self, reg: RegisterCreateInfo) -> RegisterRef {
        let kind = InstKind::AllocaKind(Alloca {
            register: reg.clone(),
        });
        let refd = self.add(kind);
        RegisterRef {
            referenced_id: refd,
            register_create_info: reg,
        }
    }

    pub fn add_store(&mut self, target: RegisterRef, value: Operand) -> InstId {
        let kind = InstKind::StoreKind(Store { target, value });
        self.add(kind)
    }

    pub fn add_load(&mut self, target: RegisterCreateInfo, source: Operand) -> RegisterRef {
        let kind = InstKind::LoadKind(Load {
            target: target.clone(),
            source,
        });
        let refd = self.add(kind);
        RegisterRef {
            referenced_id: refd,
            register_create_info: target,
        }
    }

    pub fn add_bin_op(
        &mut self,
        op: impl FnOnce(InstTriple) -> InstKind,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        self.create_ref(
            op(InstTriple {
                register: target.clone(),
                op1,
                op2,
            }),
            target,
        )
    }

    pub fn add_bin_add(
        &mut self,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        self.add_bin_op(InstKind::AddKind, target, op1, op2)
    }

    pub fn add_bin_sub(
        &mut self,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        self.add_bin_op(InstKind::SubKind, target, op1, op2)
    }

    pub fn add_bin_mul(
        &mut self,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        let kind = InstKind::MulKind(InstTriple {
            register: target.clone(),
            op1,
            op2,
        });
        let refd = self.add(kind);
        RegisterRef {
            referenced_id: refd,
            register_create_info: target,
        }
    }

    pub fn add_bin_div(
        &mut self,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        let kind = InstKind::DivKind(InstTriple {
            register: target.clone(),
            op1,
            op2,
        });
        let refd = self.add(kind);
        RegisterRef {
            referenced_id: refd,
            register_create_info: target,
        }
    }

    pub fn add_bin_eq(
        &mut self,
        target: RegisterCreateInfo,
        op1: Operand,
        op2: Operand,
    ) -> RegisterRef {
        let kind = InstKind::EqKind(InstTriple {
            register: target.clone(),
            op1,
            op2,
        });
        let refd = self.add(kind);
        RegisterRef {
            referenced_id: refd,
            register_create_info: target,
        }
    }

    pub fn add_un_op(&mut self, target: RegisterCreateInfo, op: Operand) -> RegisterRef {
        self.create_ref(
            InstKind::NegKind(InstPair {
                register: target.clone(),
                op,
            }),
            target,
        )
    }

    pub fn add_un_neg(&mut self, target: RegisterCreateInfo, op: Operand) -> RegisterRef {
        self.add_un_op(target, op)
    }

    pub fn add_un_pos(&mut self, target: RegisterCreateInfo, op: Operand) -> RegisterRef {
        self.add_un_op(target, op)
    }

    pub fn add_call(
        &mut self,
        target: Option<RegisterCreateInfo>,
        func_name: String,
        args: Vec<Operand>,
    ) -> Option<RegisterRef> {
        let kind = InstKind::CallInstKind(CallInst {
            target: target.clone(),
            func_name,
            args,
        });
        let refd = self.add(kind);
        target.map(|info| RegisterRef {
            referenced_id: refd,
            register_create_info: info,
        })
    }

    pub fn add(&mut self, inst: InstKind) -> InstId {
        let id = self.instructions.len() as InstId;
        let inst = Inst {
            kind: inst,
            id,
            next: None,
            prev: if self.instructions.is_empty() {
                None
            } else {
                Some((self.instructions.len() - 1) as InstId)
            },
        };

        // Update the previous instruction's next pointer
        if let Some(prev_idx) = self.instructions.len().checked_sub(1) {
            if let Some(prev_inst) = self.instructions.get_mut(prev_idx) {
                prev_inst.next = Some(id);
            }
        }

        // Add the instruction and update the index map
        self.inst_indices.insert(id, self.instructions.len());
        self.instructions.push(inst);
        id
    }

    pub fn get(&self, id: InstId) -> Option<&Inst> {
        self.inst_indices
            .get(&id)
            .and_then(|&idx| self.instructions.get(idx))
    }

    pub fn get_mut(&mut self, id: InstId) -> Option<&mut Inst> {
        if let Some(&idx) = self.inst_indices.get(&id) {
            self.instructions.get_mut(idx)
        } else {
            None
        }
    }
}
