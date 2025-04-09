use crate::ast::InternUstr;
use crate::scheduler::{ResourceId, ResourceName};
use crate::type_info::{
    TypeInfo, primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil,
};
use crate::type_registry::{StructFieldInfo, StructInfo};
use crate::yir_printer;
use indexmap::IndexMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};
use ustr::{Ustr, UstrMap};

/*
Coloring and pretty printing of YIR mostly implemented by Claude Sonnet 3.5 /
*/

#[derive(Copy, Clone)]
pub struct Variable {
    name: Ustr,
    id: i64,
    ty: &'static TypeInfo,
}

impl Hash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Variable {}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Label {
    name: Ustr,
    id: i64,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.id)
    }
}

impl Variable {
    pub fn new(name: Ustr, id: i64, ty: &'static TypeInfo) -> Self {
        Self { name, id, ty }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> i64 {
        self.id
    }

    pub fn write_unique_name(&self, out: &mut impl fmt::Write) -> fmt::Result {
        write!(out, "{}_{}", self.name, self.id)
    }

    pub fn ty(&self) -> &'static TypeInfo {
        self.ty
    }
}

impl Label {
    fn new(name: Ustr, id: i64) -> Self {
        Self { name, id }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> i64 {
        self.id
    }
}

#[derive(Clone)]
pub enum Operand {
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    BoolConst(bool),
    Variable(Variable),
    NoOp,
}

impl Operand {
    pub fn ty(&self) -> &'static TypeInfo {
        match self {
            Operand::I64Const(_) => primitive_i64(),
            Operand::F32Const(_) => primitive_f32(),
            Operand::F64Const(_) => primitive_f64(),
            Operand::BoolConst(_) => primitive_bool(),
            Operand::Variable(reg) => reg.ty(),
            Operand::NoOp => primitive_nil(),
        }
    }
}

#[derive(Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

#[derive(Clone)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone)]
pub enum Instruction {
    // SizeOf {
    //     target: Variable,
    //     ty: &'static TypeInfo,
    // },
    // AlignOf {
    //     target: Variable,
    //     ty: &'static TypeInfo,
    // },
    GetFieldPtr {
        target: Variable,
        base: Operand,
        field: Ustr,
    },
    BitwiseCopy {
        target: Variable,
        value: Operand,
    },
    Binary {
        target: Variable,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
    },
    Unary {
        target: Variable,
        op: UnaryOp,
        operand: Operand,
    },
    TakeAddress {
        target: Variable,
        value: Operand,
    },
    Store {
        dest: Variable,
        src: Operand,
    },
    Call {
        target: Option<Variable>,
        name: Ustr,
        args: Vec<Operand>,
    },
    Omega {
        target: Variable,
        writable_blocks: Arc<Mutex<Vec<Label>>>, // Share the same Arc<Mutex> as in omega_blocks
    },
}

#[derive(Clone)]
pub enum ControlFlow {
    Jump {
        target: Label,
        writes: Vec<(Variable, Operand)>, // Omikron writes at jump
    },
    Branch {
        condition: Operand,
        if_true: (Label, Vec<(Variable, Operand)>), // Writes for true branch
        if_false: (Label, Vec<(Variable, Operand)>), // Writes for false branch
    },
    Return(Option<Operand>),
    Fallthrough(Vec<(Variable, Operand)>), // Writes for fallthrough
}

#[derive(Clone)]
pub struct BasicBlock {
    pub label: Label,
    pub instructions: Vec<Instruction>,
    pub terminator: ControlFlow,
}

impl BasicBlock {
    pub fn fallthrough_to(&self) -> Option<i64> {
        match &self.terminator {
            ControlFlow::Fallthrough(_) => Some(self.label.id() + 1),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: Ustr,
    pub params: Vec<Variable>,
    pub return_type: &'static TypeInfo,
    pub blocks: IndexMap<i64, BasicBlock>,
    pub entry_block: i64,
    current_block: i64,
    next_reg_id: i64,
    next_label_id: i64,
    omega_blocks: IndexMap<Variable, Arc<Mutex<Vec<Label>>>>, // Track blocks separately
}

impl Function {
    pub fn new(name: Ustr, return_type: &'static TypeInfo) -> Self {
        let mut f = Function {
            name,
            params: Vec::new(),
            return_type,
            blocks: IndexMap::new(),
            entry_block: 0,
            current_block: 0,
            next_reg_id: 0,
            next_label_id: 0,
            omega_blocks: IndexMap::new(),
        };
        f.add_block("entry".intern());
        f
    }

    pub fn fresh_variable(&mut self, name: Ustr, ty: &'static TypeInfo) -> Variable {
        let id = self.next_reg_id;
        self.next_reg_id += 1;
        Variable::new(name, id, ty)
    }

    pub fn get_block_mut(&mut self, id: i64) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&id)
    }

    pub fn get_current_block_mut(&mut self) -> &mut BasicBlock {
        self.blocks
            .get_mut(&self.current_block)
            .expect("Compiler Error: No current block")
    }

    pub fn get_current_block(&self) -> Option<&BasicBlock> {
        self.blocks.get(&self.current_block)
    }

    pub fn get_current_block_label(&self) -> &Label {
        &self.blocks.get(&self.current_block).unwrap().label
    }

    pub fn current_block(&self) -> i64 {
        self.current_block
    }

    fn fresh_label(&mut self, name: Ustr) -> Label {
        let id = self.next_label_id;
        self.next_label_id += 1;
        Label::new(name, id)
    }

    pub fn add_block(&mut self, name: Ustr) -> Label {
        let label = self.fresh_label(name);
        self.blocks.insert(
            label.id(),
            BasicBlock {
                label: label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Fallthrough(Vec::new()),
            },
        );
        label
    }

    fn update_omega_writes<'a>(&mut self, writes: impl Iterator<Item = &'a (Variable, Operand)>) {
        let current_label = self.blocks.get(&self.current_block).unwrap().label.clone();

        for (reg, _) in writes {
            if let Some(blocks) = self.omega_blocks.get(reg) {
                let mut blocks = blocks.lock().unwrap();
                blocks.push(current_label.clone());
            }
        }
    }

    fn gen_writes_if_not_nop(
        &mut self,
        writes: Vec<(Variable, Operand)>,
    ) -> Vec<(Variable, Operand)> {
        writes
            .into_iter()
            .filter(|(_, op)| !matches!(op, Operand::NoOp))
            .collect()
    }

    pub fn has_terminator(&self, block_id: i64) -> bool {
        self.blocks
            .get(&block_id)
            .map(|block| !matches!(block.terminator, ControlFlow::Fallthrough(_)))
            .unwrap_or(false)
    }

    pub fn make_jump_if_no_terminator(&mut self, target: Label, writes: Vec<(Variable, Operand)>) {
        if !self.has_terminator(self.current_block) {
            self.make_jump(target, writes);
        }
    }

    pub fn make_jump(&mut self, target: Label, writes: Vec<(Variable, Operand)>) {
        let writes = self.gen_writes_if_not_nop(writes);
        self.update_omega_writes(writes.iter());
        self.get_current_block_mut().terminator = ControlFlow::Jump { target, writes };
    }

    pub fn make_return(&mut self, value: Option<Operand>) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = ControlFlow::Return(value);
        }
    }

    pub fn make_fallthrough(&mut self, writes: Vec<(Variable, Operand)>) {
        let writes = self.gen_writes_if_not_nop(writes);
        self.update_omega_writes(writes.iter());
        self.blocks
            .get_mut(&self.current_block)
            .expect("Compiler Error: No current block")
            .terminator = ControlFlow::Fallthrough(writes);
    }

    pub fn set_branch_writes(
        &mut self,
        condition: Operand,
        if_true: (Label, Vec<(Variable, Operand)>),
        if_false: (Label, Vec<(Variable, Operand)>),
    ) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = ControlFlow::Branch {
                condition,
                if_true,
                if_false,
            };
        }
    }

    pub fn make_unary(
        &mut self,
        target_name: Ustr,
        ty: &'static TypeInfo,
        op: UnaryOp,
        operand: Operand,
    ) -> Variable {
        let target = self.fresh_variable(target_name, ty);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Unary {
                target,
                op,
                operand,
            });
        }
        target
    }

    pub fn make_get_field_ptr(
        &mut self,
        target_name: Ustr,
        base: Operand,
        field_info: &StructFieldInfo,
    ) -> Variable {
        let ty = field_info.ty.ptr_to();
        let target = self.fresh_variable(target_name, ty);
        let block = self.get_current_block_mut();
        block.instructions.push(Instruction::GetFieldPtr {
            target,
            base,
            field: field_info.name,
        });
        target
    }

    pub fn make_binary(
        &mut self,
        target_name: Ustr,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
        ty: &'static TypeInfo,
    ) -> Variable {
        let target = self.fresh_variable(target_name, ty);
        let block = self.get_current_block_mut();
        block.instructions.push(Instruction::Binary {
            target: target.clone(),
            op,
            lhs,
            rhs,
        });
        target
    }

    pub fn make_store(&mut self, dest: Variable, src: Operand) {
        let block = self.get_current_block_mut();
        block.instructions.push(Instruction::Store { dest, src });
    }

    pub fn make_take_address(
        &mut self,
        name: Ustr,
        value: Operand,
        value_type: &'static TypeInfo,
    ) -> Variable {
        let target = self.fresh_variable(name, value_type.ptr_to());
        let block = self.get_current_block_mut();
        block
            .instructions
            .push(Instruction::TakeAddress { target, value });
        target
    }

    // pub fn make_alloca(&mut self, name: Ustr, value_type: &'static TypeInfo) -> variable {
    //     let ptr_type = value_type.ptr_to();
    //     let target = self.fresh_variable(name, ptr_type);
    //     if let Some(block) = self.blocks.get_mut(&self.current_block) {
    //         block.instructions.push(Instruction::Alloca {
    //             target: target.clone(),
    //         });
    //     }
    //     target
    // }

    // pub fn make_sizeof(&mut self, target_name: Ustr, ty: &'static TypeInfo) {
    //     let target = self.fresh_variable(target_name, primitive_i64());
    //     let block = self.get_current_block_mut();
    //     block.instructions.push(Instruction::SizeOf { target, ty });
    // }

    // pub fn make_alignof(&mut self, target: Variable, ty: &'static TypeInfo) {
    //     let block = self.get_current_block_mut();
    //     block.instructions.push(Instruction::AlignOf { target, ty });
    // }

    pub fn make_call(
        &mut self,
        name: Ustr,
        func_name: Ustr,
        args: Vec<Operand>,
        return_type: &'static TypeInfo,
    ) -> Option<Variable> {
        if return_type.is_nil() {
            return None;
        }
        let target = self.fresh_variable(name, return_type);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Call {
                target: Some(target.clone()),
                name: func_name,
                args,
            });
        }
        Some(target)
    }

    pub fn make_branch_to_existing(
        &mut self,
        condition: Operand,
        then: Label,
        else_: Label,
        then_writes: Vec<(Variable, Operand)>,
        else_writes: Vec<(Variable, Operand)>,
    ) {
        let then_writes = self.gen_writes_if_not_nop(then_writes);
        let else_writes = self.gen_writes_if_not_nop(else_writes);
        self.update_omega_writes(then_writes.iter());
        self.update_omega_writes(else_writes.iter());

        self.blocks
            .get_mut(&self.current_block)
            .expect("Compiler Error: No current block")
            .terminator = ControlFlow::Branch {
            condition,
            if_true: (then, then_writes),
            if_false: (else_, else_writes),
        };
    }

    pub fn make_branch(
        &mut self,
        condition: Operand,
        then_writes: Vec<(Variable, Operand)>,
        else_writes: Vec<(Variable, Operand)>,
    ) -> (Label, Label) {
        let true_label = self.fresh_label("then".intern());
        let false_label = self.fresh_label("else".intern());

        let then_writes = self.gen_writes_if_not_nop(then_writes);
        let else_writes = self.gen_writes_if_not_nop(else_writes);

        self.update_omega_writes(then_writes.iter());
        self.update_omega_writes(else_writes.iter());

        self.blocks
            .get_mut(&self.current_block)
            .expect("Compiler Error: No current block")
            .terminator = ControlFlow::Branch {
            condition,
            if_true: (true_label.clone(), then_writes),
            if_false: (false_label.clone(), else_writes),
        };

        self.blocks.insert(
            true_label.id(),
            BasicBlock {
                label: true_label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Fallthrough(Vec::new()),
            },
        );

        self.blocks.insert(
            false_label.id(),
            BasicBlock {
                label: false_label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Fallthrough(Vec::new()),
            },
        );

        (true_label, false_label)
    }

    pub fn set_current_block(&mut self, current: &Label) {
        self.current_block = current.id();
    }

    // fn set_terminator(&mut self, terminator: ControlFlow) {
    //     if let Some(block) = self.blocks.get_mut(&self.current_block) {
    //         block.terminator = terminator;
    //     }
    // }

    pub fn make_omega(&mut self, target_name: Ustr, ty: &'static TypeInfo) -> Variable {
        let target = self.fresh_variable(target_name, ty);
        let blocks = Arc::new(Mutex::new(Vec::new()));

        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            // Use the same Arc<Mutex> for both the instruction and the tracking map
            block.instructions.push(Instruction::Omega {
                target: target.clone(),
                writable_blocks: Arc::clone(&blocks), // Share the same Arc
            });
            self.omega_blocks.insert(target, blocks);
        }
        target
    }

    pub fn format_yir(&self, do_color: bool, f: &mut impl fmt::Write) -> fmt::Result {
        yir_printer::format_yir(self, do_color, f)
    }
}

pub struct Module {
    pub functions: UstrMap<FunctionDeclarationState>,
    pub structs: Vec<Ustr>, // Just the names... NOthing else is really needed for
}

impl ResourceId for Module {
    fn resource_name() -> ResourceName {
        "Module"
    }
}

impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: UstrMap::default(),
            structs: Vec::new(),
        }
    }

    pub fn define_struct(&mut self, name: Ustr) {
        self.structs.push(name);
    }

    pub fn declare_function(&mut self, func: Function) {
        self.functions.insert(
            func.name.clone(),
            FunctionDeclarationState::Declared(Arc::new(func)),
        );
    }

    pub fn define_function(&mut self, func: Function) {
        self.functions.insert(
            func.name.clone(),
            FunctionDeclarationState::Defined(Arc::new(func)),
        );
    }

    pub fn format_yir(&self, do_color: bool, f: &mut impl fmt::Write) -> fmt::Result {
        for (name, func_state) in &self.functions {
            match func_state {
                FunctionDeclarationState::Declared(func) => {
                    writeln!(
                        f,
                        "{} {} : {}",
                        yir_printer::format_keyword("declare", do_color),
                        name,
                        yir_printer::format_type(func.return_type, do_color)
                    )?;
                }
                FunctionDeclarationState::Defined(func) => {
                    func.format_yir(do_color, f)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum FunctionDeclarationState {
    Declared(Arc<Function>),
    Defined(Arc<Function>),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_yir(false, f)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_yir(false, f)
    }
}
