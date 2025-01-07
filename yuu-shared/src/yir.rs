use crate::scheduler::{ResourceId, ResourceName};
use crate::type_info::TypeInfo;
use hashbrown::HashMap;
use std::fmt;

#[derive(Clone)]
pub struct Register {
    name: String,
    id: i64,
    ty: &'static TypeInfo,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct Label {
    name: String,
    id: i64,
}

impl Register {
    pub fn new(name: String, id: i64, ty: &'static TypeInfo) -> Self {
        Self { name, id, ty }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> i64 {
        self.id
    }

    pub fn ty(&self) -> &'static TypeInfo {
        self.ty
    }
}

impl Label {
    fn new(name: String, id: i64) -> Self {
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
    Register(Register),
    NoOp,
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
    Assign {
        target: Register,
        value: Operand,
    },
    Binary {
        target: Register,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
    },
    Unary {
        target: Register,
        op: UnaryOp,
        operand: Operand,
    },
    Load {
        target: Register,
        address: Operand,
    },
    Store {
        address: Operand,
        value: Operand,
    },
    Alloca {
        target: Register,
    },
    Call {
        target: Option<Register>,
        name: String,
        args: Vec<Operand>,
    },
    Phi {
        target: Register,
        incoming: Vec<(Label, Operand)>,
    },
}

#[derive(Clone)]
pub enum ControlFlow {
    Jump(Label),
    Branch {
        condition: Operand,
        if_true: Label,
        if_false: Label,
    },
    Return(Option<Operand>),
}

#[derive(Clone)]
pub struct BasicBlock {
    pub label: Label,
    pub instructions: Vec<Instruction>,
    pub terminator: ControlFlow,
    pub predecessors: Vec<Label>,
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, &'static TypeInfo)>,
    pub return_type: &'static TypeInfo,
    pub blocks: HashMap<i64, BasicBlock>,
    pub entry_block: i64,
    current_block: i64,
    next_reg_id: i64,
    next_label_id: i64,
}

impl Function {
    pub fn new(name: String, return_type: &'static TypeInfo) -> Self {
        let mut f = Self {
            name,
            params: Vec::new(),
            return_type,
            blocks: HashMap::new(),
            entry_block: 0,
            current_block: 0,
            next_reg_id: 0,
            next_label_id: 0,
        };
        f.add_block("entry".to_string());
        f
    }

    pub fn fresh_register(&mut self, name: String, ty: &'static TypeInfo) -> Register {
        let id = self.next_reg_id;
        self.next_reg_id += 1;
        Register::new(name, id, ty)
    }

    pub fn get_block_mut(&mut self, id: i64) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&id)
    }

    pub fn get_current_block_mut(&mut self) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&self.current_block)
    }

    pub fn current_block(&self) -> i64 {
        self.current_block
    }

    fn fresh_label(&mut self, name: String) -> Label {
        let id = self.next_label_id;
        self.next_label_id += 1;
        Label::new(name, id)
    }

    pub fn add_block(&mut self, name: String) -> Label {
        let label = self.fresh_label(name);
        self.blocks.insert(
            label.id(),
            BasicBlock {
                label: label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Return(None),
                predecessors: Vec::new(),
            },
        );
        label
    }

    pub fn make_assign(&mut self, target: Register, value: Operand) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block
                .instructions
                .push(Instruction::Assign { target, value });
        }
    }

    pub fn make_unary(&mut self, target: Register, op: UnaryOp, operand: Operand) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Unary {
                target,
                op,
                operand,
            });
        }
    }

    pub fn make_binary(
        &mut self,
        name: String,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
        ty: &'static TypeInfo,
    ) -> Register {
        let target = self.fresh_register(name, ty);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Binary {
                target: target.clone(),
                op,
                lhs,
                rhs,
            });
        }
        target
    }

    pub fn make_store(&mut self, address: Operand, value: Operand) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block
                .instructions
                .push(Instruction::Store { address, value });
        }
    }

    pub fn make_load(
        &mut self,
        name: String,
        address: Operand,
        value_type: &'static TypeInfo,
    ) -> Register {
        let target = self.fresh_register(name, value_type);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Load {
                target: target.clone(),
                address,
            });
        }
        target
    }

    pub fn make_alloca(&mut self, name: String, value_type: &'static TypeInfo) -> Register {
        let ptr_type = value_type.ptr_to();
        let target = self.fresh_register(name, ptr_type);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Alloca {
                target: target.clone(),
            });
        }
        target
    }

    pub fn make_call(
        &mut self,
        name: String,
        func_name: String,
        args: Vec<Operand>,
        return_type: &'static TypeInfo,
    ) -> Option<Register> {
        if return_type.is_nil() {
            return None;
        }
        let target = self.fresh_register(name, return_type);
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.instructions.push(Instruction::Call {
                target: Some(target.clone()),
                name: func_name,
                args,
            });
        }
        Some(target)
    }

    pub fn branch(&mut self, condition: Operand) -> (Label, Label) {
        let true_label = self.fresh_label("then".to_string());
        let false_label = self.fresh_label("else".to_string());

        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = ControlFlow::Branch {
                condition,
                if_true: true_label.clone(),
                if_false: false_label.clone(),
            };
        }

        self.blocks.insert(
            true_label.id(),
            BasicBlock {
                label: true_label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Return(None),
                predecessors: vec![Label::new("current".to_string(), self.current_block)],
            },
        );

        self.blocks.insert(
            false_label.id(),
            BasicBlock {
                label: false_label.clone(),
                instructions: Vec::new(),
                terminator: ControlFlow::Return(None),
                predecessors: vec![Label::new("current".to_string(), self.current_block)],
            },
        );

        (true_label, false_label)
    }

    pub fn set_current_block(&mut self, label: &Label) {
        self.current_block = label.id();
    }

    pub fn set_terminator(&mut self, terminator: ControlFlow) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = terminator;
        }
    }

    pub fn make_phi(&mut self, target: Register, incoming: Vec<(Label, Operand)>) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block
                .instructions
                .push(Instruction::Phi { target, incoming });
        }
    }
}

pub struct Module {
    pub functions: HashMap<String, FunctionDeclarationState>,
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
            functions: HashMap::new(),
        }
    }

    pub fn declare_function(&mut self, name: String, func_type: &'static TypeInfo) {
        self.functions
            .insert(name, FunctionDeclarationState::Declared(func_type));
    }

    pub fn define_function(&mut self, func: Function) {
        self.functions.insert(
            func.name.clone(),
            FunctionDeclarationState::Defined(Box::new(func)),
        );
    }
}

#[derive(Clone)]
pub enum FunctionDeclarationState {
    Declared(&'static TypeInfo),
    Defined(Box<Function>),
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (name, func_state) in &self.functions {
            match func_state {
                FunctionDeclarationState::Declared(ty) => {
                    writeln!(f, "declare {} : {}", name, ty)?;
                }
                FunctionDeclarationState::Defined(func) => {
                    write!(f, "{}", func)?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print function signature
        write!(f, "fn {}(", self.name)?;
        for (i, (param_name, param_type)) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", param_name, param_type)?;
        }
        writeln!(f, ") -> {} {{", self.return_type)?;

        // Track register definitions
        let mut reg_defs = std::collections::HashMap::new();

        // Print blocks in order, starting with entry block
        let mut visited = std::collections::HashSet::new();
        let mut queue = vec![self.entry_block];

        while let Some(block_id) = queue.pop() {
            if !visited.insert(block_id) {
                continue;
            }

            if let Some(block) = self.blocks.get(&block_id) {
                // Print block label (without : prefix as this is the definition)
                writeln!(f, "{}:", block.label.name())?;

                // Print instructions
                for inst in &block.instructions {
                    write!(f, "    ")?;
                    match inst {
                        Instruction::Assign { target, value } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(f, "{} := {}", reg_name, print_operand(value, &reg_defs))?;
                        }
                        Instruction::Binary {
                            target,
                            op,
                            lhs,
                            rhs,
                        } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(
                                f,
                                "{} := {} {} {}",
                                reg_name,
                                print_operand(lhs, &reg_defs),
                                print_binop(op),
                                print_operand(rhs, &reg_defs)
                            )?;
                        }
                        Instruction::Unary {
                            target,
                            op,
                            operand,
                        } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(
                                f,
                                "{} := {}{}",
                                reg_name,
                                print_unop(op),
                                print_operand(operand, &reg_defs)
                            )?;
                        }
                        Instruction::Load { target, address } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(
                                f,
                                "{} := load {}",
                                reg_name,
                                print_operand(address, &reg_defs)
                            )?;
                        }
                        Instruction::Store { address, value } => {
                            writeln!(
                                f,
                                "store {} <- {}",
                                print_operand(address, &reg_defs),
                                print_operand(value, &reg_defs)
                            )?;
                        }
                        Instruction::Alloca { target } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(f, "{} := alloca {}", reg_name, target.ty())?;
                        }
                        Instruction::Call { target, name, args } => {
                            if let Some(target) = target {
                                let reg_name = print_register(target, &mut reg_defs);
                                write!(f, "{} := ", reg_name)?;
                            }
                            write!(f, "call {}(", name)?;
                            for (i, arg) in args.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{}", print_operand(arg, &reg_defs))?;
                            }
                            writeln!(f, ")")?;
                        }
                        Instruction::Phi { target, incoming } => {
                            let reg_name = print_register(target, &mut reg_defs);
                            writeln!(f, "{} := phi {{", reg_name)?;
                            for (label, operand) in incoming.iter() {
                                writeln!(
                                    f,
                                    "    {} -> {}",
                                    label.name(),
                                    print_operand(operand, &reg_defs)
                                )?;
                            }
                            writeln!(f, "}}")?;
                        }
                    }
                }

                // Print terminator
                write!(f, "    ")?;
                match &block.terminator {
                    ControlFlow::Jump(label) => {
                        writeln!(f, "jump :{}", label.name())?;
                        queue.push(label.id());
                    }
                    ControlFlow::Branch {
                        condition,
                        if_true,
                        if_false,
                    } => {
                        writeln!(
                            f,
                            "branch {} ? :{} : :{}",
                            print_operand(condition, &reg_defs),
                            if_true.name(),
                            if_false.name()
                        )?;
                        queue.push(if_true.id());
                        queue.push(if_false.id());
                    }
                    ControlFlow::Return(value) => {
                        write!(f, "return")?;
                        if let Some(val) = value {
                            write!(f, " {}", print_operand(val, &reg_defs))?;
                        }
                        writeln!(f)?;
                    }
                }
                writeln!(f)?;
            }
        }

        writeln!(f, "}}")
    }
}

fn print_register(
    reg: &Register,
    reg_defs: &mut std::collections::HashMap<String, usize>,
) -> String {
    let base_name = reg.name();
    let count = reg_defs.entry(base_name.to_string()).or_insert(0);
    let result = if *count == 0 {
        base_name.to_string()
    } else {
        format!("{}.{}", base_name, count)
    };
    *count += 1;
    result
}

fn print_operand(op: &Operand, reg_defs: &std::collections::HashMap<String, usize>) -> String {
    match op {
        Operand::I64Const(n) => n.to_string(),
        Operand::F32Const(f) => format!("{}f32", f),
        Operand::F64Const(f) => format!("{}f64", f),
        Operand::BoolConst(b) => b.to_string(),
        Operand::Register(reg) => {
            let count = reg_defs.get(reg.name()).unwrap_or(&0);
            if *count <= 1 {
                reg.name().to_string()
            } else {
                format!("{}.{}", reg.name(), count - 1)
            }
        }
        Operand::NoOp => "nop".to_string(),
    }
}

fn print_binop(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
    }
}

fn print_unop(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
    }
}
