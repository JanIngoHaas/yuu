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
use std::thread::current;
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

#[derive(Clone, PartialEq)]
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
    NotEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Clone)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone)]
pub enum Instruction {
    // Declares a variable on the stack with optional initial value
    DeclareVar {
        target: Variable,            // The variable being declared
        init_value: Option<Operand>, // Optional initial value
    },

    // Simple assignment between variables or from constant to variable
    Assign {
        target: Variable, // Target variable to assign to
        value: Operand,   // Source value
    },

    // Get pointer to a variable (for taking address of a variable)
    TakeAddress {
        target: Variable, // Pointer variable to store the address
        source: Variable, // Variable whose address we're taking
    },

    // Get pointer to a field within a struct
    GetFieldPtr {
        target: Variable, // Result pointer variable
        base: Operand,    // Base struct operand (either variable or pointer)
        field: Ustr,      // Field name
    },

    // Load value through a pointer
    Load {
        target: Variable, // Target variable to store loaded value
        source: Operand,  // Source pointer operand (must be a pointer type)
    },

    // Store value through a pointer
    Store {
        dest: Operand,  // Destination pointer operand (must be a pointer type)
        value: Operand, // Value to store
    },

    // Binary operation
    Binary {
        target: Variable, // Result variable
        op: BinOp,        // Operation
        lhs: Operand,     // Left operand
        rhs: Operand,     // Right operand
    },

    // Unary operation
    Unary {
        target: Variable, // Result variable
        op: UnaryOp,      // Operation
        operand: Operand, // Operand
    },

    // Function call
    Call {
        target: Option<Variable>, // Optional target for return value
        name: Ustr,               // Function name
        args: Vec<Operand>,       // Arguments
    },

    // Create a struct directly with values (not pointers)
    MakeStruct {
        target: Variable,             // Variable to hold the struct
        type_ident: Ustr,             // Name of struct type
        fields: Vec<(Ustr, Operand)>, // Field name and value pairs
    },
}

#[derive(Clone)]
pub enum ControlFlow {
    // Simple unconditional jump
    Jump {
        target: Label,
    },
    // Conditional branch
    Branch {
        condition: Operand,
        if_true: Label,
        if_false: Label,
    },
    // Return from function
    Return(Option<Operand>),
    // Represents a block that hasn't had its terminator set yet.
    // Should be replaced by a real terminator before final IR generation.
    Unterminated,
}

#[derive(Clone)]
pub struct BasicBlock {
    pub label: Label,
    pub instructions: Vec<Instruction>,
    pub terminator: ControlFlow,
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
        };
        f.add_block("entry".intern());
        f
    }

    pub fn add_param(&mut self, name: Ustr, ty: &'static TypeInfo) -> Variable {
        let param = self.fresh_variable(name, ty);
        self.params.push(param);
        param
    }

    fn fresh_variable(&mut self, name: Ustr, ty: &'static TypeInfo) -> Variable {
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
                label: label,
                instructions: Vec::new(),
                // Initialize blocks as Unterminated
                terminator: ControlFlow::Unterminated,
            },
        );
        label
    }

    pub fn has_terminator(&self, block_id: i64) -> bool {
        self.blocks
            .get(&block_id)
            .map(|block| !matches!(block.terminator, ControlFlow::Unterminated))
            .unwrap_or(false)
    }

    // Ensures the current block has a terminator. Panics if trying to overwrite one.
    fn set_terminator(&mut self, terminator: ControlFlow) {
        let current_block = self.get_current_block_mut();
        if matches!(current_block.terminator, ControlFlow::Unterminated) {
            current_block.terminator = terminator;
        } else {
            panic!(
                "Block {} already has a terminator, cannot set new terminator.",
                current_block.label.name(),
            );
        }
    }

    pub fn make_jump_if_no_terminator(&mut self, target: Label) {
        let current_block = self.get_current_block_mut();
        if matches!(current_block.terminator, ControlFlow::Unterminated) {
            current_block.terminator = ControlFlow::Jump { target };
        }
    }

    // Builder method for DeclareVar
    pub fn declare_var(
        &mut self,
        name_hint: Ustr,
        ty: &'static TypeInfo,
        init_value: Option<Operand>,
    ) -> Variable {
        let target = self.fresh_variable(name_hint, ty);
        if let Some(ref val) = init_value {
            debug_assert_eq!(
                val.ty(),
                ty,
                "Type mismatch in DeclareVar initialization: target is {}, value is {}",
                ty,
                val.ty()
            );
        }
        let instr = Instruction::DeclareVar { target, init_value };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for Assign
    pub fn make_assign(&mut self, target: Variable, value: Operand) {
        // Don't allow assignment of NoOp to a variable
        if value == Operand::NoOp {
            return;
        }
        debug_assert_eq!(
            target.ty(),
            value.ty(),
            "Type mismatch in Assign: target is {}, value is {}",
            target.ty(),
            value.ty()
        );
        let instr = Instruction::Assign { target, value };
        self.get_current_block_mut().instructions.push(instr);
    }

    pub fn make_call(
        &mut self,
        target: Ustr,
        name: Ustr,
        args: Vec<Operand>,
        return_type: &'static TypeInfo,
    ) -> Option<Variable> {
        let target = if return_type.is_nil() {
            None
        } else {
            Some(self.fresh_variable(target, return_type))
        };
        let current_block = self.get_current_block_mut();
        let instr = Instruction::Call { target, name, args };
        current_block.instructions.push(instr);
        target
    }

    // Builder method for TakeAddress
    pub fn make_take_address(&mut self, name_hint: Ustr, source: Variable) -> Variable {
        let target_type = source.ty().ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::TakeAddress { target, source };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for GetFieldPtr
    pub fn make_get_field_ptr(
        &mut self,
        name_hint: Ustr,
        base: Operand,
        field_info: &StructFieldInfo,
    ) -> Variable {
        // TODO: Add validation for checking that base is a pointer to a struct type
        let target_type = field_info.ty.ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::GetFieldPtr {
            target,
            base,
            field: field_info.name,
        };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for Load
    pub fn make_load(&mut self, name_hint: Ustr, source: Operand) -> Variable {
        let source_ty = source.ty();
        debug_assert!(
            source_ty.is_ptr(),
            "Load source must be a pointer, got {}",
            source_ty
        );
        let target_type = source_ty.deref_ptr();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::Load { target, source };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for Store
    pub fn make_store(&mut self, dest: Operand, value: Operand) {
        let dest_ty = dest.ty();
        debug_assert!(
            dest_ty.is_ptr(),
            "Store destination must be a pointer, got {}",
            dest_ty
        );
        let expected_value_type = dest_ty.deref_ptr();
        debug_assert_eq!(
            value.ty(),
            expected_value_type,
            "Store type mismatch: dest expects *{}, value is {}",
            expected_value_type,
            value.ty()
        );
        let instr = Instruction::Store { dest, value };
        self.get_current_block_mut().instructions.push(instr);
    }

    // Builder method for MakeStruct
    pub fn make_struct(
        &mut self,
        name_hint: Ustr,
        struct_info: &StructInfo,
        fields: Vec<(Ustr, Operand)>,
    ) -> Variable {
        // TODO: Add validation for field names and types against struct_type definition
        let target = self.fresh_variable(name_hint, struct_info.ty);
        let instr = Instruction::MakeStruct {
            target,
            type_ident: struct_info.name,
            fields,
        };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for Binary operation
    pub fn make_binary(
        &mut self,
        name_hint: Ustr,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
        result_type: &'static TypeInfo,
    ) -> Variable {
        let target = self.fresh_variable(name_hint, result_type);
        let instr = Instruction::Binary {
            target,
            op,
            lhs,
            rhs,
        };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for Unary operation
    pub fn make_unary(
        &mut self,
        name_hint: Ustr,
        result_type: &'static TypeInfo,
        op: UnaryOp,
        operand: Operand,
    ) -> Variable {
        let target = self.fresh_variable(name_hint, result_type);
        let instr = Instruction::Unary {
            target,
            op,
            operand,
        };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    pub fn make_jump(&mut self, target: Label) {
        self.set_terminator(ControlFlow::Jump { target });
    }

    pub fn make_return(&mut self, value: Option<Operand>) {
        match (value.as_ref().map(|op| op.ty()), self.return_type) {
            (Some(ty), ret_ty) if ty == ret_ty => {}
            (None, ret_ty) if ret_ty.is_nil() => {}
            (val_ty, ret_ty) => panic!(
                "Return type mismatch: expected {}, got {:?}",
                ret_ty,
                val_ty.map(|t| t)
            ),
        }
        self.set_terminator(ControlFlow::Return(value));
    }

    pub fn make_branch_to_existing(
        &mut self,
        condition: Operand,
        then_label: Label,
        else_label: Label,
    ) {
        debug_assert_eq!(
            condition.ty(),
            primitive_bool(),
            "Branch condition must be boolean, got {}",
            condition.ty()
        );
        self.set_terminator(ControlFlow::Branch {
            condition,
            if_true: then_label,
            if_false: else_label,
        });
    }

    pub fn make_branch(&mut self, condition: Operand) -> (Label, Label) {
        debug_assert_eq!(
            condition.ty(),
            primitive_bool(),
            "Branch condition must be boolean, got {}",
            condition.ty()
        );

        let true_label = self.add_block("then".intern());
        let false_label = self.add_block("else".intern());

        self.set_terminator(ControlFlow::Branch {
            condition,
            if_true: true_label,
            if_false: false_label,
        });

        (true_label, false_label)
    }

    pub fn set_current_block(&mut self, current: &Label) {
        self.current_block = current.id();
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
            func.name,
            FunctionDeclarationState::Declared(Arc::new(func)),
        );
    }

    pub fn define_function(&mut self, func: Function) {
        self.functions
            .insert(func.name, FunctionDeclarationState::Defined(Arc::new(func)));
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
