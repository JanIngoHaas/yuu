use crate::pass_parse::ast::InternUstr;
use crate::pass_print_yir::yir_printer;
use crate::pass_type_inference::StructFieldInfo;
use crate::pass_type_inference::{
    TypeInfo, primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil,
};
use crate::utils::scheduler::{ResourceId, ResourceName};
use indexmap::{IndexMap, IndexSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
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

#[derive(Copy, Clone, PartialEq)]
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
    // Declares a variable on the stack, returns a pointer to it
    Alloca {
        target: Variable, // The variable being declared
    },

    // Simple assignment or from constant to variable
    StoreImmediate {
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
    // MakeStruct {
    //     target: Variable,             // Variable to hold the struct
    //     type_ident: Ustr,             // Name of struct type
    //     fields: Vec<(Ustr, Operand)>, // Field name and value pairs
    // },
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

impl BasicBlock {
    pub fn calculate_var_decls(&self) -> impl Iterator<Item = &Variable> {
        self.instructions.iter().filter_map(|instr| {
            match instr {
                Instruction::Alloca { target } => {
                    return Some(target);
                }
                Instruction::StoreImmediate { target, value } => {}
                Instruction::TakeAddress { .. } => {}
                Instruction::GetFieldPtr { .. } => {}
                Instruction::Load { .. } => {}
                Instruction::Store { .. } => {}
                Instruction::Binary { .. } => {}
                Instruction::Unary { .. } => {}
                Instruction::Call { .. } => {}
            };
            return None;
        })
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: Ustr,
    pub params: Vec<Variable>,
    pub return_type: &'static TypeInfo,
    pub blocks: IndexMap<i64, BasicBlock>,
    pub entry_block: i64,
    pub follow_C_ABI: bool,
    current_block: i64,
    next_reg_id: i64,
    next_label_id: i64,
}

impl Function {
    
    pub fn calculate_var_decls(&self) -> impl Iterator<Item = &Variable> {
        self.blocks.values().flat_map(|block| block.calculate_var_decls())
    }
    
    pub fn new(name: Ustr, return_type: &'static TypeInfo) -> Self {
        let mut f = Function {
            name,
            params: Vec::new(),
            return_type,
            blocks: IndexMap::new(),
            entry_block: 0,
            follow_C_ABI: true,
            current_block: 0,
            next_reg_id: 0,
            next_label_id: 0,
        };
        f.add_block("entry".intern());
        f
    }

    pub fn add_param(&mut self, name: Ustr, ty: &'static TypeInfo) -> Variable {
        let param = self.fresh_variable(name, ty.ptr_to());
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
                label,
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
    pub fn make_alloca(
        &mut self,
        name_hint: Ustr,
        ty: &'static TypeInfo,
        init_value: Option<Operand>,
    ) -> Variable {
        let dest_ty = ty.ptr_to();
        let target = self.fresh_variable(name_hint, dest_ty);
        let instr = Instruction::Alloca { target };
        self.get_current_block_mut().instructions.push(instr);

        if let Some(value_ptr) = init_value {
            self.make_store(target, value_ptr);
        };

        target
    } // Builder method for Assign

    // Use make_store instead of make_assign
    // pub fn make_assign(&mut self, target: Variable, value: Operand) -> Variable {
    //     // Don't allow assignment of NoOp to a variable
    //     if value == Operand::NoOp {
    //         return self.make_take_address("assign_noop_ptr".intern(), target);
    //     }

    //     // Load the value if it's a pointer (Case a)
    //     let loaded_value = self.load_if_pointer(value, "assign_load".intern());

    //     debug_assert_eq!(
    //         target.ty(),
    //         loaded_value.ty(),
    //         "Type mismatch in Assign: target is {}, value is {}",
    //         target.ty(),
    //         loaded_value.ty()
    //     );
    //     let instr = Instruction::Assign {
    //         target,
    //         value: loaded_value,
    //     };
    //     self.get_current_block_mut().instructions.push(instr);

    //     // Return a pointer to stay in "pointer context"
    //     self.make_take_address("assign_result_ptr".intern(), target)
    // }
    pub fn make_call(
        &mut self,
        target: Ustr,
        name: Ustr,
        mut args: Vec<Operand>,
        return_type: &'static TypeInfo,
    ) -> Option<Variable> {
        // Load arguments if they are pointers (Case b)

        for arg in &mut args {
            *arg = self.load_if_pointer(*arg, "call_arg".intern());
        }

        let target = if return_type.is_nil() {
            None
        } else {
            Some(self.fresh_variable(target, return_type))
        };
        let current_block = self.get_current_block_mut();
        let instr = Instruction::Call { target, name, args };
        current_block.instructions.push(instr);

        // If we have a return value, wrap it in pointer context
        target.map(|result_var| self.make_take_address("call_result_ptr".intern(), result_var))
    } // Builder method for TakeAddress
    pub fn make_take_address(&mut self, name_hint: Ustr, source: Variable) -> Variable {
        // Case c) - NO loading! We expect a value here, not a pointer context
        let target_type = source.ty().ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::TakeAddress { target, source };
        self.get_current_block_mut().instructions.push(instr);
        target
    } // Builder method for GetFieldPtr
    pub fn make_get_field_ptr(
        &mut self,
        name_hint: Ustr,
        base: Operand,
        field_info: &StructFieldInfo,
    ) -> Variable {
        // Case d) - base needs to be a pointer, but here, DONT load it.
        // TODO: Add validation for checking that base is a pointer to a struct type
        let target_type = field_info.ty.ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::GetFieldPtr {
            target,
            base,
            field: field_info.name,
        };
        self.get_current_block_mut().instructions.push(instr);

        // Return as pointer for "pointer context"
        target
    } // Builder method for Load
    pub fn make_load(&mut self, name_hint: Ustr, source: Operand) -> Variable {
        // Case e) - Yeah of course. Load, source needs to be a pointer
        let loaded_value = self.make_load_internal(name_hint, source);

        // Return a pointer to the loaded value to stay in "pointer context"
        self.make_take_address(format!("{}_ptr", name_hint).intern(), loaded_value)
    } // Builder method for Store
    pub fn make_store(&mut self, dest: Variable, value: Operand) -> Variable {
        // Case f) - Yeah of course. Needs to be a pointer, obviously.
        let dest_ty = dest.ty();

        debug_assert!(
            !matches!(
                dest_ty,
                TypeInfo::Inactive | TypeInfo::Pointer(TypeInfo::Inactive)
            ),
            "Store destination cannot be an inactive type, got {}",
            dest_ty
        );

        debug_assert!(
            dest_ty.is_ptr(),
            "Store destination must be a pointer, got {}",
            dest_ty
        );

        // If we have a Immediate value, we can just store it directly
        let value_ptr = match value {
            Operand::I64Const(_)
            | Operand::F32Const(_)
            | Operand::F64Const(_)
            | Operand::BoolConst(_) => {
                let instr = Instruction::StoreImmediate {
                    target: dest,
                    value,
                };
                self.get_current_block_mut().instructions.push(instr);
                return dest;
            }
            Operand::NoOp => {
                // NoOp is a special case, we can just return the destination - there is nothing to store
                return dest;
            }
            _ => value,
        };

        debug_assert_eq!(
            value_ptr.ty(),
            dest_ty,
            "Store type mismatch: dest expects {}, value_ptr is {}",
            dest_ty,
            value_ptr.ty()
        );
        let instr = Instruction::Store {
            dest: Operand::Variable(dest),
            value: value_ptr,
        };
        self.get_current_block_mut().instructions.push(instr);

        dest
    }

    pub fn make_binary(
        &mut self,
        name_hint: Ustr,
        op: BinOp,
        lhs: Operand,
        rhs: Operand,
        result_type: &'static TypeInfo,
    ) -> Variable {
        // Case g) - Yeah, expect to load both, lhs and rhs.
        let loaded_lhs = self.load_if_pointer(lhs, "binary_lhs".intern());
        let loaded_rhs = self.load_if_pointer(rhs, "binary_rhs".intern());

        let target = self.fresh_variable(name_hint, result_type);
        let instr = Instruction::Binary {
            target,
            op,
            lhs: loaded_lhs,
            rhs: loaded_rhs,
        };
        self.get_current_block_mut().instructions.push(instr);

        // Return a pointer to stay in "pointer context"
        self.make_take_address(name_hint.intern(), target)
    } // Builder method for Unary operation
    pub fn make_unary(
        &mut self,
        name_hint: Ustr,
        result_type: &'static TypeInfo,
        op: UnaryOp,
        operand: Operand,
    ) -> Variable {
        // Case h) - Same as binary, load the operand
        let loaded_operand = self.load_if_pointer(operand, "unary_operand".intern());

        let target = self.fresh_variable(name_hint, result_type);
        let instr = Instruction::Unary {
            target,
            op,
            operand: loaded_operand,
        };
        self.get_current_block_mut().instructions.push(instr);

        // Return a pointer to stay in "pointer context"
        self.make_take_address(name_hint.intern(), target)
    }

    pub fn make_jump(&mut self, target: Label) {
        self.set_terminator(ControlFlow::Jump { target });
    }
    pub fn make_return(&mut self, value: Option<Operand>) {
        // Case i) - Of course. Load first.
        let loaded_value = value.map(|v| self.load_if_pointer(v, "return_value".intern()));

        match (loaded_value.as_ref().map(|op| op.ty()), self.return_type) {
            (Some(ty), ret_ty) if ty == ret_ty => {}
            (None, ret_ty) if ret_ty.is_nil() => {}
            (val_ty, ret_ty) => panic!(
                "Return type mismatch: expected {}, got {:?}",
                ret_ty, val_ty
            ),
        }
        self.set_terminator(ControlFlow::Return(loaded_value));
    }
    pub fn make_branch_to_existing(
        &mut self,
        condition: Operand,
        then_label: Label,
        else_label: Label,
    ) {
        // Case j) - Yes, load the condition first
        let loaded_condition = self.load_if_pointer(condition, "branch_condition".intern());

        debug_assert_eq!(
            loaded_condition.ty(),
            primitive_bool(),
            "Branch condition must be boolean, got {}",
            loaded_condition.ty()
        );
        self.set_terminator(ControlFlow::Branch {
            condition: loaded_condition,
            if_true: then_label,
            if_false: else_label,
        });
    }
    pub fn make_branch(&mut self, condition: Operand) -> (Label, Label) {
        // Case j) - Yes, load the condition first
        let loaded_condition = self.load_if_pointer(condition, "branch_condition".intern());

        debug_assert_eq!(
            loaded_condition.ty(),
            primitive_bool(),
            "Branch condition must be boolean, got {}",
            loaded_condition.ty()
        );

        let true_label = self.add_block("then".intern());
        let false_label = self.add_block("else".intern());

        self.set_terminator(ControlFlow::Branch {
            condition: loaded_condition,
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

    // Helper function to load a value from pointer context if needed
    fn load_if_pointer(&mut self, operand: Operand, name_hint: Ustr) -> Operand {
        match operand {
            // Constants don't need loading
            Operand::I64Const(_)
            | Operand::F32Const(_)
            | Operand::F64Const(_)
            | Operand::BoolConst(_)
            | Operand::NoOp => operand,
            // Variables are pointers in our context - load them
            Operand::Variable(_) => {
                let loaded = self.make_load_internal(name_hint, operand);
                Operand::Variable(loaded)
            }
        }
    }

    // Internal load function that doesn't wrap result in pointer context
    fn make_load_internal(&mut self, name_hint: Ustr, source: Operand) -> Variable {
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
