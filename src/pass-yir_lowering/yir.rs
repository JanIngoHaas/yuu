use crate::pass_parse::ast::InternUstr;
use crate::pass_print_yir::yir_printer;
use crate::pass_type_inference::{EnumVariantInfo, StructFieldInfo};
use crate::pass_type_inference::{
    TypeInfo, primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil,
    primitive_u64,
};
use indexmap::IndexMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::Arc;
use ustr::{Ustr, UstrMap};

/*
Coloring and pretty printing of YIR mostly implemented by Claude Sonnet 3.5 /
*/

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
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

    pub fn write_unique_name(&self, out: &mut impl fmt::Write) -> fmt::Result {
        write!(out, "{}_{}", self.name, self.id)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> i64 {
        self.id
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Operand {
    I64Const(i64),
    U64Const(u64),
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
            Operand::U64Const(_) => primitive_u64(),
            Operand::F32Const(_) => primitive_f32(),
            Operand::F64Const(_) => primitive_f64(),
            Operand::BoolConst(_) => primitive_bool(),
            Operand::Variable(reg) => reg.ty(),
            Operand::NoOp => primitive_nil(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Clone, Debug)]
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

    // Enum specific
    GetVariantDataPtr {
        target: Variable, // result pointer Variable to the data of the enum.
        base: Operand,    // base operand (must be an enum)
        variant: Ustr,    // variant name
    },

    LoadActiveVariantIdx {
        target: Variable, // Variable to hold the active variant index
        source: Operand,  // Base operand (must be an enum ptr)
    },

    StoreActiveVariantIdx {
        dest: Variable, // Destination pointer operand (the enum itself, not the data ptr!)
        value: Operand, // Must be of type u64
    },

    // Convert integer to pointer
    IntToPtr {
        target: Variable, // Result pointer variable
        source: Operand,  // Source integer operand (must be u64)
    },

    // Heap allocation - returns a pointer to allocated memory
    HeapAlloc {
        target: Variable,   // Pointer variable to store the heap address
        size: Operand,      // Size in bytes (must be u64)
        align: Option<u64>, // Optional alignment requirement
    },

    // Heap deallocation - frees previously allocated memory
    HeapFree {
        ptr: Operand, // Pointer to memory to free (must be heap-allocated)
    },

    // Marks that multiple stack variables are being killed (e.g., leaving scope)
    KillSet {
        vars: Vec<Variable>, // Stack variables being killed
    },
}

#[derive(Clone, Debug)]
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
    JumpTable {
        scrutinee: Operand,                 // The enum variable being matched
        jump_targets: IndexMap<u64, Label>, // Maps variant index -> label
        default: Option<Label>,             // Default jump target if no variant matches
    },
    // Return from function
    Return(Option<Operand>),
    // Represents a block that hasn't had its terminator set yet.
    // Should be replaced by a real terminator before final IR generation.
    Unterminated,
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub label: Label,
    pub instructions: Vec<Instruction>,
    pub terminator: ControlFlow,
}

impl BasicBlock {
    pub fn calculate_var_decls(&self) -> impl Iterator<Item = &Variable> {
        self.instructions.iter().filter_map(|instr| {
            match instr {
                Instruction::Alloca { target } | Instruction::HeapAlloc { target, .. } => {
                    return Some(target);
                }
                Instruction::StoreImmediate { .. } => {}
                Instruction::TakeAddress { .. } => {}
                Instruction::GetFieldPtr { .. } => {}
                Instruction::Load { .. } => {}
                Instruction::Store { .. } => {}
                Instruction::Binary { .. } => {}
                Instruction::Unary { .. } => {}
                Instruction::Call { .. } => {}
                Instruction::LoadActiveVariantIdx { .. } => {}
                Instruction::GetVariantDataPtr { .. } => {}
                Instruction::StoreActiveVariantIdx { .. } => {}
                Instruction::IntToPtr { .. } => {}
                Instruction::HeapFree { .. } => {}
                Instruction::KillSet { .. } => {}
            };
            None
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
    pub follow_c_abi: bool,
    current_block: i64,
    next_reg_id: UstrMap<i64>,
    next_label_id: i64,
}

impl Function {
    pub fn calculate_var_decls(&self) -> impl Iterator<Item = &Variable> {
        self.blocks
            .values()
            .flat_map(|block| block.calculate_var_decls())
    }

    pub fn new(name: Ustr, return_type: &'static TypeInfo) -> Self {
        let mut f = Function {
            name,
            params: Vec::new(),
            return_type,
            blocks: IndexMap::new(),
            entry_block: 0,
            follow_c_abi: true,
            current_block: 0,
            next_reg_id: UstrMap::default(),
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

    pub fn fresh_variable(&mut self, name: Ustr, ty: &'static TypeInfo) -> Variable {
        let id = self.next_reg_id.entry(name).or_insert(0);
        let var = Variable::new(name, *id, ty);
        *id += 1;
        var
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

    pub fn make_store_active_variant_idx(
        &mut self,
        dest: Variable,
        variant_info: &EnumVariantInfo,
    ) {
        let value = Operand::U64Const(variant_info.variant_idx);
        let instr = Instruction::StoreActiveVariantIdx { dest, value };
        self.get_current_block_mut().instructions.push(instr);
    }

    pub fn make_load_active_variant_idx(&mut self, name_hint: Ustr, source: Operand) -> Variable {
        let target = self.fresh_variable(name_hint, primitive_u64());
        let instr = Instruction::LoadActiveVariantIdx { target, source };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    pub fn make_get_variant_data_ptr(
        &mut self,
        name_hint: Ustr,
        base: Operand,
        variant_info: &EnumVariantInfo,
    ) -> Variable {
        let target_type = variant_info
            .variant
            .expect("Variant must have data type")
            .ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::GetVariantDataPtr {
            target,
            base,
            variant: variant_info.variant_name,
        };
        self.get_current_block_mut().instructions.push(instr);
        target
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
    }

    // Helper function to allocate only for valid types (not Inactive or pointers to Inactive)
    pub fn make_alloca_if_valid_type(
        &mut self,
        name_hint: Ustr,
        ty: &'static TypeInfo,
        init_value: Option<Operand>,
    ) -> Option<Variable> {
        // Don't allocate for inactive types following the same pattern as make_store
        if matches!(
            ty,
            TypeInfo::Inactive | TypeInfo::Pointer(TypeInfo::Inactive)
        ) {
            return None;
        }

        Some(self.make_alloca(name_hint, ty, init_value))
    }

    // Helper function to store only for valid types (not Inactive or pointers to Inactive)
    pub fn make_store_if_valid_type(&mut self, target: Variable, value: Operand) -> bool {
        // Check if the value's type is valid using the same pattern as existing debug assertions
        let value_type = value.ty();
        if matches!(
            value_type,
            TypeInfo::Inactive | TypeInfo::Pointer(TypeInfo::Inactive)
        ) {
            return false;
        }

        self.make_store(target, value);
        true
    }

    // Helper function to store only if the target variable exists (reducing if-let boilerplate)
    pub fn make_store_if_exists(&mut self, target: Option<Variable>, value: Operand) -> bool {
        if let Some(var) = target {
            self.make_store(var, value);
            true
        } else {
            false
        }
    }

    // Builder method for Assign

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
    } // Builder method for TakeAddress
    pub fn make_take_address(&mut self, name_hint: Ustr, source: Variable) -> Variable {
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
        let base_type = base.ty();
        debug_assert!(
            matches!(base_type, TypeInfo::Pointer(_)),
            "make_get_field_ptr: base operand must be a pointer type, got {:?}",
            base_type
        );

        if let TypeInfo::Pointer(inner_type) = base_type {
            debug_assert!(
                matches!(inner_type, TypeInfo::Struct(_)),
                "make_get_field_ptr: base must be pointer to struct, got pointer to {:?}",
                inner_type
            );
        }

        let target_type = field_info.ty.ptr_to();
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::GetFieldPtr {
            target,
            base,
            field: field_info.name,
        };
        self.get_current_block_mut().instructions.push(instr);

        target
    } // Builder method for Load
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
    } // Builder method for Store
    pub fn make_store(&mut self, dest: Variable, value: Operand) -> Variable {
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
            | Operand::U64Const(_)
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
            Operand::Variable(_) => value,
        };

        debug_assert_eq!(
            value_ptr.ty(),
            dest_ty.deref_ptr(),
            "Store type mismatch: dest expects to store {}, value is {}",
            dest_ty.deref_ptr(),
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
        let target = self.fresh_variable(name_hint, result_type);
        let instr = Instruction::Binary {
            target,
            op,
            lhs,
            rhs,
        };
        self.get_current_block_mut().instructions.push(instr);

        target
    } // Builder method for Unary operation
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

    // Builder method for IntToPtr operation
    pub fn make_int_to_ptr(
        &mut self,
        name_hint: Ustr,
        target_type: &'static TypeInfo,
        source: Operand,
    ) -> Variable {
        let target = self.fresh_variable(name_hint, target_type);
        let instr = Instruction::IntToPtr { target, source };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for heap allocation
    pub fn make_heap_alloc(
        &mut self,
        name_hint: Ustr,
        ty: &'static TypeInfo,
        size: Operand,
        align: Option<u64>,
    ) -> Variable {
        debug_assert_eq!(
            size.ty(),
            primitive_u64(),
            "HeapAlloc size must be u64, got {}",
            size.ty()
        );

        let target = self.fresh_variable(name_hint, ty.ptr_to());
        let instr = Instruction::HeapAlloc { target, size, align };
        self.get_current_block_mut().instructions.push(instr);
        target
    }

    // Builder method for heap deallocation
    pub fn make_heap_free(&mut self, ptr: Operand) {
        debug_assert!(
            ptr.ty().is_ptr(),
            "HeapFree ptr must be a pointer, got {}",
            ptr.ty()
        );

        let instr = Instruction::HeapFree { ptr };
        self.get_current_block_mut().instructions.push(instr);
    }

    // Builder method for killing multiple stack variables
    pub fn make_kill_set(&mut self, vars: Vec<Variable>) {
        if !vars.is_empty() {
            let instr = Instruction::KillSet { vars };
            self.get_current_block_mut().instructions.push(instr);
        }
    }

    // pub fn make_enum(
    //     &mut self,
    //     name_hint: Ustr,
    //     enum_type: &'static TypeInfo,
    //     variant_name: Ustr,
    //     variant_index: u64,
    //     data: Option<Operand>,
    // ) -> Variable {
    //     // Load data operand if it's a pointer (similar to other make_* methods)
    //     let loaded_data = data.map(|d| self.load_if_pointer(d, "enum_data".intern()));

    //     let target = self.fresh_variable(name_hint, enum_type);
    //     let target_variant_idx = self.fresh_variable(name_hint, primitive_u64());
    //     let instr = Instruction::MakeUnion {
    //         target,
    //         variant_name,
    //         variant_index,
    //         data: loaded_data,
    //     };
    //     self.get_current_block_mut().instructions.push(instr);

    //     // Return a pointer to stay in "pointer context"
    //     self.make_take_address(name_hint, target)
    // }

    pub fn make_jump(&mut self, target: Label) {
        self.set_terminator(ControlFlow::Jump { target });
    }
    pub fn make_return(&mut self, value: Option<Operand>) {
        match (value.as_ref().map(|op| op.ty()), self.return_type) {
            (Some(ty), ret_ty) if ty == ret_ty => {}
            (None, ret_ty) if ret_ty.is_nil() => {}
            (val_ty, ret_ty) => panic!(
                "Return type mismatch: expected {}, got {:?}",
                ret_ty, val_ty
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

    pub fn make_jump_table(
        &mut self,
        scrutinee: Operand,
        jump_targets: IndexMap<u64, Label>,
        default: Option<Label>,
    ) {
        //let loaded_scrutinee = self.load_if_pointer(scrutinee, "enum_scrutinee".intern());

        let loaded_scrutinee = match scrutinee.ty() {
            TypeInfo::Pointer(TypeInfo::Enum(_et)) => {
                // Load only the active variant index for this operation...
                Operand::Variable(
                    self.make_load_active_variant_idx("variant_idx".intern(), scrutinee),
                )
            }
            TypeInfo::Enum(_et) => {
                // For enum values, we also need to load the variant index
                Operand::Variable(
                    self.make_load_active_variant_idx("variant_idx".intern(), scrutinee),
                )
            }
            _ => scrutinee,
        };

        debug_assert_eq!(
            loaded_scrutinee.ty(),
            primitive_u64(),
            "Jump table scrutinee must be u64, got {}",
            loaded_scrutinee.ty()
        );

        self.set_terminator(ControlFlow::JumpTable {
            scrutinee: loaded_scrutinee,
            jump_targets,
            default,
        });
    }

    pub fn make_branch(
        &mut self,
        condition: Operand,
        name_then: Option<Ustr>,
        name_else: Option<Ustr>,
    ) -> (Label, Label) {
        debug_assert_eq!(
            condition.ty(),
            primitive_bool(),
            "Branch condition must be boolean, got {}",
            condition.ty()
        );

        let true_label = self.add_block(name_then.unwrap_or_else(|| "then".intern()));
        let false_label = self.add_block(name_else.unwrap_or_else(|| "else".intern()));

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

    pub fn sort_blocks_by_id(&mut self) {
        self.blocks.sort_unstable_by(|a, _, b, _| a.cmp(b));
    }
}

pub struct Module {
    pub functions: UstrMap<FunctionDeclarationState>,
    pub structs: Vec<Ustr>, // Just the names... NOthing else is really needed for
    pub enums: Vec<Ustr>,   // Enum names for C generation
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
            enums: Vec::new(),
        }
    }

    pub fn define_struct(&mut self, name: Ustr) {
        self.structs.push(name);
    }

    pub fn define_enum(&mut self, name: Ustr) {
        self.enums.push(name);
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
