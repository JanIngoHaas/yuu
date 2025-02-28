use crate::scheduler::{ResourceId, ResourceName};
use crate::type_info::{
    primitive_bool, primitive_f32, primitive_f64, primitive_i64, primitive_nil, TypeInfo,
};
use colored::Colorize;
use hashbrown::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex};

/*
Coloring and pretty printing of YIR mostly implemented by Claude Sonnet 3.5
*/

// Color palette system
#[derive(Clone)]
struct RGB(u8, u8, u8);

// Add color support detection
#[derive(PartialEq)]
enum ColorSupport {
    NoColor,
    Basic,     // 16 colors
    Color256,  // 256 colors
    TrueColor, // 16.7M colors
}

impl ColorSupport {
    fn detect() -> Self {
        // Check NO_COLOR first - respect user preferences
        if std::env::var("NO_COLOR").is_ok() {
            return ColorSupport::NoColor;
        }

        // Check for true color support via COLORTERM
        if let Ok(colorterm) = std::env::var("COLORTERM") {
            if colorterm.contains("truecolor") || colorterm.contains("24bit") {
                return ColorSupport::TrueColor;
            }
        }

        // On Windows, check for Windows Terminal or ConEmu
        #[cfg(windows)]
        {
            if std::env::var("WT_SESSION").is_ok() || std::env::var("ConEmuANSI").is_ok() {
                return ColorSupport::TrueColor;
            }
        }

        // Check for TERM that might indicate advanced color support
        if let Ok(term) = std::env::var("TERM") {
            if term.contains("24bit") || term.contains("direct") {
                return ColorSupport::TrueColor;
            }
            if term.contains("256color") {
                return ColorSupport::Color256;
            }
        }

        // Default to basic colors if no better support detected
        ColorSupport::Basic
    }
}

struct ColorPalette {
    colors: HashMap<&'static str, RGB>,
    color_support: ColorSupport,
}

impl ColorPalette {
    // Add fallback colors for terminals with limited color support
    fn get_fallback_color(&self, rgb: RGB) -> colored::Color {
        match self.color_support {
            ColorSupport::NoColor => colored::Color::White,
            ColorSupport::Basic => {
                // Map RGB to the closest basic color
                let RGB(r, g, b) = rgb;
                if r > 200 && g > 200 && b > 200 {
                    colored::Color::White
                } else if r > 200 {
                    colored::Color::Red
                } else if g > 200 {
                    colored::Color::Green
                } else if b > 200 {
                    colored::Color::Blue
                } else if r > 200 && g > 200 {
                    colored::Color::Yellow
                } else if g > 200 && b > 200 {
                    colored::Color::Cyan
                } else if r > 200 && b > 200 {
                    colored::Color::Magenta
                } else {
                    colored::Color::White
                }
            }
            ColorSupport::Color256 => {
                // You could implement more sophisticated 256-color mapping here
                let RGB(r, g, b) = rgb;
                colored::Color::TrueColor { r, g, b }
            }
            ColorSupport::TrueColor => {
                let RGB(r, g, b) = rgb;
                colored::Color::TrueColor { r, g, b }
            }
        }
    }

    fn deep_ocean() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 215, 0)); // Deep Gold
        colors.insert("type", RGB(64, 224, 208)); // Turquoise
        colors.insert("register", RGB(230, 230, 250)); // Lavender
        colors.insert("constant", RGB(46, 139, 87)); // Sea Green
        colors.insert("label", RGB(125, 249, 255)); // Electric Blue
        colors.insert("operator", RGB(250, 128, 114)); // Salmon
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn warm_ember() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 140, 85)); // Warm Orange
        colors.insert("type", RGB(255, 183, 138)); // Peach
        colors.insert("register", RGB(255, 121, 121)); // Soft Red
        colors.insert("constant", RGB(255, 218, 121)); // Warm Yellow
        colors.insert("label", RGB(255, 166, 158)); // Coral Pink
        colors.insert("operator", RGB(255, 110, 74)); // Burnt Orange
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn mystic_forest() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(144, 238, 144)); // Light Green
        colors.insert("type", RGB(152, 251, 152)); // Pale Green
        colors.insert("register", RGB(143, 188, 143)); // Dark Sea Green
        colors.insert("constant", RGB(50, 205, 50)); // Lime Green
        colors.insert("label", RGB(127, 255, 170)); // Aquamarine
        colors.insert("operator", RGB(34, 139, 34)); // Forest Green
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn cosmic_night() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(147, 112, 219)); // Medium Purple
        colors.insert("type", RGB(138, 43, 226)); // Blue Violet
        colors.insert("register", RGB(216, 191, 216)); // Thistle
        colors.insert("constant", RGB(221, 160, 221)); // Plum
        colors.insert("label", RGB(218, 112, 214)); // Orchid
        colors.insert("operator", RGB(186, 85, 211)); // Medium Orchid
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn neon_dreams() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 110, 199)); // Hot Pink
        colors.insert("type", RGB(0, 255, 255)); // Cyan
        colors.insert("register", RGB(191, 255, 0)); // Neon Green
        colors.insert("constant", RGB(255, 0, 255)); // Magenta
        colors.insert("label", RGB(255, 255, 0)); // Yellow
        colors.insert("operator", RGB(255, 69, 0)); // Red-Orange
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    // === Light Background Palettes ===

    fn summer_breeze() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(255, 110, 74)); // Burnt Orange
        colors.insert("type", RGB(0, 119, 182)); // Ocean Blue
        colors.insert("register", RGB(86, 130, 3)); // Olive Green
        colors.insert("constant", RGB(255, 49, 49)); // Bright Red
        colors.insert("label", RGB(106, 76, 147)); // Royal Purple
        colors.insert("operator", RGB(220, 47, 2)); // Deep Red
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn cherry_blossom() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(219, 68, 88)); // Deep Pink
        colors.insert("type", RGB(15, 76, 129)); // Navy Blue
        colors.insert("register", RGB(255, 87, 127)); // Bright Pink
        colors.insert("constant", RGB(53, 80, 112)); // Steel Blue
        colors.insert("label", RGB(234, 72, 72)); // Coral Red
        colors.insert("operator", RGB(140, 20, 84)); // Dark Magenta
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn forest_morning() -> Self {
        let mut colors = HashMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(0, 102, 0)); // Dark Green
        colors.insert("type", RGB(0, 77, 122)); // Deep Blue
        colors.insert("register", RGB(153, 51, 0)); // Brown
        colors.insert("constant", RGB(204, 51, 0)); // Dark Orange
        colors.insert("label", RGB(0, 153, 76)); // Emerald
        colors.insert("operator", RGB(153, 0, 0)); // Dark Red
        Self {
            colors,
            color_support: ColorSupport::detect(),
        }
    }

    fn get_color(&self, key: &str) -> RGB {
        self.colors.get(key).cloned().unwrap_or(RGB(255, 255, 255))
    }
}

thread_local! {
    static CURRENT_PALETTE: std::cell::RefCell<ColorPalette> = std::cell::RefCell::new(ColorPalette::warm_ember());
}

fn colorize(text: &str, color_key: &str, do_color: bool) -> String {
    if !do_color {
        return text.to_string();
    }

    CURRENT_PALETTE.with(|palette| {
        let palette = palette.borrow();
        let rgb = palette.get_color(color_key);
        let RGB(r, g, b) = rgb;

        // Always try true color first in Windows Terminal
        #[cfg(windows)]
        if std::env::var("WT_SESSION").is_ok() {
            return text.truecolor(r, g, b).to_string();
        }

        match palette.color_support {
            ColorSupport::NoColor => text.to_string(),
            ColorSupport::TrueColor => text.truecolor(r, g, b).to_string(),
            ColorSupport::Color256 => {
                // For 256 color mode, use the closest ANSI color
                let color = palette.get_fallback_color(rgb);
                text.color(color).to_string()
            }
            ColorSupport::Basic => {
                // For basic mode, use the closest ANSI color
                let color = palette.get_fallback_color(rgb);
                text.color(color).to_string()
            }
        }
    })
}

#[derive(Clone)]
pub struct Register {
    name: String,
    id: i64,
    ty: &'static TypeInfo,
}

impl Hash for Register {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Register {}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct Label {
    name: String,
    id: i64,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.name, self.id)
    }
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

    pub fn write_unique_name(&self, out: &mut impl fmt::Write) -> fmt::Result {
        write!(out, "{}_{}", self.name, self.id)
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

impl Operand {
    pub fn ty(&self) -> &'static TypeInfo {
        match self {
            Operand::I64Const(_) => primitive_i64(),
            Operand::F32Const(_) => primitive_f32(),
            Operand::F64Const(_) => primitive_f64(),
            Operand::BoolConst(_) => primitive_bool(),
            Operand::Register(reg) => reg.ty(),
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
    Omega {
        target: Register,
        writable_blocks: Arc<Mutex<Vec<Label>>>, // Share the same Arc<Mutex> as in omega_blocks
    },
}

#[derive(Clone)]
pub enum ControlFlow {
    Jump {
        target: Label,
        writes: Vec<(Register, Operand)>, // Omikron writes at jump
    },
    Branch {
        condition: Operand,
        if_true: (Label, Vec<(Register, Operand)>), // Writes for true branch
        if_false: (Label, Vec<(Register, Operand)>), // Writes for false branch
    },
    Return(Option<Operand>),
    Fallthrough(Vec<(Register, Operand)>), // Writes for fallthrough
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
    pub name: String,
    pub params: Vec<Register>,
    pub return_type: &'static TypeInfo,
    pub blocks: hashbrown::HashMap<i64, BasicBlock>,
    pub entry_block: i64,
    current_block: i64,
    next_reg_id: i64,
    next_label_id: i64,
    omega_blocks: HashMap<Register, Arc<Mutex<Vec<Label>>>>, // Track blocks separately
}

impl Function {
    pub fn new(name: String, return_type: &'static TypeInfo) -> Self {
        let mut f = Function {
            name,
            params: Vec::new(),
            return_type,
            blocks: hashbrown::HashMap::new(),
            entry_block: 0,
            current_block: 0,
            next_reg_id: 0,
            next_label_id: 0,
            omega_blocks: HashMap::new(),
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

    pub fn get_current_block(&self) -> Option<&BasicBlock> {
        self.blocks.get(&self.current_block)
    }

    pub fn get_current_block_label<'a>(&'a self) -> &'a Label {
        &self.blocks.get(&self.current_block).unwrap().label
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
                terminator: ControlFlow::Fallthrough(Vec::new()),
            },
        );
        label
    }

    fn update_omega_writes<'a>(&mut self, writes: impl Iterator<Item = &'a (Register, Operand)>) {
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
        writes: Vec<(Register, Operand)>,
    ) -> Vec<(Register, Operand)> {
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

    pub fn make_jump_if_no_terminator(&mut self, target: Label, writes: Vec<(Register, Operand)>) {
        if !self.has_terminator(self.current_block) {
            self.make_jump(target, writes);
        }
    }

    pub fn make_jump(&mut self, target: Label, writes: Vec<(Register, Operand)>) {
        let writes = self.gen_writes_if_not_nop(writes);
        self.update_omega_writes(writes.iter());
        self.blocks
            .get_mut(&self.current_block)
            .expect("Compiler Error: No current block")
            .terminator = ControlFlow::Jump { target, writes };
    }

    pub fn make_return(&mut self, value: Option<Operand>) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = ControlFlow::Return(value);
        }
    }

    pub fn make_fallthrough(&mut self, writes: Vec<(Register, Operand)>) {
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
        if_true: (Label, Vec<(Register, Operand)>),
        if_false: (Label, Vec<(Register, Operand)>),
    ) {
        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            block.terminator = ControlFlow::Branch {
                condition,
                if_true,
                if_false,
            };
        }
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

    pub fn make_branch_to_existing(
        &mut self,
        condition: Operand,
        then: Label,
        else_: Label,
        then_writes: Vec<(Register, Operand)>,
        else_writes: Vec<(Register, Operand)>,
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
        then_writes: Vec<(Register, Operand)>,
        else_writes: Vec<(Register, Operand)>,
    ) -> (Label, Label) {
        let true_label = self.fresh_label("then".to_string());
        let false_label = self.fresh_label("else".to_string());

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

    pub fn make_omega(&mut self, target: Register) {
        let blocks = Arc::new(Mutex::new(Vec::new()));

        if let Some(block) = self.blocks.get_mut(&self.current_block) {
            // Use the same Arc<Mutex> for both the instruction and the tracking map
            block.instructions.push(Instruction::Omega {
                target: target.clone(),
                writable_blocks: Arc::clone(&blocks), // Share the same Arc
            });
            self.omega_blocks.insert(target, blocks);
        }
    }

    pub fn format_yir(&self, do_color: bool, f: &mut impl fmt::Write) -> fmt::Result {
        // Print function signature
        write!(
            f,
            "{} {}(",
            format_keyword("fn", do_color),
            if do_color {
                self.name.white()
            } else {
                self.name.as_str().into()
            }
        )?;
        for (i, reg) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", format_register(reg, do_color))?;
        }
        writeln!(f, ") -> {} {{", format_type(self.return_type, do_color))?;

        // Removed queue logic: instead, collect and sort blocks by label id
        let mut blocks: Vec<&BasicBlock> = self.blocks.values().collect();
        blocks.sort_by_key(|block| block.label.id());
        for block in blocks {
            // Print block label
            writeln!(f, "{}:", format_label(block.label.name(), do_color))?;

            // Print instructions
            for inst in &block.instructions {
                write!(f, "    ")?;
                match inst {
                    Instruction::Assign { target, value } => {
                        writeln!(
                            f,
                            "{} := {}",
                            format_register(target, do_color),
                            format_operand(value, do_color)
                        )?;
                    }
                    Instruction::Binary {
                        target,
                        op,
                        lhs,
                        rhs,
                    } => {
                        writeln!(
                            f,
                            "{} := {} {} {}",
                            format_register(target, do_color),
                            format_operand(lhs, do_color),
                            format_binop(op, do_color),
                            format_operand(rhs, do_color)
                        )?;
                    }
                    Instruction::Unary {
                        target,
                        op,
                        operand,
                    } => {
                        writeln!(
                            f,
                            "{} := {}{}",
                            format_register(target, do_color),
                            format_unop(op, do_color),
                            format_operand(operand, do_color)
                        )?;
                    }
                    Instruction::Load { target, address } => {
                        writeln!(
                            f,
                            "{} := {} {}",
                            format_register(target, do_color),
                            format_keyword("load", do_color),
                            format_operand(address, do_color)
                        )?;
                    }
                    Instruction::Store { address, value } => {
                        writeln!(
                            f,
                            "{} {} <- {}",
                            format_keyword("store", do_color),
                            format_operand(address, do_color),
                            format_operand(value, do_color)
                        )?;
                    }
                    Instruction::Alloca { target } => {
                        writeln!(
                            f,
                            "{} := {}",
                            format_register(target, do_color),
                            format_keyword("alloca", do_color)
                        )?;
                    }
                    Instruction::Call { target, name, args } => {
                        if let Some(target) = target {
                            write!(f, "{} := ", format_register(target, do_color))?;
                        }
                        write!(f, "{} {}(", format_keyword("call", do_color), name)?;
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", format_operand(arg, do_color))?;
                        }
                        writeln!(f, ")")?;
                    }
                    Instruction::Omega {
                        target,
                        writable_blocks,
                    } => {
                        write!(
                            f,
                            "{} := {} [",
                            format_register(target, do_color),
                            format_keyword("Ω", do_color)
                        )?;
                        for (i, block) in writable_blocks.lock().unwrap().iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", format_label(block.name(), do_color))?;
                        }
                        writeln!(f, "]")?;
                    }
                }
            }

            // Print terminator
            write!(f, "    ")?;
            match &block.terminator {
                ControlFlow::Jump { target, writes } => {
                    writeln!(
                        f,
                        "{} {} {}",
                        format_keyword("jump", do_color),
                        format_label(target.name(), do_color),
                        format_omikron_writes(writes, do_color)
                    )?;
                }
                ControlFlow::Branch {
                    condition,
                    if_true: (true_label, true_writes),
                    if_false: (false_label, false_writes),
                } => {
                    writeln!(
                        f,
                        "{} {} ? {} {}, {} {}",
                        format_keyword("branch", do_color),
                        format_operand(condition, do_color),
                        format_label(true_label.name(), do_color),
                        format_omikron_writes(true_writes, do_color),
                        format_label(false_label.name(), do_color),
                        format_omikron_writes(false_writes, do_color)
                    )?;
                }
                ControlFlow::Return(value) => {
                    write!(f, "{}", format_keyword("return", do_color))?;
                    if let Some(val) = value {
                        write!(f, " {}", format_operand(val, do_color))?;
                    }
                    writeln!(f)?;
                }
                ControlFlow::Fallthrough(writes) => {
                    writeln!(
                        f,
                        "{} {}",
                        format_keyword("fallthrough", do_color),
                        format_omikron_writes(writes, do_color)
                    )?;
                }
            }
            writeln!(f)?;
        }

        writeln!(f, "}}")
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
                        format_keyword("declare", do_color),
                        name,
                        format_type(func.return_type, do_color)
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

// Helper functions for formatting
fn format_type(ty: &TypeInfo, do_color: bool) -> String {
    let s = format!("@{}", ty);
    colorize(&s, "type", do_color)
}

fn format_register(reg: &Register, do_color: bool) -> String {
    let base_name = reg.name();
    let name = if reg.id() == 0 {
        base_name.to_string()
    } else {
        format!("{}.{}", base_name, reg.id())
    };
    format!(
        "{}{}",
        colorize(&name, "register", do_color),
        format_type(reg.ty(), do_color)
    )
}

fn format_operand(op: &Operand, do_color: bool) -> String {
    match op {
        Operand::I64Const(n) => {
            format!(
                "{}{}",
                colorize(&n.to_string(), "constant", do_color),
                colorize("@i64", "type", do_color)
            )
        }
        Operand::F32Const(f) => {
            format!(
                "{}{}",
                colorize(&f.to_string(), "constant", do_color),
                colorize("@f32", "type", do_color)
            )
        }
        Operand::F64Const(f) => {
            format!(
                "{}{}",
                colorize(&f.to_string(), "constant", do_color),
                colorize("@f64", "type", do_color)
            )
        }
        Operand::BoolConst(b) => {
            format!(
                "{}{}",
                colorize(&b.to_string(), "constant", do_color),
                colorize("@bool", "type", do_color)
            )
        }
        Operand::Register(reg) => {
            let name = if reg.id() == 0 {
                reg.name().to_string()
            } else {
                format!("{}.{}", reg.name(), reg.id())
            };
            format!(
                "{}{}",
                colorize(&name, "register", do_color),
                format_type(reg.ty(), do_color)
            )
        }
        Operand::NoOp => colorize("nop", "operator", do_color),
    }
}

fn format_keyword(keyword: &str, do_color: bool) -> String {
    colorize(keyword, "keyword", do_color)
}

fn format_label(label: &str, do_color: bool) -> String {
    format!(":{}", colorize(label, "label", do_color))
}

fn format_operator(op: &str, do_color: bool) -> String {
    colorize(op, "operator", do_color)
}

fn format_binop(op: &BinOp, do_color: bool) -> String {
    let op_str = match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
    };
    format_operator(op_str, do_color)
}

fn format_unop(op: &UnaryOp, do_color: bool) -> String {
    let op_str = match op {
        UnaryOp::Neg => "-",
    };
    format_operator(op_str, do_color)
}

// Add helper function for formatting Omikron writes
fn format_omikron_writes(writes: &[(Register, Operand)], do_color: bool) -> String {
    if writes.is_empty() {
        format!("ο{{}}")
    } else {
        let writes_str = writes
            .iter()
            .map(|(reg, val)| {
                format!(
                    "{} = {}",
                    format_register(reg, do_color),
                    format_operand(val, do_color)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("ο{{ {} }}", writes_str)
    }
}
