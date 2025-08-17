use crate::pass_type_inference::TypeInfo;
use crate::pass_yir_lowering::Label;
use crate::pass_yir_lowering::yir::{BinOp, ControlFlow, Instruction, Operand, UnaryOp, Variable};
use indexmap::IndexMap;
use std::fmt;
use std::io::Write;
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

// Color palette system
#[derive(Clone)]
#[allow(clippy::upper_case_acronyms)]
struct RGB(u8, u8, u8);

struct ColorPalette {
    colors: IndexMap<&'static str, RGB>,
    color_choice: ColorChoice,
}

impl ColorPalette {
    fn detect_color_choice() -> ColorChoice {
        // Check NO_COLOR first - respect user preferences
        if std::env::var("NO_COLOR").is_ok() {
            return ColorChoice::Never;
        }

        // Use termcolor's auto detection
        ColorChoice::Auto
    }

    #[allow(dead_code)]
    fn deep_ocean() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 215, 0)); // Deep Gold
        colors.insert("type", RGB(64, 224, 208)); // Turquoise
        colors.insert("variable", RGB(230, 230, 250)); // Lavender
        colors.insert("constant", RGB(46, 139, 87)); // Sea Green
        colors.insert("label", RGB(125, 249, 255)); // Electric Blue
        colors.insert("operator", RGB(250, 128, 114)); // Salmon

        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    fn warm_ember() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 140, 85)); // Warm Orange
        colors.insert("type", RGB(255, 183, 138)); // Peach
        colors.insert("variable", RGB(255, 121, 121)); // Soft Red
        colors.insert("constant", RGB(255, 218, 121)); // Warm Yellow
        colors.insert("label", RGB(255, 166, 158)); // Coral Pink
        colors.insert("operator", RGB(255, 110, 74)); // Burnt Orange

        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    #[allow(dead_code)]
    fn mystic_forest() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(144, 238, 144)); // Light Green
        colors.insert("type", RGB(152, 251, 152)); // Pale Green
        colors.insert("variable", RGB(143, 188, 143)); // Dark Sea Green
        colors.insert("constant", RGB(50, 205, 50)); // Lime Green
        colors.insert("label", RGB(127, 255, 170)); // Aquamarine
        colors.insert("operator", RGB(34, 139, 34)); // Forest Green
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    #[allow(dead_code)]
    fn cosmic_night() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(147, 112, 219)); // Medium Purple
        colors.insert("type", RGB(138, 43, 226)); // Blue Violet
        colors.insert("variable", RGB(216, 191, 216)); // Thistle
        colors.insert("constant", RGB(221, 160, 221)); // Plum
        colors.insert("label", RGB(218, 112, 214)); // Orchid
        colors.insert("operator", RGB(186, 85, 211)); // Medium Orchid
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    #[allow(dead_code)]
    fn neon_dreams() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(255, 255, 255)); // White
        colors.insert("keyword", RGB(255, 110, 199)); // Hot Pink
        colors.insert("type", RGB(0, 255, 255)); // Cyan
        colors.insert("variable", RGB(191, 255, 0)); // Neon Green
        colors.insert("constant", RGB(255, 0, 255)); // Magenta
        colors.insert("label", RGB(255, 255, 0)); // Yellow
        colors.insert("operator", RGB(255, 69, 0)); // Red-Orange
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    // === Light Background Palettes ===

    #[allow(dead_code)]
    fn summer_breeze() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(255, 110, 74)); // Burnt Orange
        colors.insert("type", RGB(0, 119, 182)); // Ocean Blue
        colors.insert("variable", RGB(86, 130, 3)); // Olive Green
        colors.insert("constant", RGB(255, 49, 49)); // Bright Red
        colors.insert("label", RGB(106, 76, 147)); // Royal Purple
        colors.insert("operator", RGB(220, 47, 2)); // Deep Red
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    #[allow(dead_code)]
    fn cherry_blossom() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(219, 68, 88)); // Deep Pink
        colors.insert("type", RGB(15, 76, 129)); // Navy Blue
        colors.insert("variable", RGB(255, 87, 127)); // Bright Pink
        colors.insert("constant", RGB(53, 80, 112)); // Steel Blue
        colors.insert("label", RGB(234, 72, 72)); // Coral Red
        colors.insert("operator", RGB(140, 20, 84)); // Dark Magenta
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    #[allow(dead_code)]
    fn forest_morning() -> Self {
        let mut colors = IndexMap::new();
        colors.insert("function", RGB(0, 0, 0)); // Black
        colors.insert("keyword", RGB(0, 102, 0)); // Dark Green
        colors.insert("type", RGB(0, 77, 122)); // Deep Blue
        colors.insert("variable", RGB(153, 51, 0)); // Brown
        colors.insert("constant", RGB(204, 51, 0)); // Dark Orange
        colors.insert("label", RGB(0, 153, 76)); // Emerald
        colors.insert("operator", RGB(153, 0, 0)); // Dark Red
        Self {
            colors,
            color_choice: Self::detect_color_choice(),
        }
    }

    fn get_color(&self, key: &str) -> RGB {
        self.colors.get(key).cloned().unwrap_or(RGB(255, 255, 255))
    }

    fn get_color_choice(&self) -> ColorChoice {
        self.color_choice
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
        let RGB(r, g, b) = palette.get_color(color_key);

        // Create a buffer with the appropriate color choice
        let writer = BufferWriter::stderr(palette.get_color_choice());
        let mut buffer = writer.buffer();

        // Create color specification
        let mut color_spec = ColorSpec::new();
        color_spec.set_fg(Some(Color::Rgb(r, g, b)));

        // Write the text with color
        buffer.set_color(&color_spec).ok();
        write!(&mut buffer, "{}", text).ok();
        buffer.reset().ok();

        // Convert buffer to string
        String::from_utf8_lossy(buffer.as_slice()).into_owned()
    })
}

// Helper functions for formatting
pub fn format_type(ty: &TypeInfo, do_color: bool) -> String {
    let s = format!("@{}", ty);
    colorize(&s, "type", do_color)
}

pub fn format_variable(reg: &Variable, do_color: bool) -> String {
    let base_name = reg.name();
    let name = if reg.id() == 0 {
        base_name.to_string()
    } else {
        format!("{}.{}", base_name, reg.id())
    };
    format!(
        "{}{}",
        colorize(&name, "variable", do_color),
        format_type(reg.ty(), do_color)
    )
}

pub fn format_operand(op: &Operand, do_color: bool) -> String {
    match op {
        Operand::I64Const(n) => {
            format!(
                "{}{}",
                colorize(&n.to_string(), "constant", do_color),
                colorize("@i64", "type", do_color)
            )
        }
        Operand::U64Const(n) => {
            format!(
                "{}{}",
                colorize(&n.to_string(), "constant", do_color),
                colorize("@u64", "type", do_color)
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
        Operand::Variable(reg) => {
            let name = if reg.id() == 0 {
                reg.name().to_string()
            } else {
                format!("{}.{}", reg.name(), reg.id())
            };
            format!(
                "{}{}",
                colorize(&name, "variable", do_color),
                format_type(reg.ty(), do_color)
            )
        }
        Operand::NoOp => colorize("nop", "operator", do_color),
    }
}

pub fn format_keyword(keyword: &str, do_color: bool) -> String {
    colorize(keyword, "keyword", do_color)
}

pub fn format_label(label: &Label, do_color: bool) -> String {
    let mut lname = String::new();
    label.write_unique_name(&mut lname);
    format!(":{}", colorize(&lname, "label", do_color))
}

pub fn format_operator(op: &str, do_color: bool) -> String {
    colorize(op, "operator", do_color)
}

pub fn format_binop(op: &BinOp, do_color: bool) -> String {
    let op_str = match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Mod => "%",
        BinOp::Eq => "==",
        BinOp::NotEq => "!=",
        BinOp::LessThan => "<",
        BinOp::LessThanEq => "<=",
        BinOp::GreaterThan => ">",
        BinOp::GreaterThanEq => ">=",
    };
    format_operator(op_str, do_color)
}

pub fn format_unop(op: &UnaryOp, do_color: bool) -> String {
    let op_str = match op {
        UnaryOp::Neg => "-",
    };
    format_operator(op_str, do_color)
}

// Format function for Omikron writes
pub fn format_omikron_writes(writes: &[(Variable, Operand)], do_color: bool) -> String {
    if writes.is_empty() {
        "ο{}".to_string()
    } else {
        let writes_str = writes
            .iter()
            .map(|(reg, val)| {
                format!(
                    "{} = {}",
                    format_variable(reg, do_color),
                    format_operand(val, do_color)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("ο{{ {} }}", writes_str)
    }
}

// Functions to handle YIR printing
pub fn format_instruction(
    inst: &Instruction,
    do_color: bool,
    f: &mut impl fmt::Write,
) -> fmt::Result {
    match inst {
        Instruction::Alloca { target } => {
            writeln!(
                f,
                "{} := {}{}",
                format_variable(target, do_color),
                colorize("alloca", "operator", do_color),
                format_type(target.ty().deref_ptr(), do_color),
            )
        }
        Instruction::StoreImmediate { target, value } => {
            writeln!(
                f,
                "{} <- {}",
                format_variable(target, do_color),
                format_operand(value, do_color)
            )
        }
        Instruction::TakeAddress { target, source } => {
            writeln!(
                f,
                "{} := {} {}",
                format_variable(target, do_color),
                format_keyword("addr", do_color),
                format_variable(source, do_color)
            )
        }
        Instruction::GetFieldPtr {
            target,
            base,
            field,
        } => {
            writeln!(
                f,
                "{} := {} {} {}",
                format_variable(target, do_color),
                format_keyword("field_ptr", do_color),
                format_operand(base, do_color),
                colorize(&format!(".{}", field), "operator", do_color)
            )
        }
        Instruction::Load { target, source } => {
            writeln!(
                f,
                "{} := {} {}",
                format_variable(target, do_color),
                format_keyword("load", do_color),
                format_operand(source, do_color)
            )
        }
        Instruction::Store { dest, value } => {
            writeln!(
                f,
                "{} <- {} {}",
                format_operand(dest, do_color),
                format_keyword("load", do_color),
                format_operand(value, do_color)
            )
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
                format_variable(target, do_color),
                format_operand(lhs, do_color),
                format_binop(op, do_color),
                format_operand(rhs, do_color)
            )
        }
        Instruction::Unary {
            target,
            op,
            operand,
        } => {
            writeln!(
                f,
                "{} := {}{}",
                format_variable(target, do_color),
                format_unop(op, do_color),
                format_operand(operand, do_color)
            )
        }
        Instruction::Call { target, name, args } => {
            if let Some(target) = target {
                write!(f, "{} := ", format_variable(target, do_color))?;
            }
            write!(f, "{} {}(", format_keyword("call", do_color), name)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", format_operand(arg, do_color))?;
            }
            writeln!(f, ")")
        }
        Instruction::MakeEnum {
            target,
            variant_name,
            variant_index,
            data,
        } => {
            write!(
                f,
                "{} := {} {}#{}",
                format_variable(target, do_color),
                format_keyword("enum", do_color),
                colorize(variant_name, "literal", do_color),
                variant_index
            )?;
            if let Some(data_operand) = data {
                write!(f, "({})", format_operand(data_operand, do_color))?;
            }
            writeln!(f)
        } // Instruction::MakeStruct {
          //     target,
          //     type_ident,
          //     fields,
          // } => {
          //     write!(
          //         f,
          //         "{} := {} {{ ",
          //         format_variable(target, do_color),
          //         format_keyword("make_struct", do_color)
          //     )?;
          //     for (i, (field, val)) in fields.iter().enumerate() {
          //         if i > 0 {
          //             write!(f, ", ")?;
          //         }
          //         write!(f, "{}: {}", field, format_operand(val, do_color))?;
          //     }
          //     writeln!(f, " }}")
          // }
    }
}

pub fn format_control_flow(
    terminator: &ControlFlow,
    do_color: bool,
    f: &mut impl fmt::Write,
) -> fmt::Result {
    match terminator {
        ControlFlow::Jump { target } => {
            writeln!(
                f,
                "{} {}",
                format_keyword("jump", do_color),
                format_label(target, do_color)
            )
        }
        ControlFlow::Branch {
            condition,
            if_true,
            if_false,
        } => {
            writeln!(
                f,
                "{} {} ? {}, {}",
                format_keyword("branch", do_color),
                format_operand(condition, do_color),
                format_label(if_true, do_color),
                format_label(if_false, do_color)
            )
        }
        ControlFlow::Return(value) => {
            write!(f, "{}", format_keyword("return", do_color))?;
            if let Some(val) = value {
                write!(f, " {}", format_operand(val, do_color))?;
            }
            writeln!(f)
        }
        ControlFlow::Unterminated => writeln!(f, "<unreachable>"),
        ControlFlow::JumpTable {
            scrutinee,
            enum_name,
            jump_targets,
            default,
        } => {
            write!(
                f,
                "jump_table {} enum {} {{",
                format_operand(scrutinee, false),
                enum_name
            )?;
            writeln!(f)?;

            for (variant_name, label) in jump_targets {
                writeln!(f, "    {} -> {}", variant_name, label.name())?;
            }

            if let Some(default_label) = default {
                writeln!(f, "    default -> {}", default_label.name())?;
            }

            writeln!(f, "}}")
        }
    }
}

// Functions for formatting complete YIR structures

pub fn format_yir(
    function: &crate::pass_yir_lowering::yir::Function,
    do_color: bool,
    f: &mut impl fmt::Write,
) -> fmt::Result {
    // Function signature
    writeln!(
        f,
        "{} {}({}) -> {}",
        format_keyword("fn", do_color),
        format_keyword(&function.name, do_color),
        function
            .params
            .iter()
            .map(|p| format!(
                "{} := {}{}",
                format_variable(p, do_color),
                format_keyword("param", do_color),
                format_type(p.ty(), do_color)
            ))
            .collect::<Vec<_>>()
            .join(", "),
        format_type(function.return_type, do_color)
    )?;

    // Opening brace
    writeln!(f, "{{")?;

    // Blocks
    for (_, block) in &function.blocks {
        // Block label
        writeln!(f, "{}:", format_label(&block.label, do_color))?;

        // Instructions
        for inst in &block.instructions {
            write!(f, "    ")?;
            format_instruction(inst, do_color, f)?;
        }

        // Control flow terminator
        write!(f, "    ")?;
        format_control_flow(&block.terminator, do_color, f)?;

        writeln!(f)?;
    }

    // Closing brace
    writeln!(f, "}}")
}
