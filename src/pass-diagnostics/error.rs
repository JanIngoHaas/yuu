use logos::Span;
use miette::{
    Diagnostic, LabeledSpan, MietteDiagnostic, NamedSource, SourceSpan,
    highlighters::SyntectHighlighter,
};
use std::{fmt::Display, str::FromStr, sync::Arc};
use syntect::highlighting::ThemeItem;
use thiserror::Error;

/// Error kind enum to categorize different errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    // Syntax/Parse errors
    UnexpectedToken,
    UnexpectedEOF,
    InvalidSyntax,
    MissingToken,

    // Type errors
    TypeIncompatible,
    ReturnTypeMismatch,
    ConditionNotBoolean,

    // Callable errors
    NotCallable,
    NoMatchingOverload,
    UndefinedFunction,

    // Struct/Enum errors
    UndefinedStruct,
    UndefinedField,
    UndefinedEnum,
    UndefinedVariant,
    NotAStruct,
    VariantBindingMismatch,

    // Pointer/Memory errors
    NotAPointer,
    InvalidPointerArithmetic,
    FreeNonPointer,

    // Other semantic errors
    InvalidExpression,
    InvalidStatement,
    InvalidAddressOfExpression,
    InfinitelySizedType,
    VariableAlreadyDefined,
    NonExhaustiveMatch,

    // Lua plugin errors
    LuaPluginError,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedToken => write!(f, "Unexpected token"),
            ErrorKind::UnexpectedEOF => write!(f, "Unexpected end of file"),
            ErrorKind::InvalidSyntax => write!(f, "Invalid syntax"),
            ErrorKind::MissingToken => write!(f, "Missing token"),
            ErrorKind::TypeIncompatible => write!(f, "Incompatible types"),
            ErrorKind::ReturnTypeMismatch => write!(f, "Return type mismatch"),
            ErrorKind::ConditionNotBoolean => write!(f, "Condition must be boolean"),
            ErrorKind::NotCallable => write!(f, "Not callable"),
            ErrorKind::NoMatchingOverload => write!(f, "No matching function overload"),
            ErrorKind::UndefinedFunction => write!(f, "Undefined function"),
            ErrorKind::UndefinedStruct => write!(f, "Undefined struct"),
            ErrorKind::UndefinedField => write!(f, "Undefined field"),
            ErrorKind::UndefinedEnum => write!(f, "Undefined enum"),
            ErrorKind::UndefinedVariant => write!(f, "Undefined variant"),
            ErrorKind::NotAStruct => write!(f, "Not a struct type"),
            ErrorKind::VariantBindingMismatch => write!(f, "Variant binding mismatch"),
            ErrorKind::NotAPointer => write!(f, "Not a pointer type"),
            ErrorKind::InvalidPointerArithmetic => write!(f, "Invalid pointer arithmetic"),
            ErrorKind::FreeNonPointer => write!(f, "Cannot free non-pointer"),
            ErrorKind::InvalidExpression => write!(f, "Invalid expression"),
            ErrorKind::InvalidStatement => write!(f, "Invalid statement"),
            ErrorKind::InvalidAddressOfExpression => write!(f, "Invalid address-of expression"),
            ErrorKind::InfinitelySizedType => write!(f, "Infinitely sized type"),
            ErrorKind::VariableAlreadyDefined => write!(f, "Variable already defined"),
            ErrorKind::NonExhaustiveMatch => write!(f, "Non-exhaustive match"),
            ErrorKind::LuaPluginError => write!(f, "Lua plugin error"),
        }
    }
}

/// A flexible error type that can be dynamically built
#[derive(Clone, Error, Debug, Diagnostic)]
#[error("{message}")]
pub struct YuuError {
    /// The kind of error
    pub kind: ErrorKind,

    /// Main error message
    pub message: String,

    /// Source code where the error occurred
    #[source_code]
    pub src: NamedSource<Arc<str>>,

    /// Additional labeled spans
    #[label(collection)]
    pub labels: Vec<LabeledSpan>,

    /// Help message
    #[help]
    pub help: Option<String>,

    /// Related information or notes
    #[related]
    pub related: Vec<MietteDiagnostic>,
}

impl YuuError {
    /// Create a new error builder
    pub fn builder() -> YuuErrorBuilder {
        YuuErrorBuilder::new()
    }

    /// Quick way to create an unexpected token error
    pub fn unexpected_token(
        span: impl Into<SourceSpan>,
        expected: impl Display,
        found: impl Display,
        source_code: Arc<str>,
        file_name: Arc<str>,
    ) -> Self {
        let span_clone = span.into();

        Self::builder()
            .kind(ErrorKind::UnexpectedToken)
            .message(format!("Expected {}, found {}", expected, found))
            .source(source_code.clone(), file_name)
            .span(
                span_clone,
                format!(
                    "unexpected {}, expected {}",
                    // Try to extract the actual token from source if possible
                    if !span_clone.is_empty()
                        && span_clone.offset() + span_clone.len() <= source_code.len()
                    {
                        (source_code[span_clone.offset()..span_clone.offset() + span_clone.len()])
                            .to_string()
                    } else {
                        format!("{}", found)
                    },
                    expected
                ),
            )
            .build()
    }

    /// Quick way to create an unexpected EOF error
    pub fn unexpected_eof(
        span: impl Into<SourceSpan>,
        expected: impl Into<String>,
        source_code: Arc<str>,
        file_name: Arc<str>,
    ) -> Self {
        Self::builder()
            .kind(ErrorKind::UnexpectedEOF)
            .message(format!(
                "Unexpected end of file, expected {}",
                expected.into()
            ))
            .source(source_code, file_name)
            .span(span, "unexpected end of file")
            .build()
    }

    /// Quick way to create an invalid syntax error
    pub fn invalid_syntax(
        span: impl Into<SourceSpan>,
        message: impl Into<String>,
        label: impl Into<String>,
        source_code: Arc<str>,
        file_name: Arc<str>,
    ) -> Self {
        Self::builder()
            .kind(ErrorKind::InvalidSyntax)
            .message(message)
            .source(source_code, file_name)
            .span(span, label)
            .build()
    }

    /// Add a help message
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Add a related diagnostic
    pub fn with_related(mut self, diagnostic: impl Into<MietteDiagnostic>) -> Self {
        self.related.push(diagnostic.into());
        self
    }

    /// Add a related info note
    pub fn with_related_info(
        mut self,
        message: impl Into<String>,
        location: Option<(impl Into<SourceSpan>, impl Into<String>)>,
    ) -> Self {
        let mut diagnostic = MietteDiagnostic::new(message.into());

        if let Some((span, label)) = location {
            let span = span.into();
            let label_span = LabeledSpan::new(Some(label.into()), span.offset(), span.len());
            diagnostic = diagnostic.with_labels(vec![label_span]);
        }

        self.related.push(diagnostic);
        self
    }

    /// Add an additional labeled span
    pub fn with_label(mut self, span: impl Into<SourceSpan>, label: impl Into<String>) -> Self {
        self.labels
            .push(LabeledSpan::new_with_span(Some(label.into()), span));
        self
    }
}

/// Builder for YuuError
pub struct YuuErrorBuilder {
    kind: Option<ErrorKind>,
    message: Option<String>,
    source_code: Option<Arc<str>>,
    file_name: Option<Arc<str>>,
    span: Option<SourceSpan>,
    span_label: Option<String>,
    labels: Vec<LabeledSpan>,
    help: Option<String>,
    related: Vec<MietteDiagnostic>,
}

impl Default for YuuErrorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl YuuErrorBuilder {
    /// Create a new error builder
    pub fn new() -> Self {
        Self {
            kind: None,
            message: None,
            source_code: None,
            file_name: None,
            span: None,
            span_label: None,
            labels: Vec::new(),
            help: None,
            related: Vec::new(),
        }
    }

    /// Set the error kind
    pub fn kind(mut self, kind: ErrorKind) -> Self {
        self.kind = Some(kind);
        self
    }

    /// Set the error message
    pub fn message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }

    /// Set the source code and file name
    pub fn source(mut self, source: impl Into<Arc<str>>, file_name: impl Into<Arc<str>>) -> Self {
        self.source_code = Some(source.into());
        self.file_name = Some(file_name.into());
        self
    }

    /// Set the primary error span and label
    pub fn span(mut self, span: impl Into<SourceSpan>, label: impl Into<String>) -> Self {
        self.span = Some(span.into());
        self.span_label = Some(label.into());
        self
    }

    /// Add a help message
    pub fn help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Add an additional labeled span
    pub fn label(mut self, span: impl Into<SourceSpan>, label: impl Into<String>) -> Self {
        self.labels
            .push(LabeledSpan::new_with_span(Some(label.into()), span));
        self
    }

    /// Add a related diagnostic
    pub fn related(mut self, diagnostic: impl Into<MietteDiagnostic>) -> Self {
        self.related.push(diagnostic.into());
        self
    }

    pub fn related_info(
        mut self,
        message: Option<String>,
        span: impl Into<SourceSpan>,
        label: Option<String>,
    ) -> Self {
        let span = span.into();

        let message = message.unwrap_or_else(|| "Related information".to_string());

        let diagnostic = MietteDiagnostic::new(message)
            .with_labels(vec![LabeledSpan::new(label, span.offset(), span.len())])
            .with_severity(miette::Severity::Advice);

        self.related.push(diagnostic);
        self
    }

    pub fn build(mut self) -> YuuError {
        let kind = self.kind.unwrap_or(ErrorKind::InvalidSyntax);
        let message = self.message.unwrap_or_else(|| kind.to_string());
        let source_code = self.source_code.expect("Source code is required");
        let file_name = self.file_name.expect("File name is required");
        let span = self.span.unwrap_or_else(|| (0, 0).into());
        let primary_label = self.span_label.unwrap_or_else(|| "here".to_string());

        self.labels.push(LabeledSpan::new_primary_with_span(
            Some(primary_label),
            span,
        ));

        YuuError {
            kind,
            message,
            src: NamedSource::new(file_name.as_ref(), source_code).with_language("Rust"),
            //span: LabeledSpan::new_primary_with_span(Some(primary_label), span),
            labels: self.labels,
            help: self.help,
            related: self.related,
        }
    }
}

pub fn setup_error_formatter(
    theme_name: Option<&str>,
    use_bg_color: bool,
) -> Result<(), miette::Error> {
    use miette::{MietteHandlerOpts, RgbColors};
    use syntect::highlighting::ThemeSet;
    use syntect::parsing::SyntaxSet;

    // Load syntax set
    let syntax_set = SyntaxSet::load_defaults_newlines();

    // Select the appropriate theme
    let theme = match theme_name {
        Some("Autumn") => create_autumn_rust_theme(),
        Some("DeepOcean") => create_deep_ocean_theme(),
        Some("WarmEmber") => create_autumn_rust_theme(), // Alias for Autumn
        None => create_autumn_rust_theme(),
        _ => {
            // Load default theme set
            let theme_set = ThemeSet::load_defaults();

            // Select theme from built-in ones if not one of our custom themes
            theme_name
                .and_then(|name| theme_set.themes.get(name).cloned())
                .unwrap_or_else(|| {
                    if let Some(name) = theme_name {
                        eprintln!(
                            "Warning: Theme '{}' not found, falling back to base16-ocean.dark",
                            name
                        );
                    }
                    theme_set.themes["base16-ocean.dark"].clone()
                })
        }
    };

    // Create a custom SyntectHighlighter
    let highlighter = SyntectHighlighter::new(syntax_set, theme, use_bg_color);

    let _ = miette::set_hook(Box::new(move |_| {
        Box::new(
            MietteHandlerOpts::default()
                .terminal_links(true)
                .unicode(true)
                .show_related_errors_as_nested()
                .context_lines(2)
                .tab_width(2)
                .wrap_lines(true)
                .break_words(true)
                .width(100)
                .with_cause_chain()
                .with_syntax_highlighting(highlighter.clone())
                .rgb_colors(RgbColors::Always)
                .force_graphical(true)
                .color(true)
                .build(),
        )
    }));

    Ok(())
}

/// Reset miette handler to default
///
/// This can be useful in tests when you want to reset the formatter
pub fn reset_error_formatter() -> Result<(), miette::Error> {
    Ok(miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandler::default())
    }))?)
}

pub fn list_syntax_highlighting_themes() -> Vec<String> {
    use syntect::highlighting::ThemeSet;

    let mut themes = ThemeSet::load_defaults()
        .themes
        .keys()
        .map(|k| k.to_string())
        .collect::<Vec<_>>();

    // Add our custom themes
    themes.push("AutumnRust".to_string());
    themes.push("DeepOcean".to_string());
    themes.push("WarmEmber".to_string()); // Alias for AutumnRust

    themes.sort();
    themes
}

use syntect::highlighting::Color as SyntectColor;
use syntect::highlighting::{FontStyle, Style, StyleModifier, Theme, ThemeSettings};
use syntect::highlighting::{ScopeSelector, ScopeSelectors};

use crate::pass_parse::ast::{InternUstr, SourceInfo};
use crate::utils::type_info_table::TypeInfo;
use crate::utils::{FunctionInfo, TypeRegistry};

/// Create a custom theme based on the warm_ember color palette
pub fn create_autumn_rust_theme() -> Theme {
    let background = SyntectColor {
        r: 30,
        g: 30,
        b: 30,
        a: 255,
    };

    let foreground = SyntectColor {
        r: 230,
        g: 230,
        b: 230,
        a: 255,
    };

    // Colors from warm_ember palette
    let keyword_color = SyntectColor {
        r: 255,
        g: 140,
        b: 85,
        a: 255,
    }; // Warm Orange
    let type_color = SyntectColor {
        r: 255,
        g: 183,
        b: 138,
        a: 255,
    }; // Peach
    let function_color = SyntectColor {
        r: 255,
        g: 255,
        b: 255,
        a: 255,
    }; // White
    let constant_color = SyntectColor {
        r: 255,
        g: 218,
        b: 121,
        a: 255,
    }; // Warm Yellow
    let operator_color = SyntectColor {
        r: 255,
        g: 110,
        b: 74,
        a: 255,
    }; // Burnt Orange
    let string_color = SyntectColor {
        r: 152,
        g: 224,
        b: 178,
        a: 255,
    }; // Light Green (for strings)
    let comment_color = SyntectColor {
        r: 130,
        g: 130,
        b: 130,
        a: 255,
    }; // Grey for comments
    let macro_color = SyntectColor {
        r: 255,
        g: 166,
        b: 158,
        a: 255,
    }; // Coral Pink (for macros)

    let settings = ThemeSettings {
        background: Some(background),
        foreground: Some(foreground),
        caret: Some(foreground),
        line_highlight: Some(SyntectColor {
            r: 45,
            g: 45,
            b: 45,
            a: 255,
        }),
        gutter: Some(background),
        gutter_foreground: Some(SyntectColor {
            r: 130,
            g: 130,
            b: 130,
            a: 255,
        }),
        selection: Some(SyntectColor {
            r: 55,
            g: 55,
            b: 55,
            a: 255,
        }),
        selection_foreground: Some(foreground),
        find_highlight: Some(SyntectColor {
            r: 25,
            g: 25,
            b: 112,
            a: 255,
        }),
        find_highlight_foreground: Some(foreground),
        highlight: Some(SyntectColor {
            r: 50,
            g: 50,
            b: 50,
            a: 255,
        }),
        ..ThemeSettings::default()
    };

    // // Helper function to create a ThemeItem with proper scopes
    // fn create_theme_item(scope_str: &str, style: Style) -> ThemeItem {
    //     let selector = ScopeSelector::from_str(scope_str);
    //     ThemeItem {
    //         scope: ScopeSelectors {
    //             selectors: vec![selector],
    //         },
    //         style,
    //     }
    // }

    Theme {
        name: Some("Autumn".to_string()),
        author: Some("Hyperion + Claude 3.7".to_string()),
        settings,
        scopes: vec![
            // General
            create_theme_item(
                "comment",
                Style {
                    foreground: comment_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
            create_theme_item(
                "string",
                Style {
                    foreground: string_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "constant",
                Style {
                    foreground: constant_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "constant.numeric",
                Style {
                    foreground: constant_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Keywords and control flow
            create_theme_item(
                "keyword",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "keyword.control",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "keyword.operator",
                Style {
                    foreground: operator_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Rust specific
            create_theme_item(
                "entity.name.function",
                Style {
                    foreground: function_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "entity.name.struct",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "entity.name.enum",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "entity.name.trait",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
            create_theme_item(
                "storage.type",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Macros
            create_theme_item(
                "support.macro",
                Style {
                    foreground: macro_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Other types
            create_theme_item(
                "storage.modifier",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "variable.language",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
            create_theme_item(
                "variable.parameter",
                Style {
                    foreground,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Operators and punctuation
            create_theme_item(
                "punctuation.separator",
                Style {
                    foreground: operator_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "punctuation.terminator",
                Style {
                    foreground: operator_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Lifetime annotations
            create_theme_item(
                "storage.modifier.lifetime",
                Style {
                    foreground: operator_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
        ],
    }
}

/// Create a custom theme based on the deep_ocean color palette from YIR
pub fn create_deep_ocean_theme() -> Theme {
    let background = SyntectColor {
        r: 15,
        g: 20,
        b: 40,
        a: 255,
    };
    let foreground = SyntectColor {
        r: 230,
        g: 230,
        b: 250,
        a: 255,
    };

    // Colors from deep_ocean palette
    let function_color = SyntectColor {
        r: 255,
        g: 255,
        b: 255,
        a: 255,
    }; // White
    let keyword_color = SyntectColor {
        r: 255,
        g: 215,
        b: 0,
        a: 255,
    }; // Deep Gold
    let type_color = SyntectColor {
        r: 64,
        g: 224,
        b: 208,
        a: 255,
    }; // Turquoise

    let constant_color = SyntectColor {
        r: 46,
        g: 139,
        b: 87,
        a: 255,
    }; // Sea Green
    let label_color = SyntectColor {
        r: 125,
        g: 249,
        b: 255,
        a: 255,
    }; // Electric Blue
    let operator_color = SyntectColor {
        r: 250,
        g: 128,
        b: 114,
        a: 255,
    }; // Salmon
    let string_color = SyntectColor {
        r: 144,
        g: 238,
        b: 144,
        a: 255,
    }; // Light Green
    let comment_color = SyntectColor {
        r: 100,
        g: 110,
        b: 140,
        a: 255,
    }; // Muted Blue

    let settings = ThemeSettings {
        background: Some(background),
        foreground: Some(foreground),
        caret: Some(foreground),
        line_highlight: Some(SyntectColor {
            r: 25,
            g: 30,
            b: 50,
            a: 255,
        }),
        gutter: Some(background),
        gutter_foreground: Some(SyntectColor {
            r: 100,
            g: 110,
            b: 140,
            a: 255,
        }),
        selection: Some(SyntectColor {
            r: 30,
            g: 35,
            b: 60,
            a: 255,
        }),
        selection_foreground: Some(foreground),
        find_highlight: Some(SyntectColor {
            r: 35,
            g: 35,
            b: 95,
            a: 255,
        }),
        find_highlight_foreground: Some(foreground),
        highlight: Some(SyntectColor {
            r: 30,
            g: 40,
            b: 60,
            a: 255,
        }),
        ..ThemeSettings::default()
    };

    Theme {
        name: Some("DeepOcean".to_string()),
        author: Some("Hyperion + Claude 3.7".to_string()),
        settings,
        scopes: vec![
            // General
            create_theme_item(
                "comment",
                Style {
                    foreground: comment_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
            create_theme_item(
                "string",
                Style {
                    foreground: string_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "constant",
                Style {
                    foreground: constant_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "constant.numeric",
                Style {
                    foreground: constant_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Keywords and control flow
            create_theme_item(
                "keyword",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "keyword.control",
                Style {
                    foreground: keyword_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "keyword.operator",
                Style {
                    foreground: operator_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Rust specific
            create_theme_item(
                "entity.name.function",
                Style {
                    foreground: function_color,
                    background,
                    font_style: FontStyle::BOLD,
                },
            ),
            create_theme_item(
                "entity.name.struct",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "entity.name.enum",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            create_theme_item(
                "entity.name.trait",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::ITALIC,
                },
            ),
            create_theme_item(
                "storage.type",
                Style {
                    foreground: type_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
            // Rest of the scope mappings
            create_theme_item(
                "support.macro",
                Style {
                    foreground: label_color,
                    background,
                    font_style: FontStyle::empty(),
                },
            ),
        ],
    }
}

// Helper function to create a ThemeItem with proper scopes
fn create_theme_item(scope_str: &str, style: Style) -> ThemeItem {
    let selector = ScopeSelector::from_str(scope_str).expect("Invalid scope selector - check path");
    ThemeItem {
        scope: ScopeSelectors {
            selectors: vec![selector],
        },
        style: StyleModifier {
            foreground: Some(style.foreground),
            background: Some(style.background),
            font_style: Some(style.font_style),
        },
    }
}

pub fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.chars().count();
    let len2 = s2.chars().count();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for (i, row) in matrix.iter_mut().enumerate().take(len1 + 1) {
        row[0] = i;
    }

    #[allow(clippy::needless_range_loop)]
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let substitution = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                .min(matrix[i + 1][j] + 1)
                .min(matrix[i][j] + substitution);
        }
    }

    matrix[len1][len2]
}

pub fn create_no_overload_error(
    name: &str,
    candidates: Vec<FunctionInfo>,
    provided_args: &[&'static TypeInfo],
    reg: &TypeRegistry,
    src: &SourceInfo,
    sp: Span,
) -> YuuError {
    // TODO: Add "did you mean" suggestions - should be easy, just do a levenstein distance check in type registry
    if candidates.is_empty() {
        let suggestions = reg.get_similar_names_func(name.intern(), 3);

        let builder = YuuError::builder()
            .kind(ErrorKind::UndefinedFunction)
            .message(format!("Function '{}' not declared", name))
            .source(src.source.clone(), src.file_name.clone())
            .span((sp.start, (sp.end - sp.start)), "undefined function");

        let mut help = format!(
            "Function '{}' needs to be declared in the global scope before it can be used",
            name
        );

        // Add suggestions if any were found
        if !suggestions.is_empty() {
            help.push_str("\n\nDid you mean:");
            for (i, suggestion) in suggestions.iter().enumerate() {
                if i > 0 {
                    help.push(',');
                }
                help.push_str(&format!(" '{}'", suggestion));
            }
            help.push('?');
        }

        return builder.help(help).build();
    }
    let mut builder = YuuError::builder()
        .kind(ErrorKind::NoMatchingOverload)
        .message(format!(
            "No matching overload found for function '{}'",
            name
        ));

    builder = builder
        .source(src.source.clone(), src.file_name.clone())
        .span(
            (sp.start, (sp.end - sp.start)),
            "no matching function overload",
        );

    // Add information about each candidate
    for (i, func) in candidates.iter().enumerate() {
        if let Some(decl_span) = &func.binding_info.src_location {
            if decl_span.start == 0 && decl_span.end == 0 {
                continue;
            }
            builder = builder.related_info(
                Some(format!("Candidate Function {}", i + 1)),
                (decl_span.start, (decl_span.end - decl_span.start)),
                Some(format!("candidate {}", i + 1)),
            );
        }
    }
    // Add more detailed help
    if !candidates.is_empty() {
        let given_types = provided_args
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");

        let help_types = &format!("Provided argument types: ({})", given_types);

        let mut help = format!(
            "Function '{}' exists but arguments don't match.\n{}\n\nCandidate overloads:\n",
            name, help_types
        );
        for (i, func) in candidates.iter().enumerate() {
            let arg_types = func
                .ty
                .args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            let ret_type = func.ty.ret.to_string();
            help.push_str(&format!(
                "{}. fn {}({}) -> {}\n",
                i + 1,
                name,
                arg_types,
                ret_type
            ));
        }

        builder = builder.help(help);
    }

    builder.build()
}
