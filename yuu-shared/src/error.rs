use miette::{
    highlighters::{Highlighter, SyntectHighlighter},
    Diagnostic, LabeledSpan, MietteDiagnostic, NamedSource, SourceSpan,
};
use std::sync::Arc;
use syntect::parsing::SyntaxSet;
use thiserror::Error;

use crate::scheduler::ResourceId;

/// Source code information for error reporting
#[derive(Debug, Clone)]
pub struct SourceInfo {
    pub source: Arc<str>,
    pub file_name: Arc<str>,
}

impl ResourceId for SourceInfo {
    fn resource_name() -> &'static str {
        "UnprocessedCodeInfo"
    }
}

/// Error kind enum to categorize different errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken,
    UnexpectedEOF,
    InvalidSyntax,
    MissingToken,
    InvalidExpression,
    InvalidStatement,
    FunctionOverloadError,
    TypeMismatch,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedToken => write!(f, "Unexpected token"),
            ErrorKind::UnexpectedEOF => write!(f, "Unexpected end of file"),
            ErrorKind::InvalidSyntax => write!(f, "Invalid syntax"),
            ErrorKind::MissingToken => write!(f, "Missing token"),
            ErrorKind::InvalidExpression => write!(f, "Invalid expression"),
            ErrorKind::InvalidStatement => write!(f, "Invalid statement"),
            ErrorKind::FunctionOverloadError => write!(f, "No matching function overload"),
            ErrorKind::TypeMismatch => write!(f, "Types do not match"),
        }
    }
}

/// A flexible error type that can be dynamically built
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
pub struct YuuError {
    /// The kind of error
    pub kind: ErrorKind,

    /// Main error message
    pub message: String,

    /// Source code where the error occurred
    #[source_code]
    pub src: NamedSource<Arc<str>>,

    /// Primary location of the error
    #[label("{primary_label}")]
    pub span: SourceSpan,

    /// Label text for the primary location
    #[allow(dead_code)]
    primary_label: String,

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
        expected: impl Into<String>,
        found: impl Into<String>,
        source_code: Arc<str>,
        file_name: Arc<str>,
    ) -> Self {
        let span_clone = span.into();
        let found_str = found.into();

        Self::builder()
            .kind(ErrorKind::UnexpectedToken)
            .message(format!("Expected {}, found {}", expected.into(), found_str))
            .source(source_code.clone(), file_name)
            .span(
                span_clone.clone(),
                format!(
                    "unexpected '{}'",
                    // Try to extract the actual token from source if possible
                    if span_clone.len() > 0
                        && span_clone.offset() + span_clone.len() <= source_code.len()
                    {
                        &source_code[span_clone.offset()..span_clone.offset() + span_clone.len()]
                    } else {
                        &found_str
                    }
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
        let span = span.into();
        self.labels.push(LabeledSpan::new(
            Some(label.into()),
            span.offset(),
            span.len(),
        ));
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
        let span = span.into();
        self.labels.push(LabeledSpan::new(
            Some(label.into()),
            span.offset(),
            span.len(),
        ));
        self
    }

    /// Add a related diagnostic
    pub fn related(mut self, diagnostic: impl Into<MietteDiagnostic>) -> Self {
        self.related.push(diagnostic.into());
        self
    }

    pub fn build(self) -> YuuError {
        let kind = self.kind.unwrap_or(ErrorKind::InvalidSyntax);
        let message = self.message.unwrap_or_else(|| kind.to_string());
        let source_code = self.source_code.expect("Source code is required");
        let file_name = self.file_name.expect("File name is required");
        let span = self.span.unwrap_or_else(|| (0, 0).into());
        let primary_label = self.span_label.unwrap_or_else(|| "here".to_string());

        YuuError {
            kind,
            message,
            src: NamedSource::new(file_name.as_ref(), source_code).with_language("Rust"),
            span,
            primary_label,
            labels: self.labels,
            help: self.help,
            related: self.related,
        }
    }
}

// create related info
pub fn related_info(
    message: impl Into<String>,
    location: Option<(impl Into<SourceSpan>, impl Into<String>)>,
) -> MietteDiagnostic {
    let mut diagnostic = MietteDiagnostic::new(message.into());

    if let Some((span, label)) = location {
        let span = span.into();
        let label_span = LabeledSpan::new(Some(label.into()), span.offset(), span.len());
        diagnostic = diagnostic.with_labels(vec![label_span]);
    }

    diagnostic
}

/// Configure miette with syntax highlighting and other nice features
///
/// This function sets up miette with syntax highlighting for code in error messages.
/// You can optionally specify a custom theme name and whether to use background colors.
///
/// # Available themes
/// - "InspiredGitHub" (light)
/// - "Solarized (light)"
/// - "Solarized (dark)"
/// - "base16-ocean.dark" (default)
/// - "base16-eighties.dark"
/// - "base16-mocha.dark"
/// - "base16-ocean.light"
///
/// # Arguments
/// * `theme_name` - Optional theme name to use. Defaults to "base16-ocean.dark" if None or if the specified theme is not found.
/// * `use_bg_color` - Whether to use background colors in the syntax highlighting. Defaults to false.
///
/// # Example
/// ```
/// fn main() -> miette::Result<()> {
///     // Use default theme (base16-ocean.dark)
///     yuu_shared::error::setup_error_formatter(None, false)?;
///     
///     // Or use a specific theme:
///     // yuu_shared::error::setup_error_formatter(Some("base16-mocha.dark"), true)?;
///     
///     // Or for a light theme:
///     // yuu_shared::error::setup_error_formatter(Some("InspiredGitHub"), false)?;
///     
///     // Rest of your code...
///     Ok(())
/// }
/// ```
pub fn setup_error_formatter(
    theme_name: Option<&str>,
    use_bg_color: bool,
) -> Result<(), miette::Error> {
    use miette::{MietteHandlerOpts, RgbColors};
    use syntect::highlighting::ThemeSet;
    use syntect::parsing::SyntaxSet;

    // Load default theme set and syntax set
    let theme_set = ThemeSet::load_defaults();
    let syntax_set = SyntaxSet::load_defaults_newlines();

    // Select theme - either the specified one or the default
    let theme = theme_name
        .and_then(|name| theme_set.themes.get(name).cloned())
        .unwrap_or_else(|| {
            if let Some(name) = theme_name {
                eprintln!(
                    "Warning: Theme '{}' not found, falling back to base16-mocha.dark",
                    name
                );
            }
            theme_set.themes["base16-mocha.dark"].clone()
        });

    // Create a custom SyntectHighlighter
    let highlighter = SyntectHighlighter::new(syntax_set, theme, use_bg_color);

    Ok(miette::set_hook(Box::new(move |_| {
        Box::new(
            MietteHandlerOpts::default()
                .terminal_links(true)
                .unicode(true)
                .context_lines(4)
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
    }))?)
}

/// Reset miette handler to default
///
/// This can be useful in tests when you want to reset the formatter
pub fn reset_error_formatter() -> Result<(), miette::Error> {
    Ok(miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandler::default())
    }))?)
}

/// Return a list of all available syntax highlighting themes
///
/// This function can be useful for UI purposes or for providing options to users.
///
/// # Example
/// ```
/// fn main() {
///     let themes = yuu_shared::error::list_syntax_highlighting_themes();
///     println!("Available themes:");
///     for theme in themes {
///         println!("  - {}", theme);
///     }
/// }
/// ```
pub fn list_syntax_highlighting_themes() -> Vec<String> {
    use syntect::highlighting::ThemeSet;

    let theme_set = ThemeSet::load_defaults();
    theme_set.themes.keys().map(|k| k.to_string()).collect()
}
