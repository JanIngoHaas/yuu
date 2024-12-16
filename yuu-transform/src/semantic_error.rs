use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use std::{cell::RefCell, fmt::Display, ops::DerefMut, rc::Rc, sync::Arc};

use yuu_parse::{
    lexer::UnprocessedCodeInfo,
    parser::{SourceCodeInfo, SrcCache},
    Span,
};

pub enum Severity {
    Error,
    Warning,
    Note,
}

pub enum NoteType {
    ErrorExplanation,
    Explanation,
    Help,
    Fix,
}

pub struct Note {
    pub message: String,
    pub span: Option<Span>,
    pub note_type: NoteType,
}

impl Default for Note {
    fn default() -> Self {
        Self {
            message: String::new(),
            span: None,
            note_type: NoteType::Explanation,
        }
    }
}

pub struct GenericSemanticErrorMsg {
    pub cache: SrcCache,
    pub message: String,
    pub span: Span,
    pub notes: Vec<Note>,
    pub severity: Severity,
}

pub enum SemanticErrorMsg {
    Generic(GenericSemanticErrorMsg),
    FunctionArgTypeMismatch {
        cache: SrcCache,
        span: Span,
        expected: String,
        got: String,
    },
}

impl GenericSemanticErrorMsg {
    pub fn new(
        cache: SrcCache,
        message: String,
        span: Span,
        notes: Vec<Note>,
        severity: Severity,
    ) -> Self {
        Self {
            cache,
            message,
            span,
            notes,
            severity,
        }
    }
}

impl SemanticErrorMsg {
    pub fn eprint(&self) {
        match self {
            SemanticErrorMsg::Generic(msg) => {
                let severity = match msg.severity {
                    Severity::Error => ReportKind::Error,
                    Severity::Warning => ReportKind::Warning,
                    Severity::Note => ReportKind::Advice,
                };
                let filename = msg.cache.borrow();
                let mut report = Report::build(severity, (filename.0.clone(), msg.span.clone()))
                    .with_message(msg.message.clone());

                for note in &msg.notes {
                    let color = match note.note_type {
                        NoteType::Explanation => Color::Blue,
                        NoteType::Fix => Color::Green,
                        NoteType::ErrorExplanation => Color::Red,
                        NoteType::Help => {
                            report = report.with_help(note.message.clone());
                            continue;
                        }
                    };
                    if let Some(span) = &note.span {
                        report.add_label(
                            Label::new((filename.0.clone(), span.clone()))
                                .with_message(&note.message)
                                .with_color(color),
                        );
                    } else {
                        report = report.with_note(note.message.clone().fg(color));
                    }
                }
                let _ = drop(filename);
                let mut cache = msg.cache.borrow_mut();
                report
                    .finish()
                    .eprint(cache.deref_mut())
                    .expect("Failed to print");
            }
            SemanticErrorMsg::FunctionArgTypeMismatch {
                cache,
                span,
                expected,
                got,
            } => todo!(),
        }
    }
}
