#[derive(Clone)]
pub enum SemanticError {}

// use crate::{
//     built_in::BindingInfo,
//     type_info::{FunctionType, TypeInfo},
// };
// use ariadne::{Color, Fmt, Label, Report, ReportKind};
// use std::ops::DerefMut;
// use yuu_parse::{parser::SrcCache, Span};

// pub enum Severity {
//     Error,
//     Warning,
//     Note,
// }

// pub enum NoteType {
//     ErrorExplanation,
//     Explanation,
//     Help,
//     Fix,
// }

// pub struct Note {
//     pub message: String,
//     pub span: Option<Span>,
//     pub note_type: NoteType,
// }

// impl Default for Note {
//     fn default() -> Self {
//         Self {
//             message: String::new(),
//             span: None,
//             note_type: NoteType::Explanation,
//         }
//     }
// }

// #[derive(Debug)]
// pub enum SemanticErrorMsg {
//     // // Variable and binding errors
//     // IdentifierNotFound {
//     //     cache: SrcCache,
//     //     name: String,
//     //     span: Span,
//     //     similar_bindings: Vec<BindingInfo>, // Store actual bindings instead of strings
//     // },
//     // VariableRedefinition {
//     //     cache: SrcCache,
//     //     original_binding: BindingInfo,
//     //     new_binding: BindingInfo,
//     // },
//     // FunctionRedefinitionAsVariable {
//     //     cache: SrcCache,
//     //     function_binding: BindingInfo,
//     //     variable_binding: BindingInfo,
//     // },

//     // // Function call errors
//     // FunctionCallArgCountMismatch {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     function: BindingInfo,
//     //     function_type: FunctionType,
//     //     provided_args: Vec<TypeInfo>,
//     // },
//     // FunctionArgTypeMismatch {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     function: BindingInfo,
//     //     param_index: usize,
//     //     expected_type: TypeInfo,
//     //     provided_type: TypeInfo,
//     // },
//     // NoMatchingFunctionOverload {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     name: String,
//     //     candidates: Vec<BindingInfo>, // All available overloads
//     //     provided_args: Vec<TypeInfo>, // Types that were provided
//     // },
//     // CallingNonFunction {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     expr_binding: BindingInfo,
//     //     expr_type: TypeInfo,
//     // },

//     // // Type errors
//     // InvalidTypeForOperation {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     operation: String, // Could be an enum if operations are finite
//     //     operand_types: Vec<TypeInfo>,
//     // },
//     // TypeMismatch {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     context: TypeMismatchContext,
//     //     expected_type: TypeInfo,
//     //     provided_type: TypeInfo,
//     // },
//     // UndefinedType {
//     //     cache: SrcCache,
//     //     name: String,
//     //     span: Span,
//     // },

//     // // Return errors
//     // InvalidReturnType {
//     //     cache: SrcCache,
//     //     span: Span,
//     //     expected: String,
//     //     got: String,
//     // },
//     // ReturnOutsideFunction {
//     //     cache: SrcCache,
//     //     span: Span,
//     // },

//     // Generic error (for migration and unexpected cases)
//     Generic(GenericSemanticErrorMsg),
// }

// // Helper enum to provide context for type mismatches
// pub enum TypeMismatchContext {
//     Assignment,
//     Return { function: BindingInfo },
//     Condition,
//     ArrayIndex,
//     // etc.
// }

// pub struct GenericSemanticErrorMsg {
//     pub cache: SrcCache,
//     pub message: String,
//     pub span: Span,
//     pub notes: Vec<Note>,
//     pub severity: Severity,
// }

// impl GenericSemanticErrorMsg {
//     pub fn new(
//         cache: SrcCache,
//         message: String,
//         span: Span,
//         notes: Vec<Note>,
//         severity: Severity,
//     ) -> Self {
//         Self {
//             cache,
//             message,
//             span,
//             notes,
//             severity,
//         }
//     }
// }

// impl SemanticErrorMsg {
//     pub fn eprint(&self) {
//         match self {
//             SemanticErrorMsg::Generic(msg) => {
//                 let severity = match msg.severity {
//                     Severity::Error => ReportKind::Error,
//                     Severity::Warning => ReportKind::Warning,
//                     Severity::Note => ReportKind::Advice,
//                 };
//                 let filename = msg.cache.borrow();
//                 let mut report = Report::build(severity, (filename.0.clone(), msg.span.clone()))
//                     .with_message(msg.message.clone());

//                 for note in &msg.notes {
//                     let color = match note.note_type {
//                         NoteType::Explanation => Color::Blue,
//                         NoteType::Fix => Color::Green,
//                         NoteType::ErrorExplanation => Color::Red,
//                         NoteType::Help => {
//                             report = report.with_help(note.message.clone());
//                             continue;
//                         }
//                     };
//                     if let Some(span) = &note.span {
//                         report.add_label(
//                             Label::new((filename.0.clone(), span.clone()))
//                                 .with_message(&note.message)
//                                 .with_color(color),
//                         );
//                     } else {
//                         report = report.with_note(note.message.clone().fg(color));
//                     }
//                 }
//                 drop(filename);
//                 let mut cache = msg.cache.borrow_mut();
//                 report
//                     .finish()
//                     .eprint(cache.deref_mut())
//                     .expect("Failed to print");
//             }
//             SemanticErrorMsg::IdentifierNotFound {
//                 cache,
//                 name,
//                 span,
//                 similar_bindings,
//             } => {
//                 let mut report =
//                     Report::build(ReportKind::Error, (cache.borrow().0.clone(), span.clone()))
//                         .with_message(format!("Cannot find identifier `{}`", name));

//                 if !similar_bindings.is_empty() {
//                     report = report.with_help(format!(
//                         "Did you mean: {}",
//                         similar_bindings
//                             .iter()
//                             .map(|b| b.name.clone())
//                             .collect::<Vec<_>>()
//                             .join(", ")
//                     ));
//                 }

//                 let mut cache = cache.borrow_mut();
//                 report
//                     .finish()
//                     .eprint(cache.deref_mut())
//                     .expect("Failed to print");
//             }
//             SemanticErrorMsg::NoMatchingFunctionOverload {
//                 cache,
//                 span,
//                 name,
//                 candidates,
//                 provided_args,
//             } => {
//                 let mut report =
//                     Report::build(ReportKind::Error, (cache.borrow().0.clone(), span.clone()))
//                         .with_message(format!("No matching overload for function `{}`", name));

//                 // Now we can format the candidates in different ways for different purposes
//                 let mut notes = Vec::new();
//                 for candidate in candidates {
//                     if let Some(loc) = &candidate.src_location {
//                         notes.push(Note {
//                             message: format!("Candidate: {}", candidate),
//                             span: Some(loc.clone()),
//                             note_type: NoteType::Explanation,
//                         });
//                     }
//                 }

//                 // Add provided argument types
//                 report = report.with_note(format!(
//                     "Provided argument types: ({})",
//                     provided_args
//                         .iter()
//                         .map(|t| format!("{}", t))
//                         .collect::<Vec<_>>()
//                         .join(", ")
//                 ));

//                 // ...rest of error reporting...
//             }
//             // Implement similar error reporting for other variants...
//             _ => todo!("Implement error reporting for other error types"),
//         }
//     }

//     // Helper methods for diagnostic tools/LSP
//     pub fn get_primary_span(&self) -> &Span {
//         match self {
//             SemanticErrorMsg::NoMatchingFunctionOverload { span, .. } => span,
//             // ... etc
//             _ => todo!(),
//         }
//     }

//     pub fn get_related_spans(&self) -> Vec<Span> {
//         match self {
//             SemanticErrorMsg::NoMatchingFunctionOverload { candidates, .. } => candidates
//                 .iter()
//                 .filter_map(|c| c.src_location.clone())
//                 .collect(),
//             // ... etc
//             _ => vec![],
//         }
//     }
// }
