use std::sync::Arc;

use logos::{Logos, Span};
use thiserror::Error;
use yuu_shared::{
    scheduler::ResourceId,
    token::{Token, TokenKind},
};

pub struct UnprocessedCodeInfo {
    pub code: Arc<str>,
    pub file_name: Arc<str>,
}

impl ResourceId for UnprocessedCodeInfo {
    fn resource_name() -> &'static str {
        "UnprocessedCodeInfo"
    }
}

#[derive(Debug, Clone)]
pub struct Note {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GenericError {
    pub expected: String,
    pub found: String,
    pub note: Vec<Note>,
    pub span: Span,
}

// Parser Error with thiserror
#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("")]
    UnexpectedToken(Span),
    #[error("")]
    UnexpectedEOF,
    #[error("")]
    GenericSyntaxError(GenericError),
}

impl ParseError {
    // pub fn print(&self, code_info: &UnprocessedCodeInfo) {
    //     match self {
    //         ParseError::UnexpectedToken(span) => {
    //             Report::build(
    //                 ReportKind::Error,
    //                 (code_info.file_name.clone(), span.clone()),
    //             )
    //             .with_label(
    //                 Label::new((code_info.file_name.clone(), span.clone()))
    //                     .with_message("Unexpected token")
    //                     .with_color(Color::Red),
    //             )
    //             .finish()
    //             .print((
    //                 code_info.file_name.clone(),
    //                 Source::from(code_info.code.clone()),
    //             ))
    //             .unwrap();
    //         }
    //         ParseError::UnexpectedEOF => {
    //             Report::build(
    //                 ReportKind::Error,
    //                 (
    //                     code_info.file_name.clone(),
    //                     code_info.code.len()..code_info.code.len(),
    //                 ),
    //             )
    //             .with_message("Unexpected end of file")
    //             .finish()
    //             .print((
    //                 code_info.file_name.clone(),
    //                 Source::from(code_info.code.clone()),
    //             ))
    //             .unwrap();
    //         }
    //         ParseError::GenericSyntaxError(error) => {
    //             let mut report = Report::build(
    //                 ReportKind::Error,
    //                 (code_info.file_name.clone(), error.span.clone()),
    //             )
    //             .with_message("Syntax error")
    //             .with_label(
    //                 Label::new((code_info.file_name.clone(), error.span.clone()))
    //                     .with_message(format!(
    //                         "Expected {}, but found '{}'",
    //                         error.expected, error.found
    //                     ))
    //                     .with_color(Color::Red),
    //             );

    //             // Add all notes with their respective spans
    //             for note in &error.note {
    //                 report = report.with_label(
    //                     Label::new((code_info.file_name.clone(), note.span.clone()))
    //                         .with_message(&note.message)
    //                         .with_color(Color::Blue),
    //                 );
    //             }

    //             report
    //                 .finish()
    //                 .print((
    //                     code_info.file_name.clone(),
    //                     Source::from(code_info.code.clone()),
    //                 ))
    //                 .unwrap();
    //         }
    //     }
    // }
}

pub struct Lexer<'a> {
    lexer: logos::Lexer<'a, TokenKind>,
    lookahead: Vec<Result<Token, ParseError>>,
    buffer_size: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(code_info: &'a UnprocessedCodeInfo) -> Self {
        let mut lexer = TokenKind::lexer(code_info.code.as_ref());
        let mut lookahead = Vec::new();

        // Initialize with one token by default
        let t = lexer.next();
        let span = lexer.span();
        let first_token = match t {
            Some(t) => match t {
                Ok(tkn) => Ok(Token { kind: tkn, span }),
                Err(()) => Err(ParseError::UnexpectedToken(span)),
            },
            None => Err(ParseError::UnexpectedEOF),
        };
        lookahead.push(first_token);

        Self {
            lexer,
            lookahead,
            buffer_size: 1,
        }
    }

    pub fn set_lookahead(&mut self, size: usize) -> Result<(), ParseError> {
        if size < 1 {
            self.buffer_size = 1;
            self.lookahead.truncate(1);
            return Ok(());
        }

        self.buffer_size = size;
        while self.lookahead.len() < size {
            let next_token = self.next_token_internal();
            self.lookahead.push(next_token);
        }
        Ok(())
    }

    fn next_token_internal(&mut self) -> Result<Token, ParseError> {
        let t = self.lexer.next();
        let span = self.lexer.span();
        match t {
            Some(t) => match t {
                Ok(tkn) => Ok(Token { kind: tkn, span }),
                Err(()) => Err(ParseError::UnexpectedToken(self.lexer.span())),
            },
            None => Ok(Token {
                kind: TokenKind::EOF,
                span,
            }),
        }
    }

    pub fn peek(&mut self) -> Result<&Token, ParseError> {
        self.peek_at(0)
    }

    pub fn peek_at(&mut self, n: usize) -> Result<&Token, ParseError> {
        if n >= self.buffer_size {
            return Err(ParseError::UnexpectedEOF);
        }

        while self.lookahead.len() <= n {
            let next_token = self.next_token_internal();
            self.lookahead.push(next_token);
        }

        self.lookahead[n].as_ref().map_err(|x| x.clone())
    }

    pub fn peek_n<const N: usize>(&mut self) -> Result<&[Result<Token, ParseError>], ParseError> {
        self.set_lookahead(N)?;

        let slice = self.lookahead.as_slice();
        Ok(&slice[..N])
    }

    pub fn expect(&mut self, expected: &[TokenKind], note: Vec<Note>) -> Result<Token, ParseError> {
        let t = self.next_token()?;
        if expected.contains(&t.kind) {
            Ok(t)
        } else {
            Err(ParseError::GenericSyntaxError(GenericError {
                expected: format!("{:?}", expected),
                found: format!("{:?}", t.kind),
                note,
                span: t.span,
            }))
        }
    }

    pub fn next_token(&mut self) -> Result<Token, ParseError> {
        let result = self.lookahead.remove(0);
        let next_token = self.next_token_internal();
        self.lookahead.push(next_token);
        result
    }
}
