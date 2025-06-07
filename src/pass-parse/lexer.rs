use crate::{
    pass_diagnostics::{ErrorKind, YuuError},
    pass_parse::{
        ast::SourceInfo,
        token::{Token, TokenKind},
    },
};
use logos::{Logos, Span};

pub type ParseError = YuuError;

// Enum for different synchronization points
#[derive(Debug, Clone, Copy)]
pub enum CatchIn {
    Statement,
    FunctionDecl,
}

// Result type for parser functions
pub type ParseResult<T> = std::result::Result<T, CatchIn>;

pub struct Lexer {
    tokens: Vec<Token>,
    current: usize,
    pub code_info: SourceInfo,
}

impl Lexer {
    pub fn new(code_info: &SourceInfo) -> Self {
        let mut lexer = TokenKind::lexer(code_info.source.as_ref());
        let mut tokens = Vec::new();

        // Pre-lex the entire file at once
        while let Some(token_result) = lexer.next() {
            let span = lexer.span();
            match token_result {
                Ok(kind) => tokens.push(Token { kind, span }),
                Err(_) => {
                    // TODO: Improve error handling
                    panic!("Unknown Token lexed");
                }
            }
        }

        // Add an explicit EOF token at the end
        let end_pos = code_info.source.len();
        tokens.push(Token {
            kind: TokenKind::EOF,
            span: end_pos..end_pos,
        });

        Self {
            tokens,
            current: 0,
            code_info: code_info.clone(),
        }
    }

    pub fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            // This should never happen since we always add an EOF token at the end,
            // but just in case, return a reference to the last token (which should be EOF)
            &self.tokens[self.tokens.len() - 1]
        })
    }

    pub fn peek_at(&self, offset: usize) -> &Token {
        self.tokens.get(self.current + offset).unwrap_or_else(|| {
            // Return EOF token if we're past the end
            &self.tokens[self.tokens.len() - 1]
        })
    }

    // get n tokens ahead
    pub fn peek_n<const N: usize>(&self) -> [&Token; N] {
        let mut result = [self.peek(); N];

        for (i, item) in result.iter_mut().enumerate().take(N) {
            if let Some(token) = self.tokens.get(self.current + i) {
                *item = token;
            } else {
                // Past the end, use EOF token
                *item = &self.tokens[self.tokens.len() - 1];
            }
        }

        result
    }

    // advance
    pub fn next_token(&mut self) -> Token {
        if self.current < self.tokens.len() {
            let token = self.tokens[self.current].clone();
            self.current += 1;
            token
        } else {
            // Return EOF if we've reached the end
            let end_pos = self.code_info.source.len();
            Token {
                kind: TokenKind::EOF,
                span: end_pos..end_pos,
            }
        }
    }

    // nom
    pub fn eat(&mut self) {
        if self.current < self.tokens.len() {
            self.current += 1;
        }
    }

    pub fn expect_semicolon(&mut self, errors: &mut Vec<ParseError>) -> ParseResult<Token> {
        let next = self.next_token();
        if next.kind == TokenKind::Semicolon {
            Ok(next)
        } else {
            // Error case: Expected semicolon
            let span = next.span.clone();
            let expected_str = format!("{:?}", TokenKind::Semicolon);
            let found_str = format!("{:?}", next.kind);

            let mut error = YuuError::unexpected_token(
                span,
                expected_str,
                found_str,
                self.code_info.source.clone(),
                self.code_info.file_name.clone(),
            );

            error = error.with_help("Expected a ';' after the statement");

            errors.push(error);
            Err(self.synchronize())
        }
    }

    pub fn expect_tag(
        &mut self,
        expected: TokenKind,
        errors: &mut Vec<ParseError>,
    ) -> ParseResult<Token> {
        let next = self.next_token();
        if std::mem::discriminant(&expected) == std::mem::discriminant(&next.kind) {
            Ok(next)
        } else {
            // Error case: Tags don't match
            let span = next.span.clone();
            let expected_str = format!("{}", expected);
            let found_str = format!("{}", next.kind);

            let mut error = YuuError::unexpected_token(
                span,
                expected_str,
                found_str,
                self.code_info.source.clone(),
                self.code_info.file_name.clone(),
            );

            error = error.with_help(format!("Expected token of type: {}", expected));

            errors.push(error);
            Err(self.synchronize())
        }
    }

    // Check if the current token matches the expected kind, and consume it if it does
    pub fn expect(
        &mut self,
        expected: &[TokenKind],
        errors: &mut Vec<ParseError>,
    ) -> ParseResult<Token> {
        if let Some(token) = self.peek_maybe() {
            if expected.contains(&token.kind) {
                return Ok(self.next_token());
            }

            // Not found... )-:

            let found = format!("{}", token.kind);
            let expected_str = expected
                .iter()
                .map(|k| format!("{}", k))
                .collect::<Vec<_>>()
                .join(" or ");

            let span = token.span.clone();

            // Create a diagnostic with our new error system
            let mut error = YuuError::unexpected_token(
                span,
                expected_str,
                found,
                self.code_info.source.clone(),
                self.code_info.file_name.clone(),
            );

            // Add a help message if there are expected tokens
            if !expected.is_empty() {
                let expected = expected
                    .iter()
                    .map(|k| format!("{}", k))
                    .collect::<Vec<_>>()
                    .join(", ");

                error = error.with_help(format!(
                    "Expected one of the following tokens: {}",
                    expected
                ));
            }

            // Add related info for any notes

            errors.push(error);
            Err(self.synchronize())
        } else {
            // End of file
            let span = if let Some(last) = self.tokens.last() {
                last.span.clone()
            } else {
                0..0
            };

            let expected_str = expected
                .iter()
                .map(|k| format!("{:?}", k))
                .collect::<Vec<_>>()
                .join(" or ");

            // Create an EOF error with our new error system
            let error = YuuError::builder()
                .kind(ErrorKind::UnexpectedEOF)
                .message(format!("Unexpected end of file, expected {}", expected_str))
                .source(
                    self.code_info.source.clone(),
                    self.code_info.file_name.clone(),
                )
                .span(span, "unexpected end of file")
                .help("Unexpected end of file while parsing")
                .build();

            errors.push(error);
            Err(self.synchronize())
        }
    }

    // Get source code snippet for a span
    pub fn get_source_snippet(&self, span: &Span) -> &str {
        &self.code_info.source[span.start..span.end]
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.tokens[self.current].kind == TokenKind::EOF
    }

    pub fn peek_maybe(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    // Synchronize after an error for error recovery
    pub fn synchronize(&mut self) -> CatchIn {
        while let Some(token) = self.peek_maybe() {
            match token.kind {
                TokenKind::Semicolon => return CatchIn::Statement,
                TokenKind::FnKw => return CatchIn::FunctionDecl,
                _ => {
                    self.eat();
                }
            }
        }
        CatchIn::FunctionDecl // That's basically nonsense, but yeah, critical error btw.
    }

    pub fn sync_to(&mut self, tokens: &[TokenKind]) {
        while let Some(t) = self.peek_maybe() {
            if tokens.contains(&t.kind) {
                return;
            }
            self.eat();
        }
    }
}
