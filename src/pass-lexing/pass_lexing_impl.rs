use crate::pass_diagnostics::error::YuuError;
use crate::pass_lexing::{Token, TokenKind};
use crate::pass_parse::ast::SourceInfo;
use logos::Logos;

pub struct LexingErrors(pub Vec<YuuError>);

pub struct LexingPass;

impl LexingPass {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, source_info: &SourceInfo) -> miette::Result<(Vec<Token>, LexingErrors)> {
        let mut lexer = TokenKind::lexer(source_info.source.as_ref());
        let mut tokens = Vec::new();
        let errors = Vec::new();

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
        let end_pos = source_info.source.len();
        tokens.push(Token {
            kind: TokenKind::EOF,
            span: end_pos..end_pos,
        });

        Ok((tokens, LexingErrors(errors)))
    }
}

impl Default for LexingPass {
    fn default() -> Self {
        Self::new()
    }
}
