use crate::pass_diagnostics::error::YuuError;
use crate::pass_lexing::Token;
use crate::pass_parse::{
    ast::{AST, SourceInfo},
    parser::Parser,
};

pub struct SyntaxErrors(pub Vec<YuuError>);

pub struct Parse;

impl Parse {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, tokens: Vec<Token>, source_info: SourceInfo) -> miette::Result<(AST, crate::pass_parse::add_ids::IdGenerator, SyntaxErrors)> {
        let mut parser = Parser::new(tokens, source_info);
        let (ast, id_generator) = parser.parse_and_add_ids();
        let (syntax_errors, _) = parser.dismantle();
        Ok((ast, id_generator, SyntaxErrors(syntax_errors)))
    }
}

impl Default for Parse {
    fn default() -> Self {
        Self::new()
    }
}
