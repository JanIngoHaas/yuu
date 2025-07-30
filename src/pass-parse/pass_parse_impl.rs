use crate::pass_diagnostics::error::YuuError;
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

    pub fn run(&self, source_info: &SourceInfo) -> miette::Result<(AST, SyntaxErrors)> {
        let mut parser = Parser::new(source_info);
        let ast = parser.parse_and_add_ids();
        let (syntax_errors, _) = parser.dismantle();
        Ok((ast, SyntaxErrors(syntax_errors)))
    }
}

impl Default for Parse {
    fn default() -> Self {
        Self::new()
    }
}
