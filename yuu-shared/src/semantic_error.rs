use std::{error::Error, fmt};

#[derive(Debug)]
pub struct SemanticError;

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "semantic error")
    }
}

impl Error for SemanticError {}
