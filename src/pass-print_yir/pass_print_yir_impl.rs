use miette::IntoDiagnostic;
use crate::{
    pass_yir_lowering::yir::Module,
};

pub struct YirToString;

impl Default for YirToString {
    fn default() -> Self {
        Self::new()
    }
}

impl YirToString {
    pub fn new() -> Self {
        Self
    }
}

pub struct YirToColoredString;

impl Default for YirToColoredString {
    fn default() -> Self {
        Self
    }
}

impl YirToColoredString {
    pub fn new() -> Self {
        Self
    }
}

impl YirToColoredString {
    pub fn run(&self, module: &Module) -> miette::Result<YirTextualRepresentation> {
        let mut f = String::new();
        module.format_yir(true, &mut f).into_diagnostic()?;
        Ok(YirTextualRepresentation(f))
    }
}

pub struct YirTextualRepresentation(pub String);

impl YirToString {
    pub fn run(&self, module: &Module) -> miette::Result<YirTextualRepresentation> {
        let ir_string = format!("{}", module);
        Ok(YirTextualRepresentation(ir_string))
    }
}
