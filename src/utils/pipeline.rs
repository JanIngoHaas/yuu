use miette::Result;
use crate::pass_c_lowering::pass_yir_to_c::{CSourceCode, CLowering};
use crate::pass_c_compilation::pass_c_compilation_impl::{CCompilation, CExecutable};
use crate::pass_parse::{SourceInfo, AST};
use crate::pass_parse::pass_parse_impl::{Parse, SyntaxErrors};
use crate::pass_print_yir::pass_print_yir_impl::{YirTextualRepresentation, YirToString, YirToColoredString};
use crate::pass_type_inference::{TypeInferenceErrors, TypeRegistry};
use crate::pass_type_inference::pass_type_inference_impl::TypeInference;
use crate::pass_yir_lowering::{Module, RootBlock};
use crate::pass_yir_lowering::pass_ast_to_yir_impl::YirLowering;
use crate::pass_diagnostics::pass_diagnostics_impl::Diagnostics;

#[derive(Default)]
pub struct Pipeline {
    ast: Option<AST>,
    source_info: Option<SourceInfo>,
    syntax_errors: Option<SyntaxErrors>,
    type_registry: Option<TypeRegistry>,
    root_block: Option<Box<RootBlock>>,
    type_errors: Option<TypeInferenceErrors>,
    module: Option<Module>,
    c_code: Option<CSourceCode>,
}

impl Pipeline {
    pub fn parse(source: String, file_name: String) -> Result<Self> {
        let source_info = SourceInfo {
            source: std::sync::Arc::from(source),
            file_name: std::sync::Arc::from(file_name),
        };
        
        let parse_pass = Parse::new();
        let (ast, syntax_errors) = parse_pass.run(&source_info)?;
        
        Ok(Pipeline {
            ast: Some(ast),
            source_info: Some(source_info),
            syntax_errors: Some(syntax_errors),
            type_registry: None,
            root_block: None,
            type_errors: None,
            module: None,
            c_code: None,
        })
    }
    
    pub fn type_inference(mut self) -> Result<Self> {
        // If already computed, return as-is
        if self.type_registry.is_some() && self.root_block.is_some() && self.type_errors.is_some() {
            return Ok(self);
        }
        
        let ast = self.ast.as_ref().ok_or_else(|| miette::miette!("AST not available"))?;
        let source_info = self.source_info.as_ref().ok_or_else(|| miette::miette!("SourceInfo not available"))?;
        
        let (type_registry, root_block, type_errors) = TypeInference::new().run(ast, source_info.clone())?;
        
        self.type_registry = Some(type_registry);
        self.root_block = Some(root_block);
        self.type_errors = Some(type_errors);

        // Run diagnostics
        self.diagnostics()
    }
    
    pub fn yir_lowering(mut self) -> Result<Self> {
        // If already computed, return as-is
        if self.module.is_some() {
            return Ok(self);
        }
        
        // Auto-compute type inference if not available
        if self.type_registry.is_none() {
            self = self.type_inference()?;
        }
        
        let ast = self.ast.as_ref().ok_or_else(|| miette::miette!("AST not available"))?;
        let type_registry = self.type_registry.as_ref().ok_or_else(|| miette::miette!("TypeRegistry not available"))?;
        
        let module = YirLowering::new().run(ast, type_registry)?;
        self.module = Some(module);
        
        Ok(self)
    }
    
    pub fn c_lowering(mut self) -> Result<Self> {
        // If already computed, return as-is
        if self.c_code.is_some() {
            return Ok(self);
        }
        
        // Auto-compute YIR if not available
        if self.module.is_none() {
            self = self.yir_lowering()?;
        }
        
        let module = self.module.as_ref().ok_or_else(|| miette::miette!("Module not available"))?;
        let type_registry = self.type_registry.as_ref().ok_or_else(|| miette::miette!("TypeRegistry not available"))?;
        
        let c_code = CLowering::new().run(module, type_registry)?;
        self.c_code = Some(c_code);
        
        Ok(self)
    }
    
    pub fn diagnostics(mut self) -> Result<Self> {
        // Auto-compute type inference if type errors not available
        if self.type_errors.is_none() {
            self = self.type_inference()?;
        }
        
        let syntax_errors = self.syntax_errors.as_ref().ok_or_else(|| miette::miette!("SyntaxErrors not available"))?;
        let type_errors = self.type_errors.as_ref().ok_or_else(|| miette::miette!("TypeInferenceErrors not available"))?;
        
        Diagnostics::new().run(syntax_errors, type_errors)?;
        Ok(self)
    }
    
    pub fn print_yir(mut self) -> Result<YirTextualRepresentation> {
        // Auto-compute YIR if not available
        if self.module.is_none() {
            self = self.yir_lowering()?;
        }
        
        let module = self.module.as_ref().ok_or_else(|| miette::miette!("Module not available"))?;
        YirToString::new().run(module)
    }
    
    pub fn print_yir_colored(mut self) -> Result<YirTextualRepresentation> {
        // Auto-compute YIR if not available
        if self.module.is_none() {
            self = self.yir_lowering()?;
        }
        
        let module = self.module.as_ref().ok_or_else(|| miette::miette!("Module not available"))?;
        YirToColoredString::new().run(module)
    }
    
    pub fn get_c_code(&mut self) -> Result<&CSourceCode> {
        // Auto-compute C code if not available
        if self.c_code.is_none() {
            *self = std::mem::take(self).c_lowering()?;
        }
        
        self.c_code.as_ref().ok_or_else(|| miette::miette!("C code not available"))
    }
    
    pub fn get_module(&mut self) -> Result<&Module> {
        // Auto-compute module if not available
        if self.module.is_none() {
            *self = std::mem::take(self).yir_lowering()?;
        }
        
        self.module.as_ref().ok_or_else(|| miette::miette!("Module not available"))
    }
    
    pub fn compile_executable(&mut self) -> Result<CExecutable> {
        // Auto-compute C code if not available
        if self.c_code.is_none() {
            *self = std::mem::take(self).c_lowering()?;
        }
        
        let c_code = self.c_code.as_ref().ok_or_else(|| miette::miette!("C code not available"))?;
        CCompilation::new().compile_c_code(c_code)
    }
}

