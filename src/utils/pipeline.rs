use crate::pass_c_compilation::pass_c_compilation_impl::{CCompilation, CExecutable};
use crate::pass_c_lowering::pass_yir_to_c::{CLowering, CSourceCode};
use crate::pass_check_decl_def::{CheckDeclDef, CheckDeclDefErrors};
use crate::pass_control_flow_analysis::pass_control_flow_analysis_impl::{
    ControlFlowAnalysis, ControlFlowAnalysisErrors,
};
use crate::pass_diagnostics::pass_diagnostics_impl::Diagnostics;
use crate::pass_parse::pass_parse_impl::{Parse, SyntaxErrors};
use crate::pass_parse::{AST, SourceInfo};
use crate::pass_parse::add_ids::IdGenerator;
use crate::pass_print_yir::pass_print_yir_impl::{
    YirTextualRepresentation, YirToColoredString, YirToString,
};
use crate::pass_type_dependency_analysis::{
    TypeDependencyAnalysis, TypeDependencyAnalysisErrors, TypeDependencyGraph,
};
use crate::pass_type_inference::TypeInferenceErrors;
use crate::pass_type_inference::pass_type_inference_impl::TypeInference;
use crate::pass_yir_lowering::Module;
use crate::pass_yir_lowering::pass_ast_to_yir_impl::YirLowering;
use crate::utils::{BlockTree, TypeRegistry};
use miette::{IntoDiagnostic, Result};
use std::time::{Duration, Instant};

#[derive(Debug, Clone)]
pub struct PassTiming {
    pub pass_name: String,
    pub duration: Duration,
}

#[derive(Debug, Default)]
pub struct PassTimings {
    pub timings: Vec<PassTiming>,
    pub total_duration: Duration,
    pub total_loc: usize,
}

impl PassTimings {
    pub fn new(total_loc: usize) -> Self {
        Self {
            timings: Vec::new(),
            total_duration: Duration::new(0, 0),
            total_loc,
        }
    }

    pub fn add_timing(&mut self, timing: PassTiming) {
        self.total_duration += timing.duration;
        self.timings.push(timing);
    }

    pub fn get_timing(&self, pass_name: &str) -> Option<&PassTiming> {
        self.timings.iter().find(|t| t.pass_name == pass_name)
    }

    pub fn print_summary(&self) {
        println!("=== Timing Breakdown ===");
        let total_secs = self.total_duration.as_secs_f64();
        println!("Total duration: {:.3}ms", total_secs * 1000.0);
        if self.total_loc > 0 {
            println!("Total LOC: {}", self.total_loc);
            println!(
                "Overall Speed: {:.2} LOC/s",
                self.total_loc as f64 / total_secs
            );
        }
        println!();

        println!(
            "{:<25} | {:<12} | {:<8} | {:<12}",
            "Pass", "Time", "%", "Speed"
        );
        println!("{:-<25}-|-{:-<12}-|-{:-<8}-|-{:-<12}", "", "", "", "");

        for timing in &self.timings {
            let duration_secs = timing.duration.as_secs_f64();
            let percent = if total_secs > 0.0 {
                (duration_secs / total_secs) * 100.0
            } else {
                0.0
            };

            let speed_str = if self.total_loc > 0 && duration_secs > 0.0 {
                format!("{:.2} LOC/s", self.total_loc as f64 / duration_secs)
            } else {
                "-".to_string()
            };

            println!(
                "{:<25} | {:8.3}ms | {:5.1}% | {:<12}",
                timing.pass_name,
                duration_secs * 1000.0,
                percent,
                speed_str
            );
        }
    }
}

pub struct Pipeline {
    pub ast: Option<AST>,
    source_info: Option<SourceInfo>,
    pub id_generator: Option<crate::pass_parse::add_ids::IdGenerator>,
    syntax_errors: Option<SyntaxErrors>,
    type_registry: Option<TypeRegistry>,
    binding_table: Option<crate::utils::BindingTable>,
    type_info_table: Option<crate::utils::type_info_table::TypeInfoTable>,
    root_block: Option<BlockTree>,
    type_errors: Option<TypeInferenceErrors>,

    sema_done: bool,
    control_flow_errors: Option<ControlFlowAnalysisErrors>,
    decl_def_errors: Option<CheckDeclDefErrors>,

    type_dependency_errors: Option<TypeDependencyAnalysisErrors>,
    type_dependency_order: Option<TypeDependencyGraph>,

    module: Option<Module>,
    c_code: Option<CSourceCode>,

    pub timing_enabled: bool,
    pub timings: PassTimings,
}

impl Default for Pipeline {
    fn default() -> Self {
        Self {
            ast: None,
            source_info: None,
            id_generator: None,
            syntax_errors: None,
            type_registry: None,
            binding_table: None,
            type_info_table: None,
            decl_def_errors: None,
            root_block: None,
            type_errors: None,
            sema_done: false,
            control_flow_errors: None,
            type_dependency_errors: None,
            type_dependency_order: None,
            module: None,
            c_code: None,
            timing_enabled: false,
            timings: PassTimings::new(0),
        }
    }
}

impl Pipeline {
    pub fn new(source: String, file_name: String) -> Self {
        let loc = source.lines().count();
        let source_info = SourceInfo {
            source: std::sync::Arc::from(source),
            file_name: std::sync::Arc::from(file_name),
        };

        Pipeline {
            ast: None,
            source_info: Some(source_info),
            id_generator: None,
            syntax_errors: None,
            type_registry: None,
            binding_table: None,
            type_info_table: None,
            decl_def_errors: None,
            root_block: None,
            type_errors: None,
            sema_done: false,
            control_flow_errors: None,
            type_dependency_errors: None,
            type_dependency_order: None,
            module: None,
            c_code: None,
            timing_enabled: false,
            timings: PassTimings::new(loc),
        }
    }

    pub fn with_timing(mut self) -> Self {
        self.timing_enabled = true;
        self
    }

    fn record_pass_timing(&mut self, pass_name: &str, duration: Duration) {
        if self.timing_enabled {
            self.timings.add_timing(PassTiming {
                pass_name: pass_name.to_string(),
                duration,
            });
        }
    }

    fn parse(&mut self) -> Result<()> {
        if self.ast.is_some() {
            return Ok(());
        }

        let source_info = self
            .source_info
            .as_ref()
            .ok_or_else(|| miette::miette!("No source provided"))?;

        let parse_pass = Parse::new();
        let start = Instant::now();
        let (ast, id_generator, syntax_errors) = parse_pass.run(source_info)?;
        let parse_duration = start.elapsed();

        self.ast = Some(ast);
        self.id_generator = Some(id_generator);
        self.syntax_errors = Some(syntax_errors);

        self.record_pass_timing("parse", parse_duration);
        Ok(())
    }

    fn type_inference(&mut self) -> Result<()> {
        if self.type_registry.is_some() && self.root_block.is_some() && self.type_errors.is_some() {
            return Ok(());
        }

        if self.ast.is_none() {
            self.parse()?;
        }

        let ast = self.ast.as_ref().unwrap();
        let source_info = self.source_info.as_ref().unwrap();

        let start = Instant::now();
        let expr_count = self.id_generator.as_ref().unwrap().expr_count();
        let (type_registry, root_block, binding_table, type_info_table, type_errors) =
            TypeInference::new().run(ast, expr_count, source_info.clone())?;
        let duration = start.elapsed();

        self.record_pass_timing("type_inference", duration);
        self.type_registry = Some(type_registry);
        self.binding_table = Some(binding_table);
        self.type_info_table = Some(type_info_table);
        self.root_block = Some(root_block);
        self.type_errors = Some(type_errors);
        Ok(())
    }

    fn semantic_analysis(&mut self) -> Result<()> {
        if self.sema_done {
            return Ok(());
        }

        if self.type_registry.is_none() {
            self.type_inference()?;
        }

        let ast = self.ast.as_ref().unwrap();
        let type_registry = self.type_registry.as_ref().unwrap();
        let source_info = self.source_info.as_ref().unwrap();

        let start = Instant::now();
        let (type_dependency_order, type_dependency_errors) = TypeDependencyAnalysis.run(
            self.type_registry.as_ref().unwrap(),
            self.source_info.as_ref().unwrap(),
        );

        self.control_flow_errors =
            Some(ControlFlowAnalysis.run(ast, type_registry, self.type_info_table.as_ref().unwrap(), source_info)?);

        self.decl_def_errors = Some(CheckDeclDef.run(ast, type_registry, source_info)?);
        let duration = start.elapsed();

        self.record_pass_timing("semantic_analysis", duration);
        self.type_dependency_errors = Some(type_dependency_errors);
        self.type_dependency_order = Some(type_dependency_order);

        self.sema_done = true;

        Ok(())
    }

    fn yir_lowering(&mut self) -> Result<()> {
        if self.module.is_some() {
            return Ok(());
        }

        if self.control_flow_errors.is_none() {
            self.diagnostics()?;
        }

        let ast = self.ast.as_ref().unwrap();
        let type_registry = self.type_registry.as_ref().unwrap();

        let start = Instant::now();
        let module = YirLowering::new().run(
            ast,
            type_registry,
            self.type_info_table.as_ref().unwrap(),
            self.binding_table.as_ref().unwrap(),
            false
        )?;
        let duration = start.elapsed();

        self.record_pass_timing("yir_lowering", duration);
        self.module = Some(module);

        Ok(())
    }

    fn c_lowering(&mut self) -> Result<()> {
        if self.c_code.is_some() {
            return Ok(());
        }

        if self.module.is_none() {
            self.yir_lowering()?;
        }

        let module = self.module.as_ref().unwrap();
        let type_registry = self.type_registry.as_ref().unwrap();
        let type_dependency_order = self.type_dependency_order.as_ref().unwrap();

        let start = Instant::now();
        let c_code = CLowering::new().run(module, type_registry, type_dependency_order)?;
        let duration = start.elapsed();

        self.record_pass_timing("c_lowering", duration);
        self.c_code = Some(c_code);

        Ok(())
    }

    fn diagnostics(&mut self) -> Result<()> {
        if self.control_flow_errors.is_none() {
            self.semantic_analysis()?;
        }

        let syntax_errors = self.syntax_errors.as_ref().unwrap();
        let type_inference_errors = self.type_errors.as_ref().unwrap();

        let cf_errors = self.control_flow_errors.as_mut().unwrap();
        let type_dependency_errors = self.type_dependency_errors.as_ref().unwrap();
        let decl_def_errors = self.decl_def_errors.as_ref().unwrap();

        let sema_errors = cf_errors
            .0
            .iter()
            .chain(type_dependency_errors.0.iter())
            .chain(decl_def_errors.0.iter())
            .cloned()
            .collect::<Vec<_>>();

        Diagnostics.run(&syntax_errors.0, &type_inference_errors.0, &sema_errors)?;
        Ok(())
    }

    pub fn calc_diagnostics(&mut self) -> Result<()> {
        self.diagnostics()?;
        Ok(())
    }

    pub fn calc_yir(&mut self) -> Result<YirTextualRepresentation> {
        if self.module.is_none() {
            self.yir_lowering()?;
        }

        let module = self.module.as_ref().unwrap();
        YirToString::new().run(module)
    }

    pub fn calc_yir_colored(&mut self) -> Result<YirTextualRepresentation> {
        if self.module.is_none() {
            self.yir_lowering()?;
        }

        let module = self.module.as_ref().unwrap();
        YirToColoredString::new().run(module)
    }

    pub fn calc_ast(&mut self) -> Result<&AST> {
        if self.ast.is_none() {
            self.parse()?;
        }
        Ok(self.ast.as_ref().unwrap())
    }

    pub fn get_ast(&self) -> &AST {
        self.ast.as_ref().unwrap()
    }

    pub fn calc_c(&mut self) -> Result<&CSourceCode> {
        if self.c_code.is_none() {
            self.c_lowering()?;
        }

        Ok(self.c_code.as_ref().unwrap())
    }

    pub fn get_module(&mut self) -> Result<&Module> {
        if self.module.is_none() {
            self.yir_lowering()?;
        }

        self.module
            .as_ref()
            .ok_or_else(|| miette::miette!("Module not available"))
    }

    pub fn get_module_mut(&mut self) -> Result<&mut Module> {
        self.yir_lowering()?;

        self.module
            .as_mut()
            .ok_or_else(|| miette::miette!("Module not available"))
    }

    pub fn calc_executable(&mut self) -> Result<CExecutable> {
        if self.c_code.is_none() {
            self.c_lowering()?;
        }

        let c_code = self.c_code.as_ref().unwrap();
        let base = std::env::current_dir().into_diagnostic()?;
        let filename = self
            .source_info
            .as_ref()
            .map(|s| s.file_name.as_ref().to_string())
            .unwrap_or_else(|| "temp".to_string());

        let start = Instant::now();
        let out = CCompilation::new().compile_c_code(c_code, base, &filename)?;
        let duration = start.elapsed();

        self.record_pass_timing("c_compilation", duration);
        Ok(out)
    }

    pub fn get_source_info(&self) -> Option<&SourceInfo> {
        self.source_info.as_ref()
    }

    pub fn get_id_generator_mut(&mut self) -> Option<&mut IdGenerator> {
        self.id_generator.as_mut()
    }
}
