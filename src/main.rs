use clap::{Parser, Subcommand};
use miette::Result;
use std::fs;
use std::path::PathBuf;
use yuu::pass_diagnostics::error;
use yuu::utils::pipeline::Pipeline;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Parser)]
#[command(name = "yuu")]
#[command(about = "A multi-pass compiler for the Yuu programming language")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Compile source to YIR")]
    Yir {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(short, long, help = "Output file (default: stdout)")]
        output: Option<PathBuf>,
        #[arg(long, help = "Disable colored output")]
        no_color: bool,
        #[arg(long, help = "Show compiler pass timing breakdown")]
        compiler_timings: bool,
    },
    #[command(about = "Check source for errors")]
    Check {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(long, help = "Show compiler pass timing breakdown")]
        compiler_timings: bool,
    },
    #[command(about = "Compile source to LLVM IR")]
    Llir {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(short, long, help = "Output file (default: stdout)")]
        output: Option<PathBuf>,
        #[arg(long, help = "Show compiler pass timing breakdown")]
        compiler_timings: bool,
    },
    #[command(about = "JIT compile and execute source")]
    Jit {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(long, help = "Show compiler pass timing breakdown")]
        compiler_timings: bool,
    },
    #[command(about = "AOT compile source to native executable")]
    Aot {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(short, long, help = "Output file; default: <input>.exe")]
        output: Option<PathBuf>,
        #[arg(long, help = "Show compiler pass timing breakdown")]
        compiler_timings: bool,
    },
}

fn read_source_file(input: &PathBuf) -> Result<(String, String)> {
    let source = fs::read_to_string(input)
        .map_err(|e| miette::miette!("Failed to read input file: {}", e))?;
    let filename = input.to_string_lossy().to_string();
    Ok((source, filename))
}

fn write_output(output: Option<PathBuf>, content: &str) -> Result<()> {
    match output {
        Some(output_file) => {
            fs::write(output_file, content)
                .map_err(|e| miette::miette!("Failed to write output file: {}", e))?;
        }
        None => {
            println!("{}", content);
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    // Setup error formatter with syntax highlighting
    error::setup_error_formatter(Some("WarmEmber"), true)?;

    let cli = Cli::parse();

    match cli.command {
        Commands::Yir {
            input,
            output,
            no_color,
            compiler_timings,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename);

            if compiler_timings {
                pipeline = pipeline.with_timing();
            }

            let yir_output = if no_color {
                pipeline
                    .calc_yir()
                    .map_err(|e| miette::miette!("YIR generation error: {}", e))?
            } else {
                pipeline
                    .calc_yir_colored()
                    .map_err(|e| miette::miette!("YIR generation error: {}", e))?
            };
            write_output(output, &yir_output.0)?;

            if compiler_timings {
                pipeline.timings.print_summary();
            }
        }
        Commands::Check {
            input,
            compiler_timings,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename);

            if compiler_timings {
                pipeline = pipeline.with_timing();
            }

            pipeline
                .calc_diagnostics()
                .map_err(|e| miette::miette!("Diagnostics error: {}", e))?;

            if compiler_timings {
                pipeline.timings.print_summary();
            }
        }
        Commands::Llir {
            input,
            output,
            compiler_timings,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename);

            if compiler_timings {
                pipeline = pipeline.with_timing();
            }

            let llvm_ir = pipeline
                .calc_llvm_ir()
                .map_err(|e| miette::miette!("LLVM IR generation error: {}", e))?;
            write_output(output, &llvm_ir)?;

            if compiler_timings {
                pipeline.timings.print_summary();
            }
        }
        Commands::Jit {
            input,
            compiler_timings,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename);

            if compiler_timings {
                pipeline = pipeline.with_timing();
            }

            let exit_code = pipeline
                .jit_execute()
                .map_err(|e| miette::miette!("JIT execution error: {}", e))?;

            println!("Program exited with code: {}", exit_code);

            if compiler_timings {
                pipeline.timings.print_summary();
            }
        }
        Commands::Aot {
            input,
            output,
            compiler_timings,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename);

            if compiler_timings {
                pipeline = pipeline.with_timing();
            }

            // Create output path
            let output_path = if let Some(output_file) = output {
                output_file
            } else {
                let mut dest = input.clone();
                dest.set_extension("o"); // Object file extension
                dest
            };

            pipeline
                .aot_compile(&output_path)
                .map_err(|e| miette::miette!("AOT compilation error: {}", e))?;

            if compiler_timings {
                pipeline.timings.print_summary();
            }
        }
    }

    Ok(())
}
