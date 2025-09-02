use clap::{Parser, Subcommand};
use miette::Result;
use std::fs;
use std::path::PathBuf;
use yuu::pass_diagnostics::error;
use yuu::utils::pipeline::Pipeline;

#[derive(Parser)]
#[command(name = "yuu")]
#[command(about = "A multi-pass compiler for the Yuu programming language")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Compile source to C code")]
    C {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(short, long, help = "Output file (default: stdout)")]
        output: Option<PathBuf>,
    },
    #[command(about = "Compile source to YIR")]
    Yir {
        #[arg(help = "Input source file")]
        input: PathBuf,
        #[arg(short, long, help = "Output file (default: stdout)")]
        output: Option<PathBuf>,
        #[arg(long, help = "Disable colored output")]
        no_color: bool,
    },
    #[command(about = "Check source for errors")]
    Check {
        #[arg(help = "Input source file")]
        input: PathBuf,
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
        Commands::C { input, output } => {
            let (source, filename) = read_source_file(&input)?;
            let mut pipeline = Pipeline::new(source, filename)
                .map_err(|e| miette::miette!("Parse error: {}", e))?;
            let c_code = pipeline
                .get_c_code()
                .map_err(|e| miette::miette!("C code generation error: {}", e))?;
            write_output(output, &c_code.0)?;
        }
        Commands::Yir {
            input,
            output,
            no_color,
        } => {
            let (source, filename) = read_source_file(&input)?;
            let pipeline = Pipeline::new(source, filename)
                .map_err(|e| miette::miette!("Parse error: {}", e))?;
            let yir_output = if no_color {
                pipeline
                    .print_yir()
                    .map_err(|e| miette::miette!("YIR generation error: {}", e))?
            } else {
                pipeline
                    .print_yir_colored()
                    .map_err(|e| miette::miette!("YIR generation error: {}", e))?
            };
            write_output(output, &yir_output.0)?;
        }
        Commands::Check { input } => {
            let (source, filename) = read_source_file(&input)?;
            let pipeline = Pipeline::new(source, filename)
                .map_err(|e| miette::miette!("Parse error: {}", e))?;
            pipeline
                .diagnostics()
                .map_err(|e| miette::miette!("Diagnostics error: {}", e))?;
            println!("No errors found");
        }
    }

    Ok(())
}
