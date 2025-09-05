# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Yuu is a multi-pass compiler for a custom programming language that transpiles to C code. The compiler is built in Rust and follows a traditional compiler pipeline architecture with distinct compilation passes.

## Development Commands

### Building and Testing
```bash
# Build the project
cargo build

# Build with verbose output  
cargo build --verbose

# Run all tests
cargo test

# Run tests with verbose output
cargo test --verbose

# Run a specific test
cargo test test_name

# Check code without building
cargo check

# Run the compiler
cargo run -- <subcommand> [options]
```

### Compiler Usage
The compiler has three main subcommands:

```bash
# Compile to C code
cargo run -- c input.yuu -o output.c

# Compile to YIR (Yuu Intermediate Representation)
cargo run -- yir input.yuu -o output.yir

# Check source for errors only
cargo run -- check input.yuu
```

## Architecture Overview

### Pipeline Architecture
The compiler uses a multi-pass pipeline architecture where each pass operates on the output of the previous pass. The main pipeline is orchestrated by `src/utils/pipeline.rs:Pipeline`, which automatically manages pass dependencies and lazy computation.

**Compilation Pipeline Flow:**
1. **Parse** (`src/pass-parse/`) - Source → AST + Syntax Errors
2. **Type Inference** (`src/pass-type_inference/`) - AST → Type Registry + Type Errors  
3. **Semantic Analysis** - Multiple passes:
   - **Control Flow Analysis** (`src/pass-control_flow_analysis/`)
   - **Type Dependencies** (`src/pass-type_dependencies/`)
4. **YIR Lowering** (`src/pass-yir_lowering/`) - AST → YIR Module
5. **C Lowering** (`src/pass-c_lowering/`) - YIR → C Source Code
6. **C Compilation** (`src/pass-c_compilation/`) - C Source → Executable

### Key Components

**Main Entry Point:** `src/main.rs` - CLI interface using clap for argument parsing

**Pass Modules:** Each pass follows the pattern `src/pass-<name>/`:
- `mod.rs` - Public interface and data structures
- `pass_<name>_impl.rs` - Implementation of the pass logic

**Utilities:** `src/utils/`
- `pipeline.rs` - Central pipeline orchestration with lazy evaluation
- `layout_calculation.rs` - Memory layout calculations

**Error Handling:** `src/pass-diagnostics/`
- Comprehensive error reporting with syntax highlighting using miette
- Supports fancy error display with syntect integration

### Testing Strategy

The project uses comprehensive E2E testing located in `tests/`:
- `common/mod.rs` - Shared test utilities and helper functions
- `e2e_*.rs` - End-to-end tests for different language features
- Tests compile actual Yuu source code to executables and verify behavior

**Test Utilities in `tests/common/mod.rs`:**
- `run_to_yir()` - Compile to YIR with colored output
- `run_to_c()` - Compile to C source code  
- `run_to_executable()` - Full compilation to executable
- `run_parse_only()` - Parse AST only
- `run_executable_with_output()` - Execute and capture output

### Dependencies and External Tools

The project requires **Clang** to be installed for C compilation (see GitHub Actions workflow). Key Rust dependencies include:
- `clap` - CLI argument parsing
- `miette` - Advanced error reporting
- `logos` - Lexer generation
- `serde` - Serialization
- `assert_cmd` - Test command execution