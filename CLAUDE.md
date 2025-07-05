# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Yuu is a multi-pass compiler for a custom programming language, written in Rust. It follows a sophisticated pass-based architecture that compiles source code through several intermediate representations, ultimately generating C code.

## Build and Development Commands

```bash
# Build the project
cargo build

# Run tests
cargo test

# Run the compiler
cargo run

# Run a specific test
cargo test test_name

# Check code without building
cargo check
```

## Architecture Overview

### Pass-Based Compilation Pipeline

The compiler is built around a **pass scheduling system** in `src/scheduling/` that:
- Automatically resolves dependencies between compilation passes
- Executes independent passes in parallel using Rayon
- Manages resources through a type-safe context system
- Uses `Arc<Mutex<T>>` for thread-safe resource sharing

### Compilation Flow

**Source Code → Tokens → AST → Type-Annotated AST → YIR → C Code**

Major passes (each in `src/pass-*/`):
1. **`pass-parse/`** - Lexical analysis and AST construction using Logos
2. **`pass-type_inference/`** - Type system and binding analysis
3. **`pass-yir_lowering/`** - Lower AST to YIR (Yuu Intermediate Representation)
4. **`pass-print_yir/`** - YIR visualization with syntax highlighting
5. **`pass-c_lowering/`** - Generate C code from YIR
6. **`pass-diagnostics/`** - Rich error reporting with Miette

### Key Architectural Patterns

**Resource-Based Design:**
- Each pass declares resource requirements (`requires_resource_read`, `requires_resource_write`, `produces_resource`)
- Resources are accessed through `ResourceId` trait for type safety
- Context splitting enables concurrent execution

**Pass Implementation:**
- All passes implement the `Pass` trait with `run()`, `install()`, and `get_name()` methods
- Use `anyhow::Result` for error handling
- Install passes into a `Schedule` which the `Scheduler` executes

### Typical Development Pattern

```rust
// 1. Create context with source
let context = create_context_with_source(source_code, filename);

// 2. Set up passes
let mut schedule = Schedule::new();
PassParse.install(&mut schedule);
PassTypeInference.install(&mut schedule);
PassDiagnostics.install(&mut schedule);

// 3. Execute pipeline
let scheduler = Scheduler::new();
let result = scheduler.run(schedule, context)?;

// 4. Extract results
let output = result.get_resource::<ResourceType>();
```

## Key Files

- `src/scheduling/scheduler.rs` - Core pass execution engine
- `src/lib.rs` - Main module exports and pass registration
- `tests/common/mod.rs` - Test utilities and setup patterns
- Each `src/pass-*/mod.rs` - Entry point for compilation passes

## Error Handling

The project uses Miette for rich error reporting with:
- Source code context in error messages
- Syntax highlighting in terminal output
- Centralized diagnostics collection across passes

## Testing

Integration tests in `tests/` exercise the full compilation pipeline. Use `tests/common/mod.rs` utilities for setting up test contexts and schedules.