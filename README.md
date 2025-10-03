# Yuu

Yuu is a programming language.

*Note: More positive adjectives will be added as Yuu becomes less terrible.*

[![Build Status](https://github.com/JanIngoHaas/yuu/actions/workflows/rust.yml/badge.svg?branch=main)](https://github.com/JanIngoHaas/yuu/actions/workflows/rust.yml)
[![Rust](https://img.shields.io/badge/rust-1.90+-orange.svg)](https://www.rust-lang.org)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

**WIP**: Very early stage project, not usable yet.

## Quick Start

```bash
# Build the compiler
cargo build

# Compile Yuu source to C
cargo run -- c input.yuu -o output.c

# Generate YIR (intermediate representation)
cargo run -- yir input.yuu -o output.yir

# Check source for errors
cargo run -- check input.yuu
```

## Example Code

**Yuu Source (`example.yuu`):**
```yuu
struct Point:
    x: i64,
    y: i64,
.

fn distance(p1: Point, p2: Point) -> i64:
    let dx = p1.x - p2.x;
    let dy = p1.y - p2.y;
    return dx * dx + dy * dy .

fn main() -> i64:
    let p1 = Point { x: 3, y: 4 };
    let p2 = Point { x: 0, y: 0 };
    return distance(p1, p2) .
```

## YIR Output

The compiler generates YIR (Yuu Intermediate Representation) showing the low-level operations:

```yir
FN distance(stack_param_mem@Point := PARAM@Point, stack_param_mem.1@Point := PARAM@Point) -> @i64
{
:entry_0:
    p1@*Point := ADDR stack_param_mem@Point
    p2@*Point := ADDR stack_param_mem.1@Point
    field_ptr@*i64 := FIELD_PTR p1@*Point .x
    field_ptr.1@*i64 := FIELD_PTR p2@*Point .x
    binary_lhs@i64 := LOAD field_ptr@*i64
    binary_rhs@i64 := LOAD field_ptr.1@*i64
    bin_result@i64 := binary_lhs@i64 - binary_rhs@i64
    dx@*i64 := ALLOCA@i64
    dx@*i64 <- LOAD bin_result.1@*i64
    // ... more YIR instructions
    RETURN return_value@i64
}

FN main() -> @i64
{
:entry_0:
    Point@*Point := ALLOCA@Point
    x@*i64 := FIELD_PTR Point@*Point .x
    x@*i64 <- 3@i64
    y@*i64 := FIELD_PTR Point@*Point .y
    y@*i64 <- 4@i64
    // ... struct initialization and function call
    call_result@i64 := CALL distance(call_arg@Point, call_arg.1@Point)
    RETURN return_value@i64
}
```

## Language Features

- **Structs**: Custom data types with named fields
- **Functions**: Functions with parameters and return types  
- **Control Flow**: `if`/`else`, `while` loops with `break`
- **Pattern Matching**: `match` expressions with enums
- **Type System**: Strong static typing with inference

## Basic Syntax

```yuu
// Variable declarations
let x = 42;              // Type inference
let y: i64 = 42;         // Explicit type
let mut counter = 0;     // Mutable variable

// Control flow
if x > 5: result = 1 .
else: result = 0 .

while counter < 10:
    counter = counter + 1;
end

// Enums and pattern matching
enum Option:
    None,
    Some: i64,
end

match opt:
    Option::None: return 0 .
    Option::Some(value): return value .
end
```

## Syntax Notes

Statements end with `;`. When you have a statement termination followed by block termination (similar to `; }` in other languages), you can write ` .` (note the space) instead. Complex blocks use `end[...]`

See the `tests/` folder for more comprehensive language examples.