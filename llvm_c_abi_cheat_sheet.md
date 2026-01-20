# LLVM C ABI Cheat Sheet (The Easy Way)

## TL;DR: Let LLVM Do The Work

**Secret**: Just set the calling convention to `ccc` (C calling convention) and LLVM figures out the registers/stack for you!

## The Patterns (From Your Godbolt Output)

### Pattern 1: Small Structs (≤8 bytes) → Integer Coercion

```llvm
; C code:
; struct S { i32 a; i32 b; };  // 8 bytes
; void foo(struct S s);

; LLVM IR:
define void @foo(i64 %s.coerce) {
  ; Unpack it:
  %s = alloca %struct.S, align 4
  store i64 %s.coerce, ptr %s, align 4
  ; Now use %s normally
}
```

**Rule**: Struct ≤8 bytes → Pass as `iN` where N = size * 8

### Pattern 2: Medium Structs (9-16 bytes) → Two Parameters

```llvm
; C code:
; struct S { i64 a; i64 b; };  // 16 bytes
; void foo(struct S s);

; LLVM IR:
define void @foo(i64 %s.coerce0, i64 %s.coerce1) {
  %s = alloca %struct.S, align 8
  %0 = getelementptr inbounds { i64, i64 }, ptr %s, i32 0, i32 0
  store i64 %s.coerce0, ptr %0, align 8
  %1 = getelementptr inbounds { i64, i64 }, ptr %s, i32 0, i32 1
  store i64 %s.coerce1, ptr %1, align 8
}
```

**Rule**: Struct 9-16 bytes → Split into two i64/double parameters

### Pattern 3: Large Structs (>16 bytes) → ByVal Pointer

```llvm
; C code:
; struct S { i64 data[4]; };  // 32 bytes
; void foo(struct S s);

; LLVM IR:
define void @foo(ptr noundef byval(%struct.S) align 8 %s) {
  ; %s is already a pointer to the struct on the stack
  ; Just use it directly!
}
```

**Rule**: Struct >16 bytes → `ptr byval(Type) align N`

### Pattern 4: Floats → Vector Coercion

```llvm
; C code:
; struct Vec2 { float x, y; };  // 8 bytes
; void foo(struct Vec2 v);

; LLVM IR:
define void @foo(<2 x float> %v.coerce) {
  %v = alloca %struct.Vec2, align 4
  store <2 x float> %v.coerce, ptr %v, align 4
}
```

**Rule**: Struct of only floats → Use vector types `<N x float/double>`

### Pattern 5: Large Returns → SRet (Hidden Pointer)

```llvm
; C code:
; struct Large { i64 data[4]; };
; struct Large foo(void);

; LLVM IR:
define void @foo(ptr sret(%struct.Large) align 8 %agg.result) {
  ; Write result into %agg.result
  ; Caller allocated the memory
  store ...
  ret void  ; Note: void, not the struct!
}
```

**Rule**: Return >16 bytes → Add `sret` parameter, return `void`

## The Simple Implementation (Skip The Pain)

```rust
fn lower_struct_param_to_llvm(struct_ty: &StructType) -> Vec<LLVMType> {
    let size = struct_ty.size_in_bytes();
    
    match size {
        0 => vec![],  // Empty struct - no parameters
        
        1..=8 => {
            // Small struct - pass as single integer
            if all_fields_are_floats(struct_ty) && size == 8 {
                vec![LLVMType::Vec2Float]  // <2 x float>
            } else {
                vec![LLVMType::int(size * 8)]  // i8, i16, i32, i64
            }
        }
        
        9..=16 => {
            // Medium struct - split into two
            if all_fields_are_floats(struct_ty) {
                // Vec4 or two doubles, etc.
                vec![LLVMType::Vec2Float, LLVMType::Vec2Float]
            } else {
                // Two i64s (or i64 + smaller int for odd sizes)
                let first_size = 8;
                let second_size = size - 8;
                vec![
                    LLVMType::I64,
                    LLVMType::int(second_size * 8),
                ]
            }
        }
        
        _ => {
            // Large struct - pass by pointer
            vec![LLVMType::Ptr {
                byval: true,
                align: struct_ty.alignment(),
            }]
        }
    }
}

fn lower_struct_return_to_llvm(struct_ty: &StructType) -> ReturnLowering {
    let size = struct_ty.size_in_bytes();
    
    if size <= 8 {
        // Return as single value
        ReturnLowering::Direct(LLVMType::int(size * 8))
    } else if size <= 16 {
        // Return as { i64, i64 } or similar
        ReturnLowering::DirectPair(LLVMType::I64, LLVMType::I64)
    } else {
        // Add sret parameter, return void
        ReturnLowering::Sret
    }
}
```

## Emit Code (The Mechanical Part)

### For Parameters:

```rust
// Small struct (≤8 bytes)
let llvm_ty = context.i64_type();
let param = llvm_fn.get_nth_param(param_index).unwrap();

// Create stack slot
let struct_alloca = builder.build_alloca(struct_llvm_type, "s");
// Store coerced value
builder.build_store(struct_alloca, param);
// Now use struct_alloca as the struct

// Medium struct (9-16 bytes)
let param0 = llvm_fn.get_nth_param(param_index + 0).unwrap();
let param1 = llvm_fn.get_nth_param(param_index + 1).unwrap();

let struct_alloca = builder.build_alloca(struct_llvm_type, "s");
let field0_ptr = builder.build_struct_gep(struct_alloca, 0, "field0");
let field1_ptr = builder.build_struct_gep(struct_alloca, 1, "field1");
builder.build_store(field0_ptr, param0);
builder.build_store(field1_ptr, param1);

// Large struct (>16 bytes)
// Already a pointer! Just use it:
let struct_ptr = llvm_fn.get_nth_param(param_index).unwrap();
// Access fields directly through the pointer
```

### For Returns:

```rust
// Small return (≤8 bytes)
let struct_val = ...; // your struct value (alloca ptr)
let coerced = builder.build_load(context.i64_type(), struct_val, "coerce");
builder.build_return(Some(&coerced));

// Medium return (9-16 bytes)
let struct_val = ...;
let field0_ptr = builder.build_struct_gep(struct_val, 0, "f0");
let field1_ptr = builder.build_struct_gep(struct_val, 1, "f1");
let f0 = builder.build_load(context.i64_type(), field0_ptr, "");
let f1 = builder.build_load(context.i64_type(), field1_ptr, "");

// Pack into { i64, i64 }
let return_ty = context.struct_type(&[i64_type, i64_type], false);
let undef = return_ty.get_undef();
let res1 = builder.build_insert_value(undef, f0, 0, "").unwrap();
let res2 = builder.build_insert_value(res1, f1, 1, "").unwrap();
builder.build_return(Some(&res2));

// Large return (>16 bytes)
// Function signature has sret parameter as first param
let sret_ptr = llvm_fn.get_nth_param(0).unwrap();
// Copy your struct into sret_ptr
builder.build_memcpy(..., your_struct, sret_ptr, ...);
builder.build_return(None);  // Return void!
```

## Function Signature Examples

```rust
use inkwell::context::Context;

let context = Context::create();

// Example 1: void foo(SmallStruct s)
let small_struct_ty = context.struct_type(&[i32_type, i32_type], false);
let fn_type = context.void_type().fn_type(&[
    context.i64_type().into()  // Coerced to i64
], false);

// Example 2: void foo(MediumStruct s) 
let fn_type = context.void_type().fn_type(&[
    context.i64_type().into(),  // First 8 bytes
    context.i64_type().into(),  // Second 8 bytes
], false);

// Example 3: void foo(LargeStruct s)
let fn_type = context.void_type().fn_type(&[
    small_struct_ty.ptr_type(AddressSpace::Generic).into()
], false);
// Add byval attribute to the parameter:
fn_val.add_attribute(
    AttributeLoc::Param(0),
    context.create_type_attribute(Attribute::get_named_enum_kind_id("byval"), small_struct_ty.into())
);

// Example 4: LargeStruct foo(void) - with sret
let fn_type = context.void_type().fn_type(&[
    large_struct_ty.ptr_type(AddressSpace::Generic).into()
], false);
// Add sret attribute:
fn_val.add_attribute(
    AttributeLoc::Param(0),
    context.create_type_attribute(Attribute::get_named_enum_kind_id("sret"), large_struct_ty.into())
);
```

## Quick Decision Tree

```
Is size == 0?          → Ignore
Is size ≤ 8?           → Single iN parameter
Is size ≤ 16?          → Two parameters (split)
Is size > 16?          → ptr byval
All floats + size ≤16? → Use <N x float> vectors
Returning > 16 bytes?  → Add sret, return void
```

## The Nuclear Option: Just Use ByVal for Everything

If you're prototyping and don't care about performance:

```rust
fn lower_struct_param_lazy(struct_ty: &StructType) -> LLVMType {
    // Always pass by pointer - let LLVM optimize later
    LLVMType::Ptr { byval: true, align: struct_ty.alignment() }
}
```

This is **always correct** (just slower). Get it working first, optimize later!

## Testing

1. Compile your test.c with `clang -S -emit-llvm -O0`
2. Look at the function signatures
3. Copy them exactly
4. Profit!

That's it. No classification algorithm needed! 🎉
