use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use crate::ast::SourceInfo;
use crate::error::{ErrorKind, YuuError};
use crate::scheduler::{ResourceId, ResourceName};
use crate::Span;
use crate::{
    ast::NodeId,
    binding_info::{BindingInfo, BindingInfoKind},
    type_info::{FunctionType, PrimitiveType, TypeInfo, TypeInfoTable},
};
use hashbrown::HashMap;

pub const FUNC_BLOCK_NAME: &str = "_fn";

#[derive(Clone)]
pub struct Block {
    pub bindings: HashMap<String, BindingInfoKind>,
    pub parent: Option<usize>,
    pub root_block: *mut RootBlock,
    pub id: usize,
    pub named_block_binding: Option<(String, BindingInfo)>,
}

unsafe impl Send for RootBlock {}

impl ResourceId for Box<RootBlock> {
    fn resource_name() -> &'static str {
        "RootBlock"
    }
}

// TODO: Wrap this in a box; we have pointers to it. When the root block is moved, the pointers are invalidated and point to garbage / other data.
pub struct RootBlock {
    arena: Vec<Block>,
    root: usize,
}

#[derive(Clone, Debug)]
pub struct BindingTable(HashMap<NodeId, NodeId>);

impl ResourceId for BindingTable {
    fn resource_name() -> ResourceName {
        "BindingTable"
    }
}

impl Default for BindingTable {
    fn default() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for BindingTable {
    type Target = HashMap<NodeId, NodeId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BindingTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl RootBlock {
    pub fn new(tyt: &mut TypeInfoTable) -> Box<Self> {
        let mut arena = Vec::new();

        let mut top_level_block = Block {
            bindings: HashMap::new(),
            parent: None,
            root_block: std::ptr::null_mut(),
            id: 0,
            named_block_binding: None,
        };

        top_level_block.predefine_builtins(tyt);

        arena.push(top_level_block);

        let mut root = Box::new(Self {
            root: arena.len() - 1,
            arena,
        });

        root.arena[0].root_block = &mut *root;
        root
    }

    pub fn root(&self) -> &Block {
        self.arena.get(self.root).unwrap()
    }

    pub fn root_mut(&mut self) -> &mut Block {
        self.arena.get_mut(self.root).unwrap()
    }
}

impl Block {
    fn get_root_block(&self) -> &RootBlock {
        unsafe { &*self.root_block }
    }

    fn get_root_block_mut(&mut self) -> &mut RootBlock {
        unsafe { &mut *self.root_block }
    }

    pub fn get_parent(&self) -> Option<&Block> {
        self.parent.and_then(|p| self.get_root_block().arena.get(p))
    }

    pub fn get_parent_mut(&mut self) -> Option<&mut Block> {
        self.parent
            .and_then(|p| self.get_root_block_mut().arena.get_mut(p))
    }

    pub fn get_binding(&self, name: &str) -> Option<BindingInfoKind> {
        if let Some(binding) = self.bindings.get(name) {
            return Some(binding.clone());
        }

        self.get_parent().and_then(|p| p.get_binding(name))
    }

    pub fn get_unique_binding_forced(&self, name: &str) -> Option<BindingInfo> {
        let binding = self.get_binding(name);
        if let Some(BindingInfoKind::Variable(binding)) = binding {
            return Some(binding.clone());
        }
        debug_assert!(
            binding.is_none(),
            "Compiler bug: binding {} is not unique",
            name
        );
        None
    }

    pub fn get_block_binding(&self, searched_name: &str) -> Option<BindingInfo> {
        if let Some((name, binding)) = self.named_block_binding.as_ref() {
            if name == searched_name {
                return Some(binding.clone());
            }
        }

        self.get_parent()
            .and_then(|p| p.get_block_binding(searched_name))
    }

    pub fn get_fn_block_name(&self) -> BindingInfo {
        self.get_block_binding(FUNC_BLOCK_NAME).expect("Compiler bug: Every function has to have a root block and this is automatically assigned by the compiler. This one apparently doesn't have one.")
    }

    pub fn make_child(&mut self, name: Option<(String, BindingInfo)>) -> &mut Block {
        let id = self.id;
        let len = self.get_root_block().arena.len();
        let root_block = self.get_root_block_mut();

        let child = Block {
            bindings: HashMap::new(),
            parent: Some(id),
            root_block,
            id: len,
            named_block_binding: name,
        };
        root_block.arena.push(child);
        &mut root_block.arena[len]
    }

    pub fn declare_function(
        &mut self,
        name: String,
        id: NodeId,
        span: Span,
    ) -> Result<(), YuuError> {
        match self.bindings.get_mut(&name) {
            Some(BindingInfoKind::Variable(_)) => {
                panic!(
                    "Bug: Function {} already declared - not sure if this should ever happen",
                    name
                )
            }
            Some(BindingInfoKind::Function(funcs)) => {
                funcs.push(BindingInfo {
                    id,
                    src_location: Some(span),
                    is_mut: false,
                });
                Ok(())
            }
            None => {
                let funcs = vec![BindingInfo {
                    id,
                    src_location: Some(span),
                    is_mut: false,
                }];
                self.bindings.insert(name, BindingInfoKind::Function(funcs));
                Ok(())
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, id: NodeId, span: Span, is_mut: bool) {
        self.bindings.insert(
            name,
            BindingInfoKind::Variable(BindingInfo {
                id,
                src_location: Some(span),
                is_mut,
            }),
        );
    }

    fn register_binary_op(
        &mut self,
        type_info_table: &mut TypeInfoTable,
        op_name: &str,
        id: NodeId,
        arg_type: PrimitiveType,
        ret_type: PrimitiveType,
    ) {
        let func_type = (
            [arg_type.into(), arg_type.into()].as_slice(),
            ret_type.into(),
        )
            .into();

        type_info_table.types.insert(id, func_type);
        let _ = self.declare_function(op_name.to_string(), id, Span { start: 0, end: 0 });
    }

    pub fn predefine_builtins(&mut self, tyt: &mut TypeInfoTable) {
        let mut counter: i64 = 0;
        let mut next = || {
            counter = counter - 1;
            counter
        };

        // F32 operations
        self.register_binary_op(tyt, "add", next(), PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "sub", next(), PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "mul", next(), PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "div", next(), PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "eq", next(), PrimitiveType::F32, PrimitiveType::Bool);

        // I64 operations
        self.register_binary_op(tyt, "add", next(), PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "sub", next(), PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "mul", next(), PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "div", next(), PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "eq", next(), PrimitiveType::I64, PrimitiveType::Bool);

        // F64 operations
        self.register_binary_op(tyt, "add", next(), PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "sub", next(), PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "mul", next(), PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "div", next(), PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "eq", next(), PrimitiveType::F64, PrimitiveType::Bool);
    }

    // pub fn resolve_function<T>(
    //     &self,
    //     name: &str,
    //     type_info_table: &TypeInfoTable,
    //     resolve_arg_types: &[&'static TypeInfo],
    //     resolve_ret_type: T,
    //     src_info: Option<&SourceInfo>,
    //     span: Option<Span>,
    // ) -> Result<&'static TypeInfo, YuuError>
    // where
    //     T: Fn(&BindingInfo, &FunctionType) -> &'static TypeInfo,
    // {
    //     let binding = self.get_binding(name).ok_or_else(|| {
    //         // Create a binding not found error
    //         let mut builder = YuuError::builder()
    //             .kind(ErrorKind::FunctionOverloadError)
    //             .message(format!("Function '{}' not found", name));

    //         if let (Some(src), Some(sp)) = (src_info, span) {
    //             builder = builder
    //                 .source(src.source.clone(), src.file_name.clone())
    //                 .span(
    //                     (sp.start as usize, (sp.end - sp.start) as usize),
    //                     format!("'{}' is not defined", name),
    //                 );

    //             // Add suggestions for similar names
    //             let similar = self.get_similar_names(name, 3, 3);
    //             if !similar.is_empty() {
    //                 builder = builder.help(format!("Did you mean: {}?", similar.join(", ")));
    //             }
    //         }

    //         builder.build()
    //     })?;

    //     match binding {
    //         BindingInfoKind::Unique(binding_info) => {
    //             let func_type = type_info_table.types.get(&binding_info.id).ok_or_else(|| {
    //                 let mut builder = YuuError::builder()
    //                     .kind(ErrorKind::FunctionOverloadError)
    //                     .message(format!("'{}' is not a function", name));

    //                 if let (Some(src), Some(sp)) = (src_info, span) {
    //                     builder = builder
    //                         .source(src.source.clone(), src.file_name.clone())
    //                         .span(
    //                             (sp.start as usize, (sp.end - sp.start) as usize),
    //                             format!("'{}' is not a function", name),
    //                         );
    //                 }

    //                 builder.build()
    //             })?;

    //             if let TypeInfo::Function(func) = &**func_type {
    //                 if func.args.len() != resolve_arg_types.len() {
    //                     let mut builder = YuuError::builder()
    //                         .kind(ErrorKind::FunctionOverloadError)
    //                         .message(format!(
    //                             "Function '{}' expects {} arguments, but {} were provided",
    //                             name,
    //                             func.args.len(),
    //                             resolve_arg_types.len()
    //                         ));

    //                     if let (Some(src), Some(sp)) = (src_info, span) {
    //                         builder = builder
    //                             .source(src.source.clone(), src.file_name.clone())
    //                             .span(
    //                                 (sp.start as usize, (sp.end - sp.start) as usize),
    //                                 "incorrect number of arguments",
    //                             );

    //                         if let Some(decl_span) = binding_info.src_location {
    //                             builder = builder.label(
    //                                 (
    //                                     decl_span.start as usize,
    //                                     (decl_span.end - decl_span.start) as usize,
    //                                 ),
    //                                 format!("function '{}' defined here", name),
    //                             );
    //                         }
    //                     }

    //                     return Err(builder.build());
    //                 }

    //                 for (i, (expected, actual)) in
    //                     func.args.iter().zip(resolve_arg_types.iter()).enumerate()
    //                 {
    //                     if !actual.is_exact_same_type(expected) {
    //                         let mut builder = YuuError::builder()
    //                             .kind(ErrorKind::FunctionOverloadError)
    //                             .message(format!(
    //                                 "Type mismatch in argument {} of function '{}': expected {}, found {}",
    //                                 i + 1,
    //                                 name,
    //                                 expected,
    //                                 actual
    //                             ));

    //                         if let (Some(src), Some(sp)) = (src_info, span) {
    //                             builder = builder
    //                                 .source(src.source.clone(), src.file_name.clone())
    //                                 .span(
    //                                     (sp.start as usize, (sp.end - sp.start) as usize),
    //                                     format!("type mismatch in argument {}", i + 1),
    //                                 );

    //                             if let Some(decl_span) = binding_info.src_location {
    //                                 builder = builder.label(
    //                                     (
    //                                         decl_span.start as usize,
    //                                         (decl_span.end - decl_span.start) as usize,
    //                                     ),
    //                                     format!("function '{}' defined here", name),
    //                                 );
    //                             }
    //                         }

    //                         return Err(builder.build());
    //                     }
    //                 }

    //                 Ok(resolve_ret_type(&binding_info, func))
    //             } else {
    //                 let mut builder = YuuError::builder()
    //                     .kind(ErrorKind::FunctionOverloadError)
    //                     .message(format!("'{}' is not a function", name));

    //                 if let (Some(src), Some(sp)) = (src_info, span) {
    //                     builder = builder
    //                         .source(src.source.clone(), src.file_name.clone())
    //                         .span(
    //                             (sp.start as usize, (sp.end - sp.start) as usize),
    //                             format!("'{}' is not a function", name),
    //                         );
    //                 }

    //                 Err(builder.build())
    //             }
    //         }
    //         BindingInfoKind::Ambiguous(funcs) => {
    //             let mut candidates = Vec::new();

    //             for func in funcs.iter() {
    //                 let func_type = match type_info_table.types.get(&func.id) {
    //                     Some(ft) => ft,
    //                     None => continue,
    //                 };

    //                 if let TypeInfo::Function(func_type) = &**func_type {
    //                     candidates.push((func, func_type));

    //                     if func_type.args.len() != resolve_arg_types.len() {
    //                         continue;
    //                     }

    //                     let mut all_args_match = true;
    //                     for (expected, actual) in
    //                         func_type.args.iter().zip(resolve_arg_types.iter())
    //                     {
    //                         if !actual.is_exact_same_type(expected) {
    //                             all_args_match = false;
    //                             break;
    //                         }
    //                     }

    //                     if all_args_match {
    //                         return Ok(resolve_ret_type(func, func_type));
    //                     }
    //                 }
    //             }

    //             // If we got here, no matching overload was found
    //             let mut builder = YuuError::builder()
    //                 .kind(ErrorKind::FunctionOverloadError)
    //                 .message(format!(
    //                     "No matching overload found for function '{}'",
    //                     name
    //                 ));

    //             if let (Some(src), Some(sp)) = (src_info, span) {
    //                 builder = builder
    //                     .source(src.source.clone(), src.file_name.clone())
    //                     .span(
    //                         (sp.start as usize, (sp.end - sp.start) as usize),
    //                         "no matching function overload",
    //                     );

    //                 // Add information about each candidate
    //                 for (i, (func, func_type)) in candidates.iter().enumerate() {
    //                     let arg_types = func_type
    //                         .args
    //                         .iter()
    //                         .map(|t| t.to_string())
    //                         .collect::<Vec<_>>()
    //                         .join(", ");

    //                     if let Some(decl_span) = &func.src_location {
    //                         builder = builder.label(
    //                             (
    //                                 decl_span.start as usize,
    //                                 (decl_span.end - decl_span.start) as usize,
    //                             ),
    //                             format!("candidate {} with types ({})", i + 1, arg_types),
    //                         );
    //                     }
    //                 }

    //                 // Add more detailed help
    //                 if !candidates.is_empty() {
    //                     let mut help = format!("Function '{}' exists but arguments don't match.\nCandidate overloads:\n", name);
    //                     for (i, (_, func_type)) in candidates.iter().enumerate() {
    //                         let arg_types = func_type
    //                             .args
    //                             .iter()
    //                             .map(|t| t.to_string())
    //                             .collect::<Vec<_>>()
    //                             .join(", ");

    //                         let ret_type = func_type.ret.to_string();
    //                         help.push_str(&format!(
    //                             "{}. fn {}({}) -> {}\n",
    //                             i + 1,
    //                             name,
    //                             arg_types,
    //                             ret_type
    //                         ));
    //                     }

    //                     let given_types = resolve_arg_types
    //                         .iter()
    //                         .map(|t| t.to_string())
    //                         .collect::<Vec<_>>()
    //                         .join(", ");

    //                     help.push_str(&format!("\nProvided argument types: ({})", given_types));
    //                     builder = builder.help(help);
    //                 }
    //             }

    //             Err(builder.build())
    //         }
    //     }
    // }

    pub fn get_similar_names(&self, name: &str, amount: u64, max_dst: u64) -> Vec<String> {
        let mut similar_names = Vec::new();

        for key in self.bindings.keys() {
            let distance = levenshtein_distance(name, key);
            if distance > 0 && distance <= max_dst as usize {
                similar_names.push((key.clone(), distance));
            }
        }

        similar_names.sort_by_key(|x| x.1);
        let mut result: Vec<_> = similar_names
            .into_iter()
            .map(|(name, _)| name)
            .take(amount as usize)
            .collect();

        if let Some(parent) = self.get_parent() {
            let remaining = amount - result.len() as u64;
            if remaining > 0 {
                let mut parent_similar = parent.get_similar_names(name, remaining, max_dst);
                result.append(&mut parent_similar);
            }
        }

        result
    }

    fn create_binding_not_found_error(
        &self,
        name: &str,
        is_function: bool,
        src: &SourceInfo,
        sp: Span,
    ) -> YuuError {
        let func_or_var = if is_function { "Function" } else { "Variable" };

        let mut builder = YuuError::builder()
            .kind(ErrorKind::FunctionOverloadError)
            .message(format!("{} '{}' not found", func_or_var, name));

        builder = builder
            .source(src.source.clone(), src.file_name.clone())
            .span(
                (sp.start as usize, (sp.end - sp.start) as usize),
                format!("'{}' is not defined", name),
            );

        // Add suggestions for similar names
        let similar = self.get_similar_names(name, 3, 3);
        if !similar.is_empty() {
            builder = builder.help(format!("Did you mean: {}?", similar.join(", ")));
        }

        builder.build()
    }

    pub fn resolve_ident_as_function(
        &self,
        name: &str,
        arg_types: &[&'static TypeInfo],
        type_info_table: &TypeInfoTable,
        src: &SourceInfo,
        sp: Span,
    ) -> Result<IdentResolutionResult, YuuError> {
        self.resolve_ident(name, Some(arg_types), type_info_table, src, sp)
    }

    pub fn resolve_ident(
        &self,
        name: &str,
        arg_types: Option<&[&'static TypeInfo]>,
        type_info_table: &TypeInfoTable,
        src: &SourceInfo,
        sp: Span,
    ) -> Result<IdentResolutionResult, YuuError> {
        let binding = self.get_binding(name).ok_or_else(|| {
            self.create_binding_not_found_error(name, arg_types.is_some(), src, sp.clone())
        })?;

        match binding {
            BindingInfoKind::Variable(binding_info) if arg_types.is_none() => {
                // Found a variable
                let type_info = type_info_table
                    .types
                    .get(&binding_info.id)
                    .expect("Compiler bug: Binding not found in type info table");

                Ok(IdentResolutionResult {
                    binding: binding_info,
                    kind: IdentResolutionKind::ResolvedAsVariable {
                        type_info: *type_info,
                    },
                    contextual_appropriate_type: *type_info,
                })
            }

            BindingInfoKind::Function(funcs) if arg_types.is_some() => {
                let arg_types = unsafe { arg_types.unwrap_unchecked() };
                // Handle overloaded functions
                let mut candidates = Vec::new();

                // First pass: find an exact match
                for func in funcs.iter() {
                    if let Some(ft) = type_info_table.types.get(&func.id) {
                        if let TypeInfo::Function(func_type) = &**ft {
                            // Skip if argument count doesn't match
                            if func_type.args.len() != arg_types.len() {
                                continue;
                            }

                            // Check if all arguments match exactly
                            let mut all_args_match = true;
                            for (expected, actual) in func_type.args.iter().zip(arg_types.iter()) {
                                if !actual.is_exact_same_type(expected) {
                                    all_args_match = false;
                                    break;
                                }
                            }

                            // Found a match
                            if all_args_match {
                                return Ok(IdentResolutionResult {
                                    binding: func.clone(),
                                    kind: IdentResolutionKind::ResolvedAsFunction { func_type },
                                    contextual_appropriate_type: func_type.ret,
                                });
                            }

                            candidates.push((func, func_type));
                        }
                    }
                }

                // No exact match found - create detailed error
                Err(self.create_no_overload_error(name, candidates, arg_types, src, sp))
            }

            // No arguments given - that should never happen!
            BindingInfoKind::Function(bi) => {
                return Err(self.create_function_as_variable_error(name, &bi, src, sp));
            }

            // Error: We are looking for a variable, but it was called as a function!
            BindingInfoKind::Variable(bi) => {
                return Err(self.create_variable_as_function_error(name, &bi, src, sp));
            }
        }
    }

    fn create_variable_as_function_error(
        &self,
        name: &str,
        binding: &BindingInfo,
        src: &SourceInfo,
        sp: Span,
    ) -> YuuError {
        let mut builder = YuuError::builder()
            .kind(ErrorKind::FunctionOverloadError)
            .message(format!("Cannot call '{}' as a function", name))
            .source(src.source.clone(), src.file_name.clone())
            .span(
                (sp.start as usize, (sp.end - sp.start) as usize),
                format!("'{}' is a variable, not a function", name),
            );

        if let Some(decl_span) = &binding.src_location {
            builder = builder.label(
                (
                    decl_span.start as usize,
                    (decl_span.end - decl_span.start) as usize,
                ),
                format!("'{}' was defined here as a variable", name),
            );
        }

        builder.build()
    }

    fn create_function_as_variable_error(
        &self,
        name: &str,
        funcs: &Vec<BindingInfo>,
        src: &SourceInfo,
        sp: Span,
    ) -> YuuError {
        let mut builder = YuuError::builder()
            .kind(ErrorKind::FunctionOverloadError)
            .message(format!(
                "Identifier '{}' refers to a function, but used like a variable (no arguments were provided)",
                name
            ))
            .source(src.source.clone(), src.file_name.clone())
            .span(
                (sp.start as usize, (sp.end - sp.start) as usize),
                format!("'{}' is a function that needs to be called with arguments", name),
            );

        // If we have function definitions available, add more helpful information
        if !funcs.is_empty() {
            // Find the first function with location info to show where it was defined
            if let Some(func) = funcs.iter().find(|f| f.src_location.is_some()) {
                if let Some(decl_span) = &func.src_location {
                    builder = builder.label(
                        (
                            decl_span.start as usize,
                            (decl_span.end - decl_span.start) as usize,
                        ),
                        format!("'{}' was defined here as a function", name),
                    );
                }
            }

            builder = builder.help(format!(
                "To use this function, call it with appropriate arguments: {}(...)",
                name
            ));
        }

        builder.build()
    }

    fn create_no_overload_error(
        &self,
        name: &str,
        candidates: Vec<(&BindingInfo, &FunctionType)>,
        provided_args: &[&'static TypeInfo],
        src: &SourceInfo,
        sp: Span,
    ) -> YuuError {
        let mut builder = YuuError::builder()
            .kind(ErrorKind::FunctionOverloadError)
            .message(format!(
                "No matching overload found for function '{}'",
                name
            ));

        builder = builder
            .source(src.source.clone(), src.file_name.clone())
            .span(
                (sp.start as usize, (sp.end - sp.start) as usize),
                "no matching function overload",
            );

        // Add information about each candidate
        for (i, (func, _)) in candidates.iter().enumerate() {
            if let Some(decl_span) = &func.src_location {
                if decl_span.start == 0 && decl_span.end == 0 {
                    continue;
                }
                builder = builder.related_info(
                    Some(format!("Candidate Function {}", i + 1)),
                    (
                        decl_span.start as usize,
                        (decl_span.end - decl_span.start) as usize,
                    ),
                    Some(format!("candidate {}", i + 1)),
                );
            }
        }

        // Add more detailed help
        if !candidates.is_empty() {
            let mut help = format!(
                "Function '{}' exists but arguments don't match.\nCandidate overloads:\n",
                name
            );
            for (i, (_, func_type)) in candidates.iter().enumerate() {
                let arg_types = func_type
                    .args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                let ret_type = func_type.ret.to_string();
                help.push_str(&format!(
                    "{}. fn {}({}) -> {}\n",
                    i + 1,
                    name,
                    arg_types,
                    ret_type
                ));
            }

            let given_types = provided_args
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            help.push_str(&format!("\nProvided argument types: ({})\n", given_types));
            builder = builder.help(help);
        }

        builder.build()
    }
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.chars().count();
    let len2 = s2.chars().count();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for (i, row) in matrix.iter_mut().enumerate().take(len1 + 1) {
        row[0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let substitution = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = (matrix[i][j + 1] + 1)
                .min(matrix[i + 1][j] + 1)
                .min(matrix[i][j] + substitution);
        }
    }

    matrix[len1][len2]
}

pub enum IdentResolutionKind {
    ResolvedAsFunction { func_type: &'static FunctionType },
    ResolvedAsVariable { type_info: &'static TypeInfo },
}

pub struct IdentResolutionResult {
    pub binding: BindingInfo,
    pub kind: IdentResolutionKind,
    /// If resolved as a function, this is the RETURN type of the function - if you want the argument types or the function type itself, you have to match on the 'kind' field.
    /// If resolved as a variable, this is the type of the variable.
    pub contextual_appropriate_type: &'static TypeInfo,
}
