// TODO: THIS NEEDS TO BE REWRITTEN - USE BUMPALO OR SIMILAR FOR MEMORY MANAGEMENT, DON'T GIVE OUT REFERENCES TO BLOCKS - for now just save things in Box<> so the pointers do not become invalidated when the vec needs to realloc.

use std::ops::{Deref, DerefMut};

use crate::pass_diagnostics::error::{ErrorKind, YuuError, levenshtein_distance};
use crate::pass_parse::ast::SourceInfo;
use crate::pass_type_inference::VariableBinding;
use crate::scheduling::scheduler::{ResourceId, ResourceName};
use crate::{
    pass_parse::ast::NodeId,
    pass_type_inference::BindingInfo,
    pass_type_inference::{FunctionType, TypeInfo},
};
use indexmap::IndexMap;
use logos::Span;
use ustr::{Ustr, UstrMap};

pub const FUNC_BLOCK_NAME: &str = "_fn";

pub struct Block {
    pub bindings: UstrMap<VariableBinding>,
    pub parent: Option<usize>,
    pub root_block: *mut RootBlock,
    pub id: usize,
    pub named_block_binding: (Option<Ustr>, BindingInfo),
}

unsafe impl Send for RootBlock {}

impl ResourceId for Box<RootBlock> {
    fn resource_name() -> &'static str {
        "RootBlock"
    }
}

// TODO: Wrap this in a box; we have pointers to it. When the root block is moved, the pointers are invalidated and point to garbage / other data.
pub struct RootBlock {
    arena: Vec<Box<Block>>,
    root: usize,
}

#[derive(Clone, Debug)]
pub struct BindingTable(IndexMap<NodeId, NodeId>);

impl ResourceId for BindingTable {
    fn resource_name() -> ResourceName {
        "BindingTable"
    }
}

impl Default for BindingTable {
    fn default() -> Self {
        Self(IndexMap::new())
    }
}

impl Deref for BindingTable {
    type Target = IndexMap<NodeId, NodeId>;

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
    pub fn new() -> Box<Self> {
        let mut arena = Vec::new();

        let top_level_block = Block {
            bindings: UstrMap::default(),
            parent: None,
            root_block: std::ptr::null_mut(),
            id: 0,
            named_block_binding: (
                None,
                BindingInfo {
                    id: i64::MIN,
                    src_location: None,
                },
            ),
        };

        arena.push(Box::new(top_level_block));

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
        self.parent
            .and_then(|p| self.get_root_block().arena.get(p))
            .map(|v| &**v)
    }

    pub fn get_parent_mut(&mut self) -> Option<&mut Block> {
        self.parent
            .and_then(|p| self.get_root_block_mut().arena.get_mut(p))
            .map(|v| &mut **v)
    }

    pub fn get_binding(&self, name: Ustr) -> Option<VariableBinding> {
        if let Some(binding) = self.bindings.get(&name) {
            return Some(binding.clone());
        }

        self.get_parent().and_then(|p| p.get_binding(name))
    }

    pub fn get_block_binding(&self, searched_name: &str) -> Option<BindingInfo> {
        if let (Some(name), binding) = &self.named_block_binding {
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

    pub fn make_child(&mut self, name: (Option<Ustr>, BindingInfo)) -> &mut Block {
        let id = self.id;
        let len = self.get_root_block().arena.len();
        let root_block = self.get_root_block_mut();

        let child = Block {
            bindings: UstrMap::default(),
            parent: Some(id),
            root_block,
            id: len,
            named_block_binding: name,
        };
        root_block.arena.push(Box::new(child));
        &mut root_block.arena[len]
    }

    pub fn insert_variable(&mut self, name: Ustr, id: NodeId, span: Option<Span>, is_mut: bool) {
        self.bindings.insert(
            name,
            VariableBinding {
                binding_info: BindingInfo {
                    id,
                    src_location: span,
                },
                is_mut,
            },
        );
    }

    pub fn get_similar_names(&self, name: &str, amount: u64, max_dst: u64) -> Vec<String> {
        let mut similar_names = Vec::new();

        for key in self.bindings.keys() {
            let distance = levenshtein_distance(name, key);
            if distance > 0 && distance <= max_dst as usize {
                let key = key.to_string();
                similar_names.push((key, distance));
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

    fn create_binding_not_found_error(&self, name: &str, src: &SourceInfo, sp: Span) -> YuuError {
        let mut builder = YuuError::builder()
            .kind(ErrorKind::FunctionOverloadError)
            .message(format!("Variable '{}' not found", name));

        builder = builder
            .source(src.source.clone(), src.file_name.clone())
            .span(
                (sp.start, (sp.end - sp.start)),
                format!("'{}' is not defined", name),
            );

        // Add suggestions for similar names
        let similar = self.get_similar_names(name, 3, 3);
        if !similar.is_empty() {
            builder = builder.help(format!("Did you mean: {}?", similar.join(", ")));
        }

        builder.build()
    }

    pub fn resolve_variable(
        &self,
        name: Ustr,
        src: &SourceInfo,
        sp: Span,
    ) -> Result<VariableBinding, Box<YuuError>> {
        self.get_binding(name).ok_or_else(|| {
            Box::new(self.create_binding_not_found_error(name.as_ref(), src, sp.clone()))
        })

        // match binding {
        //     BindingInfoKind::Variable(binding_info) if arg_types.is_none() => {
        //         // Found a variable
        //         let type_info = type_info_table
        //             .types
        //             .get(&binding_info.id)
        //             .expect("Compiler bug: Binding not found in type info table");

        //         Ok(IdentResolutionResult {
        //             binding: binding_info,
        //             kind: IdentResolutionKind::ResolvedAsVariable { type_info },
        //             contextual_appropriate_type: type_info,
        //         })
        //     }

        //     BindingInfoKind::Function(funcs) if arg_types.is_some() => {
        //         let arg_types = unsafe { arg_types.unwrap_unchecked() };
        //         // Handle overloaded functions
        //         let mut candidates = Vec::new();

        //         // First pass: find an exact match
        //         for func in funcs.iter() {
        //             if let Some(TypeInfo::Function(func_type)) = type_info_table.types.get(&func.id)
        //             {
        //                 // Skip if argument count doesn't match
        //                 if func_type.args.len() != arg_types.len() {
        //                     continue;
        //                 }

        //                 // Check if all arguments match exactly
        //                 let mut all_args_match = true;
        //                 for (expected, actual) in func_type.args.iter().zip(arg_types.iter()) {
        //                     if !actual.is_exact_same_type(expected) {
        //                         all_args_match = false;
        //                         break;
        //                     }
        //                 }

        //                 // Found a match
        //                 if all_args_match {
        //                     return Ok(IdentResolutionResult {
        //                         binding: func.clone(),
        //                         kind: IdentResolutionKind::ResolvedAsFunction { func_type },
        //                         contextual_appropriate_type: func_type.ret,
        //                     });
        //                 }

        //                 candidates.push((func, func_type));
        //             }
        //         }

        //         // No exact match found - create detailed error
        //         Err(Box::new(self.create_no_overload_error(
        //             &name.to_string(),
        //             candidates,
        //             arg_types,
        //             src,
        //             sp,
        //         )))
        //     }

        //     // No arguments given - that should never happen!
        //     BindingInfoKind::Function(bi) => Err(Box::new(self.create_function_as_variable_error(
        //         &name.to_string(),
        //         &bi,
        //         src,
        //         sp,
        //     ))),

        //     // Error: We are looking for a variable, but it was called as a function!
        //     BindingInfoKind::Variable(bi) => Err(Box::new(self.create_variable_as_function_error(
        //         &name.to_string(),
        //         &bi,
        //         src,
        //         sp,
        //     ))),
        // }
    }

    // fn create_variable_as_function_error(
    //     &self,
    //     name: &str,
    //     binding: &BindingInfo,
    //     src: &SourceInfo,
    //     sp: Span,
    // ) -> YuuError {
    //     let mut builder = YuuError::builder()
    //         .kind(ErrorKind::FunctionOverloadError)
    //         .message(format!("Cannot call '{}' as a function", name))
    //         .source(src.source.clone(), src.file_name.clone())
    //         .span(
    //             (sp.start, (sp.end - sp.start)),
    //             format!("'{}' is a variable, not a function", name),
    //         );

    //     if let Some(decl_span) = &binding.src_location {
    //         builder = builder.label(
    //             (decl_span.start, (decl_span.end - decl_span.start)),
    //             format!("'{}' was defined here as a variable", name),
    //         );
    //     }

    //     builder.build()
    // }

    // fn create_function_as_variable_error(
    //     &self,
    //     name: &str,
    //     funcs: &[BindingInfo],
    //     src: &SourceInfo,
    //     sp: Span,
    // ) -> YuuError {
    //     let mut builder = YuuError::builder()
    //         .kind(ErrorKind::FunctionOverloadError)
    //         .message(format!(
    //             "Identifier '{}' refers to a function, but used like a variable (no arguments were provided)",
    //             name
    //         ))
    //         .source(src.source.clone(), src.file_name.clone())
    //         .span(
    //             (sp.start, (sp.end - sp.start)),
    //             format!("'{}' is a function that needs to be called with arguments", name),
    //         );

    //     // If we have function definitions available, add more helpful information
    //     if !funcs.is_empty() {
    //         // Find the first function with location info to show where it was defined
    //         if let Some(func) = funcs.iter().find(|f| f.src_location.is_some()) {
    //             if let Some(decl_span) = &func.src_location {
    //                 builder = builder.label(
    //                     (decl_span.start, (decl_span.end - decl_span.start)),
    //                     format!("'{}' was defined here as a function", name),
    //                 );
    //             }
    //         }

    //         builder = builder.help(format!(
    //             "To use this function, call it with appropriate arguments: {}(...)",
    //             name
    //         ));
    //     }

    //     builder.build()
    // }
}

// pub enum IdentResolutionKind {
//     ResolvedAsFunction { func_type: &'static FunctionType },
//     ResolvedAsVariable { type_info: &'static TypeInfo },
// }

// pub struct IdentResolutionResult {
//     pub binding: BindingInfo,
//     pub kind: IdentResolutionKind,
//     /// If resolved as a function, this is the RETURN type of the function - if you want the argument types or the function type itself, you have to match on the 'kind' field.
//     /// If resolved as a variable, this is the type of the variable.
//     pub contextual_appropriate_type: &'static TypeInfo,
// }
