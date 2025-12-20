// The BlockTree manages memory for Blocks using a Vec arena and indices for safety.
// Future optimization: Consider using something like bumpalo if allocation overhead becomes an issue.

use std::ops::{Deref, DerefMut};

use crate::pass_diagnostics::error::{ErrorKind, YuuError, levenshtein_distance};
use crate::pass_parse::ast::NodeId;
use crate::pass_parse::ast::SourceInfo;
use crate::utils::collections::FastHashMap;
use crate::utils::{BindingInfo, VariableBinding};
use logos::Span;
use ustr::{Ustr, UstrMap};

pub struct Block {
    pub bindings: UstrMap<VariableBinding>,
    pub parent: Option<usize>,
    pub id: usize,
    pub block_binding: BindingInfo,
}

impl Block {
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
}

pub struct BlockTree {
    arena: Vec<Block>,
    root: usize,
}

#[derive(Clone, Debug)]
pub struct BindingTable(FastHashMap<NodeId, NodeId>);

impl Default for BindingTable {
    fn default() -> Self {
        Self(FastHashMap::default())
    }
}

impl Deref for BindingTable {
    type Target = FastHashMap<NodeId, NodeId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for BindingTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for BlockTree {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockTree {
    pub fn new() -> Self {
        let mut arena = Vec::new();

        let top_level_block = Block {
            bindings: UstrMap::default(),
            parent: None,
            id: 0,
            block_binding: BindingInfo {
                id: usize::MIN,
                src_location: None,
            },
        };

        arena.push(top_level_block);

        Self {
            root: arena.len() - 1,
            arena,
        }
    }

    pub fn root(&self) -> &Block {
        self.arena.get(self.root).unwrap()
    }

    pub fn root_mut(&mut self) -> &mut Block {
        self.arena.get_mut(self.root).unwrap()
    }

    pub fn root_id(&self) -> usize {
        self.root
    }

    pub fn get_block(&self, id: usize) -> &Block {
        self.arena.get(id).unwrap()
    }

    pub fn get_block_mut(&mut self, id: usize) -> &mut Block {
        self.arena.get_mut(id).unwrap()
    }

    pub fn make_child(&mut self, parent_id: usize, bi: BindingInfo) -> usize {
        let id = self.arena.len();

        let child = Block {
            bindings: UstrMap::default(),
            parent: Some(parent_id),
            id,
            block_binding: bi,
        };
        self.arena.push(child);
        id
    }

    pub fn get_parent(&self, block_id: usize) -> Option<&Block> {
        let block = self.get_block(block_id);
        block.parent.map(|p| self.get_block(p))
    }

    pub fn get_binding(&self, block_id: usize, name: Ustr) -> Option<VariableBinding> {
        let block = self.get_block(block_id);
        if let Some(binding) = block.bindings.get(&name) {
            return Some(binding.clone());
        }

        block.parent.and_then(|p| self.get_binding(p, name))
    }

    pub fn get_similar_names(
        &self,
        block_id: usize,
        name: &str,
        amount: u64,
        max_dst: u64,
    ) -> Vec<String> {
        let block = self.get_block(block_id);
        let mut similar_names = Vec::new();

        for key in block.bindings.keys() {
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

        if let Some(parent) = block.parent {
            let remaining = amount - result.len() as u64;
            if remaining > 0 {
                let mut parent_similar = self.get_similar_names(parent, name, remaining, max_dst);
                result.append(&mut parent_similar);
            }
        }

        result
    }

    fn create_binding_not_found_error(
        &self,
        block_id: usize,
        name: &str,
        src: &SourceInfo,
        sp: Span,
    ) -> YuuError {
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
        let similar = self.get_similar_names(block_id, name, 3, 3);
        if !similar.is_empty() {
            builder = builder.help(format!("Did you mean: {}?", similar.join(", ")));
        }

        builder.build()
    }

    pub fn resolve_variable(
        &self,
        block_id: usize,
        name: Ustr,
        src: &SourceInfo,
        sp: Span,
    ) -> Result<VariableBinding, Box<YuuError>> {
        self.get_binding(block_id, name).ok_or_else(|| {
            Box::new(self.create_binding_not_found_error(block_id, name.as_ref(), src, sp.clone()))
        })
    }
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
