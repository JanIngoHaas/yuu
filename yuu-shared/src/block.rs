use crate::scheduler::ResourceId;
use crate::Span;
use crate::{
    ast::NodeId,
    binding_info::{BindingInfo, BindingInfoKind},
    type_info::{FunctionType, PrimitiveType, TypeInfo, TypeInfoTable},
};
use hashbrown::HashMap;

use crate::semantic_error::SemanticError;

pub const FUNC_BLOCK_NAME: &str = "_fn";

#[derive(Clone)]
pub struct Block {
    pub bindings: HashMap<String, BindingInfoKind>,
    pub parent: Option<usize>,
    pub root_block: *mut RootBlock,
    pub id: usize,
    pub named_block_binding: Option<(String, BindingInfo)>,
}

#[derive(Copy, Clone)]
pub struct BlockIterator<'a> {
    current: &'a Block,
    root_block: &'a RootBlock,
}

impl<'a> BlockIterator<'a> {
    pub fn new(start: &'a Block) -> Self {
        Self {
            current: start,
            root_block: start.get_root_block(),
        }
    }

    /// Simply move to next block in arena
    pub fn descend(&mut self) {
        let next_id = self.current.id + 1;
        if let Some(next_block) = self.root_block.arena.get(next_id) {
            self.current = next_block;
        }
    }

    /// Get current block without advancing
    pub fn peek(&self) -> &'a Block {
        self.current
    }
}

impl<'a> Iterator for BlockIterator<'a> {
    type Item = &'a Block;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.current)
    }
}

unsafe impl Send for RootBlock {}

impl ResourceId for Box<RootBlock> {
    fn resource_name() -> &'static str {
        "RootBlock"
    }
}

#[derive(Debug)]
pub enum FunctionOverloadError {
    NotAFunction,
    NoOverloadFound,
    BindingNotFound,
}

// TODO: Wrap this in a box; we have pointers to it. When the root block is moved, the pointers are invalidated and point to garbage / other data.
pub struct RootBlock {
    arena: Vec<Block>,
    root: usize,
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
        if let Some(BindingInfoKind::Unique(binding)) = binding {
            return Some(binding.clone());
        }
        debug_assert!(
            binding.is_none(),
            "Compiler bug: binding {} is not unique",
            name
        );
        None
    }

    pub fn get_name(&self, searched_name: &str) -> Option<BindingInfo> {
        if let Some((name, binding)) = self.named_block_binding.as_ref() {
            if name == searched_name {
                return Some(binding.clone());
            }
        }

        self.get_parent().and_then(|p| p.get_name(searched_name))
    }

    pub fn get_fn_block_name(&self) -> BindingInfo {
        self.get_name(FUNC_BLOCK_NAME).expect("Compiler bug: Every function has to have a root block and this is automatically assigned by the compiler. This one apparently doesn't have one.")
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
    ) -> Result<(), SemanticError> {
        match self.bindings.get_mut(&name) {
            Some(BindingInfoKind::Unique(_)) => {
                panic!(
                    "Bug: Function {} already declared - not sure if this should ever happen",
                    name
                )
            }
            Some(BindingInfoKind::Ambiguous(funcs)) => {
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
                self.bindings
                    .insert(name, BindingInfoKind::Ambiguous(funcs));
                Ok(())
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, id: NodeId, span: Span, is_mut: bool) {
        self.bindings.insert(
            name,
            BindingInfoKind::Unique(BindingInfo {
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
        // F32 operations
        self.register_binary_op(tyt, "add", -13, PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "sub", -14, PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "mul", -15, PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "div", -16, PrimitiveType::F32, PrimitiveType::F32);
        self.register_binary_op(tyt, "eq", -17, PrimitiveType::F32, PrimitiveType::Bool);

        // I64 operations
        self.register_binary_op(tyt, "add", -1, PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "sub", -3, PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "mul", -5, PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "div", -7, PrimitiveType::I64, PrimitiveType::I64);
        self.register_binary_op(tyt, "eq", -20, PrimitiveType::I64, PrimitiveType::Bool);

        // F64 operations
        self.register_binary_op(tyt, "add", -2, PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "sub", -4, PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "mul", -6, PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "div", -8, PrimitiveType::F64, PrimitiveType::F64);
        self.register_binary_op(tyt, "eq", -9, PrimitiveType::F64, PrimitiveType::Bool);
    }

    pub fn resolve_function<T>(
        &self,
        name: &str,
        type_info_table: &TypeInfoTable,
        resolve_arg_types: &[&'static TypeInfo],
        resolve_ret_type: T,
    ) -> Result<&'static TypeInfo, FunctionOverloadError>
    where
        T: Fn(&BindingInfo, &FunctionType) -> &'static TypeInfo,
    {
        let binding = self
            .get_binding(name)
            .ok_or(FunctionOverloadError::BindingNotFound)?;

        match binding {
            BindingInfoKind::Unique(binding_info) => {
                let func_type = type_info_table
                    .types
                    .get(&binding_info.id)
                    .ok_or(FunctionOverloadError::NotAFunction)?;

                if let TypeInfo::Function(func) = &**func_type {
                    if func.args.len() != resolve_arg_types.len() {
                        return Err(FunctionOverloadError::NoOverloadFound);
                    }

                    for (expected, actual) in func.args.iter().zip(resolve_arg_types.iter()) {
                        if !actual.is_exact_same_type(expected) {
                            return Err(FunctionOverloadError::NoOverloadFound);
                        }
                    }

                    Ok(resolve_ret_type(&binding_info, func))
                } else {
                    Err(FunctionOverloadError::NotAFunction)
                }
            }
            BindingInfoKind::Ambiguous(funcs) => {
                for func in funcs.iter() {
                    let func_type = type_info_table
                        .types
                        .get(&func.id)
                        .ok_or(FunctionOverloadError::NotAFunction)?;

                    if let TypeInfo::Function(func_type) = &**func_type {
                        if func_type.args.len() != resolve_arg_types.len() {
                            continue;
                        }

                        let mut all_args_match = true;
                        for (expected, actual) in
                            func_type.args.iter().zip(resolve_arg_types.iter())
                        {
                            if !actual.is_exact_same_type(expected) {
                                all_args_match = false;
                                break;
                            }
                        }

                        if all_args_match {
                            return Ok(resolve_ret_type(func, func_type));
                        }
                    }
                }
                Err(FunctionOverloadError::NoOverloadFound)
            }
        }
    }

    pub fn get_similar_names(&self, name: &str, amount: u64, min_dst: u64) -> Vec<String> {
        let mut similar_names = Vec::new();

        for key in self.bindings.keys() {
            let distance = levenshtein_distance(name, key);
            if distance > 0 && distance <= min_dst as usize {
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
                let mut parent_similar = parent.get_similar_names(name, remaining, min_dst);
                result.append(&mut parent_similar);
            }
        }

        result
    }

    pub fn iter(&self) -> BlockIterator {
        BlockIterator::new(self)
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
