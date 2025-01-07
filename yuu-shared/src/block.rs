use crate::scheduler::ResourceId;
use crate::Span;
use crate::{
    ast::NodeId,
    binding_info::{BindingInfo, BindingInfoKind},
    type_info::{FunctionType, PrimitiveType, TypeInfo, TypeInfoTable},
};
use hashbrown::HashMap;

use crate::semantic_error::SemanticError;

#[derive(Clone)]
pub struct Block {
    pub bindings: HashMap<String, BindingInfoKind>,
    pub parent: Option<usize>,
    pub root_block: *mut RootBlock,
    pub id: usize,
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

    pub fn make_child(&mut self) -> &mut Block {
        let id = self.id;
        let len = self.get_root_block().arena.len();
        let root_block = self.get_root_block_mut();

        let child = Block {
            bindings: HashMap::new(),
            parent: Some(id),
            root_block,
            id: len,
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
                panic!("User bug: Function {} already declared", name)
            }
            Some(BindingInfoKind::Ambiguous(funcs)) => {
                funcs.push(BindingInfo {
                    id,
                    src_location: Some(span),
                });
                Ok(())
            }
            None => {
                let funcs = vec![BindingInfo {
                    id,
                    src_location: Some(span),
                }];
                self.bindings
                    .insert(name, BindingInfoKind::Ambiguous(funcs));
                Ok(())
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, id: NodeId, span: Span) {
        self.bindings.insert(
            name,
            BindingInfoKind::Unique(BindingInfo {
                id,
                src_location: Some(span),
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
        args: &[&'static TypeInfo],
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
                    if func.args.len() != args.len() {
                        return Err(FunctionOverloadError::NoOverloadFound);
                    }

                    for (expected, actual) in func.args.iter().zip(args.iter()) {
                        if !actual.does_coerce_to_same_type(expected) {
                            return Err(FunctionOverloadError::NoOverloadFound);
                        }
                    }

                    Ok(resolve_ret_type(&binding_info, func))
                } else {
                    Err(FunctionOverloadError::NotAFunction)
                }
            }
            BindingInfoKind::Ambiguous(funcs) => {
                println!("funcs: {:?}", funcs);
                println!("type_info_table: {:?}", type_info_table);
                for func in funcs.iter() {
                    let func_type = type_info_table
                        .types
                        .get(&func.id)
                        .ok_or(FunctionOverloadError::NotAFunction)?;

                    if let TypeInfo::Function(func_type) = &**func_type {
                        if func_type.args.len() != args.len() {
                            continue;
                        }

                        let mut all_args_match = true;
                        for (expected, actual) in func_type.args.iter().zip(args.iter()) {
                            if !actual.does_coerce_to_same_type(expected) {
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
