use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    rc::Rc,
};

use hashbrown::HashMap;
use yuu_parse::Span;
use yuu_shared::ast::NodeId;

use crate::{
    binding_info::{BindingInfo, BindingInfoKind},
    semantic_error::SemanticError,
    type_info::{BuiltInType, FunctionType, TypeInfo, TypeInfoTable},
};

const MAX_SIMILAR_NAMES: u64 = 3;
const MIN_DST_SIMILAR_NAMES: u64 = 3;

#[derive(Clone)]
pub struct Block {
    pub bindings: HashMap<String, BindingInfoKind>,
    pub parent: Option<Rc<RefCell<Block>>>,
}

#[derive(Debug)]
pub enum FunctionOverloadError {
    NotAFunction,
    NoOverloadFound,
    BindingNotFound,
}

impl Block {
    pub fn root(tyt: &mut TypeInfoTable) -> Rc<RefCell<Self>> {
        let mut block = Self {
            bindings: HashMap::new(),
            parent: None,
        };

        block.predefine_builtins(tyt);

        Rc::new(RefCell::new(block))
    }

    pub fn get_binding_from_root(&self, name: &str) -> Option<BindingInfoKind> {
        if let Some(parent) = &self.parent {
            let mut current = parent.clone();

            loop {
                let next_parent = {
                    let borrow = current.as_ref().borrow();
                    borrow.parent.clone()
                };

                match next_parent {
                    Some(p) => current = p,
                    None => break,
                }
            }

            let binding = current.as_ref().borrow().bindings.get(name).cloned();
            return binding;
        } else {
            self.get_binding(name)
        }
    }

    pub fn get_binding(&self, name: &str) -> Option<BindingInfoKind> {
        if let Some(k) = self.bindings.get(name) {
            return Some(k.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.as_ref().borrow().get_binding(name);
        }

        None
    }

    pub fn from_parent(parent: Rc<RefCell<Block>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
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

        if let Some(parent) = &self.parent {
            let remaining = amount - result.len() as u64;
            if remaining > 0 {
                let mut parent_similar = parent
                    .as_ref()
                    .borrow()
                    .get_similar_names(name, remaining, min_dst);
                result.append(&mut parent_similar);
            }
        }

        result
    }

    pub fn declare_function(
        &mut self,
        name: String,
        id: NodeId,
        span: Span,
    ) -> Result<(), SemanticError> {
        let functions = self
            .bindings
            .entry(name)
            .or_insert(BindingInfoKind::Ambiguous(Rc::new(
                RefCell::new(Vec::new()),
            )));

        match functions {
            BindingInfoKind::Ambiguous(f) => {
                f.as_ref().borrow_mut().push(BindingInfo {
                    id,
                    src_location: Some(span),
                });
                Ok(())
            }
            BindingInfoKind::Unique(_) => {
                panic!("User bug: Cannot redefine a variable as a function");
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, id: NodeId, span: Span) -> bool {
        let variables = self.bindings.insert(
            name,
            BindingInfoKind::Unique(BindingInfo {
                id,
                src_location: Some(span),
            }),
        );

        match variables {
            Some(BindingInfoKind::Ambiguous(_)) => false,
            _ => true,
        }
    }

    pub fn resolve_function<'a, T>(
        &self,
        name: &str,
        type_info_table: &'a TypeInfoTable,
        args: &'a [&'a Rc<TypeInfo>],
        resolver: T,
    ) -> Result<Rc<TypeInfo>, FunctionOverloadError>
    where
        T: Fn(&BindingInfo, &FunctionType) -> Rc<TypeInfo>,
    {
        let overload = self.get_binding_from_root(name);

        overload.map(|binding| match binding {
            BindingInfoKind::Ambiguous(f) => {
                for binding in f.as_ref().borrow().iter() {
                    let func_type = type_info_table.types.get(&binding.id).expect(
                        "Compiler Bug: Block has a function binding, but can't find the associated type info",
                    );

                    match func_type.as_ref() {
                        TypeInfo::Function(func) => {
                            if func.args.len() != args.len() {
                                continue;
                            }

                            let mut found = true;
                            for (arg, expected) in args.iter().zip(func.args.iter()) {
                                if !arg.does_coerce_to_same_type(expected) {
                                    found = false;
                                    break;
                                }
                            }

                            if found {
                                return Ok(resolver(binding, func));
                            }
                        }
                        _ => unreachable!("Compiler Bug: Function binding is not a function type"),
                    }
                }
                Err(FunctionOverloadError::NoOverloadFound)
            }
            BindingInfoKind::Unique(_) => Err(FunctionOverloadError::NotAFunction),
        }).ok_or(FunctionOverloadError::BindingNotFound)?
    }

    fn register_binary_op(
        &mut self,
        type_info_table: &mut TypeInfoTable,
        op_name: &str,
        id: NodeId,
        type_: BuiltInType,
    ) {
        type_info_table.types.insert(
            id,
            Rc::new(TypeInfo::Function(FunctionType {
                args: Rc::new([
                    Rc::new(TypeInfo::BuiltIn(type_.clone())),
                    Rc::new(TypeInfo::BuiltIn(type_.clone())),
                ]),
                ret: Rc::new(TypeInfo::BuiltIn(type_)),
            })),
        );
        let _ = self.declare_function(op_name.to_string(), id, Span { start: 0, end: 0 });
    }

    pub fn predefine_builtins(&mut self, type_info_table: &mut TypeInfoTable) {
        // F32 operations
        self.register_binary_op(type_info_table, "add", -13, BuiltInType::F32);
        self.register_binary_op(type_info_table, "sub", -14, BuiltInType::F32);
        self.register_binary_op(type_info_table, "mul", -15, BuiltInType::F32);
        self.register_binary_op(type_info_table, "div", -16, BuiltInType::F32);

        // I64 operations
        self.register_binary_op(type_info_table, "add", -1, BuiltInType::I64);
        self.register_binary_op(type_info_table, "sub", -3, BuiltInType::I64);
        self.register_binary_op(type_info_table, "mul", -5, BuiltInType::I64);
        self.register_binary_op(type_info_table, "div", -7, BuiltInType::I64);

        // F64 operations
        self.register_binary_op(type_info_table, "add", -2, BuiltInType::F64);
        self.register_binary_op(type_info_table, "sub", -4, BuiltInType::F64);
        self.register_binary_op(type_info_table, "mul", -6, BuiltInType::F64);
        self.register_binary_op(type_info_table, "div", -8, BuiltInType::F64);
    }
}

fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.chars().count();
    let len2 = s2.chars().count();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
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
