use std::{fmt::Display, hash::Hasher, ops::Deref, sync::LazyLock};

use std::hash::Hash;

use crate::{
    ast::*,
    scheduler::{ResourceId, ResourceName},
};
use scc::HashMap;

pub struct GiveMePtrHashes<T: 'static>(&'static T);

impl<T> Hash for GiveMePtrHashes<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Cast to pointer and hash
        let ptr = self.0 as *const T;
        ptr.hash(state);
    }
}

impl<T> PartialEq for GiveMePtrHashes<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<T> Deref for GiveMePtrHashes<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<T> Eq for GiveMePtrHashes<T> {}
pub enum TypeCombination {
    Pointer(GiveMePtrHashes<TypeInfo>),
    Function(Vec<GiveMePtrHashes<TypeInfo>>, GiveMePtrHashes<TypeInfo>),
}

impl Hash for TypeCombination {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypeCombination::Pointer(ptr) => ptr.hash(state),
            TypeCombination::Function(args, ret) => {
                args.hash(state);
                ret.hash(state);
            }
        }
    }
}

impl PartialEq for TypeCombination {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeCombination::Pointer(a), TypeCombination::Pointer(b)) => a == b,
            (TypeCombination::Function(a, b), TypeCombination::Function(c, d)) => a == c && b == d,
            _ => false,
        }
    }
}

impl Eq for TypeCombination {}

pub struct TypeInterner {
    pub combination_to_type: HashMap<TypeCombination, Box<TypeInfo>>,
}

pub fn primitive_i64() -> &'static TypeInfo {
    const PRIMITIVE_U64: TypeInfo = TypeInfo::BuiltIn(PrimitiveType::I64);
    &PRIMITIVE_U64
}

pub fn primitive_f32() -> &'static TypeInfo {
    const PRIMITIVE_F32: TypeInfo = TypeInfo::BuiltIn(PrimitiveType::F32);
    &PRIMITIVE_F32
}

pub fn primitive_f64() -> &'static TypeInfo {
    const PRIMITIVE_F64: TypeInfo = TypeInfo::BuiltIn(PrimitiveType::F64);
    &PRIMITIVE_F64
}

pub fn primitive_nil() -> &'static TypeInfo {
    const PRIMITIVE_NIL: TypeInfo = TypeInfo::BuiltIn(PrimitiveType::Nil);
    &PRIMITIVE_NIL
}

pub fn primitive_bool() -> &'static TypeInfo {
    const PRIMITIVE_BOOL: TypeInfo = TypeInfo::BuiltIn(PrimitiveType::Bool);
    &PRIMITIVE_BOOL
}

pub fn inactive_type() -> &'static TypeInfo {
    const INACTIVE_TYPE: TypeInfo = TypeInfo::Inactive;
    &INACTIVE_TYPE
}

impl From<PrimitiveType> for &'static TypeInfo {
    fn from(value: PrimitiveType) -> Self {
        match value {
            PrimitiveType::I64 => primitive_i64(),
            PrimitiveType::F32 => primitive_f32(),
            PrimitiveType::F64 => primitive_f64(),
            PrimitiveType::Nil => primitive_nil(),
            PrimitiveType::Bool => primitive_bool(),
        }
    }
}

impl Default for TypeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInterner {
    pub fn new() -> Self {
        Self {
            combination_to_type: HashMap::new(),
        }
    }

    pub fn ptr_to(&'static self, ty: &'static TypeInfo) -> &'static TypeInfo {
        let key = TypeCombination::Pointer(GiveMePtrHashes(ty));
        let out = self
            .combination_to_type
            .entry(key)
            .or_insert_with(|| Box::new(TypeInfo::Pointer(ty)));

        // SAFETY: The Box lives as long as TypeInterner and we never remove from the map
        unsafe { &*(out.as_ref() as *const TypeInfo) }
    }

    pub fn deref_ptr(ty: &'static TypeInfo) -> &'static TypeInfo {
        match ty {
            TypeInfo::Pointer(inner) => inner,
            TypeInfo::BuiltIn(_) => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Function(_) => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Inactive => panic!("Cannot dereference non-pointer type: {}", ty),
        }
    }

    pub fn function_type(
        &'static self,
        args: &[&'static TypeInfo],
        ret: &'static TypeInfo,
    ) -> &'static TypeInfo {
        let key = TypeCombination::Function(
            args.iter().map(|arg| GiveMePtrHashes(*arg)).collect(),
            GiveMePtrHashes(ret),
        );

        let out = self.combination_to_type.entry(key).or_insert_with(|| {
            Box::new(TypeInfo::Function(FunctionType {
                args: args.to_vec(),
                ret,
            }))
        });

        // SAFETY: The Box lives as long as TypeInterner and we never remove from the map
        unsafe { &*(out.as_ref() as *const TypeInfo) }
    }
}

static TYPE_CACHE: LazyLock<TypeInterner> = LazyLock::new(TypeInterner::new);

#[derive(Clone, Debug)]
pub struct TypeInfoTable {
    pub types: hashbrown::HashMap<NodeId, &'static TypeInfo>,
}

impl ResourceId for TypeInfoTable {
    fn resource_name() -> ResourceName {
        "TypeInfoTable"
    }
}

impl Default for TypeInfoTable {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeInfoTable {
    pub fn new() -> Self {
        Self {
            types: hashbrown::HashMap::new(),
        }
    }

    pub fn unify_and_insert(
        &mut self,
        id: NodeId,
        ty: &'static TypeInfo,
    ) -> Result<&'static TypeInfo, UnificationError> {
        let existing = self.types.get(&id);
        let unified = if let Some(existing) = existing {
            ty.unify(existing)?
        } else {
            ty
        };
        self.types.insert(id, unified);
        Ok(unified)
    }

    pub fn unify_nil_and_insert(&mut self, id: NodeId) -> Result<&'static TypeInfo, UnificationError> {
        self.unify_and_insert(id, primitive_nil())
    }
}

#[derive(Clone, PartialEq, Eq, Copy, Debug)]
pub enum PrimitiveType {
    I64,
    F32,
    F64,
    Nil,
    Bool,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Nil => write!(f, "nil"),
            PrimitiveType::Bool => write!(f, "bool"),
        }
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|arg| format!("{}", arg))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "fn({}) -> {}", args, self.ret)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub args: Vec<&'static TypeInfo>,
    pub ret: &'static TypeInfo,
}

impl From<FunctionType> for &'static TypeInfo {
    fn from(value: FunctionType) -> Self {
        TYPE_CACHE.function_type(&value.args, value.ret)
    }
}

impl From<(&[&'static TypeInfo], &'static TypeInfo)> for &'static TypeInfo {
    fn from(value: (&[&'static TypeInfo], &'static TypeInfo)) -> Self {
        TYPE_CACHE.function_type(value.0, value.1)
    }
}

#[derive(Clone, Debug)]
pub enum TypeInfo {
    BuiltIn(PrimitiveType),
    Function(FunctionType),
    Pointer(&'static TypeInfo),
    Inactive, // Used to represent a 'inactive' type, i.e. a an expression which produces __no__ value and thus has no type
              // This happens for example when you 'unwind' in a block - the block does not have a value.
              // Example: Return in some child block - the block itself has no value, intrinsically it's the value of the top-most block.
}

impl TypeInfo {
    pub fn is_primitive(&self) -> bool {
        match self {
            TypeInfo::BuiltIn(_) => true,
            TypeInfo::Pointer(inner) => inner.is_primitive(),
            TypeInfo::Function(_) => false,
            TypeInfo::Inactive => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, TypeInfo::BuiltIn(PrimitiveType::Nil))
    }

    pub fn ptr_to(&'static self) -> &'static Self {
        TYPE_CACHE.ptr_to(self)
    }

    pub fn deref_ptr(&'static self) -> &'static Self {
        TypeInterner::deref_ptr(self)
    }

    pub fn is_exact_same_type(&self, other: &'static Self) -> bool {
        std::ptr::eq(self, other)
    }

    pub fn unify(&'static self, target: &'static TypeInfo) -> Result<&'static TypeInfo, UnificationError> {
        // Unification with inactive yields the other type
        match (self, target) {
            (TypeInfo::Inactive, _) => Ok(target),
            (_, TypeInfo::Inactive) => Ok(self),
            _ if self.is_exact_same_type(target) => Ok(target),
            _ => Err(UnificationError {
                from: self.to_string(),
                to: target.to_string(),
            })
        }
    }

    pub fn does_coerce_to_same_type(&'static self, other: &'static TypeInfo) -> bool {
        self.unify(other).is_ok()
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::BuiltIn(built_in) => write!(f, "{}", built_in),
            TypeInfo::Function(function_type) => write!(f, "{}", function_type),
            TypeInfo::Pointer(inner) => write!(f, "*{}", inner),
            TypeInfo::Inactive => write!(f, "<no value>"),
        }
    }
}

#[derive(Debug)]
pub struct UnificationError {
    pub from: String,
    pub to: String,
}
