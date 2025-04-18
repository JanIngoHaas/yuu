use std::{fmt::Display, hash::Hasher, ops::Deref, sync::LazyLock};

use std::hash::Hash;

use crate::error::YuuError;
use crate::{
    ast::*,
    scheduler::{ResourceId, ResourceName},
};
use indexmap::IndexMap;
use scc::HashMap;
use ustr::Ustr;

pub struct GiveMePtrHashes<T: 'static>(pub &'static T);

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

pub fn error_type() -> &'static TypeInfo {
    const ERROR_TYPE: TypeInfo = TypeInfo::Error;
    &ERROR_TYPE
}

impl<T> Eq for GiveMePtrHashes<T> {}
pub enum TypeCombination {
    Pointer(GiveMePtrHashes<TypeInfo>),
    Function((Vec<GiveMePtrHashes<TypeInfo>>, GiveMePtrHashes<TypeInfo>)),
    Struct(Ustr),
}

impl Hash for TypeCombination {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            TypeCombination::Pointer(ptr) => ptr.hash(state),
            TypeCombination::Function(args) => {
                args.hash(state);
            }
            TypeCombination::Struct(ustr) => {
                (*ustr).hash(state);
            }
        }
    }
}

impl PartialEq for TypeCombination {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeCombination::Pointer(a), TypeCombination::Pointer(b)) => a == b,
            (TypeCombination::Function(a), TypeCombination::Function(c)) => a == c,
            (TypeCombination::Struct(a), TypeCombination::Struct(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for TypeCombination {}

pub struct TypeInterner {
    pub combination_to_type: HashMap<TypeCombination, Box<TypeInfo>>,
}

pub fn primitive_i64() -> &'static TypeInfo {
    const PRIMITIVE_U64: TypeInfo = TypeInfo::BuiltInPrimitive(PrimitiveType::I64);
    &PRIMITIVE_U64
}

pub fn primitive_f32() -> &'static TypeInfo {
    const PRIMITIVE_F32: TypeInfo = TypeInfo::BuiltInPrimitive(PrimitiveType::F32);
    &PRIMITIVE_F32
}

pub fn primitive_f64() -> &'static TypeInfo {
    const PRIMITIVE_F64: TypeInfo = TypeInfo::BuiltInPrimitive(PrimitiveType::F64);
    &PRIMITIVE_F64
}

pub fn primitive_nil() -> &'static TypeInfo {
    const PRIMITIVE_NIL: TypeInfo = TypeInfo::BuiltInPrimitive(PrimitiveType::Nil);
    &PRIMITIVE_NIL
}

pub fn primitive_bool() -> &'static TypeInfo {
    const PRIMITIVE_BOOL: TypeInfo = TypeInfo::BuiltInPrimitive(PrimitiveType::Bool);
    &PRIMITIVE_BOOL
}

pub fn inactive_type() -> &'static TypeInfo {
    const INACTIVE_TYPE: TypeInfo = TypeInfo::Inactive;
    &INACTIVE_TYPE
}

pub fn ptr_to(ty: &'static TypeInfo) -> &'static TypeInfo {
    TYPE_CACHE.ptr_to(ty)
}

pub fn deref_ptr(ty: &'static TypeInfo) -> &'static TypeInfo {
    TypeInterner::deref_ptr(ty)
}

pub fn function_type(
    args: &[&'static TypeInfo],
    ret: &'static TypeInfo,
) -> (&'static FunctionType, &'static TypeInfo) {
    TYPE_CACHE.function_type(args, ret)
}

// pub fn function_type_from_args(
//     args: &[&'static TypeInfo],
// ) -> Option<(&'static FunctionType, &'static TypeInfo)> {
//     let key = TypeCombination::Function(args.iter().map(|arg| GiveMePtrHashes(*arg)).collect());
//     let out = TYPE_CACHE.combination_to_type.get(&key).and_then(|ty| {
//         let out = unsafe { &*(ty.as_ref() as *const TypeInfo) };
//         if let TypeInfo::Function(f) = out {
//             Some((f, out))
//         } else {
//             None
//         }
//     });
//     return out;
// }

pub fn struct_type(name: Ustr) -> &'static TypeInfo {
    TYPE_CACHE.struct_type(name)
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

    pub fn struct_type(&'static self, struct_: Ustr) -> &'static TypeInfo {
        let key = TypeCombination::Struct(struct_);
        let out = self
            .combination_to_type
            .entry(key)
            .or_insert_with(|| Box::new(TypeInfo::Struct(StructType { name: struct_ })));

        // SAFETY: The Box lives as long as TypeInterner and we never remove from the map
        unsafe { &*(out.as_ref() as *const TypeInfo) }
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
            TypeInfo::BuiltInPrimitive(_) => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Function(_) => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Inactive => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Error => panic!("Cannot dereference non-pointer type: {}", ty),
            TypeInfo::Struct(struct_type) => {
                panic!("Cannot dereference non-pointer type: {}", struct_type.name)
            }
        }
    }

    pub fn function_type(
        &'static self,
        args: &[&'static TypeInfo],
        ret: &'static TypeInfo,
    ) -> (&'static FunctionType, &'static TypeInfo) {
        let key = TypeCombination::Function((
            args.iter().map(|arg| GiveMePtrHashes(*arg)).collect(),
            GiveMePtrHashes(ret),
        ));

        let out = self.combination_to_type.entry(key).or_insert_with(|| {
            Box::new(TypeInfo::Function(FunctionType {
                args: args.to_vec(),
                ret,
            }))
        });

        // SAFETY: The Box lives as long as TypeInterner and we never remove from the map
        let type_info = unsafe { &*(out.as_ref() as *const TypeInfo) };

        if let TypeInfo::Function(f) = type_info {
            (f, type_info)
        } else {
            unreachable!("We just created this as a FunctionType")
        }
    }
}

static TYPE_CACHE: LazyLock<TypeInterner> = LazyLock::new(TypeInterner::new);

#[derive(Clone, Debug)]
pub struct TypeInfoTable {
    pub types: IndexMap<NodeId, &'static TypeInfo>,
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
            types: IndexMap::new(),
        }
    }

    pub fn insert(&mut self, id: NodeId, ty: &'static TypeInfo) {
        self.types.insert(id, ty);
    }

    pub fn get(&self, id: NodeId) -> Option<&'static TypeInfo> {
        self.types.get(&id).copied()
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

    pub fn unify_nil_and_insert(
        &mut self,
        id: NodeId,
    ) -> Result<&'static TypeInfo, UnificationError> {
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

impl<'a> From<&'a FunctionType> for &'static TypeInfo {
    fn from(value: &'a FunctionType) -> Self {
        TYPE_CACHE.function_type(&value.args, value.ret).1
    }
}

impl From<FunctionType> for &'static TypeInfo {
    fn from(value: FunctionType) -> Self {
        TYPE_CACHE.function_type(&value.args, value.ret).1
    }
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub name: Ustr,
}

#[derive(Clone, Debug)]
pub enum TypeInfo {
    BuiltInPrimitive(PrimitiveType),
    Function(FunctionType),
    Pointer(&'static TypeInfo),
    Inactive, // Used to represent a 'inactive' type, i.e. a an expression which produces __no__ value and thus has no type
    // This happens for example when you 'unwind' in a block - the block does not have a value.
    // Example: Return in some child block - the block itself has no value, intrinsically it's the value of the top-most block.
    Error,
    Struct(StructType),
}

impl TypeInfo {
    pub fn is_ptr(&self) -> bool {
        match self {
            TypeInfo::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            TypeInfo::BuiltInPrimitive(_) => true,
            TypeInfo::Pointer(inner) => inner.is_primitive(),
            TypeInfo::Function(_) => false,
            TypeInfo::Inactive => false,
            TypeInfo::Error => false,
            TypeInfo::Struct(_) => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, TypeInfo::BuiltInPrimitive(PrimitiveType::Nil))
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

    pub fn unify(
        &'static self,
        target: &'static TypeInfo,
    ) -> Result<&'static TypeInfo, UnificationError> {
        // Unification with inactive yields the other type
        match (self, target) {
            (TypeInfo::Inactive, x) | (x, TypeInfo::Inactive) => Ok(x),
            _ if self.is_exact_same_type(target) => Ok(target),
            _ => Err(UnificationError {
                left: self.to_string(),
                right: target.to_string(),
            }),
        }
    }

    pub fn can_unify(&'static self, other: &'static TypeInfo) -> bool {
        self.unify(other).is_ok()
    }
}

impl Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::BuiltInPrimitive(built_in) => write!(f, "{}", built_in),
            TypeInfo::Function(function_type) => write!(f, "{}", function_type),
            TypeInfo::Pointer(inner) => write!(f, "*{}", inner),
            TypeInfo::Inactive => write!(f, "<no value>"),
            TypeInfo::Error => write!(f, "<error>"),
            TypeInfo::Struct(struct_type) => write!(f, "{}", struct_type.name),
        }
    }
}

#[derive(Debug)]
pub struct UnificationError {
    pub left: String,
    pub right: String,
}

impl UnificationError {
    pub fn into_yuu_error(
        &self,
        info: SourceInfo,
        span: impl Into<miette::SourceSpan>,
    ) -> YuuError {
        YuuError::builder().kind(crate::error::ErrorKind::TypeMismatch)
            .message("Type mismatch: cannot unify types".to_string())
            .source(info.source, info.file_name)
            .span(span, format!("expected '{}', found '{}'", self.right, self.left))
            .help("These types are incompatible. Consider adding an explicit type conversion or changing your expression to match the expected type.".to_string())
            .build()
    }
}
