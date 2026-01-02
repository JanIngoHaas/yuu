use std::str::FromStr;

use ustr::Ustr;

use crate::utils::collections::{FastHashMap, UstrHashMap, UstrIndexMap, UstrIndexSet};
use crate::utils::type_info_table::{
    FunctionType, GiveMePtrHashes, TypeInfo, function_type, primitive_bool,
    primitive_f32, primitive_f64, primitive_i64, primitive_nil, primitive_u64, struct_type,
    union_type,
};
use crate::utils::BindingInfo;
use crate::{
    pass_diagnostics::levenshtein_distance,
    pass_parse::ast::{InternUstr, NodeId},
};

#[derive(Clone)]
pub struct StructFieldInfo {
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

#[derive(Clone)]
pub struct UnionFieldInfo {
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub field_idx: u64,
    pub binding_info: BindingInfo,
}

#[derive(Clone)]
pub struct StructInfo {
    pub fields: UstrIndexMap<StructFieldInfo>,
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

#[derive(Copy, Clone)]
pub struct EnumInfo<'a> {
    pub wrapper_info: &'a StructInfo,
    pub variants_info: &'a UnionInfo,
}

pub struct EnumInfoMut<'a> {
    pub wrapper_info: &'a mut StructInfo,
    pub variants_info: &'a mut UnionInfo,
}

#[derive(Clone)]
pub struct UnionInfo {
    pub name: Ustr,
    pub fields: UstrIndexMap<UnionFieldInfo>,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

/// Combined type for resolving either struct or enum information - this is just for convenience -> Represents the Discriminant
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComposableTypeDiscriminant {
    Struct,
    Union,
}

impl ComposableTypeDiscriminant {
    pub fn as_str(&self) -> &'static str {
        match self {
            ComposableTypeDiscriminant::Struct => "struct",
            ComposableTypeDiscriminant::Union => "union",
        }
    }
}

#[derive(Clone, Copy)]
pub enum ComposableTypeInfo<'a> {
    Struct(&'a StructInfo),
    Union(&'a UnionInfo),
}

impl<'a> ComposableTypeInfo<'a> {
    pub fn discriminant(&self) -> ComposableTypeDiscriminant {
        match self {
            ComposableTypeInfo::Struct(_) => ComposableTypeDiscriminant::Struct,
            ComposableTypeInfo::Union(_) => ComposableTypeDiscriminant::Union,
        }
    }

    pub fn ty(&self) -> &'static TypeInfo {
        match self {
            ComposableTypeInfo::Struct(info) => info.ty,
            ComposableTypeInfo::Union(info) => info.ty,
        }
    }

    pub fn name(&self) -> Ustr {
        match self {
            ComposableTypeInfo::Struct(info) => info.name,
            ComposableTypeInfo::Union(info) => info.name,
        }
    }

    pub fn iterate(
        &self,
    ) -> impl Iterator<
        Item = (
            Ustr,
            &'static TypeInfo,
            BindingInfo,
            ComposableTypeDiscriminant,
        ),
    > {
        let out: Box<
            dyn Iterator<
                Item = (
                    Ustr,
                    &'static TypeInfo,
                    BindingInfo,
                    ComposableTypeDiscriminant,
                ),
            >,
        > = match self {
            ComposableTypeInfo::Struct(info) => Box::new(info.fields.iter().map(|(n, i)| {
                (
                    *n,
                    i.ty,
                    i.binding_info.clone(),
                    ComposableTypeDiscriminant::Struct,
                )
            })),
            ComposableTypeInfo::Union(info) => Box::new(info.fields.iter().map(|(n, i)| {
                (
                    *n,
                    i.ty,
                    i.binding_info.clone(),
                    ComposableTypeDiscriminant::Union,
                )
            })),
        };
        out
    }
}

#[derive(Clone)]
pub struct FunctionInfo {
    pub name: Ustr,
    pub ty: &'static FunctionType,
    pub general_ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

pub struct TypeRegistry {
    structs: UstrHashMap<StructInfo>,
    enums: UstrHashMap<(Ustr, Ustr)>, // wrapper struct name + union name
    unions: UstrHashMap<UnionInfo>,
    functions: UstrHashMap<FastHashMap<Vec<GiveMePtrHashes<TypeInfo>>, FunctionInfo>>, // Maps from Name -> (types of Args -> FunctionInfo).
    structural_types_by_node: FastHashMap<NodeId, &'static TypeInfo>, // Cache structural types by their definition NodeId
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut reg = Self {
            structs: UstrHashMap::default(),
            enums: UstrHashMap::default(),
            unions: UstrHashMap::default(),
            functions: UstrHashMap::default(),
            structural_types_by_node: FastHashMap::default(),
        };

        // Use a proper IdGenerator for built-in operators
        let mut id_gen = crate::pass_parse::add_ids::IdGenerator::new();
        let mut next = || id_gen.next_non_expr();

        // F32 operations
        reg.register_binary_op(
            "_add".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_f32(),
        );
        reg.register_binary_op(
            "_sub".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_f32(),
        );
        reg.register_binary_op(
            "_mul".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_f32(),
        );
        reg.register_binary_op(
            "_div".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_f32(),
        );
        reg.register_binary_op(
            "_mod".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_f32(),
        );
        reg.register_binary_op(
            "_eq".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_lt".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_gt".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_le".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ge".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ne".intern(),
            next(),
            &[primitive_f32(), primitive_f32()],
            primitive_bool(),
        );

        // I64 operations
        reg.register_binary_op(
            "_add".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_i64(),
        );
        reg.register_binary_op(
            "_sub".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_i64(),
        );
        reg.register_binary_op(
            "_mul".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_i64(),
        );
        reg.register_binary_op(
            "_div".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_i64(),
        );
        reg.register_binary_op(
            "_mod".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_i64(),
        );
        reg.register_binary_op(
            "_eq".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_lt".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_gt".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_le".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ge".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ne".intern(),
            next(),
            &[primitive_i64(), primitive_i64()],
            primitive_bool(),
        );

        // U64 operations
        reg.register_binary_op(
            "_add".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_u64(),
        );
        reg.register_binary_op(
            "_sub".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_u64(),
        );
        reg.register_binary_op(
            "_mul".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_u64(),
        );
        reg.register_binary_op(
            "_div".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_u64(),
        );
        reg.register_binary_op(
            "_mod".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_u64(),
        );
        reg.register_binary_op(
            "_eq".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_lt".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_gt".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_le".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ge".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ne".intern(),
            next(),
            &[primitive_u64(), primitive_u64()],
            primitive_bool(),
        );

        // F64 operations
        reg.register_binary_op(
            "_add".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_f64(),
        );
        reg.register_binary_op(
            "_sub".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_f64(),
        );
        reg.register_binary_op(
            "_mul".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_f64(),
        );
        reg.register_binary_op(
            "_div".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_f64(),
        );
        reg.register_binary_op(
            "_mod".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_f64(),
        );
        reg.register_binary_op(
            "_eq".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_lt".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_gt".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_le".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ge".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );
        reg.register_binary_op(
            "_ne".intern(),
            next(),
            &[primitive_f64(), primitive_f64()],
            primitive_bool(),
        );

        // Unary operations for i64
        reg.register_unary_op("_neg".intern(), next(), primitive_i64(), primitive_i64());
        reg.register_unary_op("_pos".intern(), next(), primitive_i64(), primitive_i64());

        // Unary operations for u64
        reg.register_unary_op("_pos".intern(), next(), primitive_u64(), primitive_u64());

        // Unary operations for f32
        reg.register_unary_op("_neg".intern(), next(), primitive_f32(), primitive_f32());
        reg.register_unary_op("_pos".intern(), next(), primitive_f32(), primitive_f32());

        // Unary operations for f64
        reg.register_unary_op("_neg".intern(), next(), primitive_f64(), primitive_f64());
        reg.register_unary_op("_pos".intern(), next(), primitive_f64(), primitive_f64());

        reg
    }

    // pub fn bind_type_to_node(&mut self, id: NodeId, ty: &'static TypeInfo) {
    //     self.type_info_table.types.insert(id, ty);
    // }

    // pub fn bind_type_to_ident_expr(
    //     &mut self,
    //     block: &mut Block,
    //     name: Ustr,
    //     id: NodeId,
    //     ty: &'static TypeInfo,
    // ) {
    //     block.insert_variable(name, id, None, false);
    //     self.type_info_table.types.insert(id, ty);
    // }

    fn register_binary_op(
        &mut self,
        name: Ustr,
        id: NodeId,
        operand_type: &[&'static TypeInfo],
        ret_type: &'static TypeInfo,
    ) {
        let binding_info = BindingInfo {
            id,
            src_location: None,
        };

        let out = self.add_function(operand_type, ret_type, name, binding_info);
        debug_assert!(out);
    }

    fn register_unary_op(
        &mut self,
        name: Ustr,
        id: NodeId,
        operand_type: &'static TypeInfo,
        ret_type: &'static TypeInfo,
    ) {
        let binding_info = BindingInfo {
            id,
            src_location: None,
        };

        let out = self.add_function(&[operand_type], ret_type, name, binding_info);
        debug_assert!(out);
    }

    pub fn all_structs(&self) -> &UstrHashMap<StructInfo> {
        &self.structs
    }

    pub fn all_unions(&self) -> &UstrHashMap<UnionInfo> {
        &self.unions
    }

    pub fn all_enums<'a>(&'a self) -> impl Iterator<Item = EnumInfo<'a>> + 'a {
        self.enums.keys().flat_map(
            |n| self.resolve_enum(*n)
        )
    }

    pub fn add_struct(
        &mut self,
        fields: UstrIndexMap<StructFieldInfo>,
        name: Ustr,
        binding_info: BindingInfo,
    ) -> bool {
        let ty = struct_type(name);
        let info = StructInfo {
            fields,
            name,
            ty,
            binding_info: binding_info.clone(),
        };
        
        // Cache the struct type by its definition NodeId
        self.structural_types_by_node.insert(binding_info.id, ty);
        
        self.structs.insert(info.name, info).is_none()
    }

    pub fn add_enum(
        &mut self,
        name: Ustr,
        variants: UstrIndexMap<UnionFieldInfo>,
        definition_location: BindingInfo,
    ) -> bool {
        let uty = union_type(name);
        let union_name: Ustr = format!("_U_{}", name).intern();
        
        let union_info = UnionInfo {
            ty: uty,
            name: union_name,
            fields: variants,
            binding_info: definition_location.clone(),
        };

        if self.unions.insert(union_name, union_info).is_some() {
            return false;
        }
        
        let struct_name: Ustr = format!("_S_{}", name).intern();
        let sty = struct_type(struct_name);
        let fields = vec![
            StructFieldInfo {
                name: "tag".intern(),
                ty: primitive_u64(),
                binding_info: definition_location.clone(),
            },
            StructFieldInfo {
                name: "payload".intern(),
                ty: uty,
                binding_info: definition_location.clone(),
            },
        ];

        let wrapper_struct_info = StructInfo {
            fields: fields.into_iter().map(|f| (f.name, f)).collect(),
            name: struct_name,
            ty: sty,
            binding_info: definition_location.clone(),
        };

        if self.structs.insert(wrapper_struct_info.name, wrapper_struct_info).is_some() {
            return false;
        }

        // Cache the enum type by its definition NodeId
        self.structural_types_by_node.insert(definition_location.id, sty);
        self.enums.insert(name, (struct_name, union_name)).is_none()
    }

    pub fn add_union(
        &mut self,
        name: Ustr,
        fields: UstrIndexMap<UnionFieldInfo>,
        ty: &'static TypeInfo,
        binding_info: BindingInfo,
    ) -> bool {
        let union_info = UnionInfo { name, fields, ty, binding_info: binding_info.clone() };
        self.unions.insert(name, union_info).is_none()
    }

    pub fn add_function(
        &mut self,
        arg_type: &[&'static TypeInfo],
        ret_type: &'static TypeInfo,
        name: Ustr,
        binding_info: BindingInfo,
    ) -> bool {
        let (func_type, general_type) = function_type(arg_type, ret_type);
        let info = FunctionInfo {
            name,
            ty: func_type,
            general_ty: general_type,
            binding_info: binding_info.clone(),
        };

        // Cache the function type by its definition NodeId
        self.structural_types_by_node.insert(binding_info.id, general_type);

        let funcs = self.functions.entry(info.name).or_default();

        let out = funcs
            .insert(arg_type.iter().map(|x| GiveMePtrHashes(*x)).collect(), info)
            .is_none();

        // TODO: Make this an actual error that gets printed to the user
        debug_assert!(out, "Function overload already exists");

        out
    }

    pub fn resolve_struct(&self, name: Ustr) -> Option<&StructInfo> {
        self.structs.get(&name)
    }

    pub fn resolve_struct_mut(&mut self, name: Ustr) -> Option<&mut StructInfo> {
        self.structs.get_mut(&name)
    }

    pub fn resolve_enum(&self, name: Ustr) -> Option<EnumInfo<'_>> {
        let (struct_name, union_name) = self.enums.get(&name)?;
        Some(EnumInfo {
            wrapper_info: self.structs.get(struct_name).expect("Compiler Bug: Enum name registered, but wrapper struct not found"),
            variants_info: self.unions.get(union_name).expect("Compiler Bug: Enum name registered, but variants union not found"),
        })
    }

    pub fn resolve_enum_mut(&mut self, name: Ustr) -> Option<EnumInfoMut<'_>> {
        let (struct_name, union_name) = self.enums.get(&name)?;
        Some(EnumInfoMut {
            wrapper_info: self.structs.get_mut(struct_name).expect("Compiler Bug: Enum name registered, but wrapper struct not found"),
            variants_info: self.unions.get_mut(union_name).expect("Compiler Bug: Enum name registered, but variants union not found"),
        })
    }

    pub fn resolve_union(&self, name: Ustr) -> Option<&UnionInfo> {
        self.unions.get(&name)
    }

    pub fn resolve_union_mut(&mut self, name: Ustr) -> Option<&mut UnionInfo> {
        self.unions.get_mut(&name)
    }

    /// Resolves either a struct, enum or union type by name
    pub fn resolve_composable_type(&self, name: Ustr) -> Option<ComposableTypeInfo<'_>> {
        if let Some(struct_info) = self.resolve_struct(name) {
            Some(ComposableTypeInfo::Struct(struct_info))
        } else {
            self.resolve_union(name).map(ComposableTypeInfo::Union)
        }
    }

    pub fn resolve_type(&self, name: Ustr) -> Option<&'static TypeInfo> {
        // First try primitive types
        let name_str = name.as_str();
        match name_str {
            "i64" => Some(primitive_i64()),
            "u64" => Some(primitive_u64()),
            "f32" => Some(primitive_f32()),
            "f64" => Some(primitive_f64()),
            "bool" => Some(primitive_bool()),
            "nil" => Some(primitive_nil()),
            _ => {
                self.resolve_composable_type(name).map(|info| info.ty())
            }
        }
    }

    pub fn get_similar_names_func(&self, name: Ustr, max_dst: usize) -> Vec<Ustr> {
        self.functions
            .keys()
            .filter(|x| levenshtein_distance(name.as_str(), x.as_str()) <= max_dst)
            .cloned()
            .collect()
    }

    pub fn get_similar_names_struct(&self, name: Ustr, max_dst: usize) -> Vec<Ustr> {
        self.structs
            .keys()
            .filter(|x| levenshtein_distance(name.as_str(), x.as_str()) <= max_dst)
            .cloned()
            .collect()
    }

    /// Cache a structural type by its NodeId (for functions, structs, enums)
    pub fn cache_structural_type(&mut self, node_id: NodeId, type_info: &'static TypeInfo) {
        self.structural_types_by_node.insert(node_id, type_info);
    }

    /// Get a cached structural type by its NodeId, returns unknown_type() if not found
    pub fn get_structural_type(&self, node_id: NodeId) -> &'static TypeInfo {
        self.structural_types_by_node.get(&node_id).copied().unwrap_or_else(|| crate::utils::type_info_table::unknown_type())
    }

    pub fn resolve_function(
        &self,
        name: Ustr,
        args: &[&'static TypeInfo],
    ) -> Result<FunctionInfo, Vec<FunctionInfo>> {
        let funcs = self.functions.get(&name);
        if let Some(funcs) = funcs {
            let key = args.iter().map(|x| GiveMePtrHashes(*x)).collect::<Vec<_>>();
            let finfo = funcs.get(&key);
            match finfo {
                Some(finfo) => Ok(finfo.clone()),
                None => {
                    let candidates = funcs.values().cloned().collect();
                    Err(candidates)
                }
            }
        } else {
            Err(Vec::default())
        }
    }
}
