use std::hash::BuildHasherDefault;

use indexmap::{IndexMap, IndexSet};
use logos::Span;
use ustr::{IdentityHasher, Ustr};

use crate::pass_type_inference::enum_type;
use crate::{
    pass_diagnostics::levenshtein_distance,
    pass_parse::ast::{InternUstr, NodeId},
    pass_type_inference::{
        binding_info::BindingInfo,
        type_info::{
            FunctionType, GiveMePtrHashes, TypeInfo, TypeInfoTable, function_type, primitive_bool,
            primitive_f32, primitive_f64, primitive_i64, primitive_u64, struct_type,
        },
    },
    pass_yir_lowering::block::{BindingTable, Block},
};

#[derive(Clone)]
pub struct StructFieldInfo {
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

#[derive(Clone, Debug)]
pub struct EnumVariantInfo {
    pub variant_name: Ustr,
    pub variant_idx: u64,
    pub variant: Option<&'static TypeInfo>,
    pub binding_info: BindingInfo,
}

pub type FieldsMap<V> = IndexMap<Ustr, V, BuildHasherDefault<IdentityHasher>>;
pub type IndexUstrSet = IndexSet<Ustr, BuildHasherDefault<IdentityHasher>>;
pub type IndexUstrMap<V> = FieldsMap<V>;

#[derive(Clone)]
pub struct StructInfo {
    pub fields: FieldsMap<StructFieldInfo>,
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

#[derive(Clone)]
pub struct EnumInfo {
    pub variants: FieldsMap<EnumVariantInfo>,
    pub name: Ustr,
    pub ty: &'static TypeInfo,
    pub binding_info: BindingInfo,
}

/// Combined type for resolving either struct or enum information - this is just for convenience -> Represents the Discriminant
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserDefinedTypeDiscriminant {
    Struct,
    Enum,
}

impl UserDefinedTypeDiscriminant {
    pub fn as_str(&self) -> &'static str {
        match self {
            UserDefinedTypeDiscriminant::Struct => "struct",
            UserDefinedTypeDiscriminant::Enum => "enum",
        }
    }
}

#[derive(Clone, Copy)]
pub enum StructOrEnumInfo<'a> {
    Struct(&'a StructInfo),
    Enum(&'a EnumInfo),
}

impl<'a> StructOrEnumInfo<'a> {
    pub fn discriminant(&self) -> UserDefinedTypeDiscriminant {
        match self {
            StructOrEnumInfo::Struct(_) => UserDefinedTypeDiscriminant::Struct,
            StructOrEnumInfo::Enum(_) => UserDefinedTypeDiscriminant::Enum,
        }
    }

    pub fn ty(&self) -> &'static TypeInfo {
        match self {
            StructOrEnumInfo::Struct(info) => info.ty,
            StructOrEnumInfo::Enum(info) => info.ty,
        }
    }

    pub fn name(&self) -> Ustr {
        match self {
            StructOrEnumInfo::Struct(info) => info.name,
            StructOrEnumInfo::Enum(info) => info.name,
        }
    }

    pub fn iterate(
        &self,
    ) -> impl Iterator<
        Item = (
            Ustr,
            &'static TypeInfo,
            BindingInfo,
            UserDefinedTypeDiscriminant,
        ),
    > {
        let out: Box<
            dyn Iterator<
                Item = (
                    Ustr,
                    &'static TypeInfo,
                    BindingInfo,
                    UserDefinedTypeDiscriminant,
                ),
            >,
        > = match self {
            StructOrEnumInfo::Struct(info) => Box::new(info.fields.iter().map(|(n, i)| {
                (
                    *n,
                    i.ty,
                    i.binding_info.clone(),
                    UserDefinedTypeDiscriminant::Struct,
                )
            })),
            StructOrEnumInfo::Enum(info) => {
                Box::new(info.variants.iter().filter_map(|(n, evi)| {
                    evi.variant.map(|v| {
                        (
                            *n,
                            v,
                            evi.binding_info.clone(),
                            UserDefinedTypeDiscriminant::Enum,
                        )
                    })
                }))
            }
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
    structs: FieldsMap<StructInfo>,
    enums: FieldsMap<EnumInfo>,
    functions: FieldsMap<IndexMap<Vec<GiveMePtrHashes<TypeInfo>>, FunctionInfo>>, // Maps from Name -> (types of Args -> FunctionInfo).
    pub type_info_table: TypeInfoTable,
    pub bindings: BindingTable,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut reg = Self {
            structs: FieldsMap::default(),
            enums: FieldsMap::default(),
            functions: FieldsMap::default(),
            type_info_table: TypeInfoTable::new(),
            bindings: BindingTable::default(),
        };

        let mut id_counter = 0;
        let mut next = || {
            id_counter -= 1;
            id_counter
        };

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

    pub fn add_literal(&mut self, id: NodeId, ty: &'static TypeInfo) {
        self.type_info_table.types.insert(id, ty);
    }

    pub fn all_structs(&self) -> &FieldsMap<StructInfo> {
        &self.structs
    }

    pub fn all_enums(&self) -> &FieldsMap<EnumInfo> {
        &self.enums
    }

    pub fn add_struct(
        &mut self,
        fields: FieldsMap<StructFieldInfo>,
        name: Ustr,
        binding_info: BindingInfo,
    ) -> bool {
        let ty = struct_type(name);
        let id = binding_info.id;
        let info = StructInfo {
            fields,
            name,
            ty,
            binding_info,
        };
        let succ = self.structs.insert(info.name, info).is_none();
        self.type_info_table.types.insert(id, ty).is_none() && succ
    }

    pub fn add_enum(
        &mut self,
        name: Ustr,
        variants: FieldsMap<EnumVariantInfo>,
        definition_location: BindingInfo,
    ) -> bool {
        let ty = enum_type(name);
        let id = definition_location.id;
        let info = EnumInfo {
            ty,
            name,
            variants,
            binding_info: definition_location,
        };
        let succ = self.enums.insert(name, info).is_none();
        self.type_info_table.types.insert(id, ty).is_none() && succ
    }

    pub fn add_variable(
        &mut self,
        block: &mut Block,
        name: Ustr,
        id: NodeId,
        span: Option<Span>,
        ty: &'static TypeInfo,
    ) {
        block.insert_variable(name, id, span, false);
        self.type_info_table.types.insert(id, ty);
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
            binding_info,
        };

        let funcs = self.functions.entry(info.name).or_default();

        let succ = self
            .type_info_table
            .types
            .insert(info.binding_info.id, general_type)
            .is_none();

        // TODO: Make this an actual error that gets printed to the user
        debug_assert!(succ, "ID already exists in type table");

        let out = funcs
            .insert(arg_type.iter().map(|x| GiveMePtrHashes(*x)).collect(), info)
            .is_none();

        debug_assert!(out, "Function overload already exists");

        out
    }

    pub fn resolve_struct(&self, name: Ustr) -> Option<&StructInfo> {
        self.structs.get(&name)
    }

    pub fn resolve_struct_mut(&mut self, name: Ustr) -> Option<&mut StructInfo> {
        self.structs.get_mut(&name)
    }

    pub fn resolve_enum(&self, name: Ustr) -> Option<&EnumInfo> {
        self.enums.get(&name)
    }

    pub fn resolve_enum_mut(&mut self, name: Ustr) -> Option<&mut EnumInfo> {
        self.enums.get_mut(&name)
    }

    /// Resolves either a struct or enum type by name
    pub fn resolve_struct_or_enum(&self, name: Ustr) -> Option<StructOrEnumInfo<'_>> {
        if let Some(struct_info) = self.resolve_struct(name) {
            Some(StructOrEnumInfo::Struct(struct_info))
        } else {
            self.resolve_enum(name).map(StructOrEnumInfo::Enum)
        }
    }

    pub fn resolve_type(&self, name: Ustr) -> Option<&'static TypeInfo> {
        // First try primitive types
        let name_str = name.as_str();
        match name_str {
            "i64" => Some(crate::pass_type_inference::type_info::primitive_i64()),
            "u64" => Some(crate::pass_type_inference::type_info::primitive_u64()),
            "f32" => Some(crate::pass_type_inference::type_info::primitive_f32()),
            "f64" => Some(crate::pass_type_inference::type_info::primitive_f64()),
            "bool" => Some(crate::pass_type_inference::type_info::primitive_bool()),
            "nil" => Some(crate::pass_type_inference::type_info::primitive_nil()),
            _ => {
                // Fall back to user-defined types
                self.resolve_struct_or_enum(name).map(|info| info.ty())
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
