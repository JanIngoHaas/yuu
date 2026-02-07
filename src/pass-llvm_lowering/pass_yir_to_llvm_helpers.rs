use super::*;

impl LLVMLowerer {
    // implemented by @gpt-5.3-codex
    pub(super) fn function_abi_kind(&self, func: &yir::Function) -> FunctionAbiKind {
        if func.follow_c_abi {
            FunctionAbiKind::C
        } else {
            FunctionAbiKind::Internal
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn lowered_param_component_count(lowering: ParamLowering) -> usize {
        match lowering {
            ParamLowering::Split { .. }
            | ParamLowering::SplitIntFloat { .. }
            | ParamLowering::SplitFloatInt { .. } => 2,
            _ => 1,
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn lowered_param_count(lowerings: &[ParamLowering]) -> usize {
        lowerings
            .iter()
            .map(|lowering| Self::lowered_param_component_count(*lowering))
            .sum()
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn build_function_signature(
        &self,
        func: &yir::Function,
        td: &TransientData,
    ) -> AbiSignature {
        let kind = self.function_abi_kind(func);

        let return_lowering = match kind {
            FunctionAbiKind::Internal => ReturnLowering::Direct,
            FunctionAbiKind::C => td
                .c_abi_lowerer
                .classify_return(func.return_type, td.type_registry),
        };

        let param_lowerings = match kind {
            FunctionAbiKind::Internal => vec![ParamLowering::Direct; func.params.len()],
            FunctionAbiKind::C => func
                .params
                .iter()
                .map(|param| {
                    td.c_abi_lowerer
                        .classify_param(param.ty().deref_ptr(), td.type_registry)
                })
                .collect(),
        };

        AbiSignature {
            kind,
            return_type: func.return_type,
            return_lowering,
            param_lowerings,
        }
    }

    // partially implemented by @gpt-5.3-codex
    pub(super) fn declare_function_with_signature(
        &self,
        func: &yir::Function,
        signature: &AbiSignature,
        td: &mut TransientData,
    ) {
        let total_lowered_params = Self::lowered_param_count(&signature.param_lowerings);
        let mut llvm_params =
            Vec::with_capacity(total_lowered_params + usize::from(signature.uses_sret()));
        let mut logical_param_starts = Vec::with_capacity(signature.param_lowerings.len());

        if signature.uses_sret() {
            llvm_params.push(td.context.ptr_type(inkwell::AddressSpace::default()).into());
        }

        for (param, lowering) in func.params.iter().zip(signature.param_lowerings.iter()) {
            let param_ty = param.ty().deref_ptr();
            logical_param_starts.push(llvm_params.len());
            match lowering {
                ParamLowering::Direct => {
                    llvm_params.push(
                        self.lower_type_basic(td.type_registry, param_ty, td.context)
                            .into(),
                    );
                }
                ParamLowering::CoerceToInt { bits } => {
                    llvm_params.push(td.context.custom_width_int_type(*bits).into());
                }
                ParamLowering::CoerceToVec2Float => {
                    llvm_params.push(td.context.f32_type().vec_type(2).into());
                }
                ParamLowering::Split { lo_bits, hi_bits } => {
                    llvm_params.push(td.context.custom_width_int_type(*lo_bits).into());
                    llvm_params.push(td.context.custom_width_int_type(*hi_bits).into());
                }
                ParamLowering::SplitIntFloat {
                    lo_bits,
                    hi_float_bits,
                } => {
                    llvm_params.push(td.context.custom_width_int_type(*lo_bits).into());
                    llvm_params.push(self.float_type_for_bits(*hi_float_bits, td).into());
                }
                ParamLowering::SplitFloatInt {
                    lo_float_bits,
                    hi_bits,
                } => {
                    llvm_params.push(self.float_type_for_bits(*lo_float_bits, td).into());
                    llvm_params.push(td.context.custom_width_int_type(*hi_bits).into());
                }
                ParamLowering::ByVal => {
                    llvm_params.push(td.context.ptr_type(inkwell::AddressSpace::default()).into());
                }
            }
        }

        let fn_type = match signature.return_lowering {
            ReturnLowering::Sret => td.context.void_type().fn_type(&llvm_params, false),
            ReturnLowering::Direct => {
                match self.lower_type(td.type_registry, signature.return_type, td.context) {
                    AnyTypeEnum::VoidType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::IntType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::FloatType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::PointerType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::StructType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::ArrayType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::VectorType(t) => t.fn_type(&llvm_params, false),
                    AnyTypeEnum::ScalableVectorType(t) => t.fn_type(&llvm_params, false),
                    _ => panic!("Unsupported direct return type"),
                }
            }
            ReturnLowering::CoerceToInt { bits } => td
                .context
                .custom_width_int_type(bits)
                .fn_type(&llvm_params, false),
            ReturnLowering::CoerceToVec2Float => td
                .context
                .f32_type()
                .vec_type(2)
                .fn_type(&llvm_params, false),
            ReturnLowering::Pair { lo_bits, hi_bits } => td
                .context
                .struct_type(
                    &[
                        td.context.custom_width_int_type(lo_bits).into(),
                        td.context.custom_width_int_type(hi_bits).into(),
                    ],
                    false,
                )
                .fn_type(&llvm_params, false),
            ReturnLowering::PairIntFloat {
                lo_bits,
                hi_float_bits,
            } => td
                .context
                .struct_type(
                    &[
                        td.context.custom_width_int_type(lo_bits).into(),
                        self.float_type_for_bits(hi_float_bits, td).into(),
                    ],
                    false,
                )
                .fn_type(&llvm_params, false),
            ReturnLowering::PairFloatInt {
                lo_float_bits,
                hi_bits,
            } => td
                .context
                .struct_type(
                    &[
                        self.float_type_for_bits(lo_float_bits, td).into(),
                        td.context.custom_width_int_type(hi_bits).into(),
                    ],
                    false,
                )
                .fn_type(&llvm_params, false),
        };

        let llvm_fn = td.module.add_function(&func.name, fn_type, None);

        if signature.kind == FunctionAbiKind::Internal {
            // LLVM Fast calling convention = 8
            llvm_fn.set_call_conventions(8);
        }

        if signature.uses_sret() {
            let ret_ty = self.lower_type_basic(td.type_registry, signature.return_type, td.context);
            llvm_fn.add_attribute(
                inkwell::attributes::AttributeLoc::Param(0),
                td.context.create_type_attribute(
                    inkwell::attributes::Attribute::get_named_enum_kind_id("sret"),
                    self.basic_to_any_type(ret_ty),
                ),
            );

            let ret_align = crate::utils::c_packing::calculate_type_layout(
                signature.return_type,
                td.type_registry,
            )
            .alignment as u64;
            self.add_enum_attribute(
                llvm_fn,
                inkwell::attributes::AttributeLoc::Param(0),
                "align",
                ret_align,
                td,
            );
        }

        for ((param, lowering), start_idx) in func
            .params
            .iter()
            .zip(signature.param_lowerings.iter())
            .zip(logical_param_starts.iter())
        {
            if let ParamLowering::ByVal = lowering {
                let param_ty = param.ty().deref_ptr();
                let pointee_ty = self.lower_type_basic(td.type_registry, param_ty, td.context);
                llvm_fn.add_attribute(
                    inkwell::attributes::AttributeLoc::Param(*start_idx as u32),
                    td.context.create_type_attribute(
                        inkwell::attributes::Attribute::get_named_enum_kind_id("byval"),
                        self.basic_to_any_type(pointee_ty),
                    ),
                );

                let param_align =
                    crate::utils::c_packing::calculate_type_layout(param_ty, td.type_registry)
                        .alignment as u64;
                self.add_enum_attribute(
                    llvm_fn,
                    inkwell::attributes::AttributeLoc::Param(*start_idx as u32),
                    "align",
                    param_align,
                    td,
                );
                self.add_enum_attribute(
                    llvm_fn,
                    inkwell::attributes::AttributeLoc::Param(*start_idx as u32),
                    "noundef",
                    0,
                    td,
                );
            } else if matches!(lowering, ParamLowering::Direct)
                && self.is_noundef_safe_type(param.ty().deref_ptr())
            {
                self.add_enum_attribute(
                    llvm_fn,
                    inkwell::attributes::AttributeLoc::Param(*start_idx as u32),
                    "noundef",
                    0,
                    td,
                );
            }
        }

        if matches!(signature.return_lowering, ReturnLowering::Direct)
            && self.is_noundef_safe_type(signature.return_type)
        {
            self.add_enum_attribute(
                llvm_fn,
                inkwell::attributes::AttributeLoc::Return,
                "noundef",
                0,
                td,
            );
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn basic_to_any_type<'ctx>(&self, ty: BasicTypeEnum<'ctx>) -> AnyTypeEnum<'ctx> {
        match ty {
            BasicTypeEnum::ArrayType(t) => AnyTypeEnum::ArrayType(t),
            BasicTypeEnum::FloatType(t) => AnyTypeEnum::FloatType(t),
            BasicTypeEnum::IntType(t) => AnyTypeEnum::IntType(t),
            BasicTypeEnum::PointerType(t) => AnyTypeEnum::PointerType(t),
            BasicTypeEnum::StructType(t) => AnyTypeEnum::StructType(t),
            BasicTypeEnum::VectorType(t) => AnyTypeEnum::VectorType(t),
            BasicTypeEnum::ScalableVectorType(t) => AnyTypeEnum::ScalableVectorType(t),
        }
    }

    // implemented by @gpt-5.3-codex
    fn add_enum_attribute<'ctx>(
        &self,
        llvm_fn: inkwell::values::FunctionValue<'ctx>,
        loc: inkwell::attributes::AttributeLoc,
        attr_name: &str,
        value: u64,
        td: &TransientData<'ctx>,
    ) {
        let kind_id = inkwell::attributes::Attribute::get_named_enum_kind_id(attr_name);
        if kind_id != 0 {
            llvm_fn.add_attribute(loc, td.context.create_enum_attribute(kind_id, value));
        }
    }

    // implemented by @gpt-5.3-codex
    fn is_noundef_safe_type(&self, ty: &TypeInfo) -> bool {
        matches!(
            ty,
            TypeInfo::BuiltInPrimitive(PrimitiveType::I8)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::U8)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::I64)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::U64)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::F32)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::F64)
                | TypeInfo::BuiltInPrimitive(PrimitiveType::Bool)
                | TypeInfo::Pointer(_)
        )
    }

    // implemented by @gpt-5.3-codex
    fn add_callsite_enum_attribute<'ctx>(
        &self,
        call_site: CallSiteValue<'ctx>,
        loc: inkwell::attributes::AttributeLoc,
        attr_name: &str,
        value: u64,
        td: &TransientData<'ctx>,
    ) {
        let kind_id = inkwell::attributes::Attribute::get_named_enum_kind_id(attr_name);
        if kind_id != 0 {
            call_site.add_attribute(loc, td.context.create_enum_attribute(kind_id, value));
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn apply_callsite_abi_attributes<'ctx>(
        &self,
        call_site: CallSiteValue<'ctx>,
        cmd: &yir::CallCmd,
        signature: &AbiSignature,
        td: &TransientData<'ctx>,
    ) {
        if signature.uses_sret() {
            let ret_ty = self.lower_type_basic(td.type_registry, signature.return_type, td.context);
            call_site.add_attribute(
                inkwell::attributes::AttributeLoc::Param(0),
                td.context.create_type_attribute(
                    inkwell::attributes::Attribute::get_named_enum_kind_id("sret"),
                    self.basic_to_any_type(ret_ty),
                ),
            );

            let ret_align = crate::utils::c_packing::calculate_type_layout(
                signature.return_type,
                td.type_registry,
            )
            .alignment as u64;
            self.add_callsite_enum_attribute(
                call_site,
                inkwell::attributes::AttributeLoc::Param(0),
                "align",
                ret_align,
                td,
            );
        }

        let mut logical_param_idx = usize::from(signature.uses_sret());
        for (arg, lowering) in cmd.args.iter().zip(signature.param_lowerings.iter()) {
            match lowering {
                ParamLowering::ByVal => {
                    let arg_ty = arg.ty();
                    let pointee_ty = self.lower_type_basic(td.type_registry, arg_ty, td.context);
                    call_site.add_attribute(
                        inkwell::attributes::AttributeLoc::Param(logical_param_idx as u32),
                        td.context.create_type_attribute(
                            inkwell::attributes::Attribute::get_named_enum_kind_id("byval"),
                            self.basic_to_any_type(pointee_ty),
                        ),
                    );

                    let arg_align =
                        crate::utils::c_packing::calculate_type_layout(arg_ty, td.type_registry)
                            .alignment as u64;
                    self.add_callsite_enum_attribute(
                        call_site,
                        inkwell::attributes::AttributeLoc::Param(logical_param_idx as u32),
                        "align",
                        arg_align,
                        td,
                    );
                    self.add_callsite_enum_attribute(
                        call_site,
                        inkwell::attributes::AttributeLoc::Param(logical_param_idx as u32),
                        "noundef",
                        0,
                        td,
                    );
                }
                ParamLowering::Direct => {
                    if self.is_noundef_safe_type(arg.ty()) {
                        self.add_callsite_enum_attribute(
                            call_site,
                            inkwell::attributes::AttributeLoc::Param(logical_param_idx as u32),
                            "noundef",
                            0,
                            td,
                        );
                    }
                }
                _ => {}
            }

            logical_param_idx += Self::lowered_param_component_count(*lowering);
        }

        if matches!(signature.return_lowering, ReturnLowering::Direct)
            && self.is_noundef_safe_type(signature.return_type)
        {
            self.add_callsite_enum_attribute(
                call_site,
                inkwell::attributes::AttributeLoc::Return,
                "noundef",
                0,
                td,
            );
        }
    }

    fn float_type_for_bits<'ctx>(
        &self,
        bits: u32,
        td: &TransientData<'ctx>,
    ) -> inkwell::types::FloatType<'ctx> {
        match bits {
            32 => td.context.f32_type(),
            64 => td.context.f64_type(),
            _ => panic!("Unsupported float bit width for ABI coercion: {}", bits),
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn spill_value_to_slot<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> PointerValue<'ctx> {
        let slot = td.builder.build_alloca(value.get_type(), name).unwrap();
        td.builder.build_store(slot, value).unwrap();
        slot
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn byte_offset_ptr<'ctx>(
        &self,
        base_ptr: PointerValue<'ctx>,
        byte_offset: u64,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> PointerValue<'ctx> {
        let offset = td.context.i64_type().const_int(byte_offset, false);
        unsafe {
            td.builder
                .build_gep(td.context.i8_type(), base_ptr, &[offset], name)
                .unwrap()
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn store_value_at_byte_offset<'ctx>(
        &self,
        base_ptr: PointerValue<'ctx>,
        byte_offset: u64,
        value: BasicValueEnum<'ctx>,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) {
        let ptr = if byte_offset == 0 {
            base_ptr
        } else {
            self.byte_offset_ptr(base_ptr, byte_offset, name, td)
        };
        td.builder.build_store(ptr, value).unwrap();
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn load_value_at_byte_offset<'ctx>(
        &self,
        base_ptr: PointerValue<'ctx>,
        byte_offset: u64,
        value_ty: BasicTypeEnum<'ctx>,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let ptr = if byte_offset == 0 {
            base_ptr
        } else {
            self.byte_offset_ptr(base_ptr, byte_offset, name, td)
        };
        td.builder.build_load(value_ty, ptr, name).unwrap()
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn coerce_value_to_int<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> inkwell::values::IntValue<'ctx> {
        let int_ty = td.context.custom_width_int_type(bits);

        match value {
            BasicValueEnum::IntValue(int_val) => {
                if int_val.get_type().get_bit_width() == bits {
                    int_val
                } else {
                    td.builder.build_int_cast(int_val, int_ty, name).unwrap()
                }
            }
            BasicValueEnum::FloatValue(float_val) => td
                .builder
                .build_bit_cast(float_val, int_ty, name)
                .unwrap()
                .into_int_value(),
            _ => {
                let slot = self.spill_value_to_slot(value, &format!("{name}_slot"), td);
                self.load_value_at_byte_offset(slot, 0, int_ty.into(), &format!("{name}_load"), td)
                    .into_int_value()
            }
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn coerce_value_to_vec2_f32<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> inkwell::values::VectorValue<'ctx> {
        let vec_ty = td.context.f32_type().vec_type(2);

        match value {
            BasicValueEnum::VectorValue(vec_val) => vec_val,
            _ => {
                let slot = self.spill_value_to_slot(value, &format!("{name}_slot"), td);
                self.load_value_at_byte_offset(slot, 0, vec_ty.into(), &format!("{name}_load"), td)
                    .into_vector_value()
            }
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn coerce_value_to_split_ints<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        lo_bits: u32,
        hi_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> (
        inkwell::values::IntValue<'ctx>,
        inkwell::values::IntValue<'ctx>,
    ) {
        let slot = self.spill_value_to_slot(value, &format!("{name}_slot"), td);

        let lo_ty = td.context.custom_width_int_type(lo_bits);
        let lo = self
            .load_value_at_byte_offset(slot, 0, lo_ty.into(), &format!("{name}_lo"), td)
            .into_int_value();

        let hi_ty = td.context.custom_width_int_type(hi_bits);
        let hi = self
            .load_value_at_byte_offset(
                slot,
                (lo_bits / 8) as u64,
                hi_ty.into(),
                &format!("{name}_hi"),
                td,
            )
            .into_int_value();

        (lo, hi)
    }

    fn coerce_value_to_split_int_float<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        lo_bits: u32,
        hi_float_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> (
        inkwell::values::IntValue<'ctx>,
        inkwell::values::FloatValue<'ctx>,
    ) {
        let slot = self.spill_value_to_slot(value, &format!("{name}_slot"), td);

        let lo_ty = td.context.custom_width_int_type(lo_bits);
        let lo = self
            .load_value_at_byte_offset(slot, 0, lo_ty.into(), &format!("{name}_lo"), td)
            .into_int_value();

        let hi_ty = self.float_type_for_bits(hi_float_bits, td);
        let hi = self
            .load_value_at_byte_offset(slot, 8, hi_ty.into(), &format!("{name}_hi"), td)
            .into_float_value();

        (lo, hi)
    }

    fn coerce_value_to_split_float_int<'ctx>(
        &self,
        value: BasicValueEnum<'ctx>,
        lo_float_bits: u32,
        hi_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> (
        inkwell::values::FloatValue<'ctx>,
        inkwell::values::IntValue<'ctx>,
    ) {
        let slot = self.spill_value_to_slot(value, &format!("{name}_slot"), td);

        let lo_ty = self.float_type_for_bits(lo_float_bits, td);
        let lo = self
            .load_value_at_byte_offset(slot, 0, lo_ty.into(), &format!("{name}_lo"), td)
            .into_float_value();

        let hi_ty = td.context.custom_width_int_type(hi_bits);
        let hi = self
            .load_value_at_byte_offset(slot, 8, hi_ty.into(), &format!("{name}_hi"), td)
            .into_int_value();

        (lo, hi)
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn reconstruct_value_from_int<'ctx>(
        &self,
        int_val: inkwell::values::IntValue<'ctx>,
        target_ty: &'static TypeInfo,
        bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let target_llvm_ty = self.lower_type_basic(td.type_registry, target_ty, td.context);
        if let BasicTypeEnum::IntType(int_ty) = target_llvm_ty
            && int_ty.get_bit_width() == bits
        {
            return int_val.into();
        }

        let slot = td
            .builder
            .build_alloca(target_llvm_ty, &format!("{name}_slot"))
            .unwrap();
        self.store_value_at_byte_offset(slot, 0, int_val.into(), &format!("{name}_store"), td);
        td.builder
            .build_load(target_llvm_ty, slot, &format!("{name}_load"))
            .unwrap()
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn bind_lowered_param_to_local<'ctx, F>(
        &self,
        yir_param: yir::Variable,
        lowering: ParamLowering,
        next_param: &mut F,
        td: &mut TransientData<'ctx>,
    ) where
        F: FnMut() -> BasicValueEnum<'ctx>,
    {
        if let ParamLowering::ByVal = lowering {
            td.var_map.insert(yir_param, next_param());
            return;
        }

        let param_ty = yir_param.ty().deref_ptr();
        let alloca_ty = self.lower_type_basic(td.type_registry, param_ty, td.context);
        let alloca = td
            .builder
            .build_alloca(alloca_ty, yir_param.name())
            .unwrap();

        match lowering {
            ParamLowering::Split { lo_bits, .. } => {
                td.builder.build_store(alloca, next_param()).unwrap();
                self.store_value_at_byte_offset(
                    alloca,
                    (lo_bits / 8) as u64,
                    next_param(),
                    "param_split_hi",
                    td,
                );
            }
            ParamLowering::SplitIntFloat {
                lo_bits,
                hi_float_bits: _,
            } => {
                td.builder.build_store(alloca, next_param()).unwrap();
                self.store_value_at_byte_offset(
                    alloca,
                    (lo_bits / 8) as u64,
                    next_param(),
                    "param_split_hi_float",
                    td,
                );
            }
            ParamLowering::SplitFloatInt {
                lo_float_bits: _,
                hi_bits: _,
            } => {
                td.builder.build_store(alloca, next_param()).unwrap();
                self.store_value_at_byte_offset(alloca, 8, next_param(), "param_split_hi_int", td);
            }
            _ => {
                td.builder.build_store(alloca, next_param()).unwrap();
            }
        }

        td.var_map.insert(yir_param, alloca.into());
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn pack_return_value_for_abi<'ctx>(
        &self,
        return_value: BasicValueEnum<'ctx>,
        lowering: ReturnLowering,
        name_prefix: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match lowering {
            ReturnLowering::Direct => return_value,
            ReturnLowering::CoerceToInt { bits } => self
                .coerce_value_to_int(return_value, bits, &format!("{name_prefix}_coerce"), td)
                .into(),
            ReturnLowering::CoerceToVec2Float => self
                .coerce_value_to_vec2_f32(return_value, &format!("{name_prefix}_coerce"), td)
                .into(),
            ReturnLowering::Pair { lo_bits, hi_bits } => {
                let (lo, hi) = self.coerce_value_to_split_ints(
                    return_value,
                    lo_bits,
                    hi_bits,
                    &format!("{name_prefix}_pair"),
                    td,
                );

                let pair_ty = td
                    .context
                    .struct_type(&[lo.get_type().into(), hi.get_type().into()], false);
                let with_lo = td
                    .builder
                    .build_insert_value(
                        pair_ty.get_undef(),
                        lo,
                        0,
                        &format!("{name_prefix}_pair_lo"),
                    )
                    .unwrap()
                    .into_struct_value();
                td.builder
                    .build_insert_value(with_lo, hi, 1, &format!("{name_prefix}_pair_hi"))
                    .unwrap()
                    .into_struct_value()
                    .as_basic_value_enum()
            }
            ReturnLowering::PairIntFloat {
                lo_bits,
                hi_float_bits,
            } => {
                let (lo, hi) = self.coerce_value_to_split_int_float(
                    return_value,
                    lo_bits,
                    hi_float_bits,
                    &format!("{name_prefix}_pair_if"),
                    td,
                );

                let pair_ty = td
                    .context
                    .struct_type(&[lo.get_type().into(), hi.get_type().into()], false);
                let with_lo = td
                    .builder
                    .build_insert_value(
                        pair_ty.get_undef(),
                        lo,
                        0,
                        &format!("{name_prefix}_pair_if_lo"),
                    )
                    .unwrap()
                    .into_struct_value();
                td.builder
                    .build_insert_value(with_lo, hi, 1, &format!("{name_prefix}_pair_if_hi"))
                    .unwrap()
                    .into_struct_value()
                    .as_basic_value_enum()
            }
            ReturnLowering::PairFloatInt {
                lo_float_bits,
                hi_bits,
            } => {
                let (lo, hi) = self.coerce_value_to_split_float_int(
                    return_value,
                    lo_float_bits,
                    hi_bits,
                    &format!("{name_prefix}_pair_fi"),
                    td,
                );

                let pair_ty = td
                    .context
                    .struct_type(&[lo.get_type().into(), hi.get_type().into()], false);
                let with_lo = td
                    .builder
                    .build_insert_value(
                        pair_ty.get_undef(),
                        lo,
                        0,
                        &format!("{name_prefix}_pair_fi_lo"),
                    )
                    .unwrap()
                    .into_struct_value();
                td.builder
                    .build_insert_value(with_lo, hi, 1, &format!("{name_prefix}_pair_fi_hi"))
                    .unwrap()
                    .into_struct_value()
                    .as_basic_value_enum()
            }
            ReturnLowering::Sret => panic!("Compiler Bug: Sret cannot be packed as direct return"),
        }
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn unpack_call_result_for_abi<'ctx>(
        &self,
        target: yir::Variable,
        call_result: CallSiteValue<'ctx>,
        signature: &AbiSignature,
        sret_slot: Option<PointerValue<'ctx>>,
        td: &mut TransientData<'ctx>,
    ) {
        let unpacked = match signature.return_lowering {
            ReturnLowering::Direct => call_result
                .try_as_basic_value()
                .expect_basic("Compiler Bug: Expected non-void call result"),
            ReturnLowering::CoerceToInt { bits } => {
                let raw = call_result
                    .try_as_basic_value()
                    .expect_basic("Compiler Bug: Expected integer call result")
                    .into_int_value();
                self.reconstruct_value_from_int(raw, target.ty(), bits, "call_ret_unpack_int", td)
            }
            ReturnLowering::CoerceToVec2Float => {
                let raw = call_result
                    .try_as_basic_value()
                    .expect_basic("Compiler Bug: Expected vector call result")
                    .into_vector_value();
                self.reconstruct_value_from_vec2_f32(raw, target.ty(), "call_ret_unpack_vec", td)
            }
            ReturnLowering::Pair { lo_bits, hi_bits } => {
                let pair_val = call_result
                    .try_as_basic_value()
                    .expect_basic("Compiler Bug: Expected pair call result")
                    .into_struct_value();
                let lo = td
                    .builder
                    .build_extract_value(pair_val, 0, "call_pair_lo")
                    .unwrap()
                    .into_int_value();
                let hi = td
                    .builder
                    .build_extract_value(pair_val, 1, "call_pair_hi")
                    .unwrap()
                    .into_int_value();
                self.reconstruct_value_from_split_ints(
                    lo,
                    hi,
                    target.ty(),
                    lo_bits,
                    hi_bits,
                    "call_ret_unpack_pair",
                    td,
                )
            }
            ReturnLowering::PairIntFloat {
                lo_bits,
                hi_float_bits,
            } => {
                let pair_val = call_result
                    .try_as_basic_value()
                    .expect_basic("Compiler Bug: Expected mixed pair call result")
                    .into_struct_value();
                let lo = td
                    .builder
                    .build_extract_value(pair_val, 0, "call_pair_if_lo")
                    .unwrap()
                    .into_int_value();
                let hi = td
                    .builder
                    .build_extract_value(pair_val, 1, "call_pair_if_hi")
                    .unwrap()
                    .into_float_value();
                self.reconstruct_value_from_split_int_float(
                    lo,
                    hi,
                    target.ty(),
                    lo_bits,
                    hi_float_bits,
                    "call_ret_unpack_pair_if",
                    td,
                )
            }
            ReturnLowering::PairFloatInt {
                lo_float_bits,
                hi_bits,
            } => {
                let pair_val = call_result
                    .try_as_basic_value()
                    .expect_basic("Compiler Bug: Expected mixed pair call result")
                    .into_struct_value();
                let lo = td
                    .builder
                    .build_extract_value(pair_val, 0, "call_pair_fi_lo")
                    .unwrap()
                    .into_float_value();
                let hi = td
                    .builder
                    .build_extract_value(pair_val, 1, "call_pair_fi_hi")
                    .unwrap()
                    .into_int_value();
                self.reconstruct_value_from_split_float_int(
                    lo,
                    hi,
                    target.ty(),
                    lo_float_bits,
                    hi_bits,
                    "call_ret_unpack_pair_fi",
                    td,
                )
            }
            ReturnLowering::Sret => {
                let slot = sret_slot
                    .expect("Compiler Bug: Missing sret slot for sret-returning call target");
                let target_ty = self.lower_type_basic(td.type_registry, target.ty(), td.context);
                td.builder
                    .build_load(target_ty, slot, "call_sret_ret")
                    .unwrap()
            }
        };

        td.var_map.insert(target, unpacked);
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn reconstruct_value_from_split_ints<'ctx>(
        &self,
        lo: inkwell::values::IntValue<'ctx>,
        hi: inkwell::values::IntValue<'ctx>,
        target_ty: &'static TypeInfo,
        lo_bits: u32,
        _hi_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let target_llvm_ty = self.lower_type_basic(td.type_registry, target_ty, td.context);
        let slot = td
            .builder
            .build_alloca(target_llvm_ty, &format!("{name}_slot"))
            .unwrap();

        self.store_value_at_byte_offset(slot, 0, lo.into(), &format!("{name}_lo_store"), td);
        self.store_value_at_byte_offset(
            slot,
            (lo_bits / 8) as u64,
            hi.into(),
            &format!("{name}_hi_store"),
            td,
        );

        td.builder
            .build_load(target_llvm_ty, slot, &format!("{name}_load"))
            .unwrap()
    }

    fn reconstruct_value_from_split_int_float<'ctx>(
        &self,
        lo: inkwell::values::IntValue<'ctx>,
        hi: inkwell::values::FloatValue<'ctx>,
        target_ty: &'static TypeInfo,
        _lo_bits: u32,
        _hi_float_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let target_llvm_ty = self.lower_type_basic(td.type_registry, target_ty, td.context);
        let slot = td
            .builder
            .build_alloca(target_llvm_ty, &format!("{name}_slot"))
            .unwrap();

        self.store_value_at_byte_offset(slot, 0, lo.into(), &format!("{name}_lo_store"), td);
        self.store_value_at_byte_offset(slot, 8, hi.into(), &format!("{name}_hi_store"), td);

        td.builder
            .build_load(target_llvm_ty, slot, &format!("{name}_load"))
            .unwrap()
    }

    fn reconstruct_value_from_split_float_int<'ctx>(
        &self,
        lo: inkwell::values::FloatValue<'ctx>,
        hi: inkwell::values::IntValue<'ctx>,
        target_ty: &'static TypeInfo,
        _lo_float_bits: u32,
        _hi_bits: u32,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let target_llvm_ty = self.lower_type_basic(td.type_registry, target_ty, td.context);
        let slot = td
            .builder
            .build_alloca(target_llvm_ty, &format!("{name}_slot"))
            .unwrap();

        self.store_value_at_byte_offset(slot, 0, lo.into(), &format!("{name}_lo_store"), td);
        self.store_value_at_byte_offset(slot, 8, hi.into(), &format!("{name}_hi_store"), td);

        td.builder
            .build_load(target_llvm_ty, slot, &format!("{name}_load"))
            .unwrap()
    }

    // implemented by @gpt-5.3-codex
    fn reconstruct_value_from_vec2_f32<'ctx>(
        &self,
        vec_val: inkwell::values::VectorValue<'ctx>,
        target_ty: &'static TypeInfo,
        name: &str,
        td: &mut TransientData<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let target_llvm_ty = self.lower_type_basic(td.type_registry, target_ty, td.context);
        let slot = td
            .builder
            .build_alloca(target_llvm_ty, &format!("{name}_slot"))
            .unwrap();

        self.store_value_at_byte_offset(slot, 0, vec_val.into(), &format!("{name}_store"), td);

        td.builder
            .build_load(target_llvm_ty, slot, &format!("{name}_load"))
            .unwrap()
    }

    // implemented by @gpt-5.3-codex
    pub(super) fn append_packed_call_arg<'ctx>(
        &self,
        arg: &yir::Operand,
        lowering: ParamLowering,
        packed_args: &mut Vec<BasicMetadataValueEnum<'ctx>>,
        td: &mut TransientData<'ctx>,
    ) {
        let arg_value = self.lower_operand(arg, td);
        match lowering {
            ParamLowering::Direct => packed_args.push(arg_value.into()),
            ParamLowering::CoerceToInt { bits } => {
                let coerced = self.coerce_value_to_int(arg_value, bits, "call_arg_coerce_int", td);
                packed_args.push(coerced.into());
            }
            ParamLowering::CoerceToVec2Float => {
                let coerced = self.coerce_value_to_vec2_f32(arg_value, "call_arg_coerce_vec", td);
                packed_args.push(coerced.into());
            }
            ParamLowering::Split { lo_bits, hi_bits } => {
                let (lo, hi) = self.coerce_value_to_split_ints(
                    arg_value,
                    lo_bits,
                    hi_bits,
                    "call_arg_split",
                    td,
                );
                packed_args.push(lo.into());
                packed_args.push(hi.into());
            }
            ParamLowering::SplitIntFloat {
                lo_bits,
                hi_float_bits,
            } => {
                let (lo, hi) = self.coerce_value_to_split_int_float(
                    arg_value,
                    lo_bits,
                    hi_float_bits,
                    "call_arg_split_if",
                    td,
                );
                packed_args.push(lo.into());
                packed_args.push(hi.into());
            }
            ParamLowering::SplitFloatInt {
                lo_float_bits,
                hi_bits,
            } => {
                let (lo, hi) = self.coerce_value_to_split_float_int(
                    arg_value,
                    lo_float_bits,
                    hi_bits,
                    "call_arg_split_fi",
                    td,
                );
                packed_args.push(lo.into());
                packed_args.push(hi.into());
            }
            ParamLowering::ByVal => {
                let arg_ty = self.lower_type_basic(td.type_registry, arg.ty(), td.context);
                let slot = td
                    .builder
                    .build_alloca(arg_ty, "call_arg_byval_slot")
                    .unwrap();
                td.builder.build_store(slot, arg_value).unwrap();
                packed_args.push(slot.into());
            }
        }
    }
}
