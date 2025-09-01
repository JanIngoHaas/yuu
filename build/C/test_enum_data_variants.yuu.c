#include <stdint.h>
#include <stdbool.h>

struct Option {
    enum { None = 0, Some = 1 } tag;
    union {
        int64_t Some_data;
    } data;
};

int64_t fn_get_value(struct Option stack_param_mem_0);
int64_t main();

int64_t fn_get_value(struct Option stack_param_mem_0) {
lbl_entry_0:;
    struct Option* opt_0 = &stack_param_mem_0;
    goto lbl_match_header_1;

lbl_match_header_1:;
    uint64_t variant_idx_0 = opt_0->tag;
    switch (
variant_idx_0) {
        case 0: goto lbl_match_arm_3;
        case 1: goto lbl_match_arm_4;
    }

lbl_match_merge_2:;
    // UNTERMINATED BLOCK

lbl_match_arm_3:;
    return INT64_C(0);

lbl_match_arm_4:;
    int64_t mem_variant_data_0; int64_t* variant_data_0 = &mem_variant_data_0; ;
    variant_data_0 = &(opt_0->data.Some_data);
    int64_t mem_value_0; int64_t* value_0 = &mem_value_0; ;
    *value_0 = *variant_data_0;
    int64_t return_value_0 = *value_0;
    return return_value_0;

}

int64_t main() {
lbl_entry_0:;
    struct Option mem_enum_result_0; struct Option* enum_result_0 = &mem_enum_result_0; ;
        enum_result_0->tag = UINT64_C(1);
    int64_t mem_variant_ptr_0; int64_t* variant_ptr_0 = &mem_variant_ptr_0; ;
    variant_ptr_0 = &(enum_result_0->data.Some_data);
    *variant_ptr_0 = INT64_C(42);
    struct Option mem_value_0; struct Option* value_0 = &mem_value_0; ;
    *value_0 = *enum_result_0;
    struct Option call_arg_0 = *value_0;
    int64_t call_result_0 = fn_get_value(call_arg_0);
    int64_t* call_result_ptr_0 = &call_result_0;
    int64_t return_value_0 = *call_result_ptr_0;
    return return_value_0;

}

