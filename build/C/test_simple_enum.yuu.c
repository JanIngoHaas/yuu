#include <stdint.h>
#include <stdbool.h>

struct Color
{
    enum
    {
        Red = 0,
        Green = 1,
        Blue = 2
    } tag;
};

int64_t main();
int64_t fn_color_value(struct Color stack_param_mem_0);

int64_t main()
{
lbl_entry_0:;
    struct Color mem_enum_result_0;
    struct Color *enum_result_0 = &mem_enum_result_0;
    ;
    enum_result_0->tag = UINT64_C(0);
    struct Color mem_color_0;
    struct Color *color_0 = &mem_color_0;
    ;
    *color_0 = *enum_result_0;
    struct Color call_arg_0 = *color_0;
    int64_t call_result_0 = fn_color_value(call_arg_0);
    int64_t *call_result_ptr_0 = &call_result_0;
    int64_t return_value_0 = *call_result_ptr_0;
    return return_value_0;
}

int64_t fn_color_value(struct Color stack_param_mem_0)
{
lbl_entry_0:;
    struct Color *c_0 = &stack_param_mem_0;
    goto lbl_match_header_1;

lbl_match_header_1:;
    uint64_t variant_idx_0 = c_0->tag;
    switch (
        variant_idx_0)
    {
    case 0:
        goto lbl_match_arm_3;
    case 1:
        goto lbl_match_arm_4;
    case 2:
        goto lbl_match_arm_5;
    }

lbl_match_merge_2:;
    // UNTERMINATED BLOCK

lbl_match_arm_3:;
    return INT64_C(1);

lbl_match_arm_4:;
    return INT64_C(2);

lbl_match_arm_5:;
    return INT64_C(3);
}
