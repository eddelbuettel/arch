
#pragma once

#include <stdint.h>

#define arch_BUFFER_VALIDITY 1
#define arch_BUFFER_OFFSET 2
#define arch_BUFFER_UNION_TYPE 4
#define arch_BUFFER_DATA 8
#define arch_BUFFER_CHILD 16
#define arch_BUFFER_DICTIONARY 32
#define arch_BUFFER_ALL 0xff

#ifdef __cplusplus
extern "C" {
#endif

int arch_buffer_copy_value(void* dest_void, int dest_buffer_type,
                           const void* src_void, int src_buffer_type,
                           int64_t n_elements, int64_t offset);

#ifdef __cplusplus
}
#endif
