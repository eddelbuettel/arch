
#pragma once

#include "array.h"
#include "status.h"

#ifdef __cplusplus
extern "C" {
#endif

int arch_array_alloc_buffers(struct ArchArray* array, int32_t which_buffers, struct ArrowStatus* status);

#ifdef __cplusplus
}
#endif
