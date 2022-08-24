
#pragma once

#include "abi.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "array.h"

int arch_array_parse_format(struct ArchArray* array, const char* format, struct ArrowStatus* status);

#ifdef __cplusplus
}
#endif
