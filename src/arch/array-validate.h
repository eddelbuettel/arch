
#pragma once

#include "status.h"
#include "array.h"

#ifdef __cplusplus
extern "C" {
#endif

int arch_array_validate(struct ArchArray* array, struct ArrowStatus* status);

#ifdef __cplusplus
}
#endif
