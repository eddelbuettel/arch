
#pragma once

#include "abi.h"
#include "array.h"
#include "status.h"

#ifdef __cplusplus
extern "C" {
#endif


int arch_array_init(struct ArchArray* array, struct ArrowSchema* schema,
                    struct ArrowArray* array_data, struct ArrowStatus* status);
int arch_array_set_schema(struct ArchArray* array, struct ArrowSchema* schema,
                          struct ArrowStatus* status);
int arch_array_set_array(struct ArchArray* array, struct ArrowArray* array_data,
                         struct ArrowStatus* status);

#ifdef __cplusplus
}
#endif
