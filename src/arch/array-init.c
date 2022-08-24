
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>

#include "array.h"
#include "array-init.h"
#include "array-parse-format.h"
#include "status.h"

int arch_array_init(struct ArchArray* array, struct ArrowSchema* schema,
                    struct ArrowArray* array_data, struct ArrowStatus* status) {
  arch_status_reset(status);

  arch_array_set_schema(array, schema, status);
  RETURN_IF_NOT_OK(status);

  arch_array_set_array(array, array_data, status);
  RETURN_IF_NOT_OK(status);

  return 0;
}

int arch_array_set_schema(struct ArchArray* array, struct ArrowSchema* schema,
                          struct ArrowStatus* status) {
  arch_status_reset(status);
  if (array == NULL) {
    arch_status_set_error(status, EINVAL, "`array` is NULL");
    RETURN_IF_NOT_OK(status);
  }

  // reset values
  array->schema = NULL;
  array->array_data = NULL;
  array->type = arch_TYPE_MAX_ID;
  array->data_buffer_type = arch_TYPE_MAX_ID;
  array->args = "";
  array->n_buffers = 1;
  array->element_size_bytes = -1;

  array->offset_buffer_id = -1;
  array->large_offset_buffer_id = -1;
  array->union_type_buffer_id = -1;
  array->data_buffer_id = -1;

  arch_status_reset(status);

  if (schema != NULL) {
    if (schema->release == NULL) {
      arch_status_set_error(status, EINVAL, "`schema` is released");
      RETURN_IF_NOT_OK(status);
    }

    arch_array_parse_format(array, schema->format, status);
    RETURN_IF_NOT_OK(status);
  }

  array->schema = schema;
  return 0;
}

int arch_array_set_array(struct ArchArray* array, struct ArrowArray* array_data,
                         struct ArrowStatus* status) {
  arch_status_reset(status);

  if (array == NULL) {
    arch_status_set_error(status, EINVAL, "`array` is NULL");
    RETURN_IF_NOT_OK(status);
  }

  if (array_data != NULL) {
    if (array_data->release == NULL) {
      arch_status_set_error(status, EINVAL, "`array` is released");
      RETURN_IF_NOT_OK(status);
    }

    if (array_data->n_buffers != array->n_buffers) {
      arch_status_set_error(
        status, EINVAL,
        "Expected %ld buffers for schema type '%s' but found %ld buffers in array",
        array->n_buffers, array->schema->format, array_data->n_buffers
      );
      RETURN_IF_NOT_OK(status);
     }
  }

  array->array_data = array_data;
  return 0;
}
