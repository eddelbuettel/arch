
#pragma once

#include "abi.h"

#ifdef __cplusplus
extern "C" {
#endif

// Modified from the struct Type {enum type} for more predictable C semantics
// https://github.com/apache/arrow/blob/master/cpp/src/arrow/type_fwd.h#L275-L408
enum ArrowType {
  /// A NULL type having no physical storage
  arch_TYPE_NA = 0,

  /// Boolean as 1 bit, LSB bit-packed ordering
  arch_TYPE_BOOL,

  /// Unsigned 8-bit little-endian integer
  arch_TYPE_UINT8,

  /// Signed 8-bit little-endian integer
  arch_TYPE_INT8,

  /// Unsigned 16-bit little-endian integer
  arch_TYPE_UINT16,

  /// Signed 16-bit little-endian integer
  arch_TYPE_INT16,

  /// Unsigned 32-bit little-endian integer
  arch_TYPE_UINT32,

  /// Signed 32-bit little-endian integer
  arch_TYPE_INT32,

  /// Unsigned 64-bit little-endian integer
  arch_TYPE_UINT64,

  /// Signed 64-bit little-endian integer
  arch_TYPE_INT64,

  /// 2-byte floating point value
  arch_TYPE_HALF_FLOAT,

  /// 4-byte floating point value
  arch_TYPE_FLOAT,

  /// 8-byte floating point value
  arch_TYPE_DOUBLE,

  /// UTF8 variable-length string as List<Char>
  arch_TYPE_STRING,

  /// Variable-length bytes (no guarantee of UTF8-ness)
  arch_TYPE_BINARY,

  /// Fixed-size binary. Each value occupies the same number of bytes
  arch_TYPE_FIXED_SIZE_BINARY,

  /// int32_t days since the UNIX epoch
  arch_TYPE_DATE32,

  /// int64_t milliseconds since the UNIX epoch
  arch_TYPE_DATE64,

  /// Exact timestamp encoded with int64 since UNIX epoch
  /// Default unit millisecond
  arch_TYPE_TIMESTAMP,

  /// Time as signed 32-bit integer, representing either seconds or
  /// milliseconds since midnight
  arch_TYPE_TIME32,

  /// Time as signed 64-bit integer, representing either microseconds or
  /// nanoseconds since midnight
  arch_TYPE_TIME64,

  /// YEAR_MONTH interval in SQL style
  arch_TYPE_INTERVAL_MONTHS,

  /// DAY_TIME interval in SQL style
  arch_TYPE_INTERVAL_DAY_TIME,

  /// Precision- and scale-based decimal type with 128 bits.
  arch_TYPE_DECIMAL128,

  /// Defined for backward-compatibility.
  arch_TYPE_DECIMAL = arch_TYPE_DECIMAL128,

  /// Precision- and scale-based decimal type with 256 bits.
  arch_TYPE_DECIMAL256,

  /// A list of some logical data type
  arch_TYPE_LIST,

  /// Struct of logical types
  arch_TYPE_STRUCT,

  /// Sparse unions of logical types
  arch_TYPE_SPARSE_UNION,

  /// Dense unions of logical types
  arch_TYPE_DENSE_UNION,

  /// Dictionary-encoded type, also called "categorical" or "factor"
  /// in other programming languages. Holds the dictionary value
  /// type but not the dictionary itself, which is part of the
  /// ArrayData struct
  // arch_TYPE_DICTIONARY,

  /// Map, a repeated struct logical type
  arch_TYPE_MAP,

  /// Custom data type, implemented by user
  // arch_TYPE_EXTENSION,

  /// Fixed size list of some logical type
  arch_TYPE_FIXED_SIZE_LIST,

  /// Measure of elapsed time in either seconds, milliseconds, microseconds
  /// or nanoseconds.
  arch_TYPE_DURATION,

  /// Like STRING, but with 64-bit offsets
  arch_TYPE_LARGE_STRING,

  /// Like BINARY, but with 64-bit offsets
  arch_TYPE_LARGE_BINARY,

  /// Like LIST, but with 64-bit offsets
  arch_TYPE_LARGE_LIST,

  /// Calendar interval type with three fields.
  arch_TYPE_INTERVAL_MONTH_DAY_NANO,

  // Leave this at the end
  arch_TYPE_MAX_ID
};

// The archArray struct is a wrapper around the Schema and Array
// that contains the casted pointers from Array as defined by Schema
// It does not own any of its pointers, which are all borrowed from
// its schema and array members.
struct ArchArray {
  struct ArrowSchema* schema;
  struct ArrowArray* array_data;

  // arch_array_set_schema() parses schema->format to obtain a few
  // useful outputs that reduce the amount of parsing needed to
  // implement some common operations
  enum ArrowType type;
  enum ArrowType data_buffer_type;
  const char* args;
  int n_buffers;
  int64_t element_size_bytes;

  // index of array_data->buffer[], including validity buffer
  int offset_buffer_id;
  int large_offset_buffer_id;
  int union_type_buffer_id;
  int data_buffer_id;
};

static inline unsigned char* arch_array_validity_buffer(struct ArchArray* array) {
  return (unsigned char*) array->array_data->buffers[0];
}

static inline int32_t* arch_array_offset_buffer(struct ArchArray* array) {
  if (array->offset_buffer_id == -1) {
    return 0;
  }

  return (int32_t*) array->array_data->buffers[array->offset_buffer_id];
}

static inline int64_t* arch_array_large_offset_buffer(struct ArchArray* array) {
  if (array->large_offset_buffer_id == -1) {
    return 0;
  }

  return (int64_t*) array->array_data->buffers[array->large_offset_buffer_id];
}

static inline char* arch_array_union_type_buffer(struct ArchArray* array) {
  if (array->union_type_buffer_id == -1) {
    return 0;
  }

  return (char*) array->array_data->buffers[array->union_type_buffer_id];
}

static inline void* arch_array_data_buffer(struct ArchArray* array) {
  if (array->data_buffer_id == -1) {
    return 0;
  }

  return (void*) array->array_data->buffers[array->data_buffer_id];
}

#ifdef __cplusplus
}
#endif
