#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "carrow/carrow.h"
#include "vctr.h"
#include "schema.h"
#include "array-data.h"
#include "bitmask.h"
#include "int64.h"

#define LOOP_NODATA(_body) \
  unsigned char* validity_buffer = carrow_vector_validity_buffer(&vector); \
  if (validity_buffer != NULL || size == 0) { \
    for (int64_t i = 0; i < size; i++) { \
      if (!bitmask_value(validity_buffer, i + offset)) { \
        _body; \
      } \
    } \
  }

SEXP carrow_c_logical_from_vctr(SEXP vctr_sexp) {
  struct ArrowVector vector;
  vctr_from_vctr(vctr_sexp, &vector, "x");
  int64_t size = vector.array_data->length;
  int64_t offset = vector.array_data->offset;

  SEXP result_sexp = PROTECT(Rf_allocVector(LGLSXP, size));
  int* result = LOGICAL(result_sexp);

  int copy_result = carrow_buffer_copy_value(
    result, CARROW_TYPE_INT32,
    carrow_vector_data_buffer(&vector), vector.type,
    size, offset
  );

  if (copy_result != 0) {
    Rf_error("Can't convert vctr with format '%s' to logical()", vector.schema->format);
  }

  LOOP_NODATA(result[i] = NA_LOGICAL)

  UNPROTECT(1);
  return result_sexp;
}

SEXP carrow_c_integer_from_vctr(SEXP vctr_sexp) {
  struct ArrowVector vector;
  vctr_from_vctr(vctr_sexp, &vector, "x");
  int64_t size = vector.array_data->length;
  int64_t offset = vector.array_data->offset;

  SEXP result_sexp = PROTECT(Rf_allocVector(INTSXP, size));
  int* result = INTEGER(result_sexp);

  int copy_result = carrow_buffer_copy_value(
    result, CARROW_TYPE_INT32,
    carrow_vector_data_buffer(&vector), vector.type,
    size, offset
  );

  if (copy_result != 0) {
    Rf_error("Can't convert vctr with format '%s' to integer()", vector.schema->format);
  }

  LOOP_NODATA(result[i] = NA_INTEGER)

  UNPROTECT(1);
  return result_sexp;
}

SEXP carrow_c_double_from_vctr(SEXP vctr_sexp) {
  struct ArrowVector vector;
  vctr_from_vctr(vctr_sexp, &vector, "x");
  int64_t size = vector.array_data->length;
  int64_t offset = vector.array_data->offset;

  SEXP result_sexp = PROTECT(Rf_allocVector(REALSXP, size));
  double* result = REAL(result_sexp);

  int copy_result = carrow_buffer_copy_value(
    result, CARROW_TYPE_DOUBLE,
    carrow_vector_data_buffer(&vector), vector.type,
    size, offset
  );

  if (copy_result != 0) {
    Rf_error("Can't convert vctr with format '%s' to double()", vector.schema->format);
  }

  LOOP_NODATA(result[i] = NA_REAL)

  UNPROTECT(1);
  return result_sexp;
}

SEXP carrow_c_raw_from_vctr(SEXP vctr_sexp) {
  struct ArrowVector vector;
  vctr_from_vctr(vctr_sexp, &vector, "x");
  int64_t size = vector.array_data->length;
  int64_t offset = vector.array_data->offset;

  SEXP result_sexp = PROTECT(Rf_allocVector(RAWSXP, size));
  unsigned char* result = RAW(result_sexp);

  int copy_result = carrow_buffer_copy_value(
    result, CARROW_TYPE_UINT8,
    carrow_vector_data_buffer(&vector), vector.type,
    size, offset
  );

  if (copy_result != 0) {
    Rf_error("Can't convert vctr with format '%s' to raw()", vector.schema->format);
  }

  LOOP_NODATA(result[i] = 0x00)

  UNPROTECT(1);
  return result_sexp;
}

SEXP carrow_c_character_from_vctr(SEXP vctr_sexp) {
  struct ArrowVector vector;
  vctr_from_vctr(vctr_sexp, &vector, "x");
  int64_t size = vector.array_data->length;
  int64_t offset = vector.array_data->offset;

  SEXP result_sexp = PROTECT(Rf_allocVector(STRSXP, size));
  if (size == 0) {
    UNPROTECT(1);
    return result_sexp;
  }

  void* data_buffer = carrow_vector_data_buffer(&vector);
  if (data_buffer == NULL) {
    Rf_error(
      "Can't convert schema format '%s' to `character()` (data buffer is NULL)",
      vector.schema->format
    );
  }

  const char* char_buffer = (const char*) data_buffer;

  switch (vector.type) {
  case CARROW_TYPE_STRING:
  case CARROW_TYPE_BINARY: {
    const int32_t* offsets = carrow_vector_offset_buffer(&vector);
    for (int64_t i = 0; i < size; i++) {
      int64_t item_length = offsets[i + offset + 1] - offsets[i + offset];
      SET_STRING_ELT(
        result_sexp,
        i,
        Rf_mkCharLenCE(char_buffer + offsets[i + offset], item_length, CE_UTF8)
      );
    }
    break;
  }

  case CARROW_TYPE_LARGE_STRING:
  case CARROW_TYPE_LARGE_BINARY: {
    const int64_t* large_offsets = carrow_vector_large_offset_buffer(&vector);
    for (int64_t i = 0; i < size; i++) {
      int64_t item_length = large_offsets[i + offset + 1] - large_offsets[i + offset];
      SET_STRING_ELT(
        result_sexp,
        i,
        Rf_mkCharLenCE(char_buffer + large_offsets[i + offset], item_length, CE_UTF8)
      );
    }
    break;
  }

  case CARROW_TYPE_FIXED_SIZE_BINARY: {
    for (int64_t i = 0; i < size; i++) {
      SET_STRING_ELT(
        result_sexp,
        i,
        Rf_mkCharLenCE(
          char_buffer + vector.element_size_bytes * (i + offset),
          vector.element_size_bytes,
          CE_UTF8
        )
      );
    }
    break;
  }

  case CARROW_TYPE_INT8:
  case CARROW_TYPE_UINT8:
  case CARROW_TYPE_INT16:
  case CARROW_TYPE_UINT16:
  case CARROW_TYPE_INT32:
  case CARROW_TYPE_UINT32:
  case CARROW_TYPE_INT64: // this may be lossy for int64
  case CARROW_TYPE_UINT64: // this may be lossy for uint64
  case CARROW_TYPE_HALF_FLOAT:
  case CARROW_TYPE_FLOAT:
  case CARROW_TYPE_DOUBLE: {
    int double_copy_result;
    double double_val;
    char double_buffer[32];
    memset(double_buffer, 0, 32);
    for (int64_t i = 0; i < size; i++) {
      double_copy_result = carrow_buffer_copy_value(
        &double_val, CARROW_TYPE_DOUBLE,
        data_buffer, vector.type,
        1, offset + i
      );
      sprintf(double_buffer, "%g", double_val);
      SET_STRING_ELT(result_sexp, i, Rf_mkCharCE(double_buffer, CE_UTF8));
    }
    break;
  }

  default:
    Rf_error("Can't convert schema format '%s' to `character()`", vector.schema->format);
  }

  // propagate nodata value as NA_STRING
  LOOP_NODATA(SET_STRING_ELT(result_sexp, i, NA_STRING);)

  UNPROTECT(1);
  return result_sexp;
}
