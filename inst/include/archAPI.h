
// API header for arch

#ifndef _ARCH_API_H
#define _ARCH_API_H

//#include <arch.h>
//struct ArrowSchema;

#include <Rconfig.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

#ifdef __cplusplus
extern "C" {
#endif

  SEXP attribute_hidden arch_c_array_from_sexp(SEXP buffers_sexp, SEXP length_sexp,
                                                 SEXP null_count_sexp, SEXP int64_sexp,
                                                 SEXP children_sexp, SEXP dictionary_xptr) {
    static SEXP(*fun)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP) =
      (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("arch","arch_c_array_from_sexp");
    return fun(buffers_sexp, length_sexp, null_count_sexp, int64_sexp, children_sexp, dictionary_xptr);
  }

  SEXP attribute_hidden arch_c_array_info(SEXP array_data_xptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_array_info");
    return fun(array_data_xptr);
  }

  SEXP attribute_hidden arch_c_arch_array_stream(SEXP array_list, SEXP schema_xptr) {
    static SEXP(*fun)(SEXP,SEXP) = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("arch","arch_c_arch_array_stream");
    return fun(array_list, schema_xptr);
  }

  SEXP attribute_hidden arch_c_function_array_stream(SEXP schema_xptr, SEXP call, SEXP env) {
    static SEXP(*fun)(SEXP,SEXP,SEXP) = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("arch","arch_c_function_array_stream");
    return fun(schema_xptr, call, env);
  }

  SEXP attribute_hidden arch_c_export_array_stream(SEXP parent_xptr, SEXP xptr_dst) {
    static SEXP(*fun)(SEXP,SEXP) = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("arch","arch_c_export_array_stream");
    return fun(parent_xptr, xptr_dst);
  }

  SEXP attribute_hidden arch_c_arch_array_stream_get_schema(SEXP xptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_arch_array_stream_get_schema");
    return fun(xptr);
  }

  SEXP attribute_hidden arch_c_arch_array_stream_get_next(SEXP xptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_arch_array_stream_get_next");
    return fun(xptr);
  }

  SEXP attribute_hidden arch_c_validate(SEXP array_sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_validate");
    return fun(array_sexp);
  }

  SEXP attribute_hidden arch_c_logical_from_bitmask(SEXP bitmask, SEXP start_sexp, SEXP end_sexp) {
    static SEXP(*fun)(SEXP,SEXP,SEXP) = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("arch","arch_c_logical_from_bitmask");
    return fun(bitmask, start_sexp, end_sexp);
  }

  SEXP attribute_hidden arch_c_bitmask_from_logical(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_bitmask_from_logical");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_logical_from_array(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_logical_from_array");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_integer_from_array(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_integer_from_array");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_double_from_array(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_double_from_array");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_raw_from_array(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_raw_from_array");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_character_from_array(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_character_from_array");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_buffers_from_character(SEXP chr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_c_buffers_from");
    return fun(chr);
  }

  SEXP attribute_hidden arch_c_deep_copy(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_c_deep_copy");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_double_from_int64(SEXP value, SEXP start_sexp, SEXP end_sexp) {
    static SEXP(*fun)(SEXP,SEXP,SEXP) = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("arch","arch_c_double_from_int64");
    return fun(value, start_sexp, end_sexp);
  }

  SEXP attribute_hidden arch_c_int64_from_double(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_int64_from_double");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_int64_from_integer(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_int64_from_integer");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_pointer(SEXP sexp) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_pointer");
    return fun(sexp);
  }

  SEXP attribute_hidden arch_c_pointer_is_valid(SEXP ptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_pointer_is_valid");
    return fun(ptr);
  }

  SEXP attribute_hidden arch_c_pointer_release(SEXP ptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_pointer_release");
    return fun(ptr);
  }

  SEXP attribute_hidden arch_c_pointer_addr_dbl(SEXP ptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_pointer_addr_dbl");
    return fun(ptr);
  }

  SEXP attribute_hidden arch_c_pointer_addr_chr(SEXP ptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("arch","arch_c_pointer_addr_chr");
    return fun(ptr);
  }

  SEXP attribute_hidden arch_c_pointer_move(SEXP ptr_src, SEXP ptr_dst) {
    static SEXP(*fun)(SEXP,SEXP) = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("arch","arch_c_pointer_move");
    return fun(ptr_src, ptr_dst);
  }

  SEXP attribute_hidden arch_c_export_schema(SEXP schema_xptr, SEXP ptr_dst) {
    static SEXP(*fun)(SEXP,SEXP) = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("arch","arch_c_export_schema");
    return fun(schema_xptr, ptr_dst);
  }

  SEXP attribute_hidden arch_c_export_array_data(SEXP array_data_xptr, SEXP ptr_dst) {
    static SEXP(*fun)(SEXP,SEXP) = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("arch","arch_c_export_array_data");
    return fun(array_data_xptr, ptr_dst);
  }

  SEXP attribute_hidden arch_c_allocate_schema() {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_allocate_schema");
    return fun();
  }

  SEXP attribute_hidden arch_c_allocate_array_data() {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_allocate_array_data");
    return fun();
  }

  SEXP attribute_hidden arch_c_allocate_array_stream() {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_allocate_array_stream");
    return fun();
  }

  SEXP attribute_hidden arch_c_schema_xptr_new(SEXP format_sexp, SEXP name_sexp,
                                                 SEXP metadata_sexp, SEXP flags_sexp,
                                                 SEXP children_sexp, SEXP dictionary_xptr) {
    static SEXP(*fun)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP) =
      (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("arch","arch_c_schema_xptr_new");
    return fun(format_sexp, name_sexp, metadata_sexp, flags_sexp, children_sexp, dictionary_xptr);
  }

  SEXP attribute_hidden arch_c_schema_deep_copy(SEXP schema_xptr) {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_schema_deep_copy");
    return fun();
  }

  SEXP attribute_hidden arch_c_schema_data(SEXP schema_xptr) {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_schema_data");
    return fun();
  }

  SEXP arch_c_xptr_addr(SEXP xptr) {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("arch","arch_c_xptr_addr");
    return fun();
  }

#ifdef __cplusplus
}
#endif

#endif /* !_ARCH_API_H */
