
// API header for narrow

#ifndef _NARROW_API_H
#define _NARROW_API_H

//#include <narrow.h>
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

  SEXP attribute_hidden narrow_c_array_from_sexp(SEXP buffers_sexp, SEXP length_sexp,
                                                 SEXP null_count_sexp, SEXP int64_sexp,
                                                 SEXP children_sexp, SEXP dictionary_xptr) {
    static SEXP(*fun)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP) =
      (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("narrow","narrow_c_array_from_sexp");
    return fun(buffers_sexp, length_sexp, null_count_sexp, int64_sexp, children_sexp, dictionary_xptr);
  }

  SEXP attribute_hidden narrow_c_array_info(SEXP array_data_xptr) {
    static SEXP(*fun)(SEXP) = (SEXP(*)(SEXP)) R_GetCCallable("narrow","narrow_c_array_info");
    return fun(array_data_xptr);
  }

  // ....

  SEXP attribute_hidden narrow_c_allocate_schema() {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("narrow","narrow_c_allocate_schema");
    return fun();
  }

  SEXP attribute_hidden narrow_c_allocate_array_data() {
    static SEXP(*fun)() = (SEXP(*)()) R_GetCCallable("narrow","narrow_c_allocate_array_data");
    return fun();
  }

  SEXP attribute_hidden narrow_c_schema_xptr_new(SEXP format_sexp, SEXP name_sexp,
                                                 SEXP metadata_sexp, SEXP flags_sexp,
                                                 SEXP children_sexp, SEXP dictionary_xptr) {
    static SEXP(*fun)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP) =
      (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("narrow","narrow_c_schema_xptr_new");
    return fun(format_sexp, name_sexp, metadata_sexp, flags_sexp, children_sexp, dictionary_xptr);
  }


  // -- inline functions
  static inline SEXP schema_xptr_new(struct ArrowSchema* schema) {
    SEXP schema_xptr = PROTECT(R_MakeExternalPtr(schema, R_NilValue, R_NilValue));
    Rf_setAttrib(schema_xptr, R_ClassSymbol, Rf_mkString("narrow_schema"));
    UNPROTECT(1);
    return schema_xptr;
  }

  static inline SEXP array_data_xptr_new(struct ArrowArray* array_data) {
    SEXP array_data_xptr = PROTECT(R_MakeExternalPtr(array_data, R_NilValue, R_NilValue));
    Rf_setAttrib(array_data_xptr, R_ClassSymbol, Rf_mkString("narrow_array_data"));
    UNPROTECT(1);
    return array_data_xptr;
  }

  static inline SEXP array_sexp_new(SEXP schema_xptr, SEXP array_data_xptr) {
    const char* names[] = {"schema", "array_data", ""};
    SEXP array_sexp = PROTECT(Rf_mkNamed(VECSXP, names));
    SET_VECTOR_ELT(array_sexp, 0, schema_xptr);
    SET_VECTOR_ELT(array_sexp, 1, array_data_xptr);
    Rf_setAttrib(array_sexp, R_ClassSymbol, Rf_mkString("narrow_array"));
    UNPROTECT(1);
    return array_sexp;
  }


#ifdef __cplusplus
}
#endif

#endif /* !_NARROW_API_H */
