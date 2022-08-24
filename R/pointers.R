
#' Danger zone: low-level pointer operations
#'
#' @param ptr,ptr_src,ptr_dst An external pointer to a [arch_schema()],
#'   a [arch_array_data()], or a [arch_array_stream()].
#' @param cls One of "arch_schema", "arch_array_data", or
#'   "arch_array_stream".
#'
#' @export
#'
arch_pointer_is_valid <- function(ptr) {
  .Call(arch_c_pointer_is_valid, ptr)
}

#' @rdname arch_pointer_is_valid
#' @export
arch_pointer_release <- function(ptr) {
  invisible(.Call(arch_c_pointer_release, ptr))
}

#' @rdname arch_pointer_is_valid
#' @export
arch_pointer_move <- function(ptr_src, ptr_dst) {
  invisible(.Call(arch_c_pointer_move, ptr_src, ptr_dst))
}

#' @rdname arch_pointer_is_valid
#' @export
arch_pointer_export <- function(ptr_src, ptr_dst) {
  if (inherits(ptr_src, "arch_schema")) {
    invisible(.Call(arch_c_export_schema, ptr_src, ptr_dst))
  } else if (inherits(ptr_src, "arch_array_data")) {
    invisible(.Call(arch_c_export_array_data, ptr_src, ptr_dst))
  } else if (inherits(ptr_src, "arch_array_stream")) {
    invisible(
      .Call(
        arch_c_export_array_stream,
        ptr_src,
        arch_from_pointer(ptr_dst, "arch_array_stream")
      )
    )
  } else {
    stop(
      "`ptr_src` must inherit from 'arch_schema', 'arch_array_data', or 'arch_array_stream'"
    )
  }
}

#' @rdname arch_pointer_is_valid
#' @export
arch_allocate_schema <- function() {
  .Call(arch_c_allocate_schema)
}

#' @rdname arch_pointer_is_valid
#' @export
arch_allocate_array_data <- function() {
  .Call(arch_c_allocate_array_data)
}

#' @rdname arch_pointer_is_valid
#' @export
arch_allocate_array_stream <- function() {
  .Call(arch_c_allocate_array_stream)
}

# only needed for arrow package before 7.0.0
arch_pointer_addr_dbl <- function(ptr) {
  .Call(arch_c_pointer_addr_dbl, ptr)
}

#' @rdname arch_pointer_is_valid
#' @export
arch_pointer_addr_chr <- function(ptr) {
  .Call(arch_c_pointer_addr_chr, ptr)
}

#' @rdname arch_pointer_is_valid
#' @export
arch_schema_from_pointer <- function(ptr) {
  arch_from_pointer(ptr, "arch_schema")
}

#' @rdname arch_pointer_is_valid
#' @export
arch_array_data_from_pointer <- function(ptr) {
  arch_from_pointer(ptr, "arch_array_data")
}

#' @rdname arch_pointer_is_valid
#' @export
arch_array_stream_from_pointer <- function(ptr) {
  arch_from_pointer(ptr, "arch_array_stream")
}

#' @rdname arch_pointer_is_valid
#' @export
arch_from_pointer <- function(ptr, cls) {
  structure(.Call(arch_c_pointer, ptr), class = cls)
}
