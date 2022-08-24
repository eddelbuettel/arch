
#' Stream objects as Arrow arrays
#'
#' @param list_of_array A `list()` of [arch_array()]s.
#' @param schema The schema for all the arrays in `list_of_array`.
#' @param validate Use `FALSE` to skip validation of arrays in
#'   `list_of_array`.
#' @param x An R object to convert to an Arrow Stream
#' @param array_stream An object of class "arch_array_stream"
#' @param ... Passed to S3 methods
#' @inheritParams from_arch_array
#'
#' @return An object of class "arch_array_stream"
#' @export
#'
arch_array_stream <- function(list_of_array = list(), schema = NULL, validate = TRUE) {
  if (!is.list(list_of_array)) {
    list_of_array <- list(list_of_array)
  }

  list_of_array <- lapply(list_of_array, as_arch_array)

  if (is.null(schema) && length(list_of_array) == 0) {
    schema <- arch_schema("n")
  }

  schema <- schema %||% list_of_array[[1]]$schema
  schema <- as_arch_schema(schema)

  if (validate) {
    for (i in seq_along(list_of_array)) {
      tryCatch(
        arch_array(schema, list_of_array[[i]]$array_data, validate = TRUE),
        error = function(e) {
          msg <- conditionMessage(e)
          stop(
            sprintf("Validation of `list_of_array[[%d]]` failed:\n %s", i, msg),
            call. = FALSE
          )
        }
      )
    }
  }

  .Call(arch_c_arch_array_stream, list_of_array, schema)
}

#' @rdname arch_array_stream
#' @export
arch_array_stream_collect <- function(array_stream, ptype = NULL) {
  array_stream <- as_arch_array_stream(array_stream)
  if (is.null(ptype)) {
    ptype <- arch_default_ptype(
      arch_array_stream_get_schema(
        array_stream
      )
    )
  }

  batches <- vector("list", 1024)
  i <- 0
  while (!is.null(batch <- arch_array_stream_get_next(array_stream))) {
    i <- i + 1L
    batches[[i]] <- from_arch_array(batch, ptype = ptype)
  }

  if (length(batches) > i) {
    batches <- batches[seq_len(i)]
  }

  if (is.data.frame(ptype)) {
    do.call(rbind, batches)
  } else {
    do.call(c, batches)
  }
}

#' @rdname arch_array_stream
#' @export
arch_array_stream_get_schema <- function(array_stream) {
  .Call(arch_c_arch_array_stream_get_schema, array_stream)
}

#' @rdname arch_array_stream
#' @export
arch_array_stream_get_next <- function(array_stream, validate = TRUE) {
  array_data <- .Call(arch_c_arch_array_stream_get_next, array_stream)

  if (is.null(array_data)) {
    NULL
  } else {
    arch_array(
      arch_array_stream_get_schema(array_stream),
      array_data,
      validate = validate
    )
  }
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream <- function(x, ...) {
  UseMethod("as_arch_array_stream")
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream.arch_array_stream <- function(x, ...) {
  x
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream.list <- function(x, ...) {
  arch_array_stream(x)
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream.function <- function(x, ...) {
  as_arch_array_stream(x(...))
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream.default <- function(x, ...) {
  as_arch_array_stream.arch_array(as_arch_array(x))
}

#' @rdname arch_array_stream
#' @export
as_arch_array_stream.arch_array <- function(x, ...) {
  .Call(arch_c_arch_array_stream, list(x), x$schema)
}
