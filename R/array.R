
#' Create arrow vectors
#'
#' @param schema An [arch_schema()]
#' @param array_data An [arch_array_data()]
#' @param x An object to convert to an [arch_array()]
#' @param validate Use `FALSE` to skip validation
#' @param ... Passed to S3 methods
#'
#' @return An object of class "arch_array"
#' @export
#'
#' @examples
#' arch_array()
#'
arch_array <- function(schema = arch_schema("n"), array_data = arch_array_data(), validate = TRUE) {
  schema <- as_arch_schema(schema)
  arrays <- as_arch_array_data(array_data)
  array <- structure(list(schema = schema, array_data = array_data), class = "arch_array")
  if (validate) {
    arch_array_validate(array)
  }

  array
}

#' @rdname arch_array
#' @export
arch_array_validate <- function(x) {
  .Call(arch_c_array_validate, x)
  invisible(x)
}

#' @rdname arch_array
#' @export
as_arch_array <- function(x, ...) {
  UseMethod("as_arch_array")
}

#' @rdname arch_array
#' @export
as_arch_array.arch_array <- function(x, ...) {
  x
}

#' @export
`[[<-.arch_array` <- function(x, i, ..., value) {
  x <- unclass(x)
  x[[i]] <- value
  do.call(arch_array, x)
}

#' @export
`$<-.arch_array` <- function(x, name, ..., value) {
  x[[name]] <- value
  x
}

#' @export
format.arch_array <- function(x, ...) {
  total_length <- x$array_data$length
  sprintf("<arch_array %s[%s]>", x$schema$format, format(total_length))
}

#' @export
print.arch_array <- function(x, ...) {
  cat(sprintf("%s\n", format(x)))

  cat("- schema:\n")
  print(x$schema, indent.str = "  ")

  cat(sprintf("- array_data:\n"))
  print(x$array_data, indent.str = "  ")

  invisible(x)
}
