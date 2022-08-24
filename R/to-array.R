
#' Convert R objects to Arrow vectors
#'
#' These methods return an [arch_array()] for R objects that don't involve
#' copying or unnecessary allocating. Two excpetions are (1) ALTREP objects,
#' which will be expanded, and (2) character vectors, which will be converted
#' to UTF-8 and serialized as a single [raw()] vector.
#'
#' @inheritParams arch_array
#' @inheritParams arch_schema
#'
#' @return An [arch_array()]
#' @export
#'
#' @examples
#' as_arch_array(NULL)
#' as_arch_array(c(TRUE, FALSE, NA))
#' as_arch_array(1:10)
#' as_arch_array(c(1.1, 2.2))
#' as_arch_array(as.raw(0x00))
#' as_arch_array("fish")
#' as_arch_array(data.frame(x = 1:10, y = as.raw(1:10)))
#'
as_arch_array.NULL <- function(x, ..., name = "") {
  arch_array(arch_schema("n", name), arch_array_data(null_count = 0))
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.logical <- function(x, ..., name = "") {
  x_is_na <- is.na(x)
  arch_array(
    arch_schema("i", name),
    arch_array_data(
      buffers = if (any(x_is_na)) list(as_arch_bitmask(!x_is_na), x) else list(NULL, x),
      length = length(x),
      null_count = sum(x_is_na),
      offset = 0
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.integer <- function(x, ..., name = "") {
  x_is_na <- is.na(x)
  arch_array(
    arch_schema("i", name),
    arch_array_data(
      buffers = if (any(x_is_na)) list(as_arch_bitmask(!x_is_na), x) else list(NULL, x),
      length = length(x),
      null_count = sum(x_is_na),
      offset = 0
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.double <- function(x, ..., name = "") {
  x_is_na <- is.na(x)
  arch_array(
    arch_schema("g", name),
    arch_array_data(
      buffers = if (any(x_is_na)) list(as_arch_bitmask(!x_is_na), x) else list(NULL, x),
      length = length(x),
      null_count = sum(x_is_na),
      offset = 0
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.character <- function(x, ..., name = "") {
  x_is_na <- is.na(x)

  # flatten and check for long data vector
  buffers <- .Call(arch_c_buffers_from_character, x)
  if (length(buffers[[2]]) <= (2 ^ 31 - 1)) {
    buffers[[1]] <- as.integer(as.numeric(buffers[[1]]))
    format <- "u"
  } else {
    format <- "U"
  }

  if (any(x_is_na)) {
    buffers <- c(list(as_arch_bitmask(!x_is_na)), buffers)
  } else {
    buffers <- c(list(NULL), buffers)
  }

  arch_array(
    arch_schema(format, name),
    arch_array_data(
      buffers = buffers,
      length = length(x),
      null_count = sum(x_is_na),
      offset = 0
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.factor <- function(x, ..., name = "") {
  x_is_na <- is.na(x)

  # indices are 1-based and Arrow needs 0-based
  # could also add a factor level here to avoid copying the
  # indices vector but this makes it harder
  # to round-trip a factor() and a little disingenuous
  dictionary_array <- as_arch_array(levels(x))
  indices <- unclass(x) - 1L

  arch_array(
    arch_schema(
      "i", name,
      dictionary = dictionary_array$schema
    ),
    arch_array_data(
      buffers = if (any(x_is_na)) list(as_arch_bitmask(!x_is_na), indices) else list(NULL, indices),
      length = length(x),
      null_count = sum(x_is_na),
      offset = 0,
      dictionary = dictionary_array$array_data
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.raw <- function(x, ..., name = "") {
  arch_array(
    arch_schema("C", name),
    arch_array_data(
      buffers = list(NULL, x),
      length = length(x),
      null_count = 0,
      offset = 0
    )
  )
}

#' @export
#' @rdname as_arch_array.NULL
as_arch_array.data.frame <- function(x, ..., name = "", nullable = FALSE) {
  arrays <- Map(as_arch_array, x, name = names(x), nullable = TRUE)
  array_data <- lapply(arrays, "[[", "array_data")

  arch_array(
    arch_schema(
      "+s", name,
      flags = arch_schema_flags(nullable = nullable),
      children = lapply(arrays, "[[", "schema")),
    arch_array_data(
      buffers = list(NULL),
      length = nrow(x),
      null_count = 0,
      children = array_data
    )
  )
}
