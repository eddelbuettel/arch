
#' Create R vector wrappers around 'Arrow' arrays
#'
#' @param array A [arch_array()]
#' @param x An object to convert to a [arch_vctr()]
#' @param ... Passed to [as_arch_array()]
#'
#' @return An object of class 'arch_vctr'
#' @export
#'
#' @examples
#' as_arch_vctr(1:10)
#'
arch_vctr <- function(array = arch_array()) {
  array <- as_arch_array(array)
  new_arch_vctr(
    seq_len0(array$array_data$length),
    array = array
  )
}

#' @rdname arch_vctr
#' @export
as_arch_vctr <- function(x, ...) {
  UseMethod("as_arch_vctr")
}

#' @export
as_arch_vctr.arch_vctr <- function(x, ...) {
  x
}

#' @export
as_arch_vctr.default <- function(x, ...) {
  array <- as_arch_array(x, ...)
  new_arch_vctr(
    seq_len0(array$array_data$length),
    array = array
  )
}

#' @export
as_arch_array.arch_vctr <- function(x, ...) {
  indices <- vctr_indices(x)
  array <- attr(x, "array", exact = TRUE)

  if (identical(indices, seq_len0(array$array_data$length))) {
    array
  } else {
    assert_arrow("arch_vctr() subset")
    arrow_array <- from_arch_array(array, arrow::Array)
    as_arch_array(arrow_array$Take(indices))
  }
}

#' @rdname arch_vctr
#' @export
new_arch_vctr <- function(x = integer(), array = arch_array()) {
  stopifnot(inherits(array, "arch_array"), is.numeric(x))
  structure(x, class = arch_vctr_class(array$schema), array = array)
}

arch_vctr_class <- function(schema) {
  extension <- schema$metadata[["ARROW:extension:name"]]
  if (!is.null(extension)) {
    ext_sanitized <- gsub("[^0-9A-Za-z_.]+", "_", extension)
    ext_parts <- strsplit(extension, ".", fixed = TRUE)[[1]]
    class_ext <- vapply(
      seq_along(ext_parts),
      function(i) paste(ext_parts[seq_len(i)], collapse = "_"),
      character(1)
    )

    class_ext <- paste0("arch_vctr_", rev(class_ext))
  } else {
    class_ext <- NULL
  }

  c(class_ext, "arch_vctr")
}

vctr_indices <- function(x) {
  # probably creates a copy
  structure(x, class = NULL, array = NULL)
}

seq_len0 <- function(x) {
  if (x == 0) integer() else 0:(x - 1)
}

#' @export
format.arch_vctr <- function(x, ...) {
  format(from_arch_array(as_arch_array(x)), ...)
}

#' @export
print.arch_vctr <- function(x, ...) {
  cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))

  if (length(x) == 0) {
    return(invisible(x))
  }

  max_print <- getOption("max.print", 1000)

  x_head <- utils::head(x, max_print)
  formatted <- format(x_head)
  formatted <- stats::setNames(formatted, names(x_head))
  print(formatted, ..., quote = FALSE)

  if (length(x) > max_print) {
    cat(sprintf("Reached max.print (%s)\n", max_print))
  }

  invisible(x)
}

#' @export
str.arch_vctr <- function(object, ..., indent.str = "", width = getOption("width")) {
  if (length(object) == 0) {
    cat(paste0(" ", class(object)[1], "[0]\n"))
    return(invisible(object))
  }

  # estimate possible number of elements that could be displayed
  # to avoid formatting too many
  width <- width - nchar(indent.str) - 2
  length <- min(length(object), ceiling(width / 5))

  formatted <- format(utils::head(object, length), trim = TRUE)

  title <- paste0(" ", class(object)[1], "[1:", length(object), "]")
  cat(
    paste0(
      title,
      " ",
      strtrim(paste0(formatted, collapse = " "), width - nchar(title)),
      "\n"
    )
  )
  invisible(object)
}

#' @export
`[.arch_vctr` <- function(x, i) {
  assert_arrow("arch_vctr() subset")
  indices <- NextMethod()
  arrow_array <- from_arch_array(attr(x, "array", exact = TRUE), arrow::Array)
  array <- as_arch_array(arrow_array$Take(indices))
  array$schema <- attr(x, "array", exact = TRUE)$schema
  arch_vctr(array)
}

#' @export
`[[.arch_vctr` <- function(x, i) {
  x[i]
}

#' @export
`[<-.arch_vctr` <- function(x, i, value) {
  stop("Subset-assign is not supported for arch_vctr")
}

#' @export
`[[<-.arch_vctr` <- function(x, i, value) {
  x[i] <- value
  x
}

#' @export
c.arch_vctr <- function(...) {
  dots <- list(...)
  arrays <- lapply(dots, attr, "array", exact = TRUE)
  arrays_identical <- if (length(arrays) > 1) Reduce(identical, arrays) else TRUE

  if (arrays_identical) {
    new_arch_vctr(do.call(c, lapply(dots, vctr_indices)), array = arrays[[1]])[]
  } else {
    stop("Concatenate() is not yet exposed in Arrow C++", call. = FALSE)
    arrow_arrays <- lapply(dots, from_arch_array, arrow::Array)
    arrow_array <- NULL
    arch_vctr(as_arch_array(arrow_array))
  }
}

#' @export
rep.arch_vctr <- function(x, ...) {
  new_arch_vctr(NextMethod(), attr(x, "array", exact = TRUE))[]
}

#' @method rep_len arch_vctr
#' @export
rep_len.arch_vctr <- function(x, ...) {
  indices <- rep_len(vctr_indices(x), ...)
  new_arch_vctr(indices, attr(x, "array", exact = TRUE))[]
}

# data.frame() will call as.data.frame() with optional = TRUE
#' @export
as.data.frame.arch_vctr <- function(x, ..., optional = FALSE) {
  if (!optional) {
    NextMethod()
  } else {
    new_data_frame(list(x))
  }
}

# exported in zzz.R
vec_proxy.arch_vctr <- function(x, ...) {
  x
}

vec_restore.arch_vctr <- function(x, to, ...) {
  to_a <- attr(to, "array", exact = TRUE)
  new_arch_vctr(x, array = to_a)[]
}

#' @export
Math.arch_vctr <- function(x, ...) {
  switch(
    .Generic,
    abs =, sign =, sqrt =,
    floor =, ceiling =, trunc =,
    round =, signif =,
    exp =, log =, expm1 =, log1p =,
    cos =, sin =, tan =,
    cospi =, sinpi =, tanpi =,
    acos =, asin =, atan =,
    cosh =, sinh =, tanh =,
    acosh =, asinh =, atanh =,
    lgamma =, gamma =, digamma =, trigamma =,
    cumsum =, cumprod =, cummax =, cumin = {
      assert_arrow("Math group generics")
      array <- as_arch_array(x)
      arrow_array <- from_arch_array(array, arrow::Array)
      getNamespace("base")[[.Generic]](arrow_array)
    },
    stop(sprintf("Math generic '%s' not supported for arch_vctr()", .Generic)) # nocov
  )
}

#' @export
Ops.arch_vctr <- function(e1, e2) {
  if (missing(e2)) {
    switch(
      .Generic,
      "!" = {
        assert_arrow("Unary Ops group generics")
        array <- as_arch_array(e1)
        arrow_array <- from_arch_array(array, arrow::Array)$cast(arrow::bool())
        result <- getNamespace("base")[[.Generic]](arrow_array)
        return(as_arch_vctr(result))
      },
      "+" =, "-" = {
        assert_arrow("Unary Ops group generics")
        array <- as_arch_array(e1)
        arrow_array <- from_arch_array(array, arrow::Array)
        result <- getNamespace("base")[[.Generic]](arrow_array)
        return(as_arch_vctr(result))
      },
      # R catches these before we do with 'invalid unary operator'
      stop(sprintf("Unary '%s' not supported for arch_vctr()", .Generic)) # nocov
    )
  }

  switch(
    .Generic,
    "+" =, "-" =, "*" =, "/" =, "^" =, "%%" =, "%/%" =,
    "==" =, "!=" =, "<" =, "<=" =, ">=" =, ">" = {
      assert_arrow("Ops group generics")
      vctr1 <- as_arch_vctr(e1)
      vctr2 <- as_arch_vctr(e2)
      array1 <- as_arch_array(vctr1)
      array2 <- as_arch_array(vctr2)
      arrow_array1 <- from_arch_array(array1, arrow::Array)
      arrow_array2 <- from_arch_array(array2, arrow::Array)

      result <- getNamespace("base")[[.Generic]](arrow_array1, arrow_array2)
      as_arch_vctr(result)
    },
    "&" =, "|" = {
      assert_arrow("Ops group generics")
      vctr1 <- as_arch_vctr(e1)
      vctr2 <- as_arch_vctr(e2)
      array1 <- as_arch_array(vctr1)
      array2 <- as_arch_array(vctr2)
      arrow_array1 <- from_arch_array(array1, arrow::Array)$cast(arrow::bool())
      arrow_array2 <- from_arch_array(array2, arrow::Array)$cast(arrow::bool())

      result <- getNamespace("base")[[.Generic]](arrow_array1, arrow_array2)
      as_arch_vctr(result)
    },
    stop(sprintf("Ops generic '%s' not supported for arch_vctr()", .Generic)) # nocov
  )
}

#' @export
Summary.arch_vctr <- function(x, ..., na.rm = FALSE) {
  assert_arrow("Math group generics")
  switch(
    .Generic,
    all =, any = {
      # make sure dots are empty because we ignore them
      stopifnot(...length() == 0L)

      array <- as_arch_array(x)
      # bool compute functions don't support non-bool inputs
      arrow_array <- from_arch_array(array, arrow::Array)$cast(arrow::bool())
      getNamespace("base")[[.Generic]](arrow_array, na.rm = na.rm)
    },
    sum =, prod =,
    min =, max =,
    range = {
      # make sure dots are empty because we ignore them
      stopifnot(...length() == 0L)

      array <- as_arch_array(x)
      arrow_array <- from_arch_array(array, arrow::Array)
      getNamespace("base")[[.Generic]](arrow_array, na.rm = na.rm)
    },
    stop(sprintf("Summary generic '%s' not supported for arch_vctr()", .Generic)) # nocov
  )
}

#' @export
Complex.arch_vctr <- function(z) {
  stop("Complex group generics are not supported for arch_vctr", call. = FALSE)
}
