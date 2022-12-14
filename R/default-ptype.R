
#' Guess prototypes for Arrow schemas
#'
#' @param schema An [arch_schema()]
#'
#' @return A zero-length array prototype
#' @export
#'
#' @examples
#' arch_default_ptype(arch_schema("n"))
#' arch_default_ptype(
#'   arch_schema("+s", children = list(arch_schema("i", "colname")))
#' )
#'
arch_default_ptype <- function(schema) {
  if (!inherits(schema, "arch_schema")) {
    stop("`schema` must be a `arch_schema()`", call. = FALSE)
  }

  if (!is.null(schema$dictionary)) {
    return(arch_default_ptype(schema$dictionary))
  }

  info <- parse_format(schema$format)
  switch(
    info$abbreviation,
    n =, b = logical(),
    c =, C =, s =, S =, i = integer(),
    d =, I =, l =, L =, e =, f =, g = double(),
    u =, U =, Z = character(),
    `+s` = {
      children_ptype <- lapply(schema$children, arch_default_ptype)
      children_names <- lapply(schema$children, "[[", "name")
      children_names[vapply(children_names, is.null, logical(1))] <- ""
      names(children_ptype) <- children_names
      as.data.frame(children_ptype)
    },
    stop(sprintf("Can't guess ptype for format string '%s'", schema$format), call. = FALSE)
  )
}
