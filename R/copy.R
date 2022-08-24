
#' Create copies of Arrow vectors
#'
#' @param x An [arch_array()].
#'
#' @return An [arch_array()]
#' @export
#'
#' @examples
#' arch_deep_copy(as_arch_array(1:5))
#'
arch_deep_copy <- function(x) {
  .Call(arch_c_deep_copy, as_arch_array(x))
}
