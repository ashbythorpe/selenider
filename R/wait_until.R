#' @export
#'
#' @rdname html_expect
html_wait_until <- function(x, ..., timeout = NULL) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)

  result <- eval_conditions(x, dots, timeout)
  res <- result$res

  return(isTRUE(res))
}
