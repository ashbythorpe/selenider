#' @export
#'
#' @rdname html_expect
html_wait_until <- function(x, ..., timeout = NULL) {
  x <- enquo(x)
  dots <- enquos(...)

  result <- eval_conditions(x, dots, timeout)
  res <- result$res

  return(isTRUE(res))
}
