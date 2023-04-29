#' @export
html_wait_for <- function(x, ...) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)
  
  result <- eval_conditions(x, dots)
  timeout <- result$timeout
  error_exprs <- result$error_exprs
  exprs <- result$exprs
  res <- result$res

  return(res)
}

