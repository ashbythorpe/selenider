#' @export
html_expect <- function(x, ...) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)
  
  result <- eval_conditions(x, dots)
  timeout <- result$timeout
  error_exprs <- result$error_exprs
  exprs <- result$exprs
  res <- result$res

  if (!res) {
    for (a in seq_along(exprs)) {
      expr <- exprs[[a]]
      error_expr <- error_exprs[[a]]
      
      expr_res <- rlang::eval_tidy(expr)
      if (!expr_res) {
        cli::cli_abort(c(
          "Condition failed after waiting for {timeout} seconds:",
          "{.code {rlang::quo_get_expr(error_expr)}}"
        ))
      }
    }
  }

  if (is.logical(x_res)) {
    NULL
  } else {
    update_element(x_res)
  }
}
