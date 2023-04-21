html_expect <- function(x, ...) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)
  
  x_res <- rlang::eval_tidy(x)
  if (is.logical(x_res)) {
    timeout <- get_session()$timeout

    res <- length(dots) == 0
    
    if (length(dots) == 1) {
      val <- rlang::eval_tidy(dots[[1]])
      res <- isTRUE(val)
    } else if (!res) {
      vals <- lapply(dots, rlang::eval_tidy)
      lgls <- vapply(vals, isTRUE, FUN.VALUE = logical(1))
      res <- all(lgls)
    }

    if (!res) {
      exprs <- c(x, dots)
      error_exprs <- exprs
      
      res <- retry_with_timeout(timeout, exprs)
    }
  } else {
    timeout <- x_res$timeout

    if (length(dots) == 0) {
      cli::cli_abort(c(
        "No conditions were specified",
        "i" = "Try specifying a condition",
        "x" = "Instead of: {.code html_expect(element)}",
        "v" = "Try: {.code html_expect(element, exists)}"
      ))
    }
    
    error_exprs <- dots
    exprs <- lapply(dots, parse_condition, x_res)
    res <- retry_with_timeout(timeout, exprs)
  }

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
