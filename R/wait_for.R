html_wait_for <- function(x, ...) {
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

      res <- retry_with_timeout(timeout, exprs)
    }
  } else {
    timeout <- x_res$timeout

    if (length(dots) == 0) {
      cli::cli_abort(c(
        "No conditions were specified",
        "i" = "Try specifying a condition",
        "x" = "Instead of: {.code html_wait_for(element)}",
        "v" = "Try: {.code html_wait_for(element, exists)}"
      ))
    }

    exprs <- lapply(dots, parse_condition, x_res)
    res <- retry_with_timeout(timeout, exprs)
  }

  return(res)
}


parse_condition <- function(x, elem) {
  env <- rlang::quo_get_env(x)
  
  if (rlang::quo_is_call(x)) {
    if (rlang::is_call_simple(x)) {
      name <- rlang::call_name(x)

      if (name %in% c("!", "negate", "Negate")) {
        return(rlang::new_quosure(
          rlang::call2(name, parse_condition(rlang::call_args(x)[[1]], elem)),
          env
        ))
      } else if (name %in% c("|", "||", "&", "&&")) {
        args <- rlang::call_args(x)
        return(rlang::new_quosure(
          rlang::call2(name, parse_condition(args[[1]], elem), parse_condition(args[[2]], elem)),
          env
        ))
      }
    } else {
      name <- NA
    }

    if (!name %in% c("function", "new_function", "as_function", "as_closure")) {
      if ("x" %in% rlang::call_args_names(x)) {
        return(x)
      } else {
        return(rlang::call_modify(x, x = elem))
      }
    }
  }

  rlang::new_quosure(rlang::call2(rlang::quo_get_expr(x), x = elem), env)
}
