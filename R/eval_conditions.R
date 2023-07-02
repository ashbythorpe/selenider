eval_conditions <- function(x, dots, timeout) {
  x_res <- eval_tidy(x)

  if (inherits(x_res, c("selenider_element", "selenider_elements"))) {
    timeout <- get_timeout(timeout, x_res$timeout)
    
    if (length(dots) == 0) {
      stop_no_conditions()
    }
    
    exprs <- dots
    elem_name <- make_elem_name(dots)
    calls <- lapply(dots, parse_condition, elem_name)
    data_mask <- list(x_res)
    names(data_mask) <- elem_name
    res <- retry_with_timeout(timeout, calls, data_mask)
  } else {
    timeout <- get_timeout(timeout, NULL)
    
    res <- NULL
    end <- FALSE
    if (isTRUE(x_res)) {
      res <- retry_with_timeout(0, dots)
      if (timeout == 0) {
        end <- TRUE
        if (!isTRUE(res)) {
          res$n <- res$n + 1 # Since this doesn't include x_res
        }
      }
    } else if (timeout == 0) {
      res <- list(n = 1, val = x_res)
      end <- TRUE
    }
    
    exprs <- c(x, dots)
    calls <- exprs

    if (!isTRUE(res) && !end) {
      res <- retry_with_timeout(timeout, exprs)
    }
  }
  
  list(
    timeout = timeout,
    calls = calls,
    exprs = exprs,
    res = res,
    x_res = x_res
  )
}

eval_all_conditions <- function(x, dots, timeout) {
  timeout <- get_timeout(timeout, x$timeout)

  if (length(dots) == 0) {
    stop_no_conditions()
  } 

  elem_name <- make_elem_name(dots)
  calls <- lapply(dots, parse_condition, elem_name)
  res <- retry_with_timeout_multiple(timeout, calls, x, elem_name)
  
  list(
    timeout = timeout,
    calls = calls,
    exprs = dots,
    res = res
  )
}

parse_condition <- function(x, elem_name) {
  env <- quo_get_env(x)
  
  if (quo_is_call(x)) {
    if (is_call_simple(x)) {
      name <- call_name(x)

      if (name == "exists") {
        stop_condition_exists()
      }
      
      if (name %in% c("(", "!", "negate", "Negate")) {
        return(new_quosure(call2(
          name, parse_condition_expr(call_args(x)[[1]], elem_name)
        )))
      } else if (name %in% c("|", "||", "&", "&&")) {
        args <- call_args(x)

        return(new_quosure(
          call2(
            name, 
            parse_condition_expr(args[[1]], elem_name),
            parse_condition_expr(args[[2]], elem_name)
          ),
          env
        ))
      }
    } else {
      name <- NA
    }
    
    if (!name %in% c("function", "new_function", "as_function", "as_closure")) {
      if ("x" %in% call_args_names(x)) {
        return(x)
      } else {
        return(call_insert(x, elem_name))
      }
    }
  }
  
  if (identical(deparse(quo_get_expr(x)), "exists")) {
    stop_condition_exists()
  }

  new_quosure(call2(
    quo_get_expr(x),
    parse_expr(elem_name)
  ), env)
}

parse_condition_expr <- function(x, elem_name) {
  if (is_call(x)) {
    if (is_call_simple(x)) {
      name <- call_name(x)
      
      if (name %in% c("(", "!", "negate", "Negate")) {
        return(call2(
          name, parse_condition_expr(call_args(x)[[1]], elem_name)
        ))
      } else if (name %in% c("|", "||", "&", "&&")) {
        args <- call_args(x)

        return(call2(
          name, 
          parse_condition_expr(args[[1]], elem_name),
          parse_condition_expr(args[[2]], elem_name)
        ))
      }
    } else {
      name <- NA
    }
    
    if (!name %in% c("function", "new_function", "as_function", "as_closure")) {
      if ("x" %in% call_args_names(x)) {
        return(x)
      } else {
        return(call_insert(x, elem_name, quo = FALSE))
      }
    }
  }
  
  call2(x, parse_expr(elem_name))
}

make_elem_name <- function(x) {
  expr <- paste0(lapply(x, get_expr_string), collapse = "")

  collisions <- regmatches(expr, gregexpr("element(?:_*)", expr))[[1]]

  if (length(collisions) == 0) {
    return("element")
  }

  underscores <- nchar(collisions) - 7

  potential <- 0:length(underscores)

  underscores_needed <- setdiff(potential, underscores)[1]

  paste0(c("element", rep("_", underscores_needed)), collapse = "")
}

get_expr_string <- function(x) {
  paste0(deparse(quo_squash(x)), collapse = "")
}
