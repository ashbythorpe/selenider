#' Evaluate a set of conditions with a timeout
#'
#' Evaluates a set of conditions, checking if
#' they all return TRUE within a timeout.
#'
#' @param x A quosure, the result of `enquo(x)` on the `x` argument
#'   to e.g. [elem_expect()]. This should either evaluate to a selenider
#'   element or a condition.
#' @param dots A list of quosures, which contain the rest of the conditions.
#' @param timeout The `timeout` argument to the parent timeout. This is not
#'   the final timeout, since we can only work this out once we know whether
#'   `x` is an element or not.
#'
#' @returns
#' A list:
#' * `timeout` - The timeout that was actually used.
#' * `calls` - The conditions, having been parsed into calls.
#' * `exprs` - The original expressions, before being parsed. In
#'   the case where `x` is also a condition, `calls` and `exprs`
#'   are the same.
#' * `res` - The result of the conditions. This can either be `TRUE`,
#'   signifying that all the conditions have returned `TRUE`, or a list
#'   containing the condition number (`n`) and the returned result (`val`).
#'
#' @noRd
eval_conditions <- function(x, dots, timeout) {
  x_res <- eval_condition(x)

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

# Variation of eval_conditions() for `elem_expect_all()`
# Simpler since the x cannot be a condition
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

#' Turn a condition into an executable call
#'
#' Parses a condition, turning it into a call that can be executed with:
#' `rlang::eval_tidy(result, data = rlang::list2(!!elem_name := <element>))`
#'
#' @param x A condition.
#' @param elem_name The variable name to use for the element we are performing
#'   the condition on.
#'
#' @returns
#' A quosure
#'
#' @noRd
parse_condition <- function(x, elem_name) {
  env <- quo_get_env(x)

  if (identical(deparse(quo_get_expr(x)), "exists")) {
    stop_condition_exists()
  }

  new_expr <- parse_condition_expr(quo_get_expr(x), elem_name)

  new_quosure(new_expr, env)
}

parse_condition_expr <- function(x, elem_name) {
  if (is_call(x)) {
    if (is_call_simple(x)) {
      name <- call_name(x)

      # Handle simple cases
      result <- parse_condition_simple(x, name, elem_name)

      if (!is.null(result)) {
        return(result)
      }
    } else {
      name <- NA
    }

    if (!name %in% c("function", "new_function", "as_function", "as_closure")) {
      if ("x" %in% call_args_names(x) &&
            name %in% names(rlang::ns_env("selenider"))) {
        return(x)
      } else {
        return(call_insert(x, elem_name, quo = FALSE))
      }
    }
  }

  call2(x, parse_expr(elem_name))
}

parse_condition_simple <- function(x, name, elem_name) {
  if (name == "{") {
    return(x)
  }

  if (name %in% c("(", "!")) {
    return(call2(
      name, parse_condition_expr(
        call_args(x)[[1]],
        elem_name
      ), .ns = call_ns(x)
    ))
  } else if (name %in% c("|", "||", "&", "&&")) {
    args <- call_args(x)

    return(call2(
      name,
      parse_condition_expr(args[[1]], elem_name),
      parse_condition_expr(args[[2]], elem_name),
      .ns = call_ns(x)
    ))
  } else if (name %in% c("all", "any")) {
    args <- call_args(x)
    return(call2(
      name,
      !!!lapply(
        args[names(args) != "na.rm"],
        parse_condition_expr,
        elem_name
      ),
      !!!args[names(args) == "na.rm"],
      .ns = call_ns(x)
    ))
  }

  NULL
}

#' Make a unique name for an element
#'
#' Creates a variable name that does not appear in one or more expressions.
#' This name can then be used as a variable in these expressions, without
#' having to worry about name conflicts.
#'
#' @param x A list of quosures
#' @param name The default name to use.
#'
#' @details
#' Works by adding underscores to `name` until it is not found in any of
#' `x`.
#'
#' @returns
#' A string
#'
#' @noRd
make_elem_name <- function(x, name = "element") {
  expr <- paste0(lapply(x, get_expr_string), collapse = "")

  collisions <- regmatches(
    expr,
    gregexpr(paste0("\\Q", name, "\\E(?:_*)"), expr)
  )[[1]]

  if (length(collisions) == 0) {
    return(name)
  }

  underscores <- nchar(collisions) - nchar(name)

  potential <- 0:length(underscores)

  underscores_needed <- setdiff(potential, underscores)[1]

  paste0(c(name, rep("_", underscores_needed)), collapse = "")
}

get_expr_string <- function(x) {
  paste0(deparse(quo_squash(x)), collapse = "")
}
