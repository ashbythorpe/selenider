#' Do a set of conditions all return TRUE within a given time frame?
#'
#' Repeatedly execute a list of conditions, until all return `TRUE`,
#' or a certain number of seconds has passed.
#'
#' @param timeout The time to wait for the conditions to succeed.
#' @param exprs The conditions to evaluate: a list of quosures.
#' @param data_mask The data mask to use when evaluating the quosures.
#'
#' @details
#' The function takes care to make sure `timeout = 0` works as expected.
#'
#' @returns
#' Either `TRUE` or a list of two items:
#' * `n` - The number/index of the condition that failed.
#' * `val` - The value returned by the condition instead of `TRUE`.
#'
#' @noRd
retry_with_timeout <- function(timeout, exprs, data_mask = NULL) {
  end <- Sys.time() + timeout

  if (length(exprs) == 0) {
    TRUE
  } else if (length(exprs) == 1) {
    res <- retry_until_true(
      timeout,
      function() eval_condition(exprs[[1]], data_mask)
    )

    if (isTRUE(res)) {
      TRUE
    } else {
      list(
        n = 1,
        val = res
      )
    }
  } else {
    retry_until_true(
      timeout,
      function() {
        pass <- TRUE

        for (a in seq_along(exprs)) {
          expr <- exprs[[a]]
          val <- eval_condition(expr, data_mask)
          if (!isTRUE(val)) {
            res <- list(
              n = a,
              val = val
            )

            pass <- FALSE

            break
          }
        }

        if (pass) {
          TRUE
        } else {
          res
        }
      }
    )
  }
}

eval_condition <- function(x, data_mask = NULL) {
  try_fetch(
    with_timeout(0, eval_tidy(x, data = data_mask)),
    expect_error_continue = function(x) x
  )
}

#' Evaluate conditions on multiple elements until a timeout is reached
#'
#' A varient of [retry_with_timeout()] for [elem_expect_all()].
#'
#' @param timeout,exprs Same as [retry_with_timeout()]
#' @param elements The element collection.
#' @param name The element name, used to create the data mask.
#'
#' @noRd
retry_with_timeout_multiple <- function(timeout, exprs, elements, name) {
  end <- Sys.time() + timeout

  if (length(exprs) == 0) {
    return(TRUE)
  } else if (length(exprs) == 1) {
    if (timeout == 0) {
      val <- eval_condition_multiple(exprs[[1]], elements, name)
    } else {
      while (Sys.time() <= end) {
        val <- eval_condition_multiple(exprs[[1]], elements, name)

        if (isTRUE(val)) {
          return(TRUE)
        }
      }
    }

    if (isTRUE(val)) {
      return(TRUE)
    } else {
      res <- list(
        expr_n = 1,
        element_n = val$n,
        val = val$val
      )
    }
  } else if (timeout == 0) {
    pass <- TRUE

    for (a in seq_along(exprs)) {
      expr <- exprs[[a]]
      val <- eval_condition_multiple(expr, elements, name)
      if (!isTRUE(val)) {
        res <- list(
          expr_n = 1,
          element_n = val$n,
          val = val$val
        )

        pass <- FALSE

        break
      }
    }

    if (pass) {
      return(TRUE)
    }
  } else {
    while (Sys.time() <= end) {
      pass <- TRUE

      for (a in seq_along(exprs)) {
        expr <- exprs[[a]]
        val <- eval_condition_multiple(expr, elements, name)
        if (!isTRUE(val)) {
          res <- list(
            expr_n = 1,
            element_n = val$n,
            val = val$val
          )

          pass <- FALSE

          break
        }
      }

      if (pass) {
        return(TRUE)
      }
    }
  }

  return(res)
}

eval_condition_multiple <- function(x, elements, name) {
  len <- elem_size(elements)
  for (index in seq_len(len)) {
    element <- elements[[index]]
    data_mask <- list(element)
    names(data_mask) <- name

    res <- try_fetch(
      with_timeout(0, eval_tidy(x, data = data_mask)),
      expect_error_continue = function(x) x
    )

    if (!isTRUE(res)) {
      return(list(
        n = index,
        val = res
      ))
    }
  }

  TRUE
}
