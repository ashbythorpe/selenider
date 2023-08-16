retry_with_timeout <- function(timeout, exprs, data_mask = NULL) {
  end <- Sys.time() + timeout

  if(length(exprs) == 0) {
    return(TRUE)
  } else if (length(exprs) == 1) {
    if (timeout == 0) {
      val <- eval_condition(exprs[[1]], data_mask)
    } else {
      while (Sys.time() <= end) {
        val <- eval_condition(exprs[[1]], data_mask)

        if (isTRUE(val)) {
          return(TRUE)
        }
      }
    }

    if (isTRUE(val)) {
      return(TRUE)
    } else {
      res <- list(
        n = 1,
        val = val
      )
    }
  } else if (timeout == 0) {
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
      return(TRUE)
    }
  } else {
    while (Sys.time() <= end) {
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
        return(TRUE)
      }
    }
  }

  return(res)
}

eval_condition <- function(x, data_mask = NULL) {
  try_fetch(
    with_timeout(0, eval_tidy(x, data = data_mask)),
    # TODO: Use extensible/general error class
    selenider_error_absent_element = function(x) x
  )
}

retry_with_timeout_multiple <- function(timeout, exprs, elements, name) {
  end <- Sys.time() + timeout

  if(length(exprs) == 0) {
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
  len <- html_size(elements)
  for (index in seq_len(len)) {
    element <- elements[[index]]
    data_mask <- list(element)
    names(data_mask) <- name

    res <- try_fetch(
      with_timeout(0, eval_tidy(x, data = data_mask)),
      selenider_error_absent_element = function(x) x
    )

    if (!isTRUE(res)) {
      return(list(
        n = index,
        val = res
      ))
    }

    index <- index + 1
  }

  TRUE
}

