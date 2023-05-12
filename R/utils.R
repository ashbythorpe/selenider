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
  } else {
    if (timeout == 0) {
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
      }

      if (pass) {
        return(TRUE)
      }
    }
  }

  return(res)
}

eval_condition <- function(x, data_mask = NULL) {
  rlang::try_fetch(
    with_timeout(0, rlang::eval_tidy(x, data = data_mask)),
    error = function(x) x
  )
}

get_with_timeout <- function(timeout, .f, ...) {
  if (timeout == 0) {
    .f(...)
  } else {
    end = Sys.time() + timeout

    while (Sys.time() < timeout) {
      res <- .f(...)

      if (!is.null(res)) {
        return(res)
      }
    }

    NULL
  }
}

find_element <- function(x, using, value) {
  if (inherits(x, "webElement")) {
    rlang::try_fetch(
      suppressMessages(x$findChildElement(using = using, value = value)),
      error = function(cnd) {
        if (grepl("NoSuchElement", cnd$message, fixed = TRUE)) {
          NULL
        } else {
          rlang::zap()
        }
      }
    )
  } else {
    rlang::try_fetch(
      suppressMessages(x$findElement(using = using, value = value)),
      error = function(cnd) {
        if (grepl("NoSuchElement", cnd$message, fixed = TRUE)) {
          NULL
        } else {
          rlang::zap()
        }
      }
    )
  }
}

find_elements <- function(x, using, value) {
  if (inherits(x, "webElement")) {
    x$findChildElements(using = using, value = value)
  } else {
    x$findElements(using = using, value = value)
  }
}

# Adapted from scales::ordinal()
ordinal <- function(x) {
  res <- character(length(x))
  res[x == 1] <- "first"
  res[x == 2] <- "second"
  res[x == 3] <- "third"
  res[res == ""] <- ordinal_numbers(x[res == ""])
}

ordinal_numbers <- function(x) {
  rules <- list(
    st = "(?<!1)1$",
    nd = "(?<!1)2$",
    rd = "(?<!1)3$",
    th = "(?<=1)[123]$",
    th = "[0456789]$",
    th = "."
  )
  
  out <- utils::stack(lapply(rules, grep, x = x, perl = TRUE))
  out <- out[!duplicated(out$values), ] # only first result should be considered
  paste0(
    x,
    out$ind[order(out$values)]
  )
}

call_insert <- function(call, elem_name, quo = TRUE) {
  new_call <- rlang::call2(
    as.list(call)[[1]], 
    rlang::parse_expr(elem_name), 
    !!!rlang::call_args(call)
  )

  if (quo) {
    rlang::new_quosure(new_call, rlang::quo_get_env(call))
  } else {
    new_call
  }
}

