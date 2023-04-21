retry_with_timeout <- function(timeout, exprs) {
  end <- Sys.time() + timeout

  if(length(exprs) == 0) {
    return(TRUE)
  } else if (length(exprs) == 1) {
    while (Sys.time() <= end) {
      val <- rlang::eval_tidy(exprs[[1]])
    }
    return(isTRUE(val))
  } else {
    while (Sys.time() <= end) {
      vals <- lapply(exprs, rlang::eval_tidy)
      lgls <- vapply(vals, isTRUE, FUN.VALUE = logical(1))
    }
    return(all(lgls))
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
