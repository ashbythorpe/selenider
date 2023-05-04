lazy_filter <- function(x, .f, ...) {
  if (is.numeric(.f)) {
    return(lazy_slice(
      x = x,
      n = .f
    ))
  }
  
  res <- list(
    x = x,
    fun = .f,
    args = rlang::list2(...)
  )

  class(res) <- c("lazy_filter")

  res
}

increment_iterator <- function(x) {
  if (inherits(x, "lazy_filter")) {
    list <- x$x
    fun <- x$fun
    args <- x$args

    while (!is.null(list)) {
      curr <- current_item(list)
      if (do.call(fun, c(curr, args))) {
        return(lazy_filter(
          x = x,
          .f = fun,
          args = args
        ))
      }
      list <- increment_iterator(list)
    }

    return(NULL)
  } else {
    if (length(x) == 1) {
      NULL
    } else {
      x[-1]
    }
  }
}

current_item <- function(x) {
  if (inherits(x, "lazy_filter")) {
    x$x[[1]]
  } else {
    x[[1]]
  }
}

all_items <- function(x) {
  if (inherits(x, "lazy_filter")) {
    x$x
  } else {
    x
  }
}

item_slice <- function(x, n) {
  if (inherits(x, "lazy_filter")) {
    if (is.numeric(n) && all(n >= 0)) {
      n <- n[n != 0]
      if (length(na.rm(n)) == 0) {
        return(rep(NA, length(n)))
      }

      m <- max(n, na.rm = TRUE)
      res <- vector("list", length = length(n))

      for (a in seq_len(n)) {
        x <- increment_iterator(x)

        if (is.null(x)) {
          return(NULL)
        }
        
        res[n == a] <- list(current_item(x))
      }

      res
    } else {
      all_items(x)[n]
    }
  } else {
    x[n]
  }
}

item_extract <- function(x, n) {
  curr_n <- 1
  x <- increment_iterator(x)

  while (!is.null(x)) {
    if (curr_n == n) {
      return(current_item(x))
    }

    x <- increment_iterator(x)
    curr_n <- curr_n + 1
  }

  NULL
}

