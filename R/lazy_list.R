counter <- function() {
  x <- 0
  list(
    increment = function() x <<- x + 1,
    get = function() x,
    set = function(y) x <<- y
  )
}

state <- function(init) {
  x <- force(init)
  list(
    get = function() x,
    set = function(y) x <<- y
  )
}

lazy_list <- function(x) {
  res <- list(
    factory = x,
    instance = state(x()),
    current_value = counter()
  )

  class(res) <- "lazy_list"

  res
}

reset_iterator <- function(x) {
  if (inherits(x, "lazy_list")) {
    x$instance$set(x$factory())
  }
  x$current_value$set(0)
  invisible(x)
}

next_value <- function(x) {
  if (inherits(x, "lazy_list")) {
    x$current_value$increment()
    x$instance$get()()
  } else {
    index <- x$current_value$increment()
    if (index > length(x$data)) {
      coro::exhausted()
    } else {
      x$data[[index]]
    }
  }
}

`[[.lazy_list` <- function(x, i, ...) {
  current_value <- x$current_value$get()
  result <- if (i >= current_value) {
    for (a in seq_len(i - current_value)) {
      next_value(x)
    }
  } else {
    reset_iterator(x)
    for (a in seq_len(i)) {
      next_value(x)
    }
  }

  if (coro::is_exhausted(result)) {
    NULL
  } else {
    result
  }
}

`[[.eager_list` <- function(x, i) {
  x$data[[i]]
}

check_lazylist <- function(x) {
  if (!inherits_any(x, c("lazy_list", "eager_list"))) {
    eager_list(x)
  } else {
    x
  }
}

eager_list <- function(x) {
  res <- list(
    current_value = counter(),
    data = x
  )

  class(res) <- "eager_list"

  res
}

`[.lazy_list` <- function(x, i, ...) {
  force(x)
  force(i)

  generator <- coro::generator(function() {
    current_value <- x$current_value$get()

    if (all(i >= 0)) {
      before <- i[i < current_value]
      after <- i[i >= current_value]

      for (a in after) {
        res <- get_item(x, a)
        if (!is.null(res)) {
          coro::yield(res)
        }
      }

      for (a in before) {
        res <- get_item(x, a)
        if (!is.null(res)) {
          coro::yield(res)
        }
      }
    } else {
      to_exclude <- abs(after)
      max_exclude <- max(to_exclude)

      for (a in seq_len(max(to_exclude) - current_value)) {
        if ((current_value + a) %in% to_exclude) {
          res <- get_item(x, current_value + a)
          if (!is.null(res)) {
            coro::yield(res)
          }
        }
      }

      if (!all(seq_len(current_value) %in% to_exclude)) {
        for (a in seq_len(current_value)) {
          if (!a %in% to_exclude) {
            res <- get_item(x, a)
            if (!is.null(res)) {
              coro::yield(res)
            }
          }
        }
      }
    }

    coro::exhausted()
  })

  lazy_list(generator)
}

lazy_filter <- function(x, .f) {
  x <- check_lazylist(x)
  force(.f)

  generator <- coro::generator(function() {
    value <- next_value(x)

    while (!coro::is_exhausted(value)) {
      if (.f(value)) {
        coro::yield(value)
      }

      value <- next_value(x)
    }

    coro::exhausted()
  })
  
  lazy_list(generator)
}

combine_lazy_lists <- function(x) {
  x <- lapply(x, check_lazylist)

  generator <- coro::generator(function() {
    for (l in x) {
      value <- next_value(l)

      while (!coro::is_exhausted(value)) {
        coro::yield(value)
        value <- next_value(l)
      }
    }

    coro::exhausted()
  })

  lazy_list(generator)
}

c.lazy_list <- function(x, ...) {
  combine_lazy_lists(rlang::list2(x, ...))
}

length.lazy_list <- function(x) {
  length(coro::collect(x$instance$get())) + x$current_value$get()
}

#' @exportS3Method as.list lazy_list
as.list.lazy_list <- function(x, ...) {
  if (x$current_value$get() != 0) {
    reset_iterator(x)
  }

  coro::collect(x$instance$get())
}

get_item <- function(x, i) {
  if (!inherits(x, "lazy_list") && i > length(x)) {
    NULL
  } else {
    x[[i]]
  }
}
