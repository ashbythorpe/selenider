# Simple mutable objects
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

#' Create a lazy list
#'
#' A lazy list is an object that uses a [coro::generator()] under the
#' hood, but mimics the random access properties of a normal list. Every
#' element of a lazy list can be accessed at any time, although the list
#' performs most efficiently when accessing elements in ascending order.
#' Alternatively, use `next_value_start()` and `next_value()` to use the
#' iterator normally, while ensuring that it can be used multiple times.
#'
#' @param x A [coro::generator()]: the factory, not an instance.
#'
#' @returns
#' A lazy list contains three parts:
#' * `factory` - Stores `x`.
#' * `instance` - Stores the current instance of the generator
#' * `current_value` - Stores the index of the last value that the generator has given.
#'
#' @noRd
lazy_list <- function(x) {
  res <- list(
    factory = x,
    instance = state(x()),
    current_value = counter()
  )

  class(res) <- "lazy_list"

  res
}

#' Reset the iterator instance of a lazy list
#'
#' Resets the `instance` and `current_value` fields of a lazy list, meaning
#' that `next_value()` will access the first element.
#'
#' @param x A lazy_list
#'
#' @returns `x`. Storing the result of this function is optional, since
#' `x` is mutated either way.
#'
#' @noRd
reset_iterator <- function(x) {
  stopifnot(inherits_any(x, c("lazy_list", "eager_list")))
  if (inherits(x, "lazy_list")) {
    x$instance$set(x$factory())
  }
  x$current_value$set(0)
  invisible(x)
}

#' Get the first value of a lazy list
#'
#' Resets the lazy list if needed, then calls `next_value()` on it. Use
#' this to begin iterating over a lazy list.
#'
#' @param x A lazy list.
#' 
#' @returns The first value of `x` (or [coro::exhausted()])
#'
#' @noRd
next_value_start <- function(x) {
  stopifnot(inherits_any(x, c("lazy_list", "eager_list")))
  if (x$current_value$get() > 0) {
    reset_iterator(x)
  }

  next_value(x)
}

#' Get the next value of a lazy list
#'
#' Gets the next value of a lazy list, returning [coro::exhausted()] if we
#' have reached the end of the list.
#'
#' @param x A lazy list.
#'
#' @returns The value.
#'
#' @noRd
next_value <- function(x) {
  if (inherits(x, "lazy_list")) {
    x$current_value$increment()
    x$instance$get()()
  } else {
    stopifnot(inherits_any(x, c("lazy_list", "eager_list")))
    index <- x$current_value$increment()
    if (index > length(x$data)) {
      coro::exhausted()
    } else {
      x$data[[index]]
    }
  }
}

#' Get a specific value in a lazy list
#'
#' Subset a lazy list, getting the `i`th value. As few values as possible
#' will be iterated through to get the `i`th value.
#'
#' @param x A lazy list
#' @param i The index to use.
#' @param ... Not used.
#'
#' @returns The value at index `i`.
#'
#' @noRd
#' 
#' @export
`[[.lazy_list` <- function(x, i, ...) {
  current_value <- x$current_value$get()
  if (i > current_value) {
    for (a in seq_len(i - current_value)) {
      result <- next_value(x)
    }
  } else {
    reset_iterator(x)
    for (a in seq_len(i)) {
      result <- next_value(x)
    }
  }

  if (coro::is_exhausted(result)) {
    NULL
  } else {
    result
  }
}

#' Subset an eager list
#'
#' Get the `i`th value in an eager list.
#'
#' @param x An eager list.
#' @param i An index.
#'
#' @noRd
`[[.eager_list` <- function(x, i) {
  if (i > length(x$data)) {
    NULL
  } else {
    x$data[[i]]
  }
}

#' Check an input is a lazy list.
#'
#' Check that an argument is a lazy list or eager list, turning it into
#' an eager list if not. Ensures that normal lists work in lazy list code.
#'
#' @param x A list to check.
#' 
#' @returns A lazy/eager list.
#'
#' @noRd
check_lazylist <- function(x) {
  if (!inherits_any(x, c("lazy_list", "eager_list"))) {
    eager_list(x)
  } else {
    x
  }
}

#' Convert a normal list to a lazy_list-like object
#'
#' Make a normal list compatible with code designed for lazy lists (specifically,
#' [next_value()] and [next_value_start()]).
#'
#' @param x A list
#'
#' @returns An `eager_list` object.
#'
#' @noRd
eager_list <- function(x) {
  res <- list(
    current_value = counter(),
    data = x
  )

  class(res) <- "eager_list"

  res
}

#' Subset a lazy list
#'
#' Get one or more values from a lazy list. Values will be in order.
#'
#' @param x A lazy list
#' @param i Indices.
#' @param ... Not used.
#'
#' @returns Another lazy list, only containing the specified values.
#'
#' @noRd
`[.lazy_list` <- function(x, i, ...) {
  force(x)
  force(i)

  generator <- coro::generator(function() {
    current_value <- x$current_value$get()

    if (all(i >= 0)) {
      if (current_value < min(i, na.rm = TRUE)) {
        n <- current_value + 1
        value <- next_value(x)
      } else {
        n <- 1
        value <- next_value_start(x)
      }

      max_i <- max(i, na.rm = TRUE)

      while (!coro::is_exhausted(value)) {
        if (n %in% i) {
          coro::yield(value)
        }

        if (n == max_i) {
          break
        }

        n <- n + 1
        value <- next_value(x)
      }
    } else {
      to_exclude <- abs(i)

      if (all(seq_len(current_value) %in% to_exclude)) {
        n <- current_value + 1
        value <- next_value(x)
      } else {
        n <- 1
        value <- next_value_start(x)
      }

      while (!coro::is_exhausted(value)) {
        if (!n %in% to_exclude) {
          coro::yield(value)
        }

        n <- n + 1
        value <- next_value(x)
      }
    }
  })

  lazy_list(generator)
}

#' Filter a lazy list
#'
#' Find all values in a lazy list that satisfy a condition.
#'
#' @param x A list/lazy list.
#' @param .f A function that determines whether a value is to be accepted.
#'
#' @returns Another lazy list, containing the values that were accepted.
#'
#' @noRd
lazy_filter <- function(x, .f) {
  x <- check_lazylist(x)
  force(.f)

  generator <- coro::generator(function() {
    value <- next_value_start(x)

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

#' Transform a lazy list
#'
#' Transform each element of a lazy list using a function.
#'
#' @param x A lazy list.
#' @param .f The function to apply to each element of `x`.
#'
#' @returns A lazy list containing the transformed values.
#'
#' @noRd
lazy_map <- function(x, .f) {
  x <- check_lazylist(x)
  force(.f)

  generator <- coro::generator(function() {
    value <- next_value_start(x)

    while (!coro::is_exhausted(value)) {
      result <- .f(value)
      coro::yield(result)
      value <- next_value(x)
    }

    coro::exhausted()
  })

  lazy_list(generator)
}

#' Flatten a lazy list of lazy lists
#'
#' Flatten a lazy list containing multiple lazy lists into a single lazy list
#' containing all the values.
#'
#' @param x A lazy list, containing other lazy lists.
#'
#' @returns A flattened lazy list.
#'
#' @noRd
lazy_flatten <- function(x) {
  force(x)
  x <- check_lazylist(x)

  generator <- coro::generator(function() {
    value <- next_value_start(x)

    while(!coro::is_exhausted(value)) {
      value <- check_lazylist(value)
      inner_value <- next_value_start(value)

      while (!coro::is_exhausted(inner_value)) {
        coro::yield(inner_value)
        inner_value <- next_value(value)
      }

      value <- next_value(x)
    }
  })

  lazy_list(generator)
}

#' Combine a list of lazy lists into a single lazy list.
#'
#' Similar to `lazy_flatten()`, but works on an outer list instead of
#' a lazy list.
#'
#' @noRd
combine_lazy_lists <- function(x) {
  x <- lapply(x, check_lazylist)

  generator <- coro::generator(function() {
    for (l in x) {
      value <- next_value_start(l)

      while (!coro::is_exhausted(value)) {
        coro::yield(value)
        value <- next_value(l)
      }
    }
  })

  lazy_list(generator)
}

c.lazy_list <- function(x, ...) {
  combine_lazy_lists(rlang::list2(x, ...))
}

#' Find all common values in a list of lazy lists.
#'
#' Find all values that occur in every lazy list in a set, using
#' a custom comparison function to compare values.
#'
#' @param x A list of lazy lists.
#' @param .f A comparison function to use.
#'
#' @returns A single lazy list containing the new values.
#'
#' @noRd
lazy_intersect_by <- function(x, .f) {
  if (length(x) == 0) {
    return(NULL)
  } else if (length(x) == 1) {
    return(x[[1]])
  }

  force(.f)
  x <- lapply(x, check_lazylist)

  generator <- coro::generator(function() {
    first <- x[[1]]
    rest <- x[-1]
    
    value <- next_value_start(first)
    while(!coro::is_exhausted(value)) {
      yield_elem <- TRUE

      for (l in rest) {
        if (!element_in_lazy(value, l, .f)) {
          yield_elem <- FALSE
          break
        }
      }

      if (yield_elem) {
        coro::yield(value)
      }

      value <- next_value(first)
    }
  })
  
  lazy_list(generator)
}

#' Find all unique values in a list of lazy lists.
#'
#' Find all unique values that occur in at least one lazy list in a set,
#' assuming that the individual lazy lists do not have duplicate values.
#'
#' @param x A list of lazy lists.
#' @param .f A comparison function to use.
#'
#' @returns A single lazy list containing the new values.
#'
#' @noRd
lazy_unique <- function(x, .f) {
  if (length(x) == 0) {
    return(NULL)
  } else if (length(x) == 1) {
    return(x[[1]])
  }

  force(.f)
  x <- lapply(x, check_lazylist)

  generator <- coro::generator(function() {
    seen <- list()
    for (l in x) {
      local_seen <- list()

      value <- next_value_start(l)

      while (!coro::is_exhausted(value)) {
        if (!element_in_eager(value, seen, .f)) {
          coro::yield(value)
          local_seen <- append(local_seen, list(value))
        }
        value <- next_value(l)
      }

      seen <- c(seen, local_seen)
    }
  })

  lazy_list(generator)
}

#' Is an element in a lazy list?
#'
#' Check if an element is in a lazy list using a custom
#' comparison function.
#'
#' @param x The element.
#' @param l The lazy list.
#' @param .f The comparison function.
#'
#' @returns A boolean flag.
#'
#' @noRd
element_in_lazy <- function(x, l, .f) {
  value <- next_value_start(l)

  while (!coro::is_exhausted(value)) {
    if (.f(x, value)) {
      return(TRUE)
    }

    value <- next_value(l)
  }

  FALSE
}

#' Is an element in a list?
#'
#' Version of `element_in_lazy()` for normal lists.
#'
#' @param x The element.
#' @param l The list.
#' @param .f The comparison function.
#'
#' @returns A boolean flag.
#'
#' @noRd
element_in_eager <- function(x, l, .f) {
  for (a in l) {
    if (.f(x, a)) {
      return(TRUE)
    }
  }

  FALSE
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

#' Index a list/lazy list safely
#'
#' Like `[[`, but returns `NULL` if the item is absent, rather than throwing an
#' error.
#'
#' @param x A list/lazy list.
#' @param i The index.
#'
#' @returns An element in `x`.
#'
#' @noRd
get_item <- function(x, i) {
  if (!inherits_any(x, c("lazy_list", "eager_list")) && i > length(x)) {
    NULL
  } else {
    x[[i]]
  }
}
