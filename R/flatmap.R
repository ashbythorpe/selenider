#' Iterate over an element collection
#'
#' @description
#' Use `html_flatmap()` when you want to select further sub-elements
#' *for each* element of a collection.
#'
#' `html_flatmap()` allows you to apply a function to each element of
#' a `selenider_elements` object, provided that the function returns a
#' `selenider_element`/`selenider_elements` object itself. The result will
#' then be flattened into a single `selenider_elements` object. The benefit
#' of this over traditional iteration techniques is that the laziness of the
#' elements will be maintained, and nothing will be fetched from the DOM.
#'
#' `element_list()` transforms a `selenider_elements` object into a list of
#' `selenider_element` objects. The result can then be used in for loops and
#' higher order functions like [lapply()]/[purrr::map()] (whereas a `selenider_element`
#' object cannot)
#'
#' @param x A `selenider_elements` object.
#' @param .f A function to apply to each element of `x`.
#' @param ... Passed into `.f`.
#' @param timeout How long to wait for `x` to exist while computing its length.
#'
#' @description
#' `html_flatmap()` works by executing `.f` on a mock element, then recording the
#' results in `x`. This means that no matter the length of `x`, `.f` is only evaluated
#' once, and during the `html_flatmap()` call. For this reason, `.f` should not invoke
#' any side effects or do anything other than selecting sub-elements.
#'
#' `element_list()` essentially turns `x` into:
#' `list(x[[1]], x[[2]], ...)`
#' However, to do this, the length of `x` must be computed. This means that while
#' each element inside the list is still lazy, the list itself cannot be considered
#' lazy, since the number of elements in the DOM may change. To avoid problems, it is
#' recommended to use an element list just after it is created, to make sure the
#' list is an accurate representation of the DOM when it is being used.
#'
#' @returns
#' `html_flatmap()` returns a `selenider_element` object.
#' `element_list()` returns a list of `selenider_element` objects.
#'
#' @export
html_flatmap <- function(x, .f, ...) {
  check_class(x, "selenider_elements")

  mock_element <- list(
    driver = x$driver,
    driver_id = x$driver_id,
    element = NULL,
    timeout = x$timeout,
    selectors = list(),
    to_be_found = 0
  )

  class(mock_element) <- "selenider_element"

  fn_result <- try_fetch(
    with_timeout(0, .f(mock_element, ...)),
    error = function(cnd) {
      cli::cli_abort(c(
        "An error occurred while executing {.arg .f} on a mock element."
      ), parent = cnd)
    }
  )

  if (!inherits_any(fn_result, c("selenider_element", "selenider_elements"))) {
    cli::cli_abort(c(
      "{.arg .f} must return a {.cls {c(selenider_element, selenider_elements)}} object.",
      "When executed on a mock element, {.arg .f} returned {.obj_type_friendly {fn_result}}"
    ))
  } else if (length(fn_result$selectors) <= 0) {
    # Identity transformation
    return(x)
  }

  selectors <- fn_result$selectors

  selector <- new_flatmap_selector(x, selectors, class(fn_result))

  res <- list(
    driver = x$driver,
    driver_id = x$driver_id,
    element = NULL,
    timeout = x$timeout,
    selectors = list(selector),
    to_be_found = 1
  )

  class(res) <- c("selenider_elements", "list")

  res
}

new_flatmap_selector <- function(x, selectors, class) {
  res <- list(
    element = remove_driver(x),
    selectors = selectors,
    resulting_class = class,
    filter = NULL
  )

  class(res) <- c("selenider_flatmap_selector", "selenider_selector")

  res
}

#' @rdname html_flatmap
#'
#' @export
element_list <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")

  timeout <- get_timeout(timeout, x$timeout)

  size <- html_size(x, timeout = timeout)

  lapply(seq_len(size), function(i) x[[i]])
}

as.list.selenider_elements <- function(x, timeout = NULL, ...) {
  element_list(x, timeout)
}
