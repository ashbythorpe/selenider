#' Force an element to be collected and stored
#'
#' @description
#' `selenider_element`/`selenider_elements` objects are generally
#' *lazy*, meaning they only collect the actual element in the DOM
#' when absolutely necessary, and forget it immediately after. This
#' is to avoid situations where the DOM changes after an element
#' has been collected, resulting in errors and unreliable behaviour.
#'
#' These functions force an element to be collected and stored, making
#' it eager rather than lazy. This is useful when you are operating on
#' the same element multiple times, since only collecting the element
#' once will improve performance. However, you must be sure that the element
#' will not change on the page while you are using it.
#'
#' @param x For `cache_element()`, a `selenider_element` object. For `cache_elements()`,
#'   a `selenider_elements` object.
#' @param timeout How long to wait for the element(s) to exist while collecting them.
#'
#' @details
#' These functions do not make selenider elements *permanently* eager. Further sub-elements
#' will not be cached unless specified.
#'
#' For example, consider the following code:
#' ``` r
#' s(".class1") |>
#'   html_parent() |>
#'   cache_elements() |>
#'   html_element(".class2")
#' ```
#'
#' In this example, the parent of the element with class ".class1" will be cached,
#' but the child element with class ".class2" will not.
#'
#' @returns
#' The same type as `x`. The result of `cache_element()`/`cache_elements()` can be used
#' as a normal `selenider_element`/`selenider_elements` object.
#'
#' @export
cache_element <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "cache {.arg x}",
    timeout = timeout,
  )

  x$element <- element

  x$to_be_found <- 0
  x$to_be_filtered <- 0

  x
}

cache_elements <- function(x, timeout = NULL) {
  check_class(x, "selenider_elements")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  elements <- as.list(get_with_timeout(timeout, get_elements, x))

  if (is.null(elements)) {
    cli::cli_abort(c(
      "To get the cache {.arg x}, its parent must exist.",
      "After {.val timeout} seconds, {.arg x}'s parent did not exist."
    ))
  }

  x$element <- elements

  x$to_be_found <- 0
  x$to_be_filtered <- 0

  x
}
