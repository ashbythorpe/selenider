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
