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

  elements <- get_with_timeout(timeout, get_elements, x)

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
