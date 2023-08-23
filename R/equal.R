#' Are two elements equivalent?
#'
#' Checks if two `selenider_element` objects point to the
#' same element on the page. `html_equal()` is equivalent to
#' using `==`, but allows you to specify a timeout value if
#' needed.
#'
#' @param x,y,e1,e2 `selenider_element` objects to compare.
#' @param timeout How long to wait for the elements to be present.
#'
#' @returns
#' `TRUE` or `FALSE`.
#'
#' @examples
#' html <- "
#' <div></div>
#' <div class='second'>
#'   <p></p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' s("div") == ss("div")[[1]]
#'
#' has_p_child <- function(x) {
#'   x |>
#'     html_children() |> # Direct children
#'     html_filter(has_name("p")) |>
#'     has_at_least(1)
#' }
#'
#' ss("div") |>
#'   html_find(has_p_child) |>
#'   html_equal(s(".second")) # TRUE
#'
#' @export
html_equal <- function(x, y, timeout = NULL) {
  check_class(x, "selenider_element")
  check_class(y, "selenider_element")

  timeout <- get_timeout(timeout, x$timeout)
  if (!html_wait_until(is_present(x), is_present(y), timeout = timeout)) {
    missing_arg <- if (is_present(x)) "y" else "x"
    cli::cli_abort(c(
      "To compare {.arg x} and {.arg y}, both must be present in the DOM.",
      "After {timeout} seconds, {.arg {missing_arg}} was not found."
    ))
  }

  if (x$driver_id != y$driver_id) {
    return(FALSE)
  }

  element_x <- get_element(x)
  element_y <- get_element(y)

  if (uses_selenium(x$driver)) {
    selenium_equal(element_x, element_y, driver = x$driver)
  } else {
    element_x == element_y
  }
}

#' @rdname html_equal
#'
#' @export
`==.selenider_element` <- function(e1, e2) {
  html_equal(e1, e2)
}

selenium_equal <- function(x, y, driver) {
  driver$executeScript("return arguments[0].isSameNode(arguments[1])", list(x, y))[[1]]
}
