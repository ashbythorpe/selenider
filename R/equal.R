#' Are two elements equivalent?
#'
#' Checks if two `selenider_element` objects point to the
#' same element on the page. `elem_equal()` is equivalent to
#' using `==`, but allows you to specify a timeout value if
#' needed.
#'
#' @param x,y,e1,e2 `selenider_element` objects to compare.
#' @param timeout How long to wait for the elements to be present.
#'
#' @returns
#' `TRUE` or `FALSE`.
#'
#' @seealso
#' * [elem_filter()] and [elem_find()] for filtering collection of elements.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
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
#'     elem_children() |> # Direct children
#'     elem_filter(has_name("p")) |>
#'     has_at_least(1)
#' }
#'
#' ss("div") |>
#'   elem_find(has_p_child) |>
#'   elem_equal(s(".second")) # TRUE
#'
#' @export
elem_equal <- function(x, y, timeout = NULL) {
  check_class(x, "selenider_element")
  check_class(y, "selenider_element")

  check_active(x)

  if (x$driver_id != y$driver_id) {
    return(FALSE)
  }

  timeout <- get_timeout(timeout, x$timeout)
  if (!elem_wait_until(is_present(x), is_present(y), timeout = timeout)) {
    missing_arg <- if (is_present(x)) "y" else "x"
    stop_not_actionable(
      c(
        "To compare {.arg x} and {.arg y}, both must be present in the DOM.",
        paste0(
          format_timeout_for_error(timeout),
          "{.arg {missing_arg}} was not found."
        )
      )
    )
  }

  element_x <- get_element(x)
  element_y <- get_element(y)

  if (x$session == "chromote") {
    element_x == element_y
  } else if (x$session == "selenium") {
    selenium_equal(element_x, element_y, driver = x$driver)
  } else {
    rselenium_equal(element_x, element_y, driver = x$driver)
  }
}

#' @rdname elem_equal
#'
#' @export
`==.selenider_element` <- function(e1, e2) {
  elem_equal(e1, e2)
}

selenium_equal <- function(x, y, driver) {
  driver$execute_script("return arguments[0].isSameNode(arguments[1])", x, y)
}

rselenium_equal <- function(x, y, driver) {
  driver$executeScript(
    "return arguments[0].isSameNode(arguments[1])",
    list(x, y)
  )[[1]]
}
