#' Find multiple HTML child elements
#'
#' Find every available HTML element using a CSS selector, an XPath, or a
#' variety of other methods.
#'
#' @param x A selenider session or element.
#' @param css A css selector.
#' @param xpath An XPath.
#' @param id The id of the element you want to select.
#' @param class_name The class name of the element you want to select.
#' @param name The name attribute of the element you want to select.
#' @inheritParams rlang::args_dots_used
#'
#' @details
#' If more than one method is used to select an element (e.g. `css` and
#' `xpath`), the first element which satisfies every condition will be found.
#'
#' @returns
#' A `selenider_elements` object. Note that this is not a list, and you should
#' be careful with the functions that you use with it. See the advanced usage
#' vignette for more details:
#' `vignette("advanced-usage", package = "selenider")`.
#'
#' @seealso
#' * [ss()] to quickly select multiple elements without specifying the session.
#' * [find_element()] to select a single element.
#' * [selenider_session()] to begin a session.
#' * [elem_children()] and family to select elements using their relative
#'   position in the DOM.
#' * [elem_filter()] and [elem_find()] for filtering element collections.
#' * [as.list.selenider_elements()] to convert a `selenider_elements` object
#'   to a list.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div id='outer-div'>
#'   <div>
#'     <p>Text 1</p>
#'     <p>Text 2</p>
#'     <p>Text 3</p>
#'   </div>
#' </div>
#'
#' <div></div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' session |>
#'   find_elements("div")
#'
#' # Or:
#' ss("div")
#'
#' session |>
#'   find_element("#outer-div") |>
#'   find_elements("p")
#'
#' # The above can be shortened to:
#' s("#outer-div") |>
#'   find_elements("p")
#'
#' @export
find_elements <- function(x, ...) {
  UseMethod("find_elements")
}

#' @export
#'
#' @rdname find_elements
find_elements.selenider_session <- function(x,
                                            css = NULL,
                                            xpath = NULL,
                                            id = NULL,
                                            class_name = NULL,
                                            name = NULL,
                                            ...) {
  check_dots_used()
  check_selector_args(css, xpath, id, class_name, name)

  selector <- step_select_multiple(css, xpath, id, class_name, name)

  new_selenider_elements(x$session, x$driver, x$id, x$timeout, list(selector))
}

#' @export
#'
#' @rdname find_elements
find_elements.selenider_element <- function(x,
                                            css = NULL,
                                            xpath = NULL,
                                            id = NULL,
                                            class_name = NULL,
                                            name = NULL,
                                            ...) {
  check_dots_used()
  check_selector_args(css, xpath, id, class_name, name)

  selector <- step_select_multiple(css, xpath, id, class_name, name)

  x$steps <- append(x$steps, list(selector))

  class(x) <- c("selenider_elements", "list")

  x
}

new_selenider_elements <- function(session, driver, driver_id, timeout, steps = list()) {
  res <- list(
    session = session,
    driver = driver,
    driver_id = driver_id,
    element = NULL,
    timeout = timeout,
    steps = steps
  )

  class(res) <- c("selenider_elements", "list")

  res
}
