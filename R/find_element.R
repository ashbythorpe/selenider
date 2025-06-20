#' Find a single HTML child element
#'
#' Find the first HTML element using a CSS selector, an XPath, or a variety
#' of other methods.
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
#' `xpath`), the first element which satisfies all conditions will be found.
#'
#' CSS selectors are generally recommended over other options, since they are
#' usually the easiest to read. Use `"tag_name"` to select by tag name,
#' `".class"` to select by class, and `"#id"` to select by id.
#'
#' @returns
#' A `selenider_element` object.
#'
#' @seealso
#' * [s()] to quickly select an element without specifying the session.
#' * [find_elements()] to select multiple elements.
#' * [selenider_session()] to begin a session.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div class='class1'>
#'   <div id='id1'>
#'     <p class='class2'>Example text</p>
#'   </div>
#'   <p>Example text</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' session |>
#'   find_element("div")
#'
#' session |>
#'   find_element("div") |>
#'   find_element(xpath = "./p")
#'
#' s("div") |>
#'   find_element("#id1")
#'
#' s("div") |>
#'   find_element(id = "id1") |>
#'   find_element(class_name = "class2")
#'
#' s(xpath = "//div[contains(@class, 'class1')]/div/p")
#'
#' # Complex Xpath expressions are easier to read as chained CSS selectors.
#' # This is equivalent to above
#' s("div.class1") |>
#'   find_element("div") |>
#'   find_element("p")
#'
#' @export
find_element <- function(x, ...) {
  UseMethod("find_element")
}

#' @export
#'
#' @rdname find_element
find_element.selenider_session <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           ...) {
  check_dots_used()
  check_selector_args(css, xpath, id, class_name, name)

  selector <- step_select_single(css, xpath, id, class_name, name)

  new_selenider_element(x$session, x$driver, x$id, x$timeout, list(selector))
}

#' @export
#'
#' @rdname find_element
find_element.selenider_element <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           ...) {
  check_dots_used()
  check_selector_args(css, xpath, id, class_name, name)

  selector <- step_select_single(css, xpath, id, class_name, name)

  x$steps <- append(x$steps, list(selector))

  x
}

new_selenider_element <- function(session, driver, driver_id, timeout, steps = list()) {
  res <- list(
    session = session,
    driver = driver,
    driver_id = driver_id,
    element = NULL,
    timeout = timeout,
    steps = steps
  )

  class(res) <- "selenider_element"

  res
}
