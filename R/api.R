#' Select HTML elements
#'
#' @description
#' Both `s()` and `ss()` allow you to select elements without specifying a
#' session object.
#'
#' `s()` selects a single element, being a shorthand for [find_element()]
#' on the current session.
#'
#' `ss()` selects multiple elements, being a shorthand for [find_elements()].
#'
#' @param css A css selector.
#' @param xpath An XPath.
#' @param id The id of the element you want to select.
#' @param class_name The class name of the element you want to select.
#' @param name The name attribute of the element you want to select.
#' @param link_text The link text of the link element that you would like to
#'   select.
#'
#' @details
#' Both functions allow the starting point for chains of selectors to be made
#' more concise. Both use [get_session()] to get the global session object.
#'
#' @returns
#' `s()` returns a `selenider_element` object.
#' `ss()` returns a `selenider_elements` object.
#'
#' @seealso
#' * [find_element()] and [find_elements()]
#' * [selenider_session()] to begin a session.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div>
#' <p id='id1' class='inner'></p>
#' <div class='child'>
#' <p class='inner'></p>
#' </div>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' s("#id1")
#'
#' # This is the equivalent of:
#' find_element(session, "id1")
#'
#' ss(".inner")
#'
#' # This is the equivalent of:
#' find_element(session, ".inner")
#'
#' # This provides a more concise way to begin a chain of selectors
#' s("div") |>
#'   find_element(".child") |>
#'   find_element(".inner")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
s <- function(css = NULL,
              xpath = NULL,
              id = NULL,
              class_name = NULL,
              name = NULL,
              link_text = NULL) {
  session <- get_session(.env = caller_env())

  find_element(session, css, xpath, id, class_name, name, link_text)
}

#' @rdname s
#'
#' @export
ss <- function(css = NULL,
               xpath = NULL,
               id = NULL,
               class_name = NULL,
               name = NULL,
               link_text = NULL) {
  session <- get_session(.env = caller_env())

  find_elements(session, css, xpath, id, class_name, name, link_text)
}
