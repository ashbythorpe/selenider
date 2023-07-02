#' Quickly select HTML elements
#' 
#' @description 
#' Both `s()` and `ss()` allow you to select elements without specifying a
#' session object.
#' 
#' `s()` selects a single element, being a shorthand for [html_element()]
#' without a first argument
#' 
#' `ss()` selects multiple elements, being a shorthand for [html_elements()].
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
#' `s()` returns a `selenider_elements` object.
#' 
#' @seealso 
#' * [html_element()] and [html_elements()]
#' * [selenider_session()] to begin a session.
#' 
#' @examples 
#' session <- mock_selenider_session()
#' 
#' s(".class1")
#' 
#' # This is the equivalent of:
#' html_element(session, ".class1")
#' 
#' ss(".class2")
#' 
#' # This is the equivalent of:
#' html_element(session, ".class2")
#' 
#' # This provides a more concise way to begin a chain of selectors
#' s(".class1") |>
#'   html_element(".innerclass") |>
#'   html_element("#item1")
#' 
#' @export
s <- function(css = NULL,
              xpath = NULL,
              id = NULL,
              class_name = NULL,
              name = NULL,
              link_text = NULL) {
  session <- get_session(.env = caller_env())
  
  html_element(session, css, xpath, id, class_name, name, link_text)
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
  
  html_elements(session, css, xpath, id, class_name, name, link_text)
}

