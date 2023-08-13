#' Get a single HTML element
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
#' @param link_text The link text of the link element that you would like to
#'   select.
#' @inheritParams rlang::args_dots_used
#'
#' @details 
#' If more than one method is used to select an element (e.g. `css` and 
#' `xpath`), the first element which satisfies all conditions will be found.
#' 
#' @returns 
#' A `selenider_element` object.
#' 
#' @seealso 
#' * [s()] to quickly select an element without specifying the session.
#' * [html_elements()] to select multiple elements.
#' * [selenider_session()] to begin a session.
#' 
#' @examples 
#' session <- mock_selenider_session()
#' 
#' session |>
#'   html_element(".class1")
#'
#' session |>
#'   html_element(".class1") |>
#'   html_element(".class2")
#'   
#' # The above can be shortened to:
#' s(".class1") |>
#'   html_element(".class2")
#' 
#' @export
html_element <- function(x, ...) {
  UseMethod("html_element")
}

#' @export
#' 
#' @rdname html_element
html_element.selenider_session <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL,
                                           ...) {
  check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text)
  
  new_selenider_element(x, selector)
}

#' @export
#' 
#' @rdname html_element
html_element.selenider_element <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL,
                                           ...) {
  check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text)
  
  x$selectors <- append(x$selectors, list(selector))
  
  x$to_be_found <- x$to_be_found + 1
  
  x
}

new_selenider_element <- function(session, selector) {
  res <- list(
    driver = get_driver(session),
    driver_id = session$id,
    element = NULL,
    timeout = session$timeout,
    selectors = list(selector),
    to_be_found = 1,
    to_be_filtered = 1
  )
  
  class(res) <- "selenider_element"
  
  res
}

get_driver <- function(session) {
  if (inherits(session$driver, "ChromoteSession")) session$driver else session$driver$client
}

#' @export
format.selenider_element <- function(x, ...) {
  cli::cli_format_method({
    selectors <- x$selectors

    if (length(selectors) == 1) {
      formatted <- format(selectors[[1]], first = TRUE)

      cli::cli_text("A selenider element selecting:")
      cli::cli_text(formatted)
    } else {
      first <- format(selectors[[1]], first = TRUE)

      formatted <- vapply(selectors[-1], format, FUN.VALUE = character(1))

      names(first) <- "*"
      names(formatted) <- rep("*", length(formatted))

      cli::cli_text("A selenider element selecting:")
      cli::cli_bullets(c(first, formatted))
    }
  })
}

#' @export
print.selenider_element <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}


