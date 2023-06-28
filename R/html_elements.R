#' Get multiple HTML elements
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
#' @param link_text The link text of the link element that you would like to
#'   select.
#' @inheritParams rlang::args_dots_used
#' 
#' @details 
#' If more than one method is used to select an element (e.g. `css` and 
#' `xpath`), the first element which satisfies every condition will be found.
#' 
#' @returns 
#' A `selenider_elements` object.
#' 
#' @seealso 
#' * [ss()] to quickly select multiple elements without specifying the session.
#' * [html_element()] to select multiple elements.
#' * [selenider_session()] to begin a session.
#' 
#' @examples 
#' session <- mock_selenider_session()
#' 
#' session |>
#'   html_elements(".class1")
#'
#' # Or:
#' ss(".class1")
#'
#' session |>
#'   html_element(".class1") |>
#'   html_elements(".class2")
#'   
#' # The above can be shortened to:
#' s(".class1") |>
#'   html_elements(".class2")
#' 
#' @export
html_elements <- function(x, ...) {
  UseMethod("html_elements")
}

#' @export
#' 
#' @rdname html_elements
html_elements.selenider_session <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL,
                                           ...) {
  rlang::check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = NULL)

  new_selenider_elements(x, selector)
}

#' @export
#' 
#' @rdname html_elements
html_elements.selenider_element <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL,
                                           ...) {
  rlang::check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = NULL)
  
  x$selectors <- append(x$selectors, list(selector))
  
  x$to_be_found <- x$to_be_found + 1
  
  class(x) <- c("selenider_elements", "list")
  
  x
}

new_selenider_elements <- function(session, selector) {
  res <- list(
    driver = session$driver$client,
    element = NULL,
    timeout = session$timeout,
    selectors = list(selector),
    to_be_found = 1
  )

  class(res) <- c("selenider_elements", "list")

  res
}

cache_elements <- function(x) {
  elements <- get_elements(x)
  
  if (is.null(elements)) {
    return(NULL)
  }
  
  x$element <- elements

  x$to_be_found <- 0.5
}

#' @export
print.selenider_elements <- function(x, ...) {
  selectors <- x$selectors
  
  if (length(selectors) == 1) {
    formatted <- format(selectors[[1]], first = TRUE, multiple = TRUE)
    
    cat("A collection of selenider elements selecting:\n")
    cli::cli_text(formatted)
  } else if (length(selectors) == 2) {
    first <- format(selectors[[1]], first = TRUE)
    
    last <- format(selectors[[2]], multiple = TRUE)
    
    cat("A collection of selenider elements selecting:\n")
    cli::cli_bullets(c("*" = first, "*" = last))
  } else {
    first <- format(selectors[[1]], first = TRUE)
    
    last <- format(selectors[[length(selectors)]], multiple = TRUE)
    
    formatted <- vapply(
      selectors[c(-1, -length(selectors))], 
      format, 
      FUN.VALUE = character(1)
    )
    
    names(first) <- "*"
    names(last) <- "*"
    names(formatted) <- rep("*", length(formatted))
    
    cat("A selenider element selecting:\n")
    cli::cli_bullets(c(first, formatted, last))
  }
}
