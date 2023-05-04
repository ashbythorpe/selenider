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
                                           link_text = NULL) {
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
                                           link_text = NULL) {
  selector <- new_selector(css, xpath, id, class_name, name, link_text)
  
  x$selectors <- append(x$selectors, selector)
  
  x$to_be_found <- x$to_be_found + 1
  
  x
}

new_selenider_element <- function(session, selector) {
  res <- list(
    element = session$driver$client,
    timeout = session$timeout,
    selectors = list(selector),
    to_be_found = 1
  )
  
  class(res) <- "selenider_element"
  
  res
}

update_element <- function(x) {
  actual_element <- get_actual_webelement(x)
  
  if (is.null(actual_element)) {
    x$element <- list(actual_element)
  } else {
    x$element <- actual_element
  }
  x$to_be_found <- 0

  x
}

format_element <- function(selector, first = FALSE) {
  child <- if (first) "" else " child"

  filter <- selector$filter

  selector$filter <- NULL
  
  names <- names(selector)

  values <- unlist(selector, use.names = FALSE)
  
  names <- gsub("_", " ", names)

  values <- paste0("{.val ", values, "}")
  
  to_pluralize <- paste(names, values)

  text <- cli::pluralize("{to_pluralize}")

  if (is.numeric(filter)) {
    paste0("The ", ordinal(filter), child, " element with ", text) 
  } else {
    body <- rlang::fn_body(filter)[-1]
    if (length(body) == 1) {
      paste0("The", child, " element with ", text, " matching the following condition:\n ", body)
    } else {
      paste0("The", child, " element with ", text, " matching a custom condition")
    }
  }
}

#' @export
print.selenider_element <- function(x, ...) {
  selectors <- x$selectors

  if (length(selectors) == 1) {
    formatted <- format_element(selectors[[1]], first = TRUE)

    cat("A selenider element selecting:\n")
    cli::cli_text(formatted)
  } else {
    first <- format_element(selectors[[1]], first = TRUE)

    formatted <- vapply(selectors[-1], format_element, FUN.VALUE = character(0))

    cat("A selenider element selecting:\n")
    cli::cli_bullets(c(first, formatted))
  }
}


