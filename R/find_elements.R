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
#' * [find_element()] to select multiple elements.
#' * [selenider_session()] to begin a session.
#' * [elem_children()] and family to select elements using their relative position
#'   in the DOM.
#' * [elem_filter()] and [elem_find()] for filtering element collections.
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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
                                           link_text = NULL,
                                           ...) {
  check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = list())

  new_selenider_elements(x, selector)
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
                                           link_text = NULL,
                                           ...) {
  check_dots_used()

  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = list())
  
  x$selectors <- append(x$selectors, list(selector))
  
  x$to_be_found <- x$to_be_found + 1
  
  class(x) <- c("selenider_elements", "list")
  
  x
}

new_selenider_elements <- function(session, selector) {
  res <- list(
    driver = get_driver(session),
    driver_id = session$id,
    element = NULL,
    timeout = session$timeout,
    selectors = list(selector),
    to_be_found = 1
  )

  class(res) <- c("selenider_elements", "list")

  res
}

#' @export
format.selenider_elements <- function(x, ...) {
  cli::cli_format_method({
    bullets <- format_elements(x)
    cli::cli_text("A collection of selenider elements selecting:")

    if (length(bullets) == 1) {
      cli::cli_text(bullets)
    } else {
      cli::cli_bullets(bullets)
    }
  })
}

format_elements <- function(x, ...) {
  selectors <- x$selectors
  
  if (length(selectors) == 1) {
    res <- format(selectors[[1]], first = TRUE, multiple = TRUE, ...)
    replace_names_bullets(res)
  } else if (length(selectors) == 2) {
    first <- format(selectors[[1]], first = TRUE, ...)
    
    last <- format(selectors[[2]], multiple = TRUE, ...)

    c(replace_names_bullets(first), replace_names_bullets(last))
  } else {
    first <- format(selectors[[1]], first = TRUE, ...)
    
    last <- format(selectors[[length(selectors)]], multiple = TRUE, ...)
    
    formatted <- unlist(lapply(
      selectors[c(-1, -length(selectors))], 
      format, ...
    ))
    
    c(replace_names_bullets(first), replace_names_bullets(formatted), replace_names_bullets(last))
  }
}

#' @export
print.selenider_elements <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}
