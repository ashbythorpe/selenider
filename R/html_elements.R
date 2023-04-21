html_elements <- function(x, ...) {
  UseMethod("html_elements")
}

html_elements.selenider_session <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL) {
  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = NULL)

  new_selenider_elements(x, selector)
}

new_selenider_elements <- function(x, selector) {
  res <- list(
    element = session$driver$client,
    timeout = session$timeout,
    selectors = list(selector),
    to_be_found = 1
  )

  class(res) <- "selenider_elements"

  res
}

update_elements <- function(x) {
  elements <- get_actual_elements(x)
  
  if (is.null(elements)) {
    return(NULL)
  }
  
  x$element <- elements

  x$to_be_found <- 0.5
}

get_actual_elements <- function(x) {
  if (x$to_be_found == 0.5 && is.null(x$filter)) {
    return(x$selectors)
  } 

  element <- x$element
  
  selectors <- tail(x$selectors, x$to_be_found)
  
  for(selector in selectors) {
    element <- retry_with_timeout(x$timeout, use_selector, selector, element)
    
    if (is.null(element)) {
      return(NULL)
    }
  }
  
  element
}
