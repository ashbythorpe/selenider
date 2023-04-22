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

#' @export
html_elements.selenider_element <- function(x,
                                           css = NULL,
                                           xpath = NULL,
                                           id = NULL,
                                           class_name = NULL,
                                           name = NULL,
                                           link_text = NULL) {
  selector <- new_selector(css, xpath, id, class_name, name, link_text, filter = NULL)
  
  x$selectors <- append(x$selectors, list(selector))
  
  x$to_be_found <- x$to_be_found + 1
  
  class(x) <- "selenider_elements"
  
  x
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

format_elements <- function(selector, first = FALSE) {
  child <- if (first) "" else " child"
  
  filter <- selector$filter
  
  selector$filter <- NULL
  
  names <- names(selector)
  
  values <- unlist(selector, use.names = FALSE)
  
  names <- ifelse(
    names == "css",
    "css selector",
    gsub("_", " ", names, fixed = TRUE)
  )
  
  values <- paste0("{.val ", values, "}")
  
  to_pluralize <- paste(names, values)
  
  text <- cli::pluralize("{to_pluralize}")
  
  if (is.null(filter)) {
    paste0("The", child, " elements with ", text)
  } else {
    body <- rlang::fn_body(filter)[-1]
    if (length(body) == 1) {
      paste0("The", child, " elements with ", text, " matching the following condition:\n", body)
    } else {
      paste0("The", child, " elements with ", text, " matching a custom condition")
    }
  }
}

#' @export
print.selenider_elements <- function(x, ...) {
  selectors <- x$selectors
  
  if (length(selectors) == 1) {
    formatted <- format_elements(selectors[[1]], first = TRUE)
    
    cat("A collection of selenider elements selecting:\n")
    cli::cli_text(formatted)
  } else if (length(selectors) == 2) {
    first <- format_element(selectors[[1]], first = TRUE)
    
    last <- format_elements(selectors[[2]])
    
    cat("A collection of selenider elements selecting:\n")
    cli::cli_bullets(c("*" = first, "*" = last))
  } else {
    first <- format_element(selectors[[1]], first = TRUE)
    
    last <- format_elements(selectors[[length(selectors)]])
    
    formatted <- vapply(
      selectors[c(-1, -length(selectors))], 
      format_element, 
      FUN.VALUE = character(1)
    )
    
    names(first) <- "*"
    names(last) <- "*"
    names(formatted) <- rep("*", length(formatted))
    
    cat("A selenider element selecting:\n")
    cli::cli_bullets(c(first, formatted, last))
  }
}
