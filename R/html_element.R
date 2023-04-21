html_element <- function(x, ...) {
  UseMethod("html_element")
}

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

new_selector <- function(css,
                         xpath,
                         id,
                         class_name,
                         name,
                         link_text,
                         filter = "first") {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name,
    link_text = link_text,
    filter = "first"
  )
  
  args <- args[!vapply(args, is.null, logical(1))]
  
  if (length(args) == 0) {
    cli::cli_abort(c(
      "No arguments specified to select elements with",
      "i" = "Use `css = '*'` to select all elements"
    ))
  }
  
  class(args) <- "selenider_selector"
  
  args
}

update_element <- function(x) {
  actual_element <- get_actual_element(x)

  if (is.null(actual_element)) {
    return(NULL)
  }
  
  x$element <- actual_element
  x$to_be_found <- 0

  x
}

get_actual_element <- function(x) {
  if (x$to_be_found %% 1 == 0.5) {
    if (is.numeric(x$filter)) {
      element <- x$element[[x$filter]]
    } else {
      res <- NULL
      .f <- x$filter
      elements <- x$element

      for (element in elements) {
        if (.f(element)) {
          res <- element
          break
        }
      }

      if (is.null(res)) {
        return(NULL)
      }

      element <- res
    }
    x$to_be_found <- x$to_be_found - 0.5
  } else {
    element <- x$element
  }

  selectors <- tail(x$selectors, x$to_be_found)
 
  for(selector in selectors) {
    element <- use_selector(selector, element)
    
    if (is.null(element)) {
      return(NULL)
    }
  }

  element
}

use_selector <- function(selector, element) {
  filter <- selector$filter

  selector$filter <- NULL

  if (identical(filter, "first") && length(selector) == 1) {
    using <- switch(
      names(selector),
      css = "css selector",
      class_name = "class name",
      link_text = "link text",
      names(selector)
    )
    
    find_element(element, using = using, value = selector[[1]])
  } else {
    element_list <- .mapply(function(name, value) {
      using <- switch(
        name,
        css = "css selector",
        class_name = "class name",
        link_text = "link text",
        name
      )
      
      find_elements(element, using = using, value = value)
    }, list(names(selector), selector), NULL)

    elements <- Reduce(intersect, element_list)
 
    if (is.null(filter)) {
      elements
    } else if (is.numeric(filter)) {
      if (length(elements) < filter) {
        return(NULL)
      }
      
      elements[[filter]]
    } else {
      stopifnot(is.function(.f))
      res <- NULL
      .f <- filter
      
      for (element in elements) {
        if (.f(element)) {
          res <- element
          break
        }
      }
      
      if (is.null(res)) {
        stop()
      }
      
      res
    }
  }
}

