#' @export
html_element <- function(x, ...) {
  UseMethod("html_element")
}

#' @export
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
  actual_element <- get_actual_element(x)
  
  if (is.null(actual_element)) {
    x$element <- list(actual_element)
  } else {
    x$element <- actual_element
  }
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
  }

  element
}

format_element <- function(selector, first = FALSE) {
  child <- if (first) "" else " child"

  filter <- selector$filter

  selector$filter <- NULL
  
  names <- names(selector)

  values <- unlist(selector, use.names = FALSE)
  
  names <- gsub("_", " ", names)

  values <- paste0("{.val ", x, "}")
  
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


