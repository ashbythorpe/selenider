new_selector <- function(css,
                         xpath,
                         id,
                         class_name,
                         name,
                         link_text,
                         filter = 1) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name,
    link_text = link_text,
    filter = filter
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

use_selector <- function(selector, element) {
  filter <- selector$filter

  selector$filter <- NULL

  if (identical(filter, 1) && length(selector) == 1) {
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
      elements[[filter]]
    } else {
      res <- NULL
      .f <- filter
      
      for (element in elements) {
        if (.f(element)) {
          res <- element
          break
        }
      }
      
      if (is.null(res)) {
        return(NULL)
      }
      
      res
    }
  }
}


