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

use_selector <- function(selector, element, multiple = FALSE) {
  selector$filter <- NULL

  if (identical(filter, 1) && length(selector) == 1) {
    using <- switch(
      names(selector),
      css = "css selector",
      class_name = "class name",
      link_text = "link text",
      names(selector)
    )
    
    res <- find_element(element, using = using, value = selector[[1]])

    list(res)
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

    Reduce(intersect, element_list)
  }
}


