new_selector <- function(css,
                         xpath,
                         id,
                         class_name,
                         name,
                         link_text,
                         filter = list(1)) {
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
  args_without_filter <- args[names(args) != "filter"]

  if (length(args_without_filter) == 0) {
    stop_bad_selector()
  }

  for (i in seq_along(args_without_filter[-1])) {
    arg <- args_without_filter[[i]]
    name <- names(args_without_filter)[i]
    check_string(arg, allow_null = TRUE, arg = name, call = caller_env())
  }
  
  class(args) <- "selenider_selector"
  
  args
}

use_selector <- function(selector, element, driver, multiple = FALSE) {
  if (inherits(selector, "selenider_flattened_selector")) {
    elements <- lapply(selector$selectors, function(selector) get_elements(list(selectors = selector, driver = driver, to_be_found = length(selector))))
    return(elem_unique(elements, driver = driver))
  }

  filter <- selector$filter

  selector$filter <- NULL

  if (length(filter) == 1 && identical(filter[[1]], 1) && length(selector) == 1) {
    using <- switch(
      names(selector),
      css = "css selector",
      class_name = "class name",
      link_text = "link text",
      names(selector)
    )
    
    res <- find_element(element, using = using, value = selector[[1]], driver = driver)

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
      
      find_elements(element, using = using, value = value, driver = driver)
    }, list(names(selector), selector), NULL)
    
    elem_unique(element_list, driver = driver)
  }
}


