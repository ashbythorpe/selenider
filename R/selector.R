new_selector <- function(css = NULL,
                         xpath = NULL,
                         id = NULL,
                         class_name = NULL,
                         name = NULL,
                         filter = list(1),
                         multiple = FALSE,
                         inner_multiple = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name,
    filter = filter,
    to_be_filtered = length(filter),
    multiple = multiple,
    inner_multiple = inner_multiple
  )

  args <- args[!vapply(args, is.null, logical(1))]
  args_without_filter <- remove_filters(args)

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

remove_filters <- function(selector) {
  selector[names(selector) %in% c("css", "xpath", "id", "class_name", "name")]
}

#' Use a selector to collect one or more elements from the DOM.
#'
#' Applies a single `selenider_selector`, without applying the filters.
#'
#' @param selector The selector to apply.
#' @param element The parent element, if any.
#' @param driver The chromote::ChromoteSession or remoteDriver object.
#'
#' @noRd
use_selector <- function(selector, element, driver) {
  if (inherits(selector, "selenider_flattened_selector")) {
    elements <- lapply(
      selector$selectors,
      function(selector) {
        if (length(selector) > 0) {
          selector[[length(selector)]]$multiple <- TRUE
        }

        get_elements(list(
          selectors = selector,
          driver = driver,
          to_be_found = length(selector)
        ))
      }
    )

    return(elem_unique(elements, driver = driver))
  } else if (inherits(selector, "selenider_flatmap_selector")) {
    inner_selector <- selector
    inner_multiple <- selector$inner_multiple
    inner_selector$multiple <- selector$inner_multiple
    inner_selector$inner_multiple <- NULL
    inner_selector$filter <- list()
    inner_selector$to_be_filtered <- 0
    class(inner_selector) <- class(selector)[class(selector) != "selenider_flatmap_selector"]

    res <- lazy_map(element, function(x) {
      result <- get_elements(list(
        selectors = list(inner_selector),
        element = x,
        driver = driver,
        to_be_found = 1
      ))

      if (!inner_multiple) {
        get_items(result, 1)
      } else {
        result
      }
    })

    # print(length(res))
    # print(res)

    elements <- lazy_flatten(res)
  }

  filter <- selector$filter

  selector <- remove_filters(selector)

  if (length(filter) == 1 &&
    identical(filter[[1]], 1) &&
    length(selector) == 1) {
    # If we are getting the first element, both CDP and Selenium have a
    # shortcut for this
    using <- switch(names(selector),
      css = "css selector",
      class_name = "class name",
      names(selector)
    )

    res <- find_actual_element(
      element,
      using = using,
      value = selector[[1]],
      driver = driver
    )

    list(res)
  } else {
    element_list <- .mapply(function(name, value) {
      using <- switch(name,
        css = "css selector",
        class_name = "class name",
        name
      )

      find_actual_elements(
        element,
        using = using,
        value = value,
        driver = driver
      )
    }, list(names(selector), selector), NULL)

    elem_common(element_list, driver = driver)
  }
}
