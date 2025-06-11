new_single_selector <- function(css = NULL,
                                xpath = NULL,
                                id = NULL,
                                class_name = NULL,
                                name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_single_selector")

  args
}

new_multiple_selector <- function(css = NULL,
                                  xpath = NULL,
                                  id = NULL,
                                  class_name = NULL,
                                  name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_multiple_selector")

  args
}

new_single_inner_selector <- function(css = NULL,
                                      xpath = NULL,
                                      id = NULL,
                                      class_name = NULL,
                                      name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_single_inner_selector")

  args
}

new_multiple_inner_selector <- function(css = NULL,
                                        xpath = NULL,
                                        id = NULL,
                                        class_name = NULL,
                                        name = NULL) {
  args <- list(
    css = css,
    xpath = xpath,
    id = id,
    class_name = class_name,
    name = name
  )

  args <- args[!vapply(args, is.null, logical(1))]

  class(args) <- c("selenider_selector", "selenider_multiple_inner_selector")

  args
}

new_flatten <- function(elements) {
  res <- list(elements = elements)

  class(res) <- c("selenider_selector", "selenider_flatten")

  res
}

new_index <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_index")

  res
}

new_subset <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_subset")

  res
}

new_filter <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_predicate_filter")

  res
}

new_find <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_find")

  res
}



apply_step <- function(driver, element, step) {
  if (inherits(step, "selenider_filter") && is.null(element)) {
    cli::cli_abort("Cannot apply filter to `NULL` element.", .internal = TRUE)
  }

  if (inherits(step, "selenider_single_selector")) {
    apply_single_selector(driver, element, step)
  } else if (inherits(step, "selenider_multiple_selector")) {
    apply_multiple_selector(driver, element, step)
  } else if (inherits(step, "selenider_single_inner_selector")) {
    elem_unique(lazy_filter(lazy_map(element, function(element) {
      apply_single_selector(driver, element, step)
    }), function(x) !is.null(x)), driver = driver)
  } else if (inherits(step, "selenider_multiple_inner_selector")) {
    elem_unique(lazy_flatten(lazy_map(element, function(x) {
      apply_multiple_selector(driver, x, step)
    })), driver = driver)
  } else if (inherits(step, "selenider_flatten")) {
    lazy_flatten(lazy_map(step$elements, function(element) {
      get_element(element)
    }))
  } else if (inherits(step, "selenider_index")) {
    get_item(element, step$index)
  } else if (inherits(step, "selenider_subset")) {
    get_items(element, step$index)
  } else if (inherits(step, "selenider_predicate_filter")) {
    lazy_filter(element, step$filter)
  } else if (inherits(step, "selenider_find")) {
    get_item(lazy_filter(element, step$filter), 1)
  } else {
    cli::cli_abort("Unknown step type: {class(step)}.", .internal = TRUE)
  }
}

apply_single_selector <- function(driver, element, step) {
  if (length(step) == 1) {
    find_actual_element(
      element,
      type = names(step),
      value = step[[1]],
      driver = driver
    )
  } else {
    selectors <- lapply(names(step), function(name) list(type = name, value = step[[name]]))

    elements <- elem_common(lapply(selectors, function(selector) {
      find_actual_elements(
        element,
        type = selector$type,
        value = selector$value,
        driver = driver
      )
    }), driver = driver)

    get_item(elements, 1)
  }
}

apply_multiple_selector <- function(driver, element, step) {
  selectors <- lapply(names(step), function(name) list(type = name, value = step[[name]]))

  elem_common(lapply(selectors, function(selector) {
    find_actual_elements(
      element,
      type = selector$type,
      value = selector$value,
      driver = driver
    )
  }), driver = driver)
}
