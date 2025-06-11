#' @title Step functions
#'
#' @description
#' In selenider, elements are stored as a list of steps, which are then
#' executed in order when the corresponding HTML element(s) need to be found.
#' These functions make up the API used to create and execute these steps.
#'
#' @noRd
NULL

#' Select a single element.
#'
#' Select a single element using a CSS selector, an XPath, or a variety
#' of other methods. Multiple methods can be specified, in which case the
#' first element which satisfies all conditions will be found. If this step
#' is applied to an existing element, then child elements will be selected.
#'
#' @noRd
step_select_single <- function(css = NULL,
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

#' Select multiple elements.
#'
#' Select all element matching a CSS selector, an XPath, or other methods.
#'
#' @noRd
step_select_multiple <- function(css = NULL,
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

#' Select a child element of each existing element.
#'
#' This step should be applied to an element collection. For each element in
#' the collection, a child element matching the given conditions will be found.
#'
#' @noRd
step_select_inner_single <- function(css = NULL,
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

#' Select child elements of each existing element.
#'
#' This step should be applied to an element collection. For each element in
#' the collection, every child element matching the given conditions will be
#' found.
#'
#' @noRd
step_select_inner_multiple <- function(css = NULL,
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

#' Flatten a set of elements into a single element collection.
#'
#' @noRd
step_flatten <- function(elements) {
  res <- list(elements = elements)

  class(res) <- c("selenider_selector", "selenider_flatten")

  res
}

#' Select an element by index.
#'
#' @noRd
step_index <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_index")

  res
}

#' Select a subset of elements.
#'
#' Extracts a subset of elements from an element collection, using normal R
#' (numeric) subset rules.
#'
#' @noRd
step_subset <- function(index) {
  res <- list(index = index)

  class(res) <- c("selenider_filter", "selenider_subset")

  res
}

#' Filter elements using a predicate.
#'
#' @noRd
step_filter <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_predicate_filter")

  res
}

#' Find elements using a filter.
#'
#' @noRd
step_find <- function(filter) {
  res <- list(filter = filter)

  class(res) <- c("selenider_filter", "selenider_find")

  res
}

#' Apply a step to an element.
#'
#' @noRd
apply_step <- function(driver, element, step) {
  if (inherits(step, "selenider_filter") && is.null(element)) {
    cli::cli_abort("Cannot apply filter to `NULL` element.", .internal = TRUE)
  }

  if (inherits(step, "selenider_single_selector")) {
    apply_single_selector(driver, element, step)
  } else if (inherits(step, "selenider_multiple_selector")) {
    apply_multiple_selector(driver, element, step)
  } else if (inherits(step, "selenider_single_inner_selector")) {
    elem_unique_single(lazy_filter(lazy_map(element, function(element) {
      apply_single_selector(driver, element, step)
    }), function(x) !is.null(x)), driver = driver)
  } else if (inherits(step, "selenider_multiple_inner_selector")) {
    elem_unique(lazy_map(element, function(x) {
      apply_multiple_selector(driver, x, step)
    }), driver = driver)
  } else if (inherits(step, "selenider_flatten")) {
    lazy_flatten(lazy_map(step$elements, function(element) {
      result <- get_element(element)
      if (inherits(element, "selenider_element")) {
        list(result)
      } else {
        result
      }
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
