#' Combine multiple HTML elements
#' 
#' Combine a set of `selenider_element`/`selenider_elements` objects
#' into a single `selenider_elements` object, allowing you to
#' perform actions on them at once. `c()` and `html_flatten()` can be
#' used interchangeably.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> `selenider_element` or
#'   `selenider_elements` objects to be combined.
#' 
#' @returns A `selenider_elements` object.
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' element <- s(".class1")
#'
#' collection <- ss(".class2")
#'
#' html_flatten(element, collection)
#'
#' c(collection, element)
#'
#' @export
html_flatten <- function(...) {
  check_dots_unnamed()

  exprs <- enexprs(...)
  elements <- list2(...)

  if (length(elements) == 0) {
    cli::cli_abort(c(
      "`...` is empty.",
      "i" = "Supply one or more arguments to combine into an element collection."
    ), class = "selenider_error_dots_empty")
  }

  accepted_classes <- c("selenider_element", "selenider_elements")
<<<<<<< refs/remotes/origin/main
  for (i in seq_len(elements)) {
=======
  for (i in seq_along(elements)) {
>>>>>>> Fix R CMD CHECK issues
    element <- elements[[i]]

    if (!inherits(element, accepted_classes)) {
      expr <- exprs[[i]]
      cli::cli_abort(c(
        "Every arguments in `...` must be a {.cls {accepted_classes}} object, not {.obj_type_friendly {element}}.",
        "x" = "Problematic argument:",
        "i" = "`{expr}`"
      ))
    }
  }
  
  html_combine(elements)
}

#' @rdname html_flatten
#' 
#' @export
c.selenider_element <- function(...) {
  html_flatten(...)
}

#' @rdname html_flatten
#' 
#' @export
c.selenider_elements <- function(...) {
  html_flatten(...)
}

html_combine <- function(elements) {
  # TODO: check driver is the same for all elements
  driver <- elements[[1]]$timeout

  timeout <- elements[[1]]$timeout

  elements <- lapply(elements, remove_driver)

  selector <- new_flattened_selector(elements)

  res <- list(
    driver = driver,
    element = NULL,
    timeout = timeout,
    selectors = list(new_flattened_selector(elements)),
    to_be_found = 1
  )

  class(res) <- "selenider_elements"

  res
}

remove_driver <- function(x) {
  x$driver <- NULL
  x
}

new_flattened_selector <- function(elements) {
  selectors <- lapply(elements, function(x) x$selectors)
  
  res <- list(
    selectors = selectors,
    filter = NULL
  )

  class(res) <- c("selenider_flattened_selector", "selenider_selector")

  res
}

get_selector <- function(x) {
  if (inherits(x, "selenider_element")) {
    selectors <- x$selectors
    selectors[[length(selectors)]]$filters <- c(selectors[[length(selectors)]]$filters, list(1))
    selectors
  } else {
    x$selectors
  }
}
