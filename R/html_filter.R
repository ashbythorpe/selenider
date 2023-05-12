#' Extract a subset of HTML elements
#'
#' @description
#' Operators to extract a subset of elements, or a single element, from
#' a selenider element collection.
#'
#' `html_filter()` and `html_find()` allow you to use conditions to filter HTML
#' elements (see [html-conditions]). `html_find()` returns the *first* element that
#' satisfies one or more conditions, while `html_filter()` returns every element that
#' satisfies these conditions.
#'
#' `[` and `[[` with a numeric subscript can be used on an element collection to filter
#' the elements by position. `[` returns a single element at a specified location, while
#' `[[` returns a collection of the elements at more than one position.
#'
#' @param x A `selenider_elements` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots] Conditions (functions or function
#'   calls) that are used to filter the elements of `x`.
#' @param i A number (or for `[`, a vector of one or more numbers) used to select
#'   elements by position.
#'
#' @details
#' As with the [html_element()] and [html_elements()] functions, these functions are
#' lazy, meaning that the elements are not fetched and filtered until they are needed.
#'
#' Conditions can be functions or function calls (see [html_expect()] for more details).
#'
#' @returns
#' `html_filter()` and `[` return a `selenider_elements` object, since they can result
#' in multiple elements.
#' `html_find()` and `[[` return a single `selenider_element` object.
#'
#' @seealso
#' * [html_elements()] and [ss()] to get elements to filter.
#' * [html-conditions] for conditions to filter by.
#'
#' @examples
#' # Gives the same result as s()
#' ss(".class1")[[1]]
#'
#' ss(".class1")[1:5]
#'
#' ss(".class1") |>
#'   html_filter(is_visible)
#'
#' ss(".class1") |>
#'   html_find(is_visible)
#'
#' # The above is equivalent to:
#' ss(".class1") |>
#'   html_filter(is_visible) |>
#'   _[[1]]
#'
#' @export
html_filter <- function(x, ...) {
  exprs <- rlang::enquos(x)
  elem_name <- make_elem_name(exprs)
  calls <- lapply(dots, parse_condition, elem_name)

  functions <- lapply(calls, condition_to_function, elem_name = elem_name)

  x$filter <- c(x$filter, functions)

  x
}

#' @rdname html_filter
#'
#' @export
html_find <- function(x, ...) {
  res <- html_filter(x, ...)
  
  res <- add_numeric_filter(res, i)

  class(res) <- "selenider_element"

  res
}

#' @rdname html_filter
#'
#' @export
`[.selenider_elements` <- function(x, i) {
  example_vec <- rep(1, length(i))
  # Check that i is a valid subscript
  vctrs::vec_slice(example_vec, i)

  add_numeric_filter(x, i)
}
 
#' @rdname html_filter
#'
#' @export
`[[.selenider_elements` <- function(x, i) {
  if (!is.vector(i)) {
    stop_subscript_type(i)
  } else if (!is.numeric(i)) {
    NextMethod()
  } else if (length(i) != 0) {
    stop_subscript_length(i)
  }

  x <- add_numeric_filter(x, i)

  class(x) <- "selenider_element"

  x
}

add_numeric_filter <- function(x, i, call = rlang::caller_env()) {
  filters <- x$filter
  
  numeric_filters <- Filter(filters, is.numeric)

  min_length <- min(lengths(numeric_filters))
  max_sub <- max(i, na.rm = TRUE)

  if (max_sub >= min_length) {
    cli::cli_warn(c(
      "Invalid subscript {.arg i}.",
      "Attempt to select the {ordinal(max_sub)} element of {.arg x}.",
      "{.arg x} has a known maximum length of {.arg min_length}."
    ), class = "selenider_warning_subscript_max_length", call = call)
  }

  last_filter <- filters[[length(filters)]]
  if (is.numeric(last_filter)) {
    new_filter <- last_filter[i]

    x$filters[[length(filters)]] <- new_filter
  } else {
    x$filters <- append(x$filters, list(i))
  }

  x
}

condition_to_function <- function(x, elem_name) {
  args <- rlang::pairlist2(x = )
  names(args) <- elem_name

  rlang::new_function(
    args,
    rlang::call2(with_timeout, 0, rlang::quo_get_expr(x)),
    rlang::quo_get_env(x)
  )
}


