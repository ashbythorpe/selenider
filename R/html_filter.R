#' Extract a subset of HTML elements
#'
#' @description
#' Operators to extract a subset of elements, or a single element, from
#' a selenider element collection.
#'
#' `html_filter()` and `html_find()` allow you to use conditions to filter HTML
#' elements (see [is_present()] and other conditions). `html_find()` returns the *first*
#' element that satisfies one or more conditions, while `html_filter()` returns every
#' element that satisfies these conditions.
#'
#' `[` and `[[` with a numeric subscript can be used on an element collection to filter
#' the elements by position. `[` returns a single element at a specified location, while
#' `[[` returns a collection of the elements at more than one position.
#'
#' @param x A `selenider_elements` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Conditions (functions or function
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
#' * [is_present()] and other conditions for conditions to filter by.
#'
#' @examplesIf selenider_available(online = FALSE)
#' html <- "
#' <button disabled>Button 1</button>
#' <button>Button 2</button>
#' <p>Text</p>
#' <div style='display:none;'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' elements <- ss("./*")
#'
#' # Gives the same result as s()
#' elements[[1]]
#'
#' elements[1:3]
#'
#' elements[-2]
#'
#' elements |>
#'   html_filter(is_visible)
#'
#' elements |>
#'   html_find(is_visible)
#'
#' # The above is equivalent to:
#' visible_elems <- elements |>
#'   html_filter(is_visible)
#' visible_elems[[1]]
#'
#' # In R >= 4.3.0, we can instead do:
#' # ss(".class1") |>
#' #   html_filter(is_visible) |>
#' #   _[[1]]
#'
#' ss("button") |>
#'   html_filter(is_enabled)
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
html_filter <- function(x, ...) {
  check_class(x, "selenider_elements")

  exprs <- enquos(...)
  arg_names <- lapply(c("a1", "a2", "a3"), function(x) make_elem_name(exprs, x))
  elem_name <- make_elem_name(exprs)
  elem_expr <- filter_elem_name(elem_name, arg_names)
  calls <- lapply(exprs, parse_condition, elem_expr)

  functions <- mapply(
    condition_to_function,
    calls,
    exprs,
    MoreArgs = list(elem_name = elem_name, arg_names = arg_names, driver = x$driver, driver_id = x$driver_id, timeout = x$timeout),
    SIMPLIFY = FALSE
  )

  selectors <- x$selectors

  x$selectors[[length(selectors)]]$filter <-
    c(x$selectors[[length(selectors)]]$filter, functions)

  x$selectors[[length(x$selectors)]]$to_be_filtered <- x$selectors[[length(x$selectors)]]$to_be_filtered + 1

  x
}

#' @rdname html_filter
#'
#' @export
html_find <- function(x, ...) {
  x <- html_filter(x, ...)

  x <- add_numeric_filter(x, 1)

  class(x) <- "selenider_element"

  x
}

#' @rdname html_filter
#'
#' @export
`[.selenider_elements` <- function(x, i) {
  example_vec <- rep(1, max(abs(i), na.rm = TRUE))
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
  } else if (length(i) != 1) {
    stop_subscript_length(i)
  }

  x <- add_numeric_filter(x, i)

  class(x) <- "selenider_element"

  x
}

add_numeric_filter <- function(x, i, call = rlang::caller_env()) {
  selectors <- x$selectors

  filters <- selectors[[length(selectors)]]$filter

  numeric_filters <- Filter(is.numeric, filters)
  numeric_filters <- numeric_filters[numeric_filters > 0]

  min_length <- if (length(numeric_filters) != 0) min(lengths(numeric_filters)) else Inf
  max_sub <- max(i, na.rm = TRUE)

  if (max_sub >= min_length) {
    cli::cli_warn(c(
      "Invalid subscript {.arg i}.",
      "Attempt to select the {ordinal(max_sub)} element of {.arg x}.",
      "{.arg x} has a known maximum length of {.arg {min_length}}."
    ), class = "selenider_warning_subscript_max_length", call = call)
  }

  if (length(filters) != 0 && is.numeric(filters[[length(filters)]])) {
    filter <- filters[[length(filters)]]
    if (filter > 0) {
      new_filter <- filter[i]
    } else if (i > 0) {
      new_filter <- seq_len(i + length(filter))[filter][i]
    } else {
      new_filter <- unique(c(filter, i))
    }

    x$selectors[[length(selectors)]]$filter[[length(filters)]] <- new_filter
  } else {
    x$selectors[[length(selectors)]]$filter <-
      append(x$selectors[[length(selectors)]]$filter, list(i))
  }

  x$selectors[[length(x$selectors)]]$to_be_filtered <- x$selectors[[length(x$selectors)]]$to_be_filtered + 1

  x
}

condition_to_function <- function(x, original_call, elem_name, arg_names, driver, driver_id, timeout) {
  extra_data <- list(driver, driver_id, timeout, webelement_to_element = webelement_to_element, webelements_to_elements = webelements_to_elements)
  names(extra_data)[1:3] <- arg_names

  args <- pairlist2(x = )
  names(args) <- elem_name

  res <- new_function(
    args,
    call2(with_timeout, 0, quo_get_expr(x)),
    new_environment(extra_data, parent = quo_get_env(x))
  )

  attr(res, "original_call") <- quo_get_expr(original_call)

  res
}

filter_elem_name <- function(elem_name, arg_names) {
  args <- paste(elem_name, arg_names[[1]], arg_names[[2]], arg_names[[3]], sep = ", ")
  paste0("webelement_to_element(", args, ")")
}

webelement_to_element <- function(x, driver, driver_id, timeout) {
  res <- list(
    driver = driver,
    driver_id = driver_id,
    element = x,
    timeout = timeout,
    selectors = list(),
    to_be_found = 0
  )

  class(res) <- "selenider_element"

  res
}

webelements_to_elements <- function(x, driver, driver_id, timeout) {
  res <- list(
    driver = driver,
    driver_id = driver_id,
    element = x,
    timeout = timeout,
    selectors = list(),
    to_be_found = 0
  )

  class(res) <- c("selenider_elements", "list")

  res
}
