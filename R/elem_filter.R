#' Extract a subset of HTML elements
#'
#' @description
#' Operators to extract a subset of elements, or a single element, from
#' a selenider element collection.
#'
#' `elem_filter()` and `elem_find()` allow you to use conditions to filter HTML
#' elements (see [is_present()] and other conditions). `elem_find()` returns
#' the *first* element that satisfies one or more conditions, while
#' `elem_filter()` returns every element that satisfies these conditions.
#'
#' `[` and `[[` with a numeric subscript can be used on an element collection
#' to filter the elements by position. `[` returns a single element at a
#' specified location, while `[[` returns a collection of the elements at more
#' than one position.
#'
#' @param x A `selenider_elements` object.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Conditions (functions or
#'   function calls) that are used to filter the elements of `x`.
#' @param i A number (or for `[`, a vector of one or more numbers) used to
#'   select elements by position.
#'
#' @details
#' As with the [find_element()] and [find_elements()] functions, these
#' functions are lazy, meaning that the elements are not fetched and filtered
#' until they are needed.
#'
#' Conditions can be functions or function calls (see [elem_expect()] for more
#' details).
#'
#' @returns
#' `elem_filter()` and `[` return a `selenider_elements` object, since they can
#' result in multiple elements.
#' `elem_find()` and `[[` return a single `selenider_element` object.
#'
#' @seealso
#' * [find_elements()] and [ss()] to get elements to filter.
#' * [is_present()] and other conditions for predicates on HTML elements.
#'   (If you scroll down to the *See also* section, you will find the rest).
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button disabled>Button 1</button>
#' <button>Button 2</button>
#' <p>Text</p>
#' <div style='display:none;'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' elements <- ss("*")
#'
#' # Gives the same result as s()
#' elements[[1]]
#'
#' elements[1:3]
#'
#' elements[-2]
#'
#' elements |>
#'   elem_filter(is_visible)
#'
#' elements |>
#'   elem_find(is_visible)
#'
#' # The above is equivalent to:
#' visible_elems <- elements |>
#'   elem_filter(is_visible)
#' visible_elems[[1]]
#'
#' # In R >= 4.3.0, we can instead do:
#' # ss(".class1") |>
#' #   elem_filter(is_visible) |>
#' #   _[[1]]
#'
#' ss("button") |>
#'   elem_filter(is_enabled)
#'
#' @export
elem_filter <- function(x, ...) {
  check_class(x, "selenider_elements")

  exprs <- enquos(...)
  filters <- make_filters(x, exprs)

  x$steps <- append(x$steps, filters)

  x
}

#' @rdname elem_filter
#'
#' @export
elem_find <- function(x, ...) {
  check_class(x, "selenider_elements")

  exprs <- enquos(...)
  filters <- make_filters(x, exprs, find_last = TRUE)

  x$steps <- append(x$steps, filters)

  class(x) <- "selenider_element"

  x
}

#' @rdname elem_filter
#'
#' @export
`[.selenider_elements` <- function(x, i) {
  i <- vctrs::vec_cast(i, integer())
  i <- stats::na.omit(i)
  i <- i[i != 0]

  if (length(i) == 0) {
    return(x)
  }

  check_subscript_vctrs(i)

  filter <- step_subset(i)

  x$steps <- append(x$steps, list(filter))

  x
}

#' @rdname elem_filter
#'
#' @export
`[[.selenider_elements` <- function(x, i) {
  if (!is.vector(i)) {
    stop_subscript_type(i)
  } else if (isTRUE(is.na(i))) {
    stop_subscript_na(i)
  } else if (!is.numeric(i)) {
    NextMethod()
  } else if (length(i) != 1) {
    stop_subscript_length(i)
  }

  check_number_whole(i)
  i <- vctrs::vec_cast(i, integer())

  if (i == 0) {
    stop_subscript_0()
  }

  filter <- step_index(i)

  x$steps <- append(x$steps, list(filter))

  class(x) <- "selenider_element"

  x
}

# Checks that `i` is invalid using vctrs
# If `i` can be used to subset a vector, then it can be used to subset
# a selenider element.
check_subscript_vctrs <- function(i, call = rlang::caller_env()) {
  len <- max(abs(i))
  x <- rep.int(1L, len)
  # Check that i is a valid subscript
  vctrs::vec_slice(x, i, error_call = call)
}


make_filters <- function(x, exprs, find_last = FALSE) {
  if (length(exprs) == 0) {
    if (find_last) {
      return(list(step_index(1)))
    } else {
      return(list())
    }
  }

  arg_names <- lapply(
    c("a1", "a2", "a3", "a4"),
    function(x) make_elem_name(exprs, x)
  )
  elem_name <- make_elem_name(exprs)
  elem_expr <- filter_elem_name(elem_name, arg_names)
  calls <- lapply(exprs, parse_condition, elem_expr)

  functions <- mapply(
    condition_to_function,
    calls,
    exprs,
    MoreArgs = list(
      elem_name = elem_name,
      arg_names = arg_names,
      session = x$session,
      driver = x$driver,
      driver_id = x$driver_id,
      timeout = x$timeout
    ),
    SIMPLIFY = FALSE
  )

  filters <- lapply(utils::head(functions, n = -1L), function(x) step_filter(x))
  last_function <- functions[[length(functions)]]

  if (find_last) {
    append(filters, list(step_find(last_function)))
  } else {
    append(filters, list(step_filter(last_function)))
  }
}


condition_to_function <- function(x,
                                  original_call,
                                  elem_name,
                                  arg_names,
                                  session,
                                  driver,
                                  driver_id,
                                  timeout) {
  extra_data <- list(
    session,
    driver,
    driver_id,
    timeout,
    webelement_to_element = webelement_to_element
  )
  names(extra_data)[1:4] <- arg_names

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
  args <- paste(
    elem_name,
    arg_names[[1]],
    arg_names[[2]],
    arg_names[[3]],
    arg_names[[4]],
    sep = ", "
  )
  paste0("webelement_to_element(", args, ")")
}

webelement_to_element <- function(x, session, driver, driver_id, timeout) {
  element <- new_selenider_element(session, driver, driver_id, timeout)
  element$element <- x

  element
}
