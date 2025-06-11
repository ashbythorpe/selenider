#' Combine multiple HTML elements
#'
#' Combine a set of `selenider_element`/`selenider_elements` objects
#' into a single `selenider_elements` object, allowing you to
#' perform actions on them at once. `c()` and `elem_flatten()` do the same
#' thing, but `elem_flatten()` works when given a list of
#' `selenider_element`/`selenider_elements` objects.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> `selenider_element` or
#'   `selenider_elements` objects to be combined, or lists of such objects.
#'
#' @returns A `selenider_elements` object.
#'
#' @seealso
#' * [as.list.selenider_elements()] to iterate over element collections.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div id='id1'></div>
#' <div class='.class2'></div>
#' <button id='button1'>Click me!</button>
#' <div class='button-container'>
#'   <button id='button2'>No, click me!</button>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' button_1 <- s("#button1")
#' button_2 <- s("#button2")
#'
#' buttons <- elem_flatten(button_1, button_2)
#'
#' buttons |>
#'   elem_expect_all(is_enabled)
#'
#' buttons |>
#'   as.list() |>
#'   lapply(elem_click)
#'
#' # Doesn't just have to be single elements
#' first_2_divs <- ss("div")[1:2]
#'
#' elem_flatten(first_2_divs, button_2) |>
#'   length()
#'
#' # We would like to use multiple css selectors and combine the results
#' selectors <- c(
#'   "#id1", # Will select 1 element
#'   "button", # Will select 2 elements
#'   "p" # Will select 0 elements
#' )
#'
#' lapply(selectors, ss) |>
#'   elem_flatten() |>
#'   length() # 3
#'
#' @export
elem_flatten <- function(...) {
  check_dots_unnamed()

  exprs <- enexprs(...)
  elements <- list2(...)

  if (length(elements) == 0) {
    stop_dots_empty()
  }

  to_combine <- elem_flatten_init(elements, exprs = exprs)

  if (length(to_combine) == 0) {
    stop_flatten_empty()
  }

  elem_combine(to_combine)
}

elem_flatten_init <- function(x,
                              exprs,
                              is_nested = FALSE,
                              index = NULL,
                              call = rlang::caller_env()) {
  accepted_classes <- c("selenider_element", "selenider_elements")

  result <- list()
  for (i in seq_along(x)) {
    element <- x[[i]]

    if (!inherits_any(element, accepted_classes)) {
      if (is.list(element) && !is_nested) {
        result <- c(
          result,
          elem_flatten_init(
            element,
            exprs = exprs,
            is_nested = TRUE,
            index = i,
            call = call
          )
        )
      } else {
        stop_flatten_dots(
          x,
          exprs = exprs,
          i = i,
          index = index,
          is_nested = is_nested,
          call = call
        )
      }
    } else {
      result <- append(result, list(element))
    }
  }

  result
}

#' @rdname elem_flatten
#'
#' @export
c.selenider_element <- function(...) {
  elem_flatten(...)
}

#' @rdname elem_flatten
#'
#' @export
c.selenider_elements <- function(...) {
  elem_flatten(...)
}

elem_combine <- function(elements) {
  ids <- vapply(elements, function(x) x$driver_id, FUN.VALUE = double(1))
  if (length(unique(ids)) > 1) {
    stop_incompatible_drivers(ids)
  }

  element <- elements[[1]]

  step <- step_flatten(elements)

  new_selenider_elements(
    session = element$session,
    driver = element$driver,
    driver_id = element$driver_id,
    timeout = element$timeout,
    steps = list(step)
  )
}
