#' Combine multiple HTML elements
#' 
#' Combine a set of `selenider_element`/`selenider_elements` objects
#' into a single `selenider_elements` object, allowing you to
#' perform actions on them at once. `c()` and `html_flatten()` do the same
#' thing, but `html_flatten()` works on lists of `selenider_element`/`selenider_elements`
#' objects.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> `selenider_element` or
#'   `selenider_elements` objects to be combined, or lists of such objects.
#' 
#' @returns A `selenider_elements` object.
#'
#' @examplesIf selenider_available(online = FALSE)
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
#' buttons <- html_flatten(button_1, button_2)
#'
#' buttons |>
#'   html_expect_all(is_enabled)
#'
#' buttons |>
#'   as.list() |>
#'   lapply(click)
#'
#' # Doesn't just have to be single elements
#' first_2_divs <- ss("div")[1:2]
#'
#' html_flatten(first_2_divs, button_2) |>
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
#'   html_flatten() |>
#'   length() # 3
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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
  
  to_combine <- html_flatten_init(elements, exprs)
  
  html_combine(to_combine)
}

html_flatten_init <- function(x, exprs, is_nested = FALSE, index = NULL, call = rlang::caller_env()) {
  accepted_classes <- c("selenider_element", "selenider_elements")

  result <- list()
  for (i in seq_along(x)) {
    element <- x[[i]]

    if (!inherits_any(element, accepted_classes)) {
      if (is.list(element) && !is_nested) {
        result <- c(result, html_flatten_init(element, is_nested = TRUE, index = i, call = call))
      } else {
        stop_flatten_dots(x, exprs, i, index, is_nested, call)
      }
    } else {
      result <- append(result, list(element))
    }
  }

  result
}

stop_flatten_dots <- function(x, exprs, i, index, is_nested, call = rlang::caller_env()) {
  accepted_classes <- c("selenider_element", "selenider_elements")

  if (!is_nested) {
    expr <- exprs[[i]]
    cli::cli_abort(c(
      "Every arguments in `...` must be a {.cls {accepted_classes}} object or a list of such objects, not {.obj_type_friendly {x}}.",
      "x" = "Problematic argument ({.val {i}}):",
      "i" = "`{expr}`"
    ), class = "selenider_error_flatten_dots", call = call)
  } else {
    cli::cli_abort(c(
      "Every arguments in `...` must be a {.cls {accepted_classes}} object or a list of such objects.",
      "x" = "Argument {.val {index}} was a list, but contained {.obj_type_friendly {x}} as its {ordinal(i)} element.",
      "x" = "Problematic argument:",
      "i" = "`{expr}`"
    ), class = "selenider_error_flatten_dots", call = call)
  }
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
  ids <- lapply(elements, function(x) x$driver_id)
  if (length(unique(ids)) > 1) {
    cli::cli_abort(c(
      "Cannot combine elements with different drivers."
    ))
  }

  driver <- elements[[1]]$driver

  timeout <- elements[[1]]$timeout

  elements <- lapply(elements, remove_driver)

  selector <- new_flattened_selector(elements)

  res <- list(
    driver = driver,
    driver_id = elements[[1]]$driver_id,
    element = NULL,
    timeout = timeout,
    selectors = list(new_flattened_selector(elements)),
    to_be_found = 1
  )

  class(res) <- c("selenider_elements", "list")

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
    filter = list(),
    to_be_filtered = 0
  )

  class(res) <- c("selenider_flattened_selector", "selenider_selector")

  res
}
