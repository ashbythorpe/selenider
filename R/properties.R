#' Get the properties of selenider elements
#'
#' @description
#' Get various properties of an HTML element.
#'
#' `html_name()` gets the tag name of an element.
#'
#' `html_text()` gets the inner text of an element.
#'
#' @param x A `selenider_element` object.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns
#' A string.
#'
#' @examples
#' \dontshow{
#' # This allows `local_session()` to work when being sourced.
#' prev_options <- options(withr.hook_source = TRUE)
#' }
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_name()
#' \dontshow{
#' options(prev_options)
#' }
#'
#' @name html-properties
NULL

#' @rdname html-properties
#'
#' @export
html_name <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)
  
  element <- get_element_for_property(
    x,
    action = "get the tag name of {.arg x}",
    timeout = timeout,
  )

  element$getElementTagName()
}

#' @rdname html-properties
#'
#' @export
html_text <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the text inside {.arg x}",
    timeout = timeout,
  )

  element$getElementText()
}

get_element_for_property <- function(x, action, timeout, call = rlang::caller_env()) {
  get_element_for_action(
    x,
    action = action,
    conditions = list(),
    timeout = timeout,
    failure_messages = c(),
    conditions_text = c(),
    call = call
  )
}
