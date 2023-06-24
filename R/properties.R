#' Get tag name of an element
#'
#' Get the tag name (e.g. `"p"` for a `<p>` tag) of a `selenider_element` object.
#'
#' @param x A `selenider_element` object.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns
#' A string
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_name()
#' 
#' @family properties
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

#' Get element text
#'
#' Get the inner text of a `selenider_element` object.
#'
#' @inheritParams html_name
#'
#' @returns A string
#'
#' @family properties
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_text()
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

#' Get element attribute
#'
#' Get an attribute of a `selenider_element` object.
#'
#' @param x A `selenider_element` object.
#' @param name The name of the attribute to get; a string.
#' @param default The default value to use if the attribute does not exist in 
#'   the element.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns A character vector.
#'
#' @family properties
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_attr("href")
#'
#' @export
html_attr <- function(x, name, default = NA_character_, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" attribute of {.arg x}"),
    timeout = timeout
  )

  result <- element$getElementAttribute(name)

  if (is.null(result)) {
    default
  } else {
    result
  }
}

html_attrs <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the attributes of {.arg x}",
    timeout = timeout
  )

  x$driver$executeScript("
    let element = arguments[0];
    let attributes = {};
    for (let i = 0; i < element.attributes.length; i++) {
      attributes[element.attributes[i].name] = element.attributes[i].value;
    }
    return attributes;
  ", list(element))
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
