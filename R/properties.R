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
#' @description
#' Get an attribute of a `selenider_element` object.
#'
#' `html_attr()` returns a *single* attribute value as a string.
#'
#' `html_attrs()` returns a named list containing *every* attribute.
#'
#' `html_value()` returns the 'value' attribute.
#'
#' @param x A `selenider_element` object.
#' @param name The name of the attribute to get; a string.
#' @param default The default value to use if the attribute does not exist in 
#'   the element.
#' @param ptype The type to cast the value to. Useful when the value is an integer
#'   or decimal number. By default, the value is returned as a string.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns `html_attr()` returns a character vector of length 1. `html_attrs()`
#'   returns a named list of strings. The return value of `html_value()` has the
#'   same type as `ptype` and length 1.
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
    result[[1]]
  }
}

#' @rdname html_attr
#'
#' @export
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

#' @rdname html_attr
#'
#' @export
html_value <- function(x, ptype = character(), timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the value of {.arg x}",
    timeout = timeout
  )

  result <- element$getElementAttribute("value")

  if (length(result) == 0) {
    vctrs::vec_cast(NA, ptype)
  } else {
    vctrs::vec_cast(result[[1]], ptype)
  }
}

#' Get a CSS property of an element
#'
#' Get a CSS property of an element (e.g. `"background-color"`).
#'
#' @param x A `selenider_element` object.
#' @param name The name of the CSS property to get.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns
#' A string, or `NA` if the property does not exist.
#'
#' @family properties
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_css_property("background-color")
#'
#' @export
html_css_property <- function(x, name, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" CSS property of {.arg x}"),
    timeout = timeout
  )

  result <- element$getElementValueOfCssProperty(name)

  if (length(result) == 0) {
    NA_character_
  } else {
    result[[1]]
  }
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
