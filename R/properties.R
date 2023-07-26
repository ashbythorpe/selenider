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
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)
  
  element <- get_element_for_property(
    x,
    action = "get the tag name of {.arg x}",
    timeout = timeout,
  )

  if (uses_selenium(x$driver)) {
    element$getElementTagName()
  } else {
    driver <- x$driver
    tolower(driver$DOM$describeNode(backendNodeId = element)$node$nodeName)
  }
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
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the text inside {.arg x}",
    timeout = timeout,
  )

  if (uses_selenium(x$driver)) {
    element$getElementText()
  } else {
    driver <- x$driver
    chromote_get_text(element, driver)
  }
}

chromote_get_text <- function(x, driver) {
  actual <- driver$Runtime$callFunctionOn("function() { 
    return this.textContent; 
  }", chromote_object_id(backend_id = x, driver))$result$value
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
  check_class(x, "selenider_element")
  check_string(name)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" attribute of {.arg x}"),
    timeout = timeout
  )
  
  if (uses_selenium(x$driver)) {
    result <- element$getElementAttribute(name)

    if (is.null(result)) {
      default
    } else {
      result[[1]]
    }
  } else {
    driver <- x$driver
    chromote_get_attribute(element, name, default, driver)
  }
}

chromote_get_attribute <- function(x, name, default, driver) {
  response <- driver$DOM$getAttributes(chromote_node_id(backend_id = x))$attributes
  
  # CDP returns a list of interleaved names and values
  # So the names are the 1st, 3rd, etc. elements.
  names <- response[seq_len(length(response) / 2) * 2 - 1]
  index <- match(name, names)

  if (is.na(index)) {
    default
  } else {
    response[[index * 2]]
  }
}

chromote_get_attributes <- function(x, driver) {
  response <- driver$DOM$getAttributes(chromote_node_id(backend_id = x))$attributes

  # CDP returns a list of interleaved names and values
  # We convert this to a named list
  final_length <- length(response) / 2
  result <- vector("list", final_length)
  for (i in seq_len(final_length) * 2) {
    name <- response[[i - 1]]
    value <- response[[i]]
    result[[name]] <- value
  }

  result
}

#' @rdname html_attr
#'
#' @export
html_attrs <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the attributes of {.arg x}",
    timeout = timeout
  )

  if (uses_selenium(x$driver)) {
    x$driver$executeScript("
      let element = arguments[0];
      let attributes = {};
      for (let i = 0; i < element.attributes.length; i++) {
        attributes[element.attributes[i].name] = element.attributes[i].value;
      }
      return attributes;
    ", list(element))
  } else {
    driver <- x$driver
    chromote_get_attributes(element, driver)
  }
}

#' @rdname html_attr
#'
#' @export
html_value <- function(x, ptype = character(), timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the value of {.arg x}",
    timeout = timeout
  )

  if (uses_selenium(x$driver)) {
    result <- element$getElementAttribute("value")

    if (length(result) == 0) {
      vctrs::vec_cast(NA, ptype)
    } else {
      vctrs::vec_cast(result[[1]], ptype)
    }
  } else {
    driver <- x$driver
    vctrs::vec_cast(chromote_get_attribute(element, "value", NA, driver), ptype)
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
  check_class(x, "selenider_element")
  check_string(name)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" CSS property of {.arg x}"),
    timeout = timeout
  )

  if (uses_selenium(x$driver)) {
    result <- element$getElementValueOfCssProperty(name)

    if (length(result) == 0) {
      NA_character_
    } else {
      result[[1]]
    }
  } else {
    driver <- x$driver
    chromote_get_css_property(element, name, NA_character_, driver)
  }
}

chromote_get_css_property <- function(x, name, default, driver) {
  response <- unlist(driver$CSS$getComputedStyleForNode(chromote_node_id(backend_id = x))$computedStyle)

  # Same as chromote_get_attribute()
  final_length <- length(response) / 2
  result <- vector("list", final_length)
  for (i in seq_len(final_length) * 2) {
    name <- response[[i - 1]]
    value <- response[[i]]
    result[[name]] <- value
  }

  result
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
