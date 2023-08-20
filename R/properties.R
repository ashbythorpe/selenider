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
html_name <- function(x, ...) {
  UseMethod("html_name")
}

#' @rdname html_name
#'
#' @export
html_name.selenider_element <- function(x, timeout = NULL, ...) {
  check_dots_used()
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

#' @export
html_name.xml_missing <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_name(x)
}

#' @export
html_name.xml_node <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_name(x)
}

#' @export
html_name.xml_nodeset <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_name(x)
}

#' @export
html_name.rvest_session <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_name(x)
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
html_text <- function(x, ...) {
  UseMethod("html_text")
}

#' @rdname html_text
#'
#' @export
html_text.selenider_element <- function(x, timeout = NULL, ...) {
  check_dots_used()
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
    chromote_get_text(element, driver = driver)
  }
}

#' @export
html_text.xml_missing <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_text(x)
}

#' @export
html_text.xml_node <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_text(x)
}

#' @export
html_text.xml_nodeset <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_text(x)
}

#' @export
html_text.rvest_session <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_text(x)
}

chromote_get_text <- function(x, driver) {
  driver$Runtime$callFunctionOn("function() { 
    return this.textContent; 
  }", chromote_object_id(backend_id = x, driver = driver))$result$value
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
html_attr <- function(x, name, default = NA_character_, ...) {
  UseMethod("html_attr")
}

#' @rdname html_attr
#'
#' @export
html_attr.selenider_element <- function(x, name, default = NA_character_, timeout = NULL, ...) {
  check_dots_used()
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
    chromote_get_attribute(element, name, default, driver = driver)
  }
}

#' @export
html_attr.xml_missing <- function(x, name, default = NA_character_, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attr(x, name, default)
}

#' @export
html_attr.xml_node <- function(x, name, default = NA_character_, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attr(x, name, default)
}

#' @export
html_attr.xml_nodeset <- function(x, name, default = NA_character_, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attr(x, name, default)
}

#' @export
html_attr.rvest_session <- function(x, name, default = NA_character_, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attr(x, name, default)
}

chromote_get_attribute <- function(x, name, default, driver) {
  response <- driver$DOM$getAttributes(chromote_node_id(backend_id = x, driver = driver))$attributes
  
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
  response <- driver$DOM$getAttributes(chromote_node_id(backend_id = x, driver = driver))$attributes

  # CDP returns a list of interleaved names and values
  # We convert this to a named list
  final_length <- length(response) / 2
  result <- list()
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
html_attrs <- function(x, ...) {
  UseMethod("html_attrs")
}

#' @rdname html_attr
#'
#' @export
html_attrs <- function(x, timeout = NULL, ...) {
  check_dots_used()
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
    chromote_get_attributes(element, driver = driver)
  }
}

#' @export
html_attrs.xml_missing <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attrs(x)
}

#' @export
html_attrs.xml_node <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attrs(x)
}

#' @export
html_attrs.xml_nodeset <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attrs(x)
}

#' @export
html_attrs.rvest_session <- function(x, ...) {
  check_dots_used()
  rlang::check_installed("rvest")
  rvest::html_attrs(x)
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
    result <- chromote_get_attribute(element, "value", NA, driver = driver)

    if (is.integer(ptype)) {
      return(suppressWarnings(as.integer(result)))
    } else if (is.double(ptype)) {
      return(suppressWarnings(as.double(result)))
    }

    vctrs::vec_cast(result, ptype)
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
    chromote_get_css_property(element, name, NA_character_, driver = driver)
  }
}

chromote_get_css_property <- function(x, name, default, driver) {
  driver$CSS$enable()
  if (is.null(driver$CSS$getComputedStyleForNode)) {
    result <- driver$Runtime$callFunctionOn("function() {
      return getComputedStyle(this).getPropertyValue('aa')
    }", chromote_object_id(backend_id = x, driver = driver))$result$value

    if (result == "") {
      default
    } else {
      result
    }
  } else {
    response <- unlist(driver$CSS$getComputedStyleForNode(chromote_node_id(backend_id = x, driver = driver))$computedStyle)

    # Same as chromote_get_attribute()
    names <- response[seq_len(length(response) / 2) * 2 - 1]
    index <- match(name, names)

    if (is.na(index)) {
      default
    } else {
      response[[index * 2]]
    }
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
