#' Get the tag name of an element
#'
#' Get the tag name (e.g. `"p"` for a `<p>` tag) of a `selenider_element` object.
#'
#' @param x A `selenider_element` object.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns
#' A string.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div class='mydiv'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' s(".mydiv") |>
#'   elem_name()
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @family properties
#'
#' @export
elem_name <- function(x, timeout = NULL) {
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the tag name of {.arg x}",
    timeout = timeout,
  )

  if (uses_selenium(x$driver)) {
    unpack_list(element$getElementTagName())
  } else {
    driver <- x$driver
    tolower(driver$DOM$describeNode(backendNodeId = element)$node$nodeName)
  }
}

#' Get the text inside an element
#'
#' Get the inner text of a `selenider_element` object.
#'
#' @inheritParams elem_name
#'
#' @returns A string.
#'
#' @family properties
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <p>Example text</p>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' s("p") |>
#'   elem_text()
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
elem_text <- function(x, timeout = NULL) {
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the text inside {.arg x}",
    timeout = timeout,
  )

  if (uses_selenium(x$driver)) {
    unpack_list(element$getElementText())
  } else {
    driver <- x$driver
    chromote_get_text(element, driver = driver)
  }
}

chromote_get_text <- function(x, driver) {
  driver$Runtime$callFunctionOn("function() {
    return this.textContent;
  }", chromote_object_id(backend_id = x, driver = driver))$result$value
}

#' Get attributes of an element
#'
#' @description
#' Get an attribute of a `selenider_element` object.
#'
#' `elem_attr()` returns a *single* attribute value as a string.
#'
#' `elem_attrs()` returns a named list containing *every* attribute.
#'
#' `elem_value()` returns the 'value' attribute.
#'
#' @param x A `selenider_element` object.
#' @param name The name of the attribute to get; a string.
#' @param default The default value to use if the attribute does not exist in
#'   the element.
#' @param ptype The type to cast the value to. Useful when the value is an integer
#'   or decimal number. By default, the value is returned as a string.
#' @param timeout The time to wait for `x` to exist.
#'
#' @returns 
#' `elem_attr()` returns a character vector of length 1. `elem_attrs()`
#' returns a named list of strings. The return value of `elem_value()` has the
#' same type as `ptype` and length 1.
#'
#' @family properties
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <a class='link' href='https://r-project.org'>R</a>
#' <input type='number' value='0'>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' s("a") |>
#'   elem_attr("href")
#'
#' s("a") |>
#'   elem_attrs()
#'
#' s("input[type='number']") |>
#'   elem_value(ptype = integer())
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
elem_attr <- function(x, name, default = NA_character_, timeout = NULL) {
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

    if (length(result) == 0) {
      default
    } else {
      result[[1]]
    }
  } else {
    driver <- x$driver
    chromote_get_attribute(element, name, default, driver = driver)
  }
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

#' @rdname elem_attr
#'
#' @export
elem_attrs <- function(x, timeout = NULL) {
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

#' @rdname elem_attr
#'
#' @export
elem_value <- function(x, ptype = character(), timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the value of {.arg x}",
    timeout = timeout
  )

  if (uses_selenium(x$driver)) {
    result <- unpack_list(element$getElementAttribute("value"))

    if (is.null(result) || identical(result, "")) {
      vctrs::vec_cast(NA, ptype)
    } else {
      if (is.numeric(ptype)) {
        result <- suppressWarnings(as.numeric(result))
      }

      vctrs::vec_cast(result, ptype)
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
#' Specifically, the *computed* style is returned, meaning that,
#' for example, widths and heights will be returned in pixels, and
#' colours will be returned as an RGB value.
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
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <p style='visibility:hidden; color:red;'>Text</p>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' s("p") |>
#'   elem_css_property("visibility")
#'
#' s("p") |>
#'   elem_css_property("color")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
elem_css_property <- function(x, name, timeout = NULL) {
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
  if (!is.null(driver$CSS$enable)) {
    driver$CSS$enable()
  }

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

#' Get the number of elements in a collection
#' 
#' @description
#' Get the number of elements in a HTML element collection, waiting for the 
#' parent elements (if any) to exist before returning a value.
#' 
#' `length()` and `elem_size()` can be used interchangeably, the only
#' difference being that `elem_size()` allows you to specify a timeout.
#' 
#' @param x A `selenider_elements` object.
#' @param timeout The time to wait for the parent of `x` (if any) to exist.
#' 
#' @returns An integer representing the number of elements in the collection.
#'
#' @family properties
#' 
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div></div>
#' <div></div>
#' <div></div>
#' <div></div>
#' "
#' session <- minimal_selenider_session(html)
#' 
#' ss("div") |>
#'   length()
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#' 
#' @export
elem_size <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  elements <- get_elements_for_property(
    x,
    action = "get the number of elements in {.arg x}",
    timeout = timeout
  )

  length(elements)
}

#' @rdname elem_size
#' 
#' @export
length.selenider_elements <- function(x) {
  elem_size(x)
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

get_elements_for_property <- function(x, action, timeout, call = rlang::caller_env()) {
  elements <- get_with_timeout(timeout, get_elements, x)

  if (is.null(elements)) {
    stop_not_actionable(
      c(
        paste0("To ", action, ", its parent must exist."),
        "i" = paste0(format_timeout_for_error(timeout), "{.arg x}'s parent did not exist.")
      ),
      call = call,
      class = c(
        "selenider_error_absent_parent",
        "selenider_error_absent_element"
      )
    )
  }

  elements
}
