#' Get the tag name of an element
#'
#' Get the tag name (e.g. `"p"` for a `<p>` tag) of a `selenider_element`
#' object.
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
#' @family properties
#'
#' @export
elem_name <- function(x, timeout = NULL) {
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the tag name of {.arg x}",
    timeout = timeout,
  )

  element_name(element, x$session, x$driver)
}

element_name <- function(x, session, driver) {
  if (session == "chromote") {
    driver <- driver
    tolower(driver$DOM$describeNode(backendNodeId = x)$node$nodeName)
  } else if (session == "selenium") {
    x$get_tag_name()
  } else {
    unpack_list(x$getElementTagName())
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
#' @export
elem_text <- function(x, timeout = NULL) {
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the text inside {.arg x}",
    timeout = timeout,
  )

  element_text(element, x$session, x$driver)
}

element_text <- function(x, session, driver) {
  if (session == "chromote") {
    chromote_get_text(x, driver = driver)
  } else if (session == "selenium") {
    x$get_text()
  } else {
    unpack_list(x$getElementText())
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
#' @param ptype The type to cast the value to. Useful when the value is an
#'   integer or decimal number. By default, the value is returned as a string.
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
#' @export
elem_attr <- function(x, name, default = NULL, timeout = NULL) {
  check_string(name)
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" attribute of {.arg x}"),
    timeout = timeout
  )

  element_attribute(element, name, default, x$session, x$driver)
}

element_attribute <- function(x, name, default, session, driver) {
  if (session == "chromote") {
    driver <- driver
    chromote_get_attribute(x, name, default, driver = driver)
  } else if (session == "selenium") {
    x$get_attribute(name)
  } else {
    unpack_list(x$getElementAttribute(name))
  }
}

chromote_get_attribute <- function(x, name, default, driver) {
  response <- driver$DOM$getAttributes(chromote_node_id(
    backend_id = x,
    driver = driver
  ))$attributes

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
  response <- driver$DOM$getAttributes(chromote_node_id(
    backend_id = x,
    driver = driver
  ))$attributes

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

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the attributes of {.arg x}",
    timeout = timeout
  )

  element_attributes(element, x$session, x$driver)
}

element_attributes <- function(x, session, driver) {
  if (session == "chromote") {
    chromote_get_attributes(x, driver = driver)
  } else {
    execute_js_fn_on("function(x) {
      let attributes = {};
      for (let i = 0; i < x.attributes.length; i++) {
        attributes[x.attributes[i].name] = x.attributes[i].value;
      }
      return attributes;
    }", x, session = session, driver = driver)
  }
}

#' @rdname elem_attr
#'
#' @export
elem_value <- function(x, ptype = character(), timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = "get the value of {.arg x}",
    timeout = timeout
  )

  result <- element_value(element, x$session, x$driver)

  if (is.null(result)) {
    NULL
  } else {
    convert_value(result, ptype)
  }
}

element_value <- function(x, session, driver) {
  type <- element_input_type(x, session = session, driver = driver)

  if (type == "select") {
    element_select_value(x, session = session, driver = driver)
  } else if (type == "contenteditable") {
    element_text(x, session = session, driver = driver)
  } else {
    execute_js_fn_on("x => x.value", x, session = session, driver = driver)
  }
}

element_select_value <- function(x, session, driver) {
  result <- execute_js_fn_on("function(x) {
    if (x.type == 'select-one') {
      return x.options[x.selectedIndex].value;
    } else {
      let result = [];

      for (let i = 0; i < x.options.length; i++) {
        if (x.options[i].selected) {
          result.push(x.options[i].value);
        }
      }

      return result;
    }
  }", x, session = session, driver = driver)

  if (is.list(result)) {
    unlist(result)
  } else {
    result
  }
}

convert_value <- function(x, ptype) {
  if (is.integer(ptype)) {
    return(suppressWarnings(as.integer(x)))
  } else if (is.double(ptype)) {
    return(suppressWarnings(as.double(x)))
  }

  vctrs::vec_cast(x, ptype)
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
#' A string, or `NULL` if the property does not exist.
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
#' @export
elem_css_property <- function(x, name, timeout = NULL) {
  check_class(x, "selenider_element")
  check_string(name)
  check_number_decimal(timeout, allow_null = TRUE)

  check_active(x)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("get the \"", name, "\" CSS property of {.arg x}"),
    timeout = timeout
  )

  element_css_property(element, name, x$session, x$driver)
}

element_css_property <- function(x, name, session, driver) {
  if (session == "chromote") {
    driver <- driver
    chromote_get_css_property(x, name, NULL, driver = driver)
  } else if (session == "selenium") {
    x$get_css_value(name)
  } else {
    unpack_list(x$getElementValueOfCssProperty(name))
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
    response <- unlist(driver$CSS$getComputedStyleForNode(chromote_node_id(
      backend_id = x,
      driver = driver
    ))$computedStyle)

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
#' @export
elem_size <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  check_active(x)

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

get_element_for_property <- function(x,
                                     action,
                                     timeout,
                                     call = rlang::caller_env()) {
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

get_elements_for_property <- function(x,
                                      action,
                                      timeout,
                                      call = rlang::caller_env()) {
  get_elements_for_action(
    x,
    action = action,
    conditions = list(),
    timeout = timeout,
    failure_messages = c(),
    conditions_text = c(),
    call = call
  )
}
