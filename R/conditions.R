#' Does an element exist?
#' 
#' `is_present()` and `is_in_dom()` checks if an element is present on the page,
#' while `is_missing()` and `is_absent()` checks the opposite.
#' 
#' @param x A `selenider_element` object.
#' 
#' @details
#' These functions do not implement a retry mechanism, and only test a condition
#' once. Use [elem_expect()] or [elem_wait_until()] to use these conditions in
#' tests.
#' 
#' @returns 
#' A boolean value: TRUE or FALSE.
#' 
#' @family conditions
#' 
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <p class='class1'></p>
#' "
#'
#' session <- minimal_selenider_session(html)
#' 
#' is_present(s(".class1")) # TRUE
#'
#' is_in_dom(s(".class2")) # FALSE
#'
#' is_absent(s(".class2")) # TRUE
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#' 
#' @export
is_present <- function(x) {
  check_class(x, "selenider_element")

  element <- get_element(x)
  
  !is.null(element)
}

#' @rdname is_present
#' 
#' @export
is_in_dom <- is_present

#' @rdname is_present
#' 
#' @export
is_absent <- function(x) !is_present(x)

#' Is an element visible?
#'
#' `is_visible()` and `is_displayed()` checks that an element can be seen on the
#' page, while `is_invisible()` and `is_hidden()` checks the opposite. All
#' functions throw an error if the element is not in the DOM.
#' 
#' @inheritParams is_present
#' 
#' @inherit is_present details return
#' 
#' @family conditions
#' 
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div style='visibility:hidden;'>Content 1</div>
#' <div style='display:none'>Content 2</div>
#' <div>Content 3</div>
#' "
#'
#' session <- minimal_selenider_session(html)
#' 
#' is_visible(s("div")) # FALSE
#'
#' is_invisible(ss("div")[[2]]) # TRUE
#'
#' is_visible(ss("div")[[3]]) # TRUE
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#' 
#' @export
is_visible <- function(x) {
  check_class(x, "selenider_element")

  element <- get_element(x)
  
  if (!is.null(element)) {
    if (uses_selenium(x$driver)) {
      unpack_list(element$isElementDisplayed())
    } else {
      driver <- x$driver
      tryCatch({
        coords <- driver$DOM$getBoxModel(backendNodeId = element)$model$content
        !chromote_get_css_property(element, "visibility", default = NULL, driver = driver) %in% c("hidden", "collapse")
      }, error = function(e) FALSE)
    }
  } else {
    stop_absent_element()
  }
}

#' @rdname is_visible
#' 
#' @export
is_displayed <- is_visible

#' @rdname is_visible
#' 
#' @export
is_hidden <- function(x) !is_visible(x)

#' @rdname is_visible
#' 
#' @export
is_invisible <- is_hidden

#' Is an element enabled?
#' 
#' `is_disabled()` checks that an element has the `disabled` attribute set to 
#' `TRUE`, while `is_enabled()` checks that it does not. Both functions throw an
#' error if the element does not exist in the DOM.
#' 
#' @inheritParams is_present
#' 
#' @inherit is_present details return
#' 
#' @family conditions
#' 
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button></button>
#' <button disabled></button>
#' "
#'
#' session <- minimal_selenider_session(html)
#' 
#' is_enabled(s("button")) # TRUE
#'
#' is_disabled(ss("button")[[2]]) # TRUE
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#' 
#' @export
is_enabled <- function(x) {
  check_class(x, "selenider_element")

  element <- get_element(x)
  
  if (!is.null(element)) {
    if (uses_selenium(x$driver)) {
      unpack_list(element$isElementEnabled())
    } else {
      driver <- x$driver
      driver$Runtime$callFunctionOn("function() {
        return !this.disabled
      }", chromote_object_id(backend_id = element, driver = driver))$result$value
    }
  } else {
    stop_absent_element()
  }
}

#' @rdname is_enabled
#' 
#' @export
is_disabled <- function(x) !is_enabled(x)

#' Does an element have a tag name?
#' 
#' Check that an element has a specified tag name
#'
#' @param x A `selenider_element` object.
#' @param name A string.
#'
#' @returns
#' A boolean value.
#'
#' @family conditions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div id='mydiv'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' has_name(s("#mydiv"), "p")
#'
#' has_name(s("#mydiv"), "div")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
has_name <- function(x, name) {
  check_class(x, "selenider_element")
  check_string(name)

  element <- get_element(x)

  if (!is.null(element)) {
    if (uses_selenium(x$driver)) {
      unpack_list(element$getElementTagName()) == name
    } else {
      driver <- x$driver
      tolower(driver$DOM$describeNode(backendNodeId = element)$node$nodeName) == name
    }
  } else {
    stop_absent_element()
  }
}

#' Does an element contain a pattern?
#' 
#' `has_text()` checks that an element's inner text contains a string, while
#' `has_exact_text()` checks that the inner text *only* contains the string.
#' Both functions throw an error if the element does not exist in the DOM.
#' 
#' @inheritParams is_present
#' @param text A string, used to test the element's inner text.
#' 
#' @inherit is_present details return
#' 
#' @family conditions
#' 
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <p>Example text</p>
#' <p class='empty'></p>
#' "
#'
#' session <- minimal_selenider_session(html)
#' 
#' has_text(s("p"), "Example") # TRUE
#'
#' has_exact_text(s("p"), "Example") # FALSE
#'
#' has_exact_text(s("p"), "Example text") # TRUE
#'
#' # has_exact_text() is useful for checking when there is no text,
#' # since has_text("") will always be TRUE.
#' has_exact_text(s(".empty"), "")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#' 
#' @export
has_text <- function(x, text) {
  check_class(x, "selenider_element")
  check_string(text)

  element <- get_element(x)
  
  if (!is.null(element)) {
    if (uses_selenium(x$driver)) {
      grepl(text, unpack_list(element$getElementText()), fixed = TRUE)
    } else {
      driver <- x$driver
      actual <- chromote_get_text(element, driver = driver)
      grepl(text, actual, fixed = TRUE)
    }
  } else {
    stop_absent_element()
  }
}

#' @rdname has_text
#' 
#' @export
has_exact_text <- function(x, text) {
  check_class(x, "selenider_element")
  check_string(text)

  element <- get_element(x)
  
  if (!is.null(element)) {
    if (uses_selenium(x$driver)) {
      identical(unpack_list(element$getElementText()), text)
    } else {
      driver <- x$driver
      actual <- chromote_get_text(element, driver = driver)
      identical(actual, text)
    }
  } else {
    stop_absent_element()
  }
}

#' Does an element's attribute match a value?
#'
#' @description
#' `has_attr()` checks that an element's attribute matches a value, while
#' `attr_contains()` checks that an element's attribute contains a value.
#'
#' `has_value()` is a shortcut for `has_attr("value")`: it checks that an
#' element's value matches a string or number.
#'
#' @param x A `selenider_element` object.
#' @param name The name of the attribute.
#' @param value The value of the attribute. For `has_attr()` and `has_value()`,
#' this can be a string or a numeric value, while `attr_contains()` can only take
#' a string.
#'
#' @family conditions
#'
#' @returns A boolean value: TRUE or FALSE.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <input class='myclass' value='1.0' data-customattr='Custom attribute text'></p>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' has_attr(s("input"), "class", "myclass")
#'
#' has_attr(s("input"), "value", 1)
#' has_value(s("input"), 1)
#'
#' attr_contains(s("input"), "data-customattr", "Custom attribute")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
has_attr <- function(x, name, value) {
  check_class(x, "selenider_element")
  check_string(name)
  vctrs::vec_check_size(value, 1)

  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }
  
  result <- if (uses_selenium(x$driver)) {
    element$getElementAttribute(name)
  } else {
    driver <- x$driver
    chromote_get_attribute(element, name, list(), driver = driver)
  }
  
  if (length(result) == 0) {
    if (is.na(value)) {
      TRUE
    } else {
      FALSE
    }
  } else {
    if (is.numeric(value)) {
      numeric_result <- suppressWarnings(as.numeric(result[[1]]))

      if (is.na(numeric_result)) {
        return(FALSE)
      }

      numeric_result == value
    } else {
      result[[1]] == value
    }
  }
}

#' @rdname has_attr
#'
#' @export
attr_contains <- function(x, name, value) {
  check_class(x, "selenider_element")
  check_string(name)
  check_string(value)

  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- if (uses_selenium(x$driver)) {
    element$getElementAttribute(name)
  } else {
    driver <- x$driver
    chromote_get_attribute(element, name, list(), driver = driver)
  }

  if (length(result) == 0) {
    FALSE
  } else {
    grepl(value, result[[1]], fixed = TRUE)
  }
}

#' @rdname has_attr
#'
#' @export
has_value <- function(x, value) {
  check_class(x, "selenider_element")
  vctrs::vec_check_size(value, 1)

  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- if (uses_selenium(x$driver)) {
    element$getElementAttribute("value")
  } else {
    driver <- x$driver
    chromote_get_attribute(element, "value", list(), driver = driver)
  }
  
  if (length(result) == 0) {
    if (is.null(value)) {
      TRUE
    } else {
      FALSE
    }
  } else {
    if (is.numeric(value)) {
      numeric_result <- suppressWarnings(as.numeric(result[[1]]))

      if (is.na(numeric_result)) {
        return(FALSE)
      }

      numeric_result == value
    } else {
      result[[1]] == value
    }
  }
}

#' Does an element's css property match a value?
#'
#' @description
#' Check that the CSS property (e.g. `"background-color"`) of an element matches
#' a value.
#'
#' @param x A `selenider_element` object.
#' @param property The name of the CSS property
#' @param value The value of the attribute.
#'
#' @family conditions
#'
#' @returns A boolean value: TRUE or FALSE.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div style='display:none;'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' has_css_property(s("div"), "display", "none")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
has_css_property <- function(x, property, value) {
  check_class(x, "selenider_element")
  check_string(property)
  check_string(value)

  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- if (uses_selenium(x$driver)) {
    element$getElementValueOfCssProperty(property)
  } else {
    driver <- x$driver
    chromote_get_css_property(element, property, list(), driver = driver)
  }
  
  if (length(result) == 0) {
    if (is.null(value)) {
      TRUE
    } else {
      FALSE
    }
  } else {
    result[[1]] == value
  }
}

is_covered <- function(x) {
  check_class(x, "selenider_element")

  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  } else {
    visible <- if (uses_selenium(x$driver)) {
      unpack_list(element$isElementDisplayed())
    } else {
      driver <- x$driver
      tryCatch({
        driver$DOM$getBoxModel(backendNodeId = element)
        TRUE
      }, error = function(e) FALSE)
    }

    if (!visible) {
      stop_invisible_element()
    } else if (uses_selenium(x$driver)) {
      x$driver$executeScript("
        let element = arguments[0];

        let rect = element.getBoundingClientRect();
        let x = rect.left + rect.width/2;
        let y = rect.top + rect.height/2;

        let cover = document.elementFromPoint(x, y);
        return cover == null || element.isSameNode(cover);
      ", list(element))
    } else {
      driver <- x$driver
      coords <- chromote_get_xy(backend_id = element, driver = driver)
      node_at_location <- driver$DOM$getNodeForLocation(x = coords$x, y = coords$y)
      node_at_location$backendNodeId == element
    }
  }
}

#' Does a collection have a certain number of elements?
#'
#' `has_length()` and `has_size()` checks that a collection of HTML elements
#' contains a certain number of elements.
#'
#' `has_at_least()` checks that a collection contains *at least* `n` elements.
#'
#' @param x A `selenider_elements` object.
#' @param n A numeric vector of possible lengths of `x`. For `has_at_least()`,
#'   this must be a single number to compare to the length of `x`.
#'
#' @details
#' These functions do not implement a retry mechanism, and only test a condition
#' once. Use [elem_expect()] or [elem_wait_until()] to use these conditions in
#' tests.
#'
#' @returns A boolean value: `TRUE` or `FALSE`
#'
#' @family conditions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div class='div1'></div>
#' <div class='div2'></div>
#' <div class='div3'></div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' has_length(ss("div"), 3)
#' has_at_least(ss("div"), 2)
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
has_length <- function(x, n) {
  check_class(x, "selenider_elements")
  check_number_whole(n, min = 0)

  elements <- get_elements(x)
  n <- vctrs::vec_cast(n, integer())

  if (!is.null(x)) {
    length(elements) %in% n
  } else {
    stop_absent_parent_element()
  }
}

#' @rdname has_length
#'
#' @export
has_size <- has_length

#' @rdname has_length
#'
#' @export
has_at_least <- function(x, n) {
  check_class(x, "selenider_elements")
  check_number_whole(n, min = 0)

  x <- tryCatch(
    elem_cache(x),
    error = function(e) stop_absent_parent_element()
  )

  is_present(x[[n]])
}
