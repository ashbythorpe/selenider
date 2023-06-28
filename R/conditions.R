#' Does an element exist?
#' 
#' `is_present()` and `is_in_dom()` checks if an element is present on the page,
#' while `is_missing()` and `is_absent()` checks the opposite.
#' 
#' @param x A `selenider_element` object.
#' 
#' @details
#' These functions do not implement a retry mechanism, and only test a condition
#' once. Use [html_expect()] or [html_wait_until()] to use these conditions in
#' tests.
#' 
#' @returns 
#' A boolean value: TRUE or FALSE.
#' 
#' @family conditions
#' 
#' @examples 
#' session <- mock_selenider_session()
#' 
#' is_present(s(".class1"))
#' 
#' @export
is_present <- function(x) {
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
is_missing <- function(x) !is_present(x)

#' @rdname is_present
#' 
#' @export
is_absent <- is_missing

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
#' @examples 
#' session <- mock_selenider_session()
#' 
#' is_visible(s(".class1"))
#' 
#' @export
is_visible <- function(x) {
  element <- get_element(x)
  
  if (!is.null(element)) {
    element$isElementDisplayed()
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
#' @examples 
#' session <- mock_selenider_session()
#' 
#' is_enabled(s(".class1"))
#' 
#' @export
is_enabled <- function(x) {
  element <- get_element(x)
  
  if (!is.null(element)) {
    element$isElementEnabled()
  } else {
    stop_absent_element()
  }
}

#' @rdname is_enabled
#' 
#' @export
is_disabled <- function(x) !is_enabled(x)

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
#' @examples 
#' session <- mock_selenider_session()
#' 
#' has_text(s(".class1"), "Example")
#' 
#' @export
has_text <- function(x, text) {
  element <- get_element(x)
  
  if (!is.null(element)) {
    grepl(text, element$getElementText(), fixed = TRUE)
  } else {
    stop_absent_element()
  }
}

#' @rdname has_text
#' 
#' @export
has_exact_text <- function(x, text) {
  element <- get_element(x)
  
  if (!is.null(element)) {
    identical(element$getElementText(), text)
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
#' @examples
#' session <- mock_selenider_session()
#'
#' has_attr(s(".class1"), "class", "class1")
#'
#' @export
has_attr <- function(x, name, value) {
  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- element$getElementAttribute(name)
  
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

#' @rdname has_attr
#'
#' @export
attr_contains <- function(x, name, value) {
  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- element$getElementAttribute(name)

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
  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- element$getElementAttribute("value")
  
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
#' @examples
#' session <- mock_selenider_session()
#'
#' has_css_property(s(".class1"), "background-color", "red")
#'
#' @export
has_css_property <- function(x, property, value) {
  element <- get_element(x)
  
  if (is.null(element)) {
    stop_absent_element()
  }

  result <- element$getElementValueOfCssProperty(property)
  
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
