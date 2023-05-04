#' Conditions for selenider elements
#' 
#' @description 
#' These functions are predicates that test certain conditions on a 
#' `selenider_element` object, which are useful in conjunction with 
#' [html_expect()] and [html_wait_until()].
#' 
#' `exists()`, `is_present()` and `is_in_dom()` checks if an element is present
#' on the page, while `is_missing()` and `is_absent()` checks the opposite.
#' 
#' `is_visible()` and `is_displayed()` checks that an element can be seen on the
#' page, while `is_invisible()` and `is_hidden()` checks the opposite.
#' 
#' `is_disabled()` checks that an element has the `disabled` attribute set to 
#' `TRUE`, while `is_enabled()` checks that it does not.
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
#' @examples 
#' session <- mock_selenider_session()
#' 
#' exists(s(".class1"))
#' 
#' @rdname html-conditions
#' 
#' @export
exists <- function(x) {
  !is.null(update_element(x))
}

#' @rdname html-conditions
#' 
#' @export
is_present <- exists

#' @rdname html-conditions
#' 
#' @export
is_in_dom <- exists

#' @rdname html-conditions
#' 
#' @export
is_missing <- function(x) !exists(x)

#' @rdname html-conditions
#' 
#' @export
is_absent <- is_missing

#' @rdname html-conditions
#' 
#' @export
is_visible <- function(x) {
  element <- update_element(x)
  
  actual <- element$element
  
  if (!is.list(actual) || length(actual) != 1 || !is.null(actual[[1]])) {
    element$element$isElementDisplayed()
  } else {
    FALSE
  }
}

#' @rdname html-conditions
#' 
#' @export
is_displayed <- is_visible

#' @rdname html-conditions
#' 
#' @export
is_hidden <- function(x) {
  !is_visible(x)
}

#' @rdname html-conditions
#' 
#' @export
is_invisible <- is_hidden

#' @rdname html-conditions
#' 
#' @export
is_enabled <- function(x) {
  element <- update_element(x)
  
  actual <- element$element
  
  if (!is.list(actual) || length(actual) != 1 || !is.null(actual[[1]])) {
    element$element$isElementEnabled()
  } else {
    FALSE
  }
}


