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
#' `has_text()` checks that an element's inner text contains a string, while
#' `has_exact_text()` checks that the inner text *only* contains the string.
#' 
#' @param x A `selenider_element` object.
#' @param text A string, used to test the element's inner text.
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
#' \dontshow{
#' # This allows `local_session()` to work when being sourced.
#' prev_options <- options(withr.hook_source = TRUE)
#' }
#' session <- mock_selenider_session()
#' 
#' exists(s(".class1"))
#' \dontshow{
#' options(prev_options)
#' }
#' 
#' @name html-conditions
NULL

#' @rdname html-conditions
#' 
#' @export
exists <- function(x) {
  element <- get_element(x)
  
  !is.null(element)
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
  element <- get_element(x)
  
  if (!is.null(element)) {
    element$isElementDisplayed()
  } else {
    stop_absent_element()
  }
}

#' @rdname html-conditions
#' 
#' @export
is_displayed <- is_visible

#' @rdname html-conditions
#' 
#' @export
is_hidden <- function(x) !is_visible(x)

#' @rdname html-conditions
#' 
#' @export
is_invisible <- is_hidden

#' @rdname html-conditions
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

#' @rdname html-conditions
#' 
#' @export
is_disabled <- function(x) !is_enabled(x)

#' @rdname html-conditions
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

#' @rdname html-conditions
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
