#' Conditions for selenider element collections
#' 
#' @description 
#' These functions are predicates that test certain conditions on a 
#' `selenider_elements` object, which are useful in conjunction with 
#' [html_expect()] and [html_wait_until()].
#' 
#' `has_length()`, or `has_size()` checks if an element collection contains a
#' certain number of elements.
#' 
#' @param x A `selenider_elements` object.
#' @param size The number of elements to expect.
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
#' has_length(ss(".class1"), 2)
#'
#' @name html-conditions-multiple
NULL

#' @rdname html-conditions-multiple
#'
#' @export
has_length <- function(x, size) {
  elements <- get_elements(x)

  if (!is.null(x)) {
    length(elements) == size
  } else {
    stop_absent_parent_element()
  }
}

#' @rdname html-conditions-multiple
#'
#' @export
has_size <- has_length
