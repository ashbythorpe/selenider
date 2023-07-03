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
#' once. Use [html_expect()] or [html_wait_until()] to use these conditions in
#' tests.
#'
#' @returns A boolean value: `TRUE` or `FALSE`
#' 
#' @family collection conditions
#'
#' @examples 
#' session <- mock_selenider_session()
#' 
#' has_length(ss(".class1"), 2)
#' 
#' @export
has_length <- function(x, n) {
  check_class(x, "selenider_elements")
<<<<<<< refs/remotes/origin/main
  check_number_whole(n, min = 0L)
=======
  check_number_whole(n, min = 0)
>>>>>>> Fix R CMD CHECK issues

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
  check_number_whole(n, min = 0L)

  # TODO: make this more efficient.
  # First make sure parent element exists, then get nth element.
  elements <- get_elements(x)
  
  if (!is.null(x)) {
    length(elements) >= n
  } else {
    stop_absent_parent_element()
  }
}
