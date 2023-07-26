#' Get the number of elements in a collection
#' 
#' @description
#' Get the number of elements in a HTML element collection, waiting for the 
#' parent elements (if any) to exist before returning a value.
#' 
#' `length()` and `html_size()` can be used interchangeably, the only
#' difference being that `html_size()` allows you to specify a timeout.
#' 
#' @param x A `selenider_elements` object.
#' @param timeout The time to wait for the parent of `x` (if any) to exist.
#' 
#' @returns An integer representing the number of elements in the collection.
#' 
#' @examples
#' session <- mock_selenider_session()
#' 
#' ss(".class1") |>
#'   length()
#' 
#' @export
html_size <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  elements <- get_with_timeout(timeout, get_elements, x)

  if (is.null(elements)) {
    cli::cli_abort(c(
      "To get the number of elements in {.arg x}, its parent must exist.",
      "After {.val timeout} seconds, {.arg x}'s parent did not exist."
    ))
  } else {
    length(elements)
  }
}

#' @rdname html_size
#' 
#' @export
length.html_elements <- function(x) {
  html_size(x)
}
