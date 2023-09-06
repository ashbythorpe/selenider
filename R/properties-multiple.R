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
#' @examplesIf selenider_available(online = FALSE)
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
html_size <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  elements <- get_with_timeout(timeout, get_elements, x)

  if (is.null(elements)) {
    cli::cli_abort(
      c(
        "To get the number of elements in {.arg x}, its parent must exist.",
        "i" = paste0(format_timeout_for_error(timeout), "{.arg x}'s parent did not exist.")
      ),
      class = c(
        "error_not_actionable",
        "selenider_error_absent_parent",
        "selenider_error_absent_element",
        "expect_error_continue"
      )
    )
  } else {
    length(elements)
  }
}

#' @rdname html_size
#' 
#' @export
length.selenider_elements <- function(x) {
  html_size(x)
}
