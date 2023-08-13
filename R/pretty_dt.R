#' Format a difftime
#'
#' Internal method used to print a [selenider_session()] object. Designed to
#' be used with [prettyunits::pretty_dt()], [prettyunits::pretty_ms()] and
#' [prettyunits::pretty_sec()].
#'
#' @param x A string representing a difftime.
#' @param style,... Not used.
#'
#' @returns
#' An object of class `pretty_dt`.
#'
#' @examples
#' x <- as_pretty_dt(prettyunits::pretty_sec(10))
#'
#' cli::cli_text("{.val x}")
#'
#' @keywords internal
#'
#' @export
as_pretty_dt <- function(x) {
  class(x) <- "pretty_dt"
  x
}

#' @rdname as_pretty_dt
#'
#' @importFrom cli cli_format
#' @export
cli_format.pretty_dt <- function(x, style = NULL, ...) {
  x
}
