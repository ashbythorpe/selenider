as_pretty_dt <- function(x) {
  class(x) <- "pretty_dt"
  x
}

#' @export
cli_format.pretty_dt <- function(x, style = NULL, ...) {
  x
}
