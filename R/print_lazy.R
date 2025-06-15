#' Print an element without fetching it
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Display a summary of the steps needed to reach an element. This function
#' is deprecated, as it is not useful for most users.
#'
#' @param x A `selenider_element` or `selenider_elements` object.
#' @param ... Not used.
#'
#' @returns `x`, invisibly.
#'
#' @export
print_lazy <- function(x, ...) {
  lifecycle::deprecate_stop("0.4.0", "print_lazy()")
}
