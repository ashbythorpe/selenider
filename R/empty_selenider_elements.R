empty_selenider_elements <- function(driver, driver_id) {
  res <- list(
    driver = driver,
    driver_id = driver_id
  )

  class(res) <- c("empty_selenider_elements", "selenider_elements", "list")

  res
}

#' @export
format.empty_selenider_elements <- function(x, ...) {
  cli::cli_format_method(
    cli::cli_text("A collection of selenider elements containing no elements.")
  )
}

#' @export
print.empty_selenider_elements <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

elements_is_empty <- function(x) {
  inherits(x, "empty_selenider_elements")
}
