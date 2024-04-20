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
  lifecycle::deprecate_warn("0.4.0", "print_lazy()")

  UseMethod("print_lazy")

  invisible(x)
}

#' @rdname print_lazy
#'
#' @export
print_lazy.selenider_element <- function(x, ...) {
  cat(format_lazy_selenider_element(x, ...), sep = "\n")

  invisible(x)
}

#' @rdname print_lazy
#'
#' @export
print_lazy.selenider_elements <- function(x, ...) {
  cat(format_lazy_selenider_elements(x, ...), sep = "\n")
}

format_lazy_selenider_element <- function(x, ...) {
  cli::cli_format_method({
    bullets <- format_lazy_element(x)
    cli::cli_text("A selenider element selecting:")

    if (length(bullets[names(bullets) != " "]) == 1) {
      cli::cli_text(bullets)
    } else {
      cli::cli_bullets(bullets)
    }
  })
}

format_lazy_element <- function(x, ...) {
  selectors <- x$selectors

  if (length(selectors) == 1) {
    res <- format(selectors[[1]], first = TRUE, ...)
    replace_names_bullets(res)
  } else {
    first <- format(selectors[[1]], first = TRUE, ...)

    # Unlist since format can return a character vector of length >1
    formatted <- unlist(lapply(selectors[-1], format, ...))

    c(replace_names_bullets(first), replace_names_bullets(formatted))
  }
}

format_lazy_selenider_elements <- function(x, ...) {
  cli::cli_format_method({
    bullets <- format_lazy_elements(x)
    cli::cli_text("A collection of selenider elements selecting:")

    if (length(bullets[names(bullets) != " "]) == 1) {
      cli::cli_text(bullets)
    } else {
      cli::cli_bullets(bullets)
    }
  })
}

format_lazy_elements <- function(x, ...) {
  selectors <- x$selectors

  if (length(selectors) == 1) {
    res <- format(selectors[[1]], first = TRUE, multiple = TRUE, ...)
    replace_names_bullets(res)
  } else if (length(selectors) == 2) {
    first <- format(selectors[[1]], first = TRUE, ...)

    last <- format(selectors[[2]], multiple = TRUE, ...)

    c(replace_names_bullets(first), replace_names_bullets(last))
  } else {
    first <- format(selectors[[1]], first = TRUE, ...)

    last <- format(selectors[[length(selectors)]], multiple = TRUE, ...)

    formatted <- unlist(lapply(
      selectors[c(-1, -length(selectors))],
      format, ...
    ))

    c(
      replace_names_bullets(first),
      replace_names_bullets(formatted),
      replace_names_bullets(last)
    )
  }
}
