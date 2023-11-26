#' @rdname print.selenider_element
#'
#' @export
print_lazy <- function(x, ...) {
  UseMethod("print_lazy")

  invisible(x)
}

#' @rdname print.selenider_element
#'
#' @export
print_lazy.selenider_element <- function(x, ...) {
  cat(format_lazy_selenider_element(x, ...), sep = "\n")

  invisible(x)
}

#' @rdname print.selenider_element
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
