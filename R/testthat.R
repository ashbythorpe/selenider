#' Create a elem_expect test failure
#'
#' Uses testthat to create a test failure instead of an error if
#' [elem_expect()] fails.
#'
#' @param condition A character vector of error bullets.
#' @param parent The parent error to inherit from, if any.
#' @param call The environment of [elem_expect()], to throw the error in.
#' @param x The element on which the conditions failed.
#' @param x_name The name describing `x`.
#' @param env The environment in which to evaluate the `condition` bullets
#' using glue.
#'
#' @noRd
elem_expect_fail <- function(condition,
                             parent,
                             call,
                             x,
                             x_name,
                             env = rlang::caller_env()) {
  condition_text <- cli::format_error(condition, .envir = env)

  if (!is.null(parent)) {
    # Capture the "Caused by error in fn()" message.
    error_container <- try_fetch(abort("", parent = parent), error = identity)

    # Contains a newline at the start
    parent_text <- cnd_message(error_container)

    condition_text <- paste0(condition_text, parent_text)
  }

  if (!is.null(x)) {
    formatted <- if (inherits(x, "selenider_element")) {
      format_lazy_selenider_element(x)
    } else {
      format_lazy_selenider_elements(x)
    }
    object_text <- paste0(
      "Where `", x_name, "` is:\n",
      paste(formatted, collapse = "\n"),
      "\n"
    )

    condition_text <- paste0(condition_text, "\n\n", object_text, "\n")
  }

  testthat::fail(condition_text, trace_env = call)
}
