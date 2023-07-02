# This function must *only* be called if we know testthat is installed
html_expect_fail <- function(condition, parent, call, x, x_name, env = rlang::caller_env()) {
  condition_text <- cli::format_error(condition, .envir = env)

  if (!is.null(parent)) {
    error_container <- try_fetch(abort("", parent = parent), error = identity)
    parent_text <- cnd_message(error_container) # Contains a newline at the start

    condition_text <- paste0(condition_text, parent_text)
  }

  if (!is.null(x)) {
    object_text <- cli::cli_fmt({
      cli::cli_text("Where {.arg {x_name}} is:")
      print(x)
    })

    condition_text <- paste0(condition_text, "\n\n", object_text)
  }

  testthat::fail(condition_text, trace_env = call)
}
