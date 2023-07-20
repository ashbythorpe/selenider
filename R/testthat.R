# This function must *only* be called if we know testthat is installed
html_expect_fail <- function(condition, parent, call, x, x_name, env = rlang::caller_env()) {
  condition_text <- cli::format_error(condition, .envir = env)

  if (!is.null(parent)) {
    error_container <- try_fetch(abort("", parent = parent), error = identity)
    parent_text <- cnd_message(error_container) # Contains a newline at the start

    condition_text <- paste0(condition_text, parent_text)
  }

  if (!is.null(x)) {
    object_text <- paste0(
      "Where `", x_name, "` is:\n",
      paste(format(x), collapse = "\n"),
      "\n"
    )
    
    condition_text <- paste0(condition_text, "\n\n", object_text, "\n")
  }

  testthat::fail(condition_text, trace_env = call)
}
