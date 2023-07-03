check_class <- function(x, cls, ..., allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null) {
    if (!is.null(x) && !inherits(x, cls)) {
      what <- cli::format_inline("a {.cls remoteDriver} object or `NULL`")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  } else {
    if (!inherits(x, cls)) {
      what <- cli::format_inline("a {.cls remoteDriver} object")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  }
}
