check_class <- function(x, cls, ..., allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null) {
    if (!is.null(x) && !inherits_any(x, cls)) {
      what <- cli::format_inline("a {.cls {cls}} object or `NULL`")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  } else {
    if (!inherits_any(x, cls)) {
      what <- cli::format_inline("a {.cls {cls}} object")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  }
}
