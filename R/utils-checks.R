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

check_driver <- function(x, ..., allow_null = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (allow_null) {
    if (!is.null(x) && !is_valid_driver(x)) {
      cli::cli_abort(c(
        "{.arg {arg}} must be a valid driver object or `NULL`, not {.obj_type_friendly {x}}.",
        "i" = "A valid driver object is the result of {.fn RSelenium::rsDriver()}, or a {.cls c('AppDriver', 'ChromoteSession')} object."
      ))
    }
  } else {
    if (!is_valid_driver(x)) {
      cli::cli_abort(c(
        "{.arg {arg}} must be a valid driver object, not {.obj_type_friendly {x}}.",
        "i" = "A valid driver object is the result of {.fn RSelenium::rsDriver()}, or a {.cls c('AppDriver', 'ChromoteSession')} object."
      ))
    }
  }
}

is_valid_driver <- function(x) {
  if (inherits_any(x, c("AppDriver", "ChromoteSession"))) {
    TRUE
  } else {
    is.list(x) && inherits(x$client, "remoteDriver") && is.list(x$server) &&
      setequal(names(x$server), c("process", "output", "error", "stop", "log"))
  }
}
