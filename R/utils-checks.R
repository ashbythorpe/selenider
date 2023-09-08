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

is_selenium_server <- function(x) {
  is.list(x) && all(c("process", "log", "stop") %in% names(x))
}

check_selenium_server <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_server(x)) {
    cli::cli_abort(c(
      "{.code driver$server} must be a valid Selenium server object",
      "i" = "This can be the result of {.fun selenider::create_selenium_server} or {.fun wdman::selenium}."
    ), class = "selenider_error_invalid_server", call = call)
  }
  x
}

is_selenium_client <- function(x) {
  inherits(x, "remoteDriver")
}

check_selenium_client <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_client(x)) {
    cli::cli_abort(c(
      "{.code driver$client} must be a {.cls remoteDriver} object",
      "i" = "This can be the result of {.fun selenider::create_selenium_client} or {.fun RSelenium::rsDriver}."
    ), class = "selenider_error_invalid_client", call = call)
  }
  x
}
