stop_absent_element <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c("{.arg x} does not exist in the DOM."),
    class = c(
      "selenider_error_absent_element",
      "expect_error_continue"
    ),
    call = call
  )
}

stop_absent_parent_element <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c("{.arg x}'s parent element does not exist in the DOM."),
    class = c(
      "selenider_error_absent_parent",
      "selenider_error_absent_element",
      "expect_error_continue"
    ),
    call = call
  )
}

stop_invisible_element <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c("{.arg x} is not visible."),
    class = c(
      "selenider_error_invisible_element",
      "expect_error_continue"
    ),
    call = call
  )
}

stop_expect_error <- function(condition, parent, call, env = rlang::caller_env()) {
  cli::cli_abort(
    condition,
    class = c("selenider_expect_error", "expect_error_continue"),
    parent = parent,
    call = call,
    .envir = env
  )
}

stop_bad_selector <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "No arguments specified to select elements with",
    "i" = "Use `css = '*'` to select all elements"
  ), class = "selenider_error_bad_selector", call = call)
}

stop_not_actionable <- function(x, call = rlang::caller_env(), class = c(), env = rlang::caller_env()) {
  class <- c("selenider_error_not_actionable", "expect_error_continue", class)

  cli::cli_abort(
    x,
    class = class,
    call = call,
    .envir = env
  )
}

stop_subscript_type <- function(i, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "x" = "{.arg i} must be a vector, not {.obj_type_friendly {i}}."
    ),
    class = c("selenider_error_subscript", "selenider_error_subscript_type"),
    call = call
  )
}

stop_subscript_length <- function(i, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "x" = "{.arg i} must have length 1, not {length(i)}."
    ),
    class = c("selenider_error_subscript", "selenider_error_subscript_length"),
    call = call
  )
}

stop_subscript_na <- function(i, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "x" = "{.arg i} must be an integer, not `NA`."
    ),
    class = c("selenider_error_subscript", "selenider_error_subscript_na"),
    call = call
  )
}

stop_subscript_0 <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "x" = "{.arg i} must not be 0."
    ),
    class = c("selenider_error_subscript", "selenider_error_subscript_zero"),
    call = call
  )
}

stop_subscript_max_length <- function(i, max_length, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "Attempt to select the {ordinal(i)} element of {.arg x}.",
      "{.arg x} has a known maximum length of {.arg {max_length}}."
    ),
    class = c(
      "selenider_error_subscript",
      "selenider_error_subscript_max_length"
    ),
    call = call
  )
}

warn_subscript_max_length <- function(i, max_length, call = rlang::caller_env()) {
  cli::cli_warn(c(
    "Invalid subscript {.arg i}.",
    "Attempt to select the {ordinal(i)} element of {.arg x}.",
    "{.arg x} has a known maximum length of {.arg {max_length}}."
  ), class = "selenider_warning_subscript_max_length", call = call)
}


stop_no_conditions <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "No conditions were specified.",
    "i" = "Try specifying a condition.",
    "x" = "Instead of: {.code elem_expect(element)}",
    "v" = "Try: {.code elem_expect(element, is_present)}"
  ), class = "selenider_error_no_conditions", call = call)
}

stop_condition_exists <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.code exists} is not a selenider condition.",
    "i" = "Did you mean {.code is_present} or {.code is_in_dom}?"
  ), class = "selenider_error_base_exists", call = call)
}

stop_default_browser <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Could not find default browser.",
      "Try creating the session manually using {.fun selenider::selenider_session} and specifying {.arg browser}"
    ),
    class = c(
      "selenider_error_default_browser",
      "selenider_error_create_session_browser"
    ),
    call = call
  )
}

stop_no_dependencies <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "One of {.pkg chromote} or {.pkg RSelenium} must be installed to use {.pkg selenider}."
  ), class = "selenider_error_dependencies", call = call)
}

stop_selenium_server <- function(error, licence = NULL, call = rlang::caller_env()) {
  if (!is.null(licence)) {
    cli::cli_abort(c(
      "The server of the session could not be started.",
      "i" = "Try deleting the following directory:",
      " " = "{.file {licence}}"
    ), class = "selenider_error_selenium_server", parent = error)
  } else {
    cli::cli_abort(c(
      "The server of the session could not be started."
    ), class = "selenider_error_selenium_server", parent = error)
  }
}

stop_connect_selenium_server <- function(count, error = NULL, res = NULL, driver = NULL, call = rlang::caller_env()) {
  if (!is.null(error)) {
    cli::cli_abort(c(
      "We could not determine whether the server was successfully started after {count} attempts."
    ), class = "selenider_error_server_started", parent = error, call = call, driver = driver)
  } else if (is.list(res) && length(res) == 0) {
    cli::cli_abort(c(
      "We could not determine whether the server was successfully started after {count} attempts.",
      "{.code driver$getStatus()} is an empty list."
    ), class = "selenider_error_server_started", call = call, driver = driver)
  } else {
    cli::cli_abort(c(
      "We could not determine whether the server was successfully started after {count} attempts."
    ), class = "selenider_error_server_started", call = call, driver = driver)
  }
}

stop_selenium_client <- function(error, browser, driver, call = rlang::caller_env()) {
  cli::cli_abort(
    "The client of the session ({tools::toTitleCase(browser)}) failed to start.",
    class = "selenider_error_selenium_client",
    parent = error,
    call = call,
    driver = driver
  )
}

stop_invalid_driver <- function(x, is_list = FALSE, call = rlang::caller_env()) {
  if (is_list) {
    cli::cli_abort(
      c("{.arg driver} is a list, but seems to contain neither a Selenium client, nor a Selenium server."),
      class = "selenider_error_invalid_driver",
      call = call
    )
  } else {
    cls <- c("ChromoteSession", "AppDriver", "remoteDriver")
    what <- cli::format_inline("A {.cls {cls}} object, a list or an environment")
    stop_input_type(x, what, call = call, class = "selenider_error_invalid_driver")
  }
}

stop_close_session <- function(error, call = rlang::caller_env()) {
  cli::cli_abort(
    "Could not close session",
    class = "selenider_error_close_session",
    parent = error,
    call = call
  )
}

stop_find_empty_elements <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "Cannot extract an element from {.arg x}.",
    "{.arg x} contains 0 elements."
  ), class = c("selenider_error_empty_elements", "selenider_error_empty_elements_find"), call = call)
}

stop_incompatible_drivers <- function(ids, call = rlang::caller_env()) {
  index <- which.max(ids == unique(ids)[2])
  cli::cli_abort(
    c(
      "Cannot combine elements with different drivers.",
      "x" = "Element number {index}'s driver is incompatible with the rest."
    ),
    class = "selenider_error_incompatible_drivers",
    call = call
  )
}

stop_dots_empty <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "`...` is empty.",
    "i" = "Supply one or more arguments to combine into an element collection."
  ), class = "selenider_error_dots_empty", call = call)
}

stop_flatten_dots <- function(x, exprs, i, index, is_nested, call = rlang::caller_env()) {
  accepted_classes <- c("selenider_element", "selenider_elements")

  if (!is_nested) {
    expr <- exprs[[i]]
    cli::cli_abort(c(
      "Every arguments in `...` must be a {.cls {accepted_classes}} object or a list of such objects, not {.obj_type_friendly {x}}.",
      "x" = "Problematic argument ({.val {i}}):",
      "i" = "`{exprs[[i]]}`"
    ), class = "selenider_error_flatten_dots", call = call)
  } else {
    cli::cli_abort(c(
      "Every arguments in `...` must be a {.cls {accepted_classes}} object or a list of such objects.",
      "x" = "Argument {.val {index}} was a list, but contained {.obj_type_friendly {x}} as its {ordinal(i)} element.",
      "x" = "Problematic argument:",
      "i" = "`{exprs[[i]]}`"
    ), class = "selenider_error_flatten_dots", call = call)
  }
}

stop_flatmap_return_value <- function(x, error = FALSE, call = rlang::caller_env()) {
  if (error) {
    cli::cli_abort(c(
      "An error occurred while executing {.arg .f} on a mock element."
    ), class = "selenider_error_flatmap_result", call = call, parent = x)
  } else {
    cli::cli_abort(c(
      "{.arg .f} must return a {.cls {c('selenider_element', 'selenider_elements')}} object.",
      "When executed on a mock element, {.arg .f} returned {.obj_type_friendly {x}}."
    ), class = "selenider_error_flatmap_result", call = call)
  }
}

stop_invalid_keys <- function(key, i, expr, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "Every arguments in `...` must be a string or a {.cls selenider_key} object, not {.obj_type_friendly {key}}.",
    "x" = "Problematic argument ({.var {i}}):",
    "i" = "`{expr}`"
  ), class = "selenider_error_invalid_keys", call = call)
}

stop_js_error <- function(x, call = rlang::caller_env()) {
  cli::cli_abort(c(
    "JavaScript function returned the following error:",
    "i" = "{.val {x}}"
  ), class = "selenider_error_js", call = call)
}

stop_null_screenshot_file <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg file} can only be {.val {NULL}} if {.arg view} is {.val {TRUE}}.",
    "i" = "If you want to view a screenshot without saving it, use {.code take_screenshot(view = TRUE)}."
  ), class = "selenider_error_screenshot_file", call = call)
}

warn_default_port <- function(call = rlang::caller_env()) {
  cli::cli_warn(c(
    "Could not find port number from server object.",
    "i" = "Using default port {.val {4567L}}"
  ), class = "selenider_warning_default_port", call = call)
}

warn_history_page_not_found <- function(next_page = TRUE, call = rlang::caller_env()) {
  if (next_page) {
    cli::cli_warn("Next page in history not found", class = "selenider_warning_history_page", call = call)
  } else {
    cli::cli_warn("Next page in history not found", class = "selenider_warning_history_page", call = call)
  }
}

warn_browser_chromote <- function(call = rlang::caller_env()) {
  cli::cli_warn(c(
    "Ignoring {.arg browser}.",
    "{.pkg chromote} only supports {.val chrome}."
  ), class = "selenider_warning_ignore_browser", call = call)
}
