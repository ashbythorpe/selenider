stop_absent_element <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.arg x} does not exist in the DOM."
  ), class = "selenider_error_absent_element", call = call)
}

stop_absent_parent_element <- function(call = rlang::caller_env()) {
  cli::cli_abort(
    c("{.arg x}'s parent element does not exist in the DOM."), 
    class = c(
      "selenider_error_absent_parent_element",
      "selenider_error_absent_element"
    ),
    call = call
  )
}

stop_expect_error <- function(condition, parent, call, env = rlang::caller_env()) {
  cli::cli_abort(
    condition, 
    class = "selenider_expect_error", 
    parent = parent, 
    call = call,
    .envir = env
  )
}

stop_bad_selector <- function() {
  cli::cli_abort(c(
    "No arguments specified to select elements with",
    "i" = "Use `css = '*'` to select all elements"
  ), class = "selenider_error_bad_selector")
}

stop_not_actionable <- function(x, call, exists = FALSE, env = rlang::caller_env()) {
  class <- "selenider_error_not_actionable"

  if (exists) {
    class <- c(class, "selenider_error_absent_element")
  }

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

stop_subscript_max_length <- function(i, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Invalid subscript {.arg i}.",
      "Attempt to select the {ordinal(max_sub)} element of {.arg x}.",
      "{.arg x} has a known maximum length of {.arg min_length}."
    ), 
    class = c(
      "selenider_error_subscript",
      "selenider_error_subscript_max_length"
    ),
    call = call
  )
}


stop_no_conditions <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "No conditions were specified",
    "i" = "Try specifying a condition",
    "x" = "Instead of: {.code html_expect(element)}",
    "v" = "Try: {.code html_expect(element, is_present)}"
  ), class = "selenider_error_no_conditions", call = call)
}

stop_condition_exists <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "{.code exists} is not a selenider condition.",
    "i" = "Did you mean {.code is_present} or {.code is_in_dom}?"
  ), class = "selenider_error_base_exists", call = call)
}

stop_default_browser <- function(call = rlang::caller_env()) {
  cli::cli_abort(c(
    "Could not find default browser.",
    "i" = "Try specifying the browser specifically."
  ), class = "selenider_error_default_browser", call = call)
}

stop_create_session_browser <- function(call = rlang::caller_env()) {
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
