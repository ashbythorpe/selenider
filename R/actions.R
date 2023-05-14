#' Perform actions on a selenider element.
#'
#' @description
#' These functions define various actions to be performed on a selenider
#' element.
#'
#' `click()` clicks the element.
#'
#' `send_keys()` sends a set of keys to an input element.
#'
#' `clear_element()` clears the value of an input element.
#'
#' `set_input()` sets the value of an input to a specified string.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to cick the element using JavaScript.
#' @param ... The keys to send to the element.
#' @param text The text to set the input to.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @examples
#' \dontshow{
#' # This allows `local_session()` to work when being sourced.
#' prev_options <- options(withr.hook_source = TRUE)
#' }
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   click()
#' \dontshow{
#' options(prev_options)
#' }
#'
#' @name html-actions
NULL

#' @rdname html-actions
#'
#' @export
click <- function(x, js = FALSE, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)
  
  if (js) {
    element <- get_element_for_action(
      x,
      action = "click {.arg x} using JavaScript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    x$driver$execute_script("arguments[0].click();", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )
    
    if (identical(element$getElementTagName(), "checkbox")) {
      try({
        x$driver$executeScript(
          "arguments[0].focus();",
          list(element$findChildElement("tag", "input"))
        )

        element$findChildElement("tag", "input")$clickElement()
        return(invisible(x))
      }, silent = TRUE)
    } 

    if (x$driver$browserName == "internet explorer") {
      size <- element$getElementSize()

      x$driver$mouseMoveToLocation(
        x = round(size$width / 3),
        y = round(size$height / 3),
        webElement = element
      )
    }

    element$clickElement()
  }

  invisible(x)
}

#' @rdname html-actions
#'
#' @export
send_keys <- function(x, ..., timeout = NULL) {
  keys <- rlang::list2(...)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "send keys to {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )

  element$sendKeysToElement(keys)

  invisible(x)
}

#' @rdname html-actions
#'
#' @export
clear_input <- function(x, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "send keys to {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )

  element$clearElement()

  invisible(x)
}

#' @rdname html-actions
#'
#' @export
set_input <- function(x, text, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "set the value of {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )

  x$driver$executeScript(
    paste0("arguments[0].setAttribute('value','", text, "');"),
    list(element)
  )

  element$clearElement()
  element$sendKeysToElement(list(text))
}

get_element_for_action <- function(x, action, conditions, timeout, failure_messages, conditions_text, call = rlang::caller_env()) {
  meets_condition <- if (length(conditions) != 0) {
    rlang::inject(html_wait_until(x, exists, !!!conditions, timeout = timeout))
  } else {
    html_wait_until(x, exists, timeout = timeout)
  }

  if (!meets_condition) {
    if (length(conditions) == 0 || !exists(x)) {
      stop_not_actionable(c(
        paste0("To ", action, ", it must exist"),
        "i" = "After {timeout} seconds, {.arg x} was not present"
      ), call = call)
    }

    for (n in seq_along(conditions)) {
      condition <- conditions[[n]]
      
      if (!condition(x)) {
        stop_not_actionable(c(
          paste0("To ", action, ", it must ", conditions_text),
          "i" = "After {timeout} seconds, {.arg x} {failure_messages[[n]]}"
        ), call = call)
      }
    }
  }

  element <- get_element(x)

  if (is.null(element)) {
    stop_not_actionable(c(
      paste0("To ", action, ", it must exist"),
      "i" = "After {timeout} seconds, {.arg x} was not present"
    ))
  }

  element
}
