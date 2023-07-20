#' Click an element
#' 
#' @description
#' Clicks on an HTML element, either by simulating a mouse click or by triggering 
#' the element's "click" event.
#' 
#' `click()` left clicks on the element, `double_click()` left clicks on the
#' element two times in a short period of time, while `right_click()` right
#' clicks on an element, opening its context menu.
#' 
#' @param x A `selenider_element` object.
#' @param js Whether to click the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#' 
#' @returns `x`, invisibly
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   click()
#'
#' @family actions
#'
#' @export
click <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

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

    execute_js_fn("x => x.click()", element, x$driver)
  } else  {
    element <- get_element_for_action(
      x,
      action = "click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    if (uses_selenium(x$driver)) {
      left_click_selenium(element, x)
    } else {
      click_chromote(element, x$driver)
    }
  }

  invisible(x)
}

left_click_selenium <- function(element, x) {
  if (x$driver$browserName == "internet explorer" && identical(element$getElementTagName(), "checkbox")) {
    try({
      x$driver$executeScript(
        "arguments[0].focus();",
        list(element$findChildElement("tag", "input"))
      )

      element$findChildElement("tag", "input")$clickElement()
      return()
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

click_chromote <- function(element, driver, type = "left", count = 1) {
  chromote_scroll_into_view_if_needed(element, driver)

  coords <- chromote_get_xy(element, driver)
  x <- coords$x
  y <- coords$y

  capture_mouse_press_error(driver$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = type, clickCount = count, timeout_ = 0.1))
  capture_mouse_press_error(driver$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = type, clickCount = 0, timeout_ = 0.1))
}

# There is a chrome bug where the mouse event is left hanging when the viewer is enabled.
# This forces the event to timeout prematurely, and catches the error.
capture_mouse_press_error <- function(expr) {
  rlang::try_fetch(
    expr,
    error = function(e) {
      if (grepl("timed out waiting for response", e$message, fixed = TRUE)) {
        NULL
      } else {
        rlang::zap()
      }
    }
  )
}

#' @rdname click
#' 
#' @export
double_click <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)
  
  if (js) {
    element <- get_element_for_action(
      x,
      action = "double click {.arg x} using javascript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )
    
    execute_js_fn("function(x) {
      x.click(); 
      x.click();
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('dblclick', true, true);
      x.dispatchevent(clickevent);
    }", element, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "double click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )
    
    if (uses_selenium(x$driver)) {
      size <- element$getElementSize()

      x$driver$mouseMoveToLocation(
        x = round(size$width / 3),
        y = round(size$height / 3),
        webelement = element
      )
      
      x$driver$doubleclick()
    } else {
      click_chromote()
    }
  }
  
  invisible(x)
}

#' @rdname click
#' 
#' @export
right_click <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)
  
  if (js) {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x} using javascript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )
    
    execute_js_fn("function(element) {
      element.click(); 
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('contextmenu', true, true);
      element.dispatchevent(clickevent);
    }", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    if (uses_selenium(x$driver)) {
      size <- element$getElementSize()

      x$driver$mouseMoveToLocation(
        x = round(size$width / 3),
        y = round(size$height / 3),
        webelement = element
      )
      
      x$driver$click(2)
    } else {
      click_chromote(element, x$driver, type = "right")
    }
  }
  
  invisible(x)
}

#' Hover over an element
#' 
#' Move the mouse over to an HTML element and hover over it, without actually
#' clicking or interacting with it.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to hover the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly
#'
#' @family actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   hover()
#' 
#' @export
hover <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  if (js) {
    element <- get_element_for_action(
      x,
      action = "hover over {.arg x} using javascript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    execute_js_fn("function(element) {
      element.moveToElement(element);
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('mouseover', true, true);
      element.dispatchevent(clickevent);
    }", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "hover over {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )
    
    if (uses_selenium(x$driver)) {
      size <- element$getElementSize()

      x$driver$mouseMoveToLocation(
        x = round(size$width / 3),
        y = round(size$height / 3),
        webelement = element
      )
    } else {
      hover_chromote(element, x$driver)
    }
  }

  invisible(x)
}

hover_chromote <- function(element, driver) {
  chromote_scroll_into_view_if_needed(element, driver)

  coords <- chromote_get_xy(element, driver)
  x <- coords$x
  y <- coords$y

  driver$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
}

#' Set the value of an input
#'
#' `set_value()` sets the value of an HTML input element to a string.
#'
#' `send_keys()` sends a set of inputs to an element.
#'
#' `clear_value()` sets the value of an HTML element to `""`, removing any
#' existing content.
#'
#' @param x A `selenider_element` object.
#' @param text A string to set the input 
#' @param timeout How long to wait for the element to exist.
#' 
#' @returns `x`, invisibly
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   set_value("my text")
#'
#' @family actions
#'
#' @export
set_value <- function(x, text, timeout = NULL) {
  check_class(x, "selenider_element")
  check_string(text)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "set the value of {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )

  execute_js_fn(
    paste0("x => x.setAttribute('value','", text, "')"),
    element, x$driver
  )
  
  if (uses_selenium(x$driver)) {
    element$clearElement()
    element$sendKeysToElement(list(text))
  } else {
    chromote_clear(element, x$driver)
    chromote_send_chars(element, x$driver)
  }
}

chromote_clear <- function(x, driver) {
  click_chromote(x, driver)

  chromote_press(modifiers = 2, text = "a", unmodifiedText = "a", key = "a", code = "KeyA", windowsVirtualKeyCode = 65)
  chromote_press(type = "keyDown", windowsVirtualKeyCode = 8, code = "Backspace", key = "Backspace")
}

chromote_send_chars <- function(x, driver) {
  chars <- strsplit(x, split = NULL)[[1]]

  for (char in chars) {
    driver$Input$dispatchKeyEvent(type = "char", text = char)
  }
}

#' @rdname set_value
#'
#' @param ... A set of inputs to send to `x`.
#' @param modifiers A character vector; one or more of "shift", "ctrl", "alt", and "command"/meta".
#' 
#' @export
send_keys <- function(x, ..., modifiers = NULL, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)
  check_dots_unnamed()

  exprs <- enexprs(...)
  keys <- list2(...)

  for (i in seq_along(keys)) {
    key <- keys[[i]]

    if (!is_string(key) && !inherits(key, "selenider_key")) {
      expr <- exprs[[i]]
      cli::cli_abort(c(
        "Every arguments in `...` must be a string or a {.cls selenider_key} object, not {.obj_type_friendly {key}}.",
        "x" = "Problematic argument ({.var {i}}):",
        "i" = "`{expr}`"
      ))
    }
  }

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "send keys to {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )

  if (uses_selenium(x$driver)) {
    element$sendKeysToElement(keys)
  } else {
    chromote_send_keys(element, x$driver, keys, modifiers)
  }

  invisible(x)
}

chromote_send_keys <- function(element, driver, keys, modifiers) {
  keys <- format_keys(keys)
  modifier_number <- get_numeric_modifier(modifiers)

  for (key in keys) {
    rlang::exec(chromote_press, !!!key, modifiers = modifiers)
  }
}

format_keys <- function(keys, modifiers) {
  # Don't use `text` param if we have a modifier that is not shift
  no_text <- length(modifiers[[vapply(modifiers, function(x) identical(x, "shift"), FUN.VALUE = logical(1))]]) > 0

  result <- list()
  for (key in keys) {
    if (inherits(key, "selenider_key")) {
      key <- get_chromote_key(key)
      result <- append(result, list(key))
    } else {
      chars <- strsplit(key, split = NULL)[[1]]

      for (char in chars) {
        key <- identify_chromote_key(char)
        if (!no_text) {
          key$text <- char
        }

        result <- append(result, list(key))
      }
    }
  }

  result
}

# CDP implements the modifier argument as a bitmask
# So alt is 1, ctrl is 2, both is 3, etc.
get_numeric_modifier <- function(modifiers) {
  if (length(modifiers) == 0) {
    return(0)
  }
  
  modifiers <- vapply(modifiers, tolower, FUN.VALUE = character(1))

  1 * ("alt" %in% modifiers) + 2 * ("ctrl" %in% modifiers) +
    4 * ("meta" %in% modifiers || "command" %in% modifiers) +
    8 * ("shift" %in% modifiers)
}

#' @rdname set_value
#'
#' @export
clear_value <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "send keys to {.arg x}",
    conditions = list(is_enabled),
    timeout = timeout,
    failure_messages = c("was not enabled"),
    conditions_text = c("be enabled")
  )
  
  if (uses_selenium(x$driver)) {
    element$clearElement()
  } else {
    chromote_clear(element, x$driver)
  }

  invisible(x)
}

get_element_for_action <- function(x,
                                   action,
                                   conditions,
                                   timeout,
                                   failure_messages,
                                   conditions_text,
                                   call = rlang::caller_env()) {
  meets_condition <- 
    inject(html_wait_until(
      x,
      is_present,
      !!!conditions,
      timeout = timeout
    ))
  if (length(conditions) != 0 && !meets_condition) {
    if (length(conditions) == 0 || !is_present(x)) {
      stop_not_actionable(c(
        paste0("To ", action, ", it must exist"),
        "i" = "After {timeout} seconds, {.arg x} was not present"
      ), call = call, exists = TRUE)
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

#' Scroll to an element
#'
#' Scrolls to an HTML element.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to scroll to the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly
#'
#' @family actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   scroll_to()
#'
#' @export
scroll_to <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  if (js) {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x} using JavaScript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    x$driver$executeScript("
      element = arguments[0];
      element.scrollIntoView();
    ", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    size <- element$getElementSize()

    x$driver$mouseMoveToLocation(
      webelement = element
    )
  }

  invisible(x)
}

#' Submit an element
#'
#' If an element is an ancestor of a form, submits the form.
#' Works by walking up the DOM, checking each ancestor element until
#' the element is a <form> element, which it then submits. If such
#' an element does not exist, an error is thrown.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to submit the form using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly
#'
#' @family actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   submit()
#'
#' @export
submit <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  if (js) {
    element <- get_element_for_action(
      x,
      action = "submit {.arg x} using JavaScript",
      conditions = list(),
      timeout = timeout,
      failure_messages = c(),
      conditions_text = c()
    )

    result <- x$driver$executeScript("
      element = arguments[0];
      while element != null {
        if element.tagName == 'form' {
          element.submit();
          return true;
        }

        element = element.parentElement;
      }
      return false;
    ", list(element))

    if (!result) {
      cli::cli_abort(c(
        "To submit {.arg x}, it must be the descendant of a <form> element"
      ))
      # TODO: Add implicit waiting to check for a <form> parent.
    }
  } else {
    element <- get_element_for_action(
      x,
      action = "submit {.arg x}",
      conditions = list(),
      timeout = timeout,
      failure_messages = c(),
      conditions_text = c()
    )

    element$submitElement()
  }

  invisible(x)
}