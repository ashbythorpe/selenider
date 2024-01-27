#' Click an element
#'
#' @description
#' Clicks on an HTML element, either by simulating a mouse click or by
#' triggering the element's "click" event.
#'
#' `elem_click()` left clicks on the element, `elem_double_click()` left clicks
#' on the element two times in a short period of time, while
#' `elem_right_click()` right clicks on an element, opening its context menu.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to click the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button onclick = hidetext() oncontextmenu = showtext()></button>
#' <p id = 'texttohide'>Hello!</p>
#' "
#'
#' js <- "
#' function hidetext() {
#'   document.getElementById('texttohide').style.display = 'none'
#' }
#'
#' function showtext() {
#'   document.getElementById('texttohide').style.display = 'block'
#' }
#' "
#'
#' session <- minimal_selenider_session(html, js = js)
#'
#' elem_expect(s("p"), is_visible)
#'
#' s("button") |>
#'   elem_click()
#'
#' elem_expect(s("p"), is_invisible)
#'
#' s("button") |>
#'   elem_right_click()
#'
#' elem_expect(s("p"), is_visible)
#'
#' @family actions
#'
#' @export
elem_click <- function(x, js = FALSE, timeout = NULL) {
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

    element_click_js(element, x$session, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    element_click(element, x$session, x$driver)
  }

  invisible(x)
}

element_click_js <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    if (window.MouseEvent) {
      const ev1 = new MouseEvent('mousedown', {
          bubbles: true,
          cancelable: false,
          view: window,
          clientX: x.getBoundingClientRect().x,
          clientY: x.getBoundingClientRect().y
      });
      x.dispatchEvent(ev1);
      const ev2 = new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: false,
          view: window,
          clientX: x.getBoundingClientRect().x,
          clientY: x.getBoundingClientRect().y
      });
      x.dispatchEvent(ev2);
    } else {
      x.click();
      const event = document.createEvent('MouseEvent');
      event.initMouseEvent(
        'click',
        true,
        true,
        window,
        0,
        0,
        0,
        0,
        0,
        false,
        false,
        false,
        false,
        0,
        null
      );
      x.dispatchEvent(event);
    }
  }", x, session = session, driver = driver)
}

element_click <- function(x, session, driver) {
  if (session == "chromote") {
    promise <- driver$Page$loadEventFired(wait_ = FALSE)$catch(function(e) NULL)
    click_chromote(x, driver = driver)
    name <- driver$DOM$describeNode(backendNodeId = x)$node$nodeName
    if (identical(name, "A")) {
      driver$wait_for(promise)
    }
  } else if (session == "selenium") {
    x$click()
  } else {
    left_click_rselenium(x, driver)
  }
}

left_click_rselenium <- function(element, driver) {
  if (driver$browserName == "internet explorer" &&
        identical(element$getElementTagName(), "checkbox")) {
    try(
      {
        driver$executeScript(
          "arguments[0].focus();",
          list(element$findChildElement("tag", "input"))
        )

        element$findChildElement("tag", "input")$clickElement()
        return()
      },
      silent = TRUE
    )
  }

  if (driver$browserName == "internet explorer") {
    size <- element$getElementSize()

    driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webElement = element
    )
  }

  element$clickElement()
}

click_chromote <- function(element, driver, type = "left", count = 1) {
  chromote_scroll_into_view_if_needed(backend_id = element, driver = driver)

  coords <- chromote_clickable_point(backend_id = element, driver = driver)
  x <- coords$x
  y <- coords$y

  capture_mouse_press_error(
    driver$Input$dispatchMouseEvent(
      type = "mousePressed",
      x = x,
      y = y,
      button = type,
      clickCount = count,
      timeout_ = 0.1
    )
  )
  capture_mouse_press_error(
    driver$Input$dispatchMouseEvent(
      type = "mouseReleased",
      x = x,
      y = y,
      button = type,
      clickCount = 0,
      timeout_ = 0.1
    )
  )
}

# There is a chrome bug where the mouse event is left hanging when the viewer
# is enabled. This forces the event to timeout prematurely, and catches the
# error.
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

#' @rdname elem_click
#'
#' @export
elem_double_click <- function(x, js = FALSE, timeout = NULL) {
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

    element_double_click_js(element, x$session, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "double click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    element_double_click(element, x$session, x$driver)
  }

  invisible(x)
}

element_double_click_js <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    if (window.MouseEvent) {
      const event = new MouseEvent('dblclick');
      x.dispatchEvent(event)
    } else {
      const event = document.createEvent('mouseevents');
      event.initEvent('dblclick', true, true);
      x.dispatchEvent(event);
    }
  }", x, session = session, driver = driver)
}

element_double_click <- function(x, session, driver) {
  if (session == "chromote") {
    click_chromote(x, driver, type = "left", count = 2)
  } else if (session == "selenium") {
    actions <- selenium::actions_stream(
      selenium::actions_mousemove(
        x = 0,
        y = 0,
        origin = x
      ),
      selenium::actions_mousedown(),
      selenium::actions_mouseup(),
      selenium::actions_pause(0.1),
      selenium::actions_mousedown(),
      selenium::actions_mouseup()
    )

    driver$perform_actions(actions)
  } else {
    size <- x$getElementSize()

    driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webElement = x
    )

    driver$doubleclick()
  }
}

#' @rdname elem_click
#'
#' @export
elem_right_click <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  if (js || x$session == "rselenium") {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x} using javascript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    element_right_click_js(element, x$session, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    element_right_click(element, x$session, x$driver)
  }

  invisible(x)
}

element_right_click_js <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    if (window.MouseEvent) {
      const ev1 = new MouseEvent('mousedown', {
          bubbles: true,
          cancelable: false,
          view: window,
          button: 2,
          buttons: 2,
          clientX: x.getBoundingClientRect().x,
          clientY: x.getBoundingClientRect().y
      });
      x.dispatchEvent(ev1);
      const ev2 = new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: false,
          view: window,
          button: 2,
          buttons: 0,
          clientX: x.getBoundingClientRect().x,
          clientY: x.getBoundingClientRect().y
      });
      x.dispatchEvent(ev2);
      const ev3 = new MouseEvent('contextmenu', {
          bubbles: true,
          cancelable: false,
          view: window,
          button: 2,
          buttons: 0,
          clientX: x.getBoundingClientRect().x,
          clientY: x.getBoundingClientRect().y
      });
      x.dispatchEvent(ev3);
    } else {
      x.click();
      const event = document.createEvent('mouseevents');
      event.initEvent('contextmenu', true, true);
      x.dispatchEvent(event);
    }
  }", x, session = session, driver = driver)
}

element_right_click <- function(x, session, driver) {
  if (session == "chromote") {
    click_chromote(x, driver, type = "right")
  } else {
    actions <- selenium::actions_stream(
      selenium::actions_mousemove(
        x = 0,
        y = 0,
        origin = x
      ),
      selenium::actions_mousedown(button = "right"),
      selenium::actions_mouseup(button = "right")
    )

    driver$perform_actions(actions)
  }
}

#' Hover over an element
#'
#' @description
#' `elem_hover()` moves the mouse over to an HTML element and hovers over it,
#' without actually clicking or interacting with it.
#'
#' `elem_focus()` focuses an HTML element.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to hover over the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @family actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button onmouseover = settext()></button>
#' <p class = 'text'></p>
#' "
#'
#' js <- "
#' function settext() {
#'   const element = document.getElementsByClassName('text').item(0);
#'
#'   element.innerHTML = 'Button hovered!';
#' }
#' "
#'
#' session <- minimal_selenider_session(html, js = js)
#'
#' elem_expect(s(".text"), has_exact_text(""))
#'
#' s("button") |>
#'   elem_hover()
#'
#' elem_expect(s(".text"), has_text("Button hovered!"))
#'
#' s("button") |>
#'   elem_focus()
#'
#' @export
elem_hover <- function(x, js = FALSE, timeout = NULL) {
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

    element_hover_js(element, x$session, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "hover over {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    element_hover(element, x$session, x$driver)
  }

  invisible(x)
}

element_hover_js <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    if (window.MouseEvent) {
      const event = new MouseEvent('mouseover');
      x.dispatchEvent(event)
    } else {
      x.click();
      x.click();
      const event = document.createEvent('mouseevents');
      event.initEvent('mouseover', true, true);
      x.dispatchEvent(event);
    }
  }", x, session = session, driver = driver)
}

element_hover <- function(x, session, driver) {
  if (session == "chromote") {
    hover_chromote(x, driver = driver)
  } else if (session == "selenium") {
    actions <- selenium::actions_stream(
      selenium::actions_mousemove(
        x = 0,
        y = 0,
        origin = x
      )
    )

    driver$perform_actions(actions)
  } else {
    size <- x$getElementSize()

    driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webElement = x
    )
  }
}

hover_chromote <- function(element, driver) {
  chromote_scroll_into_view_if_needed(backend_id = element, driver = driver)

  coords <- chromote_clickable_point(backend_id = element, driver = driver)
  x <- coords$x
  y <- coords$y

  driver$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = x,
    y = y,
    button = "middle",
  )
}

#' @rdname elem_hover
#'
#' @export
elem_focus <- function(x, timeout = NULL) {
  check_class(x, "selenider_element")
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_action(
    x,
    action = "focus {.arg x}",
    conditions = list(is_visible),
    timeout = timeout,
    failure_messages = c("was not visible"),
    conditions_text = c("be visible")
  )

  element_focus(element, x$session, x$driver)

  invisible(x)
}

element_focus <- function(x, session, driver) {
  if (session == "chromote") {
    chromote_focus(x, driver = driver)
  } else {
    execute_js_fn_on("x => x.focus()", x, session = session, driver = driver)
  }
}

chromote_focus <- function(x, driver) {
  driver$DOM$focus(backendNodeId = x)
}

#' Set the value of an input
#'
#' `elem_set_value()` sets the value of an HTML input element to a string.
#'
#' `elem_send_keys()` sends a set of inputs to an element.
#'
#' `elem_clear_value()` sets the value of an HTML element to `""`, removing any
#' existing content.
#'
#' @param x A `selenider_element` object. For [elem_send_keys()], this can be
#'   `NULL`, meaning that the keys will be sent to the current page (or the
#'   currently focused element) instead of a specific element.
#' @param text A string to set the value of the input element to.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <input
#'   type='text'
#'   oninput='recordChange(event)'
#'   onkeypress='return checkEnter(event);'
#' />
#' <p></p>
#' "
#'
#' js <- "
#' function recordChange(e) {
#'   document.getElementsByTagName('p').item(0).innerText = e.target.value;
#' }
#'
#' function checkEnter(e) {
#'   // If the key pressed was Enter
#'   if (e.keyCode == 13) {
#'     document.getElementsByTagName('p').item(0).innerText = 'Enter pressed!';
#'     return false;
#'   }
#'   return true;
#' }
#' "
#'
#' session <- minimal_selenider_session(html, js = js)
#'
#' elem_expect(s("p"), has_exact_text(""))
#'
#' input <- s("input")
#'
#' elem_set_value(input, "my text")
#'
#' elem_expect(s("p"), has_text("my text"))
#'
#' elem_clear_value(input)
#'
#' elem_expect(s("p"), has_exact_text(""))
#'
#' elem_send_keys(input, keys$enter)
#'
#' elem_expect(s("p"), has_text("Enter pressed!"))
#'
#' @family actions
#'
#' @export
elem_set_value <- function(x, text, timeout = NULL) {
  check_class(x, "selenider_element")
  if (is.numeric(text) && length(text) == 1) {
    text <- as.character(text)
  }
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

  element_set_value(element, text, x$session, x$driver)

  invisible(x)
}

element_input_type <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    if (x instanceof HTMLSelectElement) {
      return 'select';
    }

    if (x instanceof HTMLTextAreaElement) {
      return 'typeable-input';
    }

    if (x instanceof HTMLInputElement) {
      if (
        [
          'textarea',
          'text',
          'url',
          'tel',
          'search',
          'password',
          'number',
          'email',
        ].includes(x.type)
      ) {
        return 'typeable-input';
      } else {
        return 'other-input';
      }
    }

    if (x.isContentEditable) {
      return 'contenteditable';
    }

    return 'unknown';
  }", x, session = session, driver = driver)
}

element_set_value <- function(x, text, session, driver) {
  type <- element_input_type(x, session = session, driver = driver)

  if (type == "select") {
    element_select(
      x,
      elem_name = "select",
      value = text,
      text = NULL,
      index = NULL,
      reset_other = TRUE,
      session = session,
      driver = driver
    )
  } else if (type %in% c("typeable-input", "contenteditable")) {
    rest <- partially_type(x, text, session, driver)
    element_focus(x, session = session, driver = driver)

    if (session == "chromote") {
      chromote_send_chars(rest, driver = driver)
    } else if (session == "selenium") {
      x$send_keys(rest)
    } else {
      x$sendKeysToElement(list(rest))
    }
  } else {
    element_focus(x, session = session, driver = driver)

    execute_js_fn_on(paste0("function(x) {
      (x as HTMLInputElement).value = '", text, "';
      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));
    }"), x, session = session, driver = driver)
  }
}

partially_type <- function(x, text, session, driver) {
  execute_js_fn_on(paste0("function(x) {
    const text = '", text, "';
    const currentValue = x.isContentEditable ? x.innerText : x.value;

    if (text.length <= currentValue.length || !text.startsWith(x.value)) {
      if (x.isContentEditable) {
        x.innerText = '';
      } else {
        x.value = '';
      }
      return text;
    }

    const originalValue = x.isContentEditable ? x.innerText : x.value;

    if (x.isContentEditable) {
      x.innerText = '';
      x.innerText = originalValue;
    } else {
      x.value = '';
      x.value = originalValue;
    }

    return text.substring(originalValue.length);
  }"), x, session = session, driver = driver)
}

chromote_clear <- function(x,
                           driver) {
  if (is_mac()) {
    click_chromote(x, driver = driver, count = 3)
  } else {
    click_chromote(x, driver = driver)

    chromote_press(
      driver,
      modifiers = 2,
      key = "a",
      code = "KeyA",
      windowsVirtualKeyCode = 65
    )
  }

  chromote_press(
    driver,
    windowsVirtualKeyCode = 8,
    code = "Backspace",
    key = "Backspace"
  )
}

chromote_send_chars <- function(x, driver) {
  chars <- strsplit(x, split = NULL)[[1]]

  for (char in chars) {
    driver$Input$dispatchKeyEvent(type = "char", text = char)
  }
}

#' @rdname elem_set_value
#'
#' @param ... A set of inputs to send to `x`.
#' @param modifiers A character vector; one or more of "shift",
#'   "ctrl"/"control", "alt", and "command"/meta". Note that when using
#'   chromote as a backend, these do not work on Mac OS.
#'
#' @export
elem_send_keys <- function(x, ..., modifiers = NULL, timeout = NULL) {
  check_class(x, c("selenider_element", "selenider_session"), allow_null = TRUE)
  check_number_decimal(timeout, allow_null = TRUE)
  check_dots_unnamed()

  if (is.null(x)) {
    x <- get_session(.env = rlang::caller_env())
  }

  exprs <- enexprs(...)
  keys <- list2(...)

  for (i in seq_along(keys)) {
    key <- keys[[i]]

    if (!is_string(key) && !inherits(key, "selenider_key")) {
      expr <- exprs[[i]]
      stop_invalid_keys(key, i, expr)
    }
  }

  timeout <- get_timeout(timeout, x$timeout)

  element <- if (inherits(x, "selenider_element")) {
    get_element_for_action(
      x,
      action = "send keys to {.arg x}",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )
  } else {
    NULL
  }

  element_send_keys(element, modifiers, keys, x$session, x$driver)

  invisible(x)
}

element_send_keys <- function(x, modifiers, keys, session, driver) {
  if (session == "chromote") {
    chromote_send_keys(x, driver, keys, modifiers)
  } else if (session == "selenium") {
    rlang::check_installed("selenium")

    keys <- lapply(keys, function(x) {
      if (inherits(x, "selenider_key")) {
        get_selenium_key(x)
      } else {
        x
      }
    })

    if (inherits(x, "selenider_session")) {
      keys <- c(
        selenium::keys$shift["shift" %in% modifiers],
        selenium::keys$control[any(c("control", "ctrl") %in% modifiers)],
        selenium::keys$alt["alt" %in% modifiers],
        selenium::keys$command_meta[any(c("command", "meta") %in% modifiers)],
        keys
      )

      driver$send_keys(!!!keys)
    } else {
      modifiers <- c(
        selenium::keys$shift["shift" %in% modifiers],
        selenium::keys$control[any(c("control", "ctrl") %in% modifiers)],
        selenium::keys$alt["alt" %in% modifiers],
        selenium::keys$command_meta[any(c("command", "meta") %in% modifiers)]
      )

      keys <- unlist(lapply(keys, function(x) strsplit(x, split = NULL)[[1]]))

      actions <- vector("list", length = (length(keys) + length(modifiers)) * 2)
      index <- 1

      for (modifier in modifiers) {
        actions[[index]] <- selenium::actions_press(modifier)
        actions[[length(actions) - index + 1]] <-
          selenium::actions_release(modifier)
        index <- index + 1
      }

      for (key in keys) {
        actions[[index]] <- selenium::actions_press(key)
        actions[[index + 1]] <- selenium::actions_release(key)
        index <- index + 2
      }

      actions <- selenium::actions_stream(!!!actions)

      driver$perform_actions(actions)
    }
  } else {
    rlang::check_installed("RSelenium")

    keys <- lapply(keys, function(x) {
      if (inherits(x, "selenider_key")) {
        get_selenium_key(x)
      } else {
        x
      }
    })

    keys <- c(
      RSelenium::selKeys$shift["shift" %in% modifiers],
      RSelenium::selKeys$control[any(c("control", "ctrl") %in% modifiers)],
      RSelenium::selKeys$alt["alt" %in% modifiers],
      RSelenium::selKeys$command_meta[any(c("command", "meta") %in% modifiers)],
      keys
    )

    if (inherits(x, "selenider_session")) {
      driver$sendKeysToActiveElement(keys)
    } else {
      x$sendKeysToElement(keys)
    }
  }
}

chromote_send_keys <- function(element, driver, keys, modifiers) {
  if (!is.null(element)) {
    chromote_focus(element, driver = driver)
  }

  keys <- format_keys(keys, modifiers)
  modifier_number <- get_numeric_modifier(modifiers)

  for (key in keys) {
    rlang::inject(chromote_press(driver, !!!key, modifiers = modifier_number))
  }
}

format_keys <- function(keys, modifiers) {
  # Don't use `text` param if we have a modifier that is not shift
  no_text <- any(vapply(
    modifiers,
    function(x) identical(x, "shift"),
    FUN.VALUE = logical(1)
  ))

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
    return(0L)
  }

  modifiers <- vapply(modifiers, tolower, FUN.VALUE = character(1))

  1L * ("alt" %in% modifiers) + 2L * any(c("ctrl", "control") %in% modifiers) +
    4L * any(c("meta", "command") %in% modifiers) +
    8L * ("shift" %in% modifiers)
}

#' @rdname elem_set_value
#'
#' @export
elem_clear_value <- function(x, timeout = NULL) {
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

  element_clear_value(element, x$session, x$driver)

  invisible(x)
}

element_clear_value <- function(x, session, driver) {
  if (session == "chromote") {
    chromote_clear(x, driver)
  } else if (session == "selenium") {
    x$clear()
    x$send_keys(" ", selenium::keys$backspace)
  } else {
    x$clearElement()
    x$sendKeysToElement(list(" ", RSelenium::selKeys$backspace))
  }
}

#' Scroll to an element
#'
#' Scrolls to an HTML element.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to scroll to the element using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @family actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div style = 'height:100%; min-height:100vh'></div>
#' <button onclick='checkScrolled()'></button>
#' <p>Scroll down to find me!</p>
#' "
#'
#' js <- "
#' function checkScrolled() {
#'   let element = document.getElementsByTagName('p').item(0);
#'   let rect = element.getBoundingClientRect();
#'   // If paragraph is in view
#'   const height = window.innerHeight || document.documentElement.clientHeight;
#'   if (rect.bottom <= height) {
#'     element.innerText = 'You found me!';
#'   }
#' }
#' "
#'
#' session <- minimal_selenider_session(html, js = js)
#'
#' s("p") |>
#'   elem_scroll_to()
#'
#' s("button") |>
#'   elem_click()
#'
#' elem_expect(s("p"), has_text("You found me!"))
#'
#' @export
elem_scroll_to <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  # Firefox does not allow you to scroll to an element if not in view.
  if (js || cant_scroll_to_manually(x)) {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x} using JavaScript",
      conditions = list(),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    element_scroll_to_js(element, x$session, x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x}",
      conditions = list(is_visible),
      timeout = timeout,
      failure_messages = c("was not visible"),
      conditions_text = c("be visible")
    )

    element_scroll_to(element, x$session, x$driver)
  }

  invisible(x)
}

cant_scroll_to_manually <- function(x) {
  x$session == "chromote" ||
    (x$session == "selenium" && x$driver$browser == "firefox") ||
    (x$session == "rselenium" && x$driver$browserName == "firefox")
}

element_scroll_to_js <- function(x, session, driver) {
  execute_js_fn_on("function(x) {
    x.scrollIntoView({
      block: 'center',
      inline: 'center',
      behaviour: 'instant',
    })
  }", x, session = session, driver = driver)
}

element_scroll_to <- function(x, session, driver) {
  if (session == "selenium") {
    actions <- selenium::actions_stream(
      selenium::actions_scroll(
        x = 0,
        y = 0,
        delta_x = 0,
        delta_y = 0,
        origin = x
      )
    )

    driver$perform_actions(actions)
  } else {
    driver$mouseMoveToLocation(
      webElement = x
    )
  }
}

#' Select an HTML element
#'
#' Select or deselect `select` and `option` elements.
#'
#' @param x A `selenider_element` object representing a `select` or `option`
#'   element.
#' @param value If `x` is a `select` element, the value of the option to
#'   select. Can be a character vector, in which case multiple options will be
#'   selected.
#' @param text The text content of the option to select. This does not have to
#'   be a complete match, and multiple options can be selected.
#' @param index A vector of indexes. The nth option elements will be selected.
#' @param timeout How long to wait for the element to exist.
#' @param reset_other If `TRUE` (the default), the other options will be
#'   deselected.
#'
#' @details
#' If no arguments apart from `x` are supplied, and `x` is a `select` element,
#' all options will be deselected.
#'
#' @returns `x`, invisibly.
#'
#' @family actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <select multiple>
#'   <option value='a'>Option A.</option>
#'   <option value='b'>Option B.</option>
#'   <option value='c'>Option C.</option>
#' </select>
#' "
#' session <- minimal_selenider_session(html)
#'
#' s("select") |>
#'   elem_select("a")
#'
#' s("select") |>
#'   elem_select(text = c("Option A.", "Option C."))
#'
#' s("select") |>
#'   elem_select(index = 2, reset_other = FALSE)
#'
#' # Reset selection
#' s("select") |>
#'   elem_select()
#'
#' s("select") |>
#'   elem_select("b")
#'
#' @export
elem_select <- function(x,
                        value = NULL,
                        text = NULL,
                        index = NULL,
                        timeout = NULL,
                        reset_other = TRUE) {
  check_class(x, c("selenider_element", "selenider_elements"))
  if (!is.null(value)) {
    vctrs::obj_check_vector(value)
    value <- as.character(value)
    check_vector(value, check_string)
  }
  if (!is.null(text)) {
    vctrs::obj_check_vector(text)
    text <- as.character(text)
    check_vector(text, check_string)
  }
  check_vector(index, check_number_whole, min = 1, allow_null = TRUE)
  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(reset_other)

  values <- c("value", "text", "index")[c(
    !is.null(value),
    !is.null(text),
    !is.null(index)
  )]

  if (length(values) > 1) {
    supplied <- if (length(values) == 2) {
      "Both {.arg {values}} were supplied."
    } else {
      "{.arg {values}} were all supplied."
    }

    cli::cli_abort(
      paste0(
        "Exactly one of {.arg value}, {.arg value}, or {.arg index} must be ",
        "supplied."
      ),
      "i" = supplied,
      class = "selenider_error_missing_argument"
    )
  } else if (inherits(x, "selenider_elements") && length(values) == 1) {
    cli::cli_abort(
      c(
        "{.arg x} is a {.cls selenider_elements} object, so none of {.arg value}, {.arg text}, or {.arg index} can be supplied.",
        "x" = "{.arg {values}} was supplied."
      ),
      class = "selenider_error_missing_argument"
    )
  }

  timeout <- get_timeout(timeout, x$timeout)

  if (inherits(x, "selenider_element")) {
    element <- get_element_for_selection(
      x,
      value = value,
      text = text,
      index = index,
      timeout = timeout,
      reset_other = reset_other
    )

    elem_name <- element_name(element, x$session, x$driver)
  } else {
    all_enabled <- function(x) {
      all(vapply(as.list(x), is_enabled, logical(1)))
    }

    element <- as.list(get_elements_for_action(
      x,
      action = "select {.arg x}",
      conditions = list(
        all_enabled
      ),
      timeout = timeout,
      failure_messages = c("an element in {.arg x} was not enabled"),
      conditions_text = c("every element in {.arg x} must be enabled")
    ))

    elem_name <- "option"
  }

  element_select(
    element,
    elem_name = elem_name,
    value = value,
    text = text,
    index = index,
    reset_other = reset_other,
    session = x$session,
    driver = x$driver
  )

  invisible(x)
}

element_select <- function(x,
                           elem_name,
                           value,
                           text,
                           index,
                           reset_other,
                           session,
                           driver) {
  reset_other_json <- tolower(as.character(reset_other))

  if (elem_name == "option" && !identical(class(x), "list")) {
    execute_js_fn_on(paste0("function(x) {
      x.selected = true;

      let selectElement = x.parentElement;

      while (selectElement.tagName != 'SELECT') {
        if (selectElement == null) {
          return false;
        }

        selectElement = selectElement.parentElement;
      }

      if (selectElement.type == 'select-one') {
        for (let i = 0; i < selectElement.options.length; i++) {
          if (selectElement.options[i] == x) {
            selectElement.selectedIndex = i;
          } else if (", reset_other_json, ") {
            selectElement.options[i].selected = false;
          }
        }
      }

      selectElement.dispatchEvent(new Event('input', {bubbles: true}));
      selectElement.dispatchEvent(new Event('change', {bubbles: true}));

      return true;
    }"), x, session = session, driver = driver)
  } else if (elem_name == "option") {
    execute_js_fn_on_multiple(paste0("function(x) {
      for (let i = 0; i < x.length; i++) {
        x[i].selected = true;
      }

      let selectElement = x[0].parentElement;

      console.log(selectElement.tagName);
      while (selectElement.tagName != 'SELECT') {
        console.log(selectElement.tagName);
        if (selectElement == null) {
          return false;
        }

        selectElement = selectElement.parentElement;
      }

      for (let i = 0; i < selectElement.options.length; i++) {
        if (x.includes(selectElement.options[i])) {
          if (selectElement.type == 'select-one') {
            selectElement.selectedIndex = i;
          }
        } else if (", reset_other_json, ") {
          selectElement.options[i].selected = false;
        }
      }

      selectElement.dispatchEvent(new Event('input', {bubbles: true}));
      selectElement.dispatchEvent(new Event('change', {bubbles: true}));

      return true;
    }"), x, session = session, driver = driver)
  } else if (length(value) == 1) {
    result <- execute_js_fn_on(paste0("function(x) {
      let selected = false;
      for (let i = 0; i < x.options.length; i++) {
        if (x.options[i].value == ", value, ") {
          if (x.type == 'select-one') {
            x.selectedIndex = i;
            x.options[i].selected = true;
            selected = true;
          } else {
            x.options[i].selected = true;
            selected = true;
          }
        } else if (", reset_other_json, ") {
          x.options[i].selected = false;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      return selected;
    }"), x, session = session, driver = driver)
  } else if (!is.null(value)) {
    execute_js_fn_on(paste0("function(x) {
      const values = [", paste0("'", value, "'", collapse = ", "), "];
      let selected = false;

      if (x.type == 'select-one') {
        return -1;
      }

      for (let i = 0; i < x.options.length; i++) {
        if (values.includes(x.options[i].value)) {
          x.options[i].selected = true;
          selected = true;
        } else if (", reset_other_json, ") {
          x.options[i].selected = false;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      if (selected) {
        return 1;
      } else {
        return 0;
      }
    }"), x, session = session, driver = driver)
  } else if (length(text) == 1) {
    execute_js_fn_on(paste0("function(x) {
      let selected = false;

      for (let i = 0; i < x.options.length; i++) {
        if (x.options[i].textContent.indexOf('", text, "') >= 0) {
          if (x.type == 'select-one') {
            x.options[i].selected = true;
            x.selectedIndex = i;
            selected = true;
          } else {
            x.options[i].selected = true;
            selected = true;
          }
        } else if (", reset_other_json, ") {
          x.options[i].selected = false;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      return selected;
    }"), x, session = session, driver = driver)
  } else if (!is.null(text)) {
    execute_js_fn_on(paste0("function(x) {
      const values = [", paste0("'", text, "'", collapse = ", "), "];
      let selected = false;

      if (x.type == 'select-one') {
        return -1;
      }

      for (let i = 0; i < x.options.length; i++) {
        if (values.some(v => x.options[i].textContent.indexOf(v) >= 0)) {
          x.options[i].selected = true;
          selected = true;
        } else if (", reset_other_json, ") {
          x.options[i].selected = false;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      if (selected) {
        return 1;
      } else {
        return 0;
      }
    }"), x, session = session, driver = driver)
  } else if (length(index) == 1) {
    index <- index - 1

    execute_js_fn_on(paste0("function(x) {
      let selected = false;

      if (", reset_other_json, ") {
        for (let i = 0; i < x.options.length; i++) {
          x.options[i].selected = false;
        }
      }

      if (x.type == 'select-one') {
        if (x.options.length > ", index, ") {
          x.selectedIndex = ", index, ";
          x.options[", index, "].selected = true;

          x.dispatchEvent(new Event('input', {bubbles: true}));
          x.dispatchEvent(new Event('change', {bubbles: true}));

          return 1;
        } else {
          return -1;
        }
      } else {
        if (x.options.length > ", index, ") {
          x.options[", index, "].selected = true;
          selected = true;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      if (selected) {
        return 1;
      } else {
        return 0;
      }
    }"), x, session = session, driver = driver)
  } else if (!is.null(index)) {
    index <- index - 1

    execute_js_fn_on(paste0("function(x) {
      if (x.type == 'select-one') {
        return -1;
      }

      if (", reset_other_json, ") {
        for (let i = 0; i < x.options.length; i++) {
          x.options[i].selected = false;
        }
      }

      const values = [", paste0(index, collapse = ", "), "];

      let selected = false;

      for (i of values) {
        if (i >= 0 && i < x.options.length) {
          x.options[i].selected = true;
          selected = true;
        }
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));

      if (selected) {
        return 1;
      } else {
        return 0;
      }
    }"), x, session = session, driver = driver)
  } else if (reset_other_json) {
    execute_js_fn_on("function(x) {
      for (let i = 0; i < x.options.length; i++) {
        x.options[i].selected = false;
      }

      if (x.type == 'select-one') {
        x.selectedIndex = -1;
      }

      x.dispatchEvent(new Event('input', {bubbles: true}));
      x.dispatchEvent(new Event('change', {bubbles: true}));
    }", x, session = session, driver = driver)
  }
}

get_element_for_selection <- function(x,
                                      value = NULL,
                                      text = NULL,
                                      index = NULL,
                                      timeout = NULL,
                                      reset_other = TRUE,
                                      call = rlang::caller_env()) {
  if (is.null(value) && is.null(text) && is.null(index)) {
    if (reset_other) {
      condition <- function(x) elem_name(x) %in% c("option", "select")

      return(get_element_for_action(
        x,
        "select {.arg x}",
        list(is_enabled, condition),
        timeout = timeout,
        failure_messages = c(
          "was not enabled",
          "did not have the correct tag name"
        ),
        conditions_text = c(
          "be enabled",
          "be a `<select>` or `<option>` element"
        ),
        call = call
      ))
    } else {
      name <- "option"

      condition_text <- paste0(
        "be an `<option>` element, not a `<select>` element (as all other ",
        "arguments are `NULL` and {.arg reset_other} is `FALSE`)"
      )
      check_multiple <- FALSE
      specific_condition <- NULL
    }
  } else {
    if (!is.null(value)) {
      name <- "select"
      check_multiple <- length(value) > 1
      arg <- "value"

      specific_condition <- function(x) {
        has_at_least(elem_filter(find_elements(x, "option"), function(x) {
          elem_value(x) %in% value
        }), 1)
      }

      specific_condition_text <- paste0(
        "have an `<option>` element with value {.or {.val ", value, "}}"
      )
      specific_failure_message <- "did not have contain the required option"
    } else if (!is.null(text)) {
      name <- "select"
      check_multiple <- length(text) > 1
      arg <- "text"

      specific_condition <- function(x) {
        has_at_least(elem_filter(find_elements(x, "option"), function(x) {
          actual_text <- elem_text(x, timeout = 0)
          any(vapply(text, function(x) {
            grepl(x, actual_text, fixed = TRUE)
          }, logical(1)))
        }), 1)
      }

      specific_condition_text <- paste0(
        "have an `<option>` element with text {.or {.val ", text, "}}"
      )
      specific_failure_message <- "did not have contain the required option"
    } else if (!is.null(index)) {
      name <- "select"
      check_multiple <- length(index) > 1
      arg <- "index"

      specific_condition <- function(x) {
        execute_js_fn(paste0("x => x.options.length >= ", min(index)), x)
      }

      specific_condition_text <- paste0(
        "have at least {.val {", min(index), "}} options"
      )
      specific_failure_message <- NULL
    }

    condition_text <- paste0(
      "be a `<select>` element, not an `<option>` element (as {.arg ", arg, "}",
      " is not `NULL`)"
    )
    if (check_multiple) {
      multiple_condition <- function(x) {
        execute_js_fn("x => x.type != 'select-one'", x)
      }
      multiple_condition_text <- paste0(
        "support multiple selections (as {.arg ", arg, "} contains multiple ",
        "items)"
      )
    }
  }

  conditions <- list(
    is_present,
    is_enabled,
    function(x) has_name(x, name)
  )

  if (!is.null(specific_condition)) {
    conditions <- append(conditions, list(specific_condition))
  }

  if (check_multiple) {
    conditions <- append(conditions, list(multiple_condition))
  }

  get_select_element(
    x,
    conditions = conditions,
    timeout = timeout,
    name = name,
    condition_text = condition_text,
    specific_condition = specific_condition,
    specific_failure_message = specific_failure_message,
    specific_condition_text = specific_condition_text,
    check_multiple = check_multiple,
    multiple_condition = multiple_condition,
    multiple_condition_text = multiple_condition_text,
    call = call
  )
}

get_select_element <- function(x,
                               conditions,
                               timeout,
                               name,
                               condition_text,
                               specific_condition,
                               specific_failure_message,
                               specific_condition_text,
                               check_multiple,
                               multiple_condition,
                               multiple_condition_text,
                               call = rlang::caller_env()) {
  meets_condition <- inject(elem_wait_until(
    x,
    !!!conditions,
    timeout = timeout
  ))

  other_name <- switch(name,
    "option" = "select",
    "select" = "option",
    stop()
  )

  if (!meets_condition) {
    if (!is_present(x)) {
      stop_not_actionable(c(
        "To select {.arg x}, it must exist.",
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x} was not present."
        )
      ), call = call, class = "selenider_error_absent_element")
    }

    if (!is_enabled(x)) {
      stop_not_actionable(c(
        "To select {.arg x}, it be enabled.",
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x} was not enabled."
        )
      ), call = call)
    }

    if (!has_name(x, name)) {
      if (elem_name(x, timeout = 0) == other_name) {
        stop_not_actionable(c(
          paste0("To select {.arg x}, it must ", condition_text, "."),
          "i" = paste0(
            format_timeout_for_error(timeout),
            "{.arg x} did not have the correct tag name."
          )
        ))
      } else {
        stop_not_actionable(c(
          "To select {.arg x}, it must be a `<select>` or `<option>` element.",
          "i" = paste0(
            format_timeout_for_error(timeout),
            "{.arg x} did not have the correct tag name."
          )
        ))
      }
    }

    if (!is.null(specific_condition) && !specific_condition(x)) {
      if (is.null(specific_failure_message)) {
        # The failure message for the `index` argument requires the element to
        # be collected.
        n_options <- execute_js_fn_on(
          "x => x.options.length",
          get_element(x),
          session = x$session,
          driver = x$driver
        )
        specific_failure_message <- paste0("had {.val {n_options}} options")
      }
      stop_not_actionable(c(
        paste0("To select {.arg x}, it must ", specific_condition_text, "."),
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x} ",
          specific_failure_message,
          "."
        )
      ))
    }

    if (check_multiple && !multiple_condition(x)) {
      stop_not_actionable(c(
        paste0("To select {.arg x}, it must ", multiple_condition_text, "."),
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x} did not support multiple selections."
        )
      ))
    }
  }

  element <- get_element(x)

  if (is.null(element)) {
    stop_not_actionable(c(
      paste0("To select {.arg x}, it must exist."),
      "i" = paste0(
        format_timeout_for_error(timeout),
        "{.arg x} was not present."
      )
    ), call = call, class = "selenider_error_absent_element")
  }

  element
}

#' Submit an element
#'
#' If an element is an ancestor of a form, submits the form.
#' Works by walking up the DOM, checking each ancestor element until
#' the element is a `<form>` element, which it then submits. If such
#' an element does not exist, an error is thrown.
#'
#' @param x A `selenider_element` object.
#' @param js Whether to submit the form using JavaScript.
#' @param timeout How long to wait for the element to exist.
#'
#' @returns `x`, invisibly.
#'
#' @family actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <form>
#' <input type='submit'>
#' <p>Random text</p>
#' </form>
#' <a>Random link</a>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' elem_submit(s("input"))
#' elem_submit(s("p"))
#'
#' # Won't work since the element doesn't have a form ancestor
#' try(elem_submit(s("a"), timeout = 0.5))
#'
#' @export
elem_submit <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  has_form_parent <- function(x) {
    has_at_least(elem_filter(elem_ancestors(x), has_name("form")), 1)
  }

  if (js || x$session != "rselenium") {
    element <- get_element_for_action(
      x,
      action = "submit {.arg x} using JavaScript",
      conditions = list(has_form_parent),
      timeout = timeout,
      failure_messages = c("did not have a form ancestor"),
      conditions_text = c("have a form element as its ancestor")
    )

    result <- element_submit_js(element, x$session, x$driver)

    if (!result) {
      # Shouldn't happen
      cli::cli_abort(c(
        "To submit {.arg x}, it must be the descendant of a <form> element"
      ))
    }
  } else {
    element <- get_element_for_action(
      x,
      action = "submit {.arg x}",
      conditions = list(has_form_parent),
      timeout = timeout,
      failure_messages = c("did not have a form ancestor"),
      conditions_text = c("have a form element as its ancestor")
    )

    element_submit(element, x$session, x$driver)
  }

  invisible(x)
}

element_submit_js <- function(x, session, driver) {
  execute_js_fn_on("function(element) {
    while (element != null) {
      if (element.tagName == 'FORM') {
        if (element.requestSubmit) {
          element.requestSubmit();
        } else {
          element.submit();
        }
        return true;
      }

      element = element.parentElement;
    }
    return false;
  }", x, session = session, driver = driver)
}

element_submit <- function(x, session, driver) {
  x$submitElement()
}

get_element_for_action <- function(x,
                                   action,
                                   conditions,
                                   timeout,
                                   failure_messages,
                                   conditions_text,
                                   call = rlang::caller_env()) {
  meets_condition <-
    inject(elem_wait_until(
      x,
      is_present,
      !!!conditions,
      timeout = timeout
    ))

  if (!meets_condition) {
    if (length(conditions) == 0 || !is_present(x)) {
      stop_not_actionable(c(
        paste0("To ", action, ", it must exist."),
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x} was not present."
        )
      ), call = call, class = "selenider_error_absent_element")
    }

    for (n in seq_along(conditions)) {
      condition <- conditions[[n]]

      if (!condition(x)) {
        stop_not_actionable(c(
          paste0("To ", action, ", it must ", conditions_text[[n]], "."),
          "i" = paste0(
            format_timeout_for_error(timeout),
            "{.arg x} {failure_messages[[n]]}."
          )
        ), call = call)
      }
    }
  }

  element <- get_element(x)

  if (is.null(element)) {
    stop_not_actionable(c(
      paste0("To ", action, ", it must exist."),
      "i" = paste0(
        format_timeout_for_error(timeout),
        "{.arg x} was not present."
      )
    ), call = call, class = "selenider_error_absent_element")
  }

  element
}

get_elements_for_action <- function(x,
                                    action,
                                    conditions,
                                    timeout,
                                    failure_messages,
                                    conditions_text,
                                    call = rlang::caller_env()) {
  get_elements_succeeds <- function(x) {
    !is.null(get_elements(x))
  }

  meets_condition <-
    inject(elem_wait_until(
      x,
      get_elements_succeeds,
      !!!conditions,
      timeout = timeout
    ))

  if (!meets_condition) {
    if (length(conditions) == 0 || !get_elements_succeeds(x)) {
      stop_not_actionable(
        c(
          paste0("To ", action, ", its parent must exist."),
          "i" = paste0(
            format_timeout_for_error(timeout),
            "{.arg x}'s parent did not exist."
          )
        ),
        call = call,
        class = c(
          "selenider_error_absent_parent",
          "selenider_error_absent_element"
        )
      )
    }

    for (n in seq_along(conditions)) {
      condition <- conditions[[n]]

      if (!condition(x)) {
        stop_not_actionable(
          c(
            paste0("To ", action, " ", conditions_text[[n]], "."),
            "i" = paste0(
              format_timeout_for_error(timeout),
              failure_messages[[n]], "."
            )
          )
        )
      }
    }
  }

  elements <- get_elements(x)

  if (is.null(elements)) {
    stop_not_actionable(
      c(
        paste0("To ", action, ", its parent must exist."),
        "i" = paste0(
          format_timeout_for_error(timeout),
          "{.arg x}'s parent did not exist."
        )
      ),
      call = call,
      class = c(
        "selenider_error_absent_parent",
        "selenider_error_absent_element"
      )
    )
  }

  elements
}

format_timeout_for_error <- function(x) {
  if (x == 0) {
    ""
  } else {
    paste("After", x, "seconds, ")
  }
}
