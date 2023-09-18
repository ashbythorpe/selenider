#' Click an element
#' 
#' @description
#' Clicks on an HTML element, either by simulating a mouse click or by triggering 
#' the element's "click" event.
#' 
#' `elem_click()` left clicks on the element, `elem_double_click()` left clicks on the
#' element two times in a short period of time, while `elem_right_click()` right
#' clicks on an element, opening its context menu.
#' 
#' @param x A `selenider_element` object.
#' @param js Whether to click the element using JavaScript. For `elem_right_click()`,
#'   this is ignored if Selenium is being used, since right clicking using
#'   RSelenium does not seem to work (so JavaScript is used instead).
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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
        event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
        x.dispatchEvent(event);
      }
    }", element, driver = x$driver)
  } else {
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
      promise <- x$driver$Page$loadEventFired(wait_ = FALSE)$catch(function(e) NULL)
      click_chromote(element, driver = x$driver)
      name <- x$driver$DOM$describeNode(backendNodeId = element)$node$nodeName
      if (identical(name, "A")) {
        x$driver$wait_for(promise)
      }
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
  chromote_scroll_into_view_if_needed(backend_id = element, driver = driver)

  coords <- chromote_get_xy(backend_id = element, driver = driver)
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
    
    execute_js_fn_on("function(x) {
      if (window.MouseEvent) {
        const event = new MouseEvent('dblclick');
        x.dispatchEvent(event)
      } else {
        const event = document.createEvent('mouseevents');
        event.initEvent('dblclick', true, true);
        x.dispatchEvent(event);
      }
    }", element, driver = x$driver)
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
        webElement = element
      )
      
      x$driver$doubleclick()
    } else {
      click_chromote(element, x$driver, type = "left", count = 2)
    }
  }
  
  invisible(x)
}

#' @rdname elem_click
#' 
#' @export
elem_right_click <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)
  
  if (js || uses_selenium(x$driver)) {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x} using javascript",
      conditions = list(is_enabled),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )
    
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
    }", element, driver = x$driver)
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
        webElement = element
      )
      
      x$driver$elem_click(2)
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
#' @returns `x`, invisibly.
#'
#' @family actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button onmouseover = settext()> </button>
#' <p class = 'text'></p>
#' "
#'
#' js <- "
#' function settext() {
#'   document.getElementsByClassName('text').item(0).innerHTML = 'Button hovered!'
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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
    }", element, driver = x$driver)
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
        webElement = element
      )
    } else {
      hover_chromote(element, driver = x$driver)
    }
  }

  invisible(x)
}

hover_chromote <- function(element, driver) {
  chromote_scroll_into_view_if_needed(backend_id = element, driver = driver)

  coords <- chromote_get_xy(backend_id = element, driver = driver)
  x <- coords$x
  y <- coords$y

  driver$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
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
#' @param x A `selenider_element` object. For [elem_send_keys()], this can be `NULL`,
#'   meaning that the keys will be sent to the current page (or the currently focused
#'   element) instead of a specific element.
#' @param text A string to set the value of the input element to.
#' @param timeout How long to wait for the element to exist.
#' 
#' @returns `x`, invisibly.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <input type='text' oninput='recordChange(event)' onkeypress='return checkEnter(event);'>
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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

  execute_js_fn_on(
    paste0("x => x.setAttribute('value','", text, "')"),
    element, driver = x$driver
  )
  
  if (uses_selenium(x$driver)) {
    element$clearElement()
    element$sendKeysToElement(list(text))
  } else {
    chromote_clear(element, driver = x$driver)
    chromote_send_chars(text, driver = x$driver)
  }
}

chromote_clear <- function(x, driver) {
  if (is_mac()) {
    click_chromote(x, driver = driver, count = 3)
  } else {
    click_chromote(x, driver = driver)

    chromote_press(driver, modifiers = 2, key = "a", code = "KeyA", windowsVirtualKeyCode = 65)
  }
  chromote_press(driver, windowsVirtualKeyCode = 8, code = "Backspace", key = "Backspace") 
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
#' @param modifiers A character vector; one or more of "shift", "ctrl"/"control", "alt", and "command"/meta".
#'   Note that when using chromote as a backend, these do not work on Mac OS.
#' 
#' @export
elem_send_keys <- function(x, ..., modifiers = NULL, timeout = NULL) {
  check_class(x, c("selenider_element", "selenider_session"), allow_null = TRUE)
  check_number_decimal(timeout, allow_null = TRUE)
  check_dots_unnamed()

  if (is.null(x)) {
    x <- get_session()
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

  if (uses_selenium(x$driver)) {
    rlang::check_installed("RSelenium")
    keys <- c(
      RSelenium::selKeys$shift["shift" %in% modifiers],
      RSelenium::selKeys$control[any(c("control", "ctrl") %in% modifiers)],
      RSelenium::selKeys$alt["alt" %in% modifiers],
      RSelenium::selKeys$command_meta[any(c("command", "meta") %in% modifiers)],
      keys
    )

    if (inherits(element, "selenider_session")) {
      element$driver$client$sendKeysToActiveElement(keys)
    } else {
      element$sendKeysToElement(keys)
    }
  } else {
    chromote_send_keys(element, x$driver, keys, modifiers)
  }

  invisible(x)
}

chromote_send_keys <- function(element, driver, keys, modifiers) {
  if (!is.null(element)) {
    click_chromote(element, driver = driver)
  }

  keys <- format_keys(keys, modifiers)
  modifier_number <- get_numeric_modifier(modifiers)

  for (key in keys) {
    rlang::inject(chromote_press(driver, !!!key, modifiers = modifier_number))
  }
}

format_keys <- function(keys, modifiers) {
  # Don't use `text` param if we have a modifier that is not shift
  no_text <- any(vapply(modifiers, function(x) identical(x, "shift"), FUN.VALUE = logical(1)))

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
  
  if (uses_selenium(x$driver)) {
    element$clearElement()
    element$sendKeysToElement(list(" ", RSelenium::selKeys$backspace))
  } else {
    chromote_clear(element, x$driver)
  }

  invisible(x)
}

format_timeout_for_error <- function(x) {
  if (x == 0) {
    ""
  } else {
    paste("After", x, "seconds, ")
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
#'   if (rect.bottom <= (window.innerHeight || document.documentElement.clientHeight)) {
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
elem_scroll_to <- function(x, js = FALSE, timeout = NULL) {
  check_class(x, "selenider_element")
  check_bool(js)
  check_number_decimal(timeout, allow_null = TRUE)

  timeout <- get_timeout(timeout, x$timeout)

  # Firefox does not allow you to scroll to an element if not in view.
  if (js || (uses_selenium(x$driver) && x$driver$browserName == "firefox")) {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x} using JavaScript",
      conditions = list(),
      timeout = timeout,
      failure_messages = c("was not enabled"),
      conditions_text = c("be enabled")
    )

    execute_js_fn_on("x => x.scrollIntoView()", element, driver = x$driver)
  } else {
    element <- get_element_for_action(
      x,
      action = "scroll to {.arg x}",
      conditions = list(is_visible),
      timeout = timeout,
      failure_messages = c("was not visible"),
      conditions_text = c("be visible")
    )
    
    if (uses_selenium(x$driver)) {
      size <- element$getElementSize()

      x$driver$mouseMoveToLocation(
        webElement = element
      )
    } else {
      chromote_scroll_into_view(backend_id = element, driver = x$driver)
    }
  }

  invisible(x)
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
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

  if (js || !uses_selenium(x$driver)) {
    element <- get_element_for_action(
      x,
      action = "submit {.arg x} using JavaScript",
      conditions = list(has_form_parent),
      timeout = timeout,
      failure_messages = c("did not have a form ancestor"),
      conditions_text = c("have a form element as its ancestor")
    )

    result <- unpack_list(execute_js_fn_on("function(element) {
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
    }", element, driver = x$driver))

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

    element$submitElement()
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
    inject(elem_wait_until(
      x,
      is_present,
      !!!conditions,
      timeout = timeout
    ))
  if (length(conditions) != 0 && !meets_condition) {
    if (length(conditions) == 0 || !is_present(x)) {
      stop_not_actionable(c(
        paste0("To ", action, ", it must exist."),
        "i" = paste0(format_timeout_for_error(timeout), "{.arg x} was not present.")
      ), call = call, class = "selenider_error_absent_element")
    }

    for (n in seq_along(conditions)) {
      condition <- conditions[[n]]
      
      if (!condition(x)) {
        stop_not_actionable(c(
          paste0("To ", action, ", it must ", conditions_text, "."),
          "i" = paste0(format_timeout_for_error(timeout), "{.arg x} {failure_messages[[n]]}.")
        ), call = call)
      }
    }
  }

  element <- get_element(x)

  if (is.null(element)) {
    stop_not_actionable(c(
      paste0("To ", action, ", it must exist."),
      "i" = paste0(format_timeout_for_error(timeout), "{.arg x} was not present.")
    ), call = call, class = "selenider_error_absent_element")
  }

  element
}
