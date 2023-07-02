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

    x$driver$executeScript("arguments[0].click();", list(element))
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

#' @rdname click
#' 
#' @export
double_click <- function(x, js = FALSE, timeout = NULL) {
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
    
    x$driver$executeScript("
      element = arguments[0];
      element.click(); 
      element.click();
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('dblclick', true, true);
      element.dispatchevent(clickevent);
    ", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "double click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )
    
    size <- element$getElementSize()

    x$driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webelement = element
    )
    
    x$driver$doubleclick()
  }
  
  invisible(x)
}

#' @rdname click
#' 
#' @export
right_click <- function(x, js = FALSE, timeout = NULL) {
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
    
    x$driver$executeScript("
      element = arguments[0];
      element.click(); 
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('contextmenu', true, true);
      element.dispatchevent(clickevent);
    ", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "right click {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )
    
    size <- element$getElementSize()

    x$driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webelement = element
    )
    
    x$driver$click(2)
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

    x$driver$executeScript("
      element = arguments[0];
      element.moveToElement(element);
      let clickevent = document.createevent('mouseevents');
      clickevent.initevent('mouseover', true, true);
      element.dispatchevent(clickevent);
    ", list(element))
  } else {
    element <- get_element_for_action(
      x,
      action = "hover over {.arg x}",
      conditions = list(is_visible, is_enabled),
      timeout = timeout,
      failure_messages = c("was not visible", "was not enabled"),
      conditions_text = c("be visible and enabled")
    )

    size <- element$getElementSize()

    x$driver$mouseMoveToLocation(
      x = round(size$width / 3),
      y = round(size$height / 3),
      webelement = element
    )
  }

  invisible(x)
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

#' @rdname set_value
#'
#' @param ... A set of inputs to send to `x`.
#' 
#' @export
send_keys <- function(x, ..., timeout = NULL) {
  keys <- list2(...)

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

#' @rdname set_value
#'
#' @export
clear_value <- function(x, timeout = NULL) {
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
        "To submit {.arg x}, it must be the child of a <form> element"
      ))
      # TODO: Add implicit waiting to check for a <form> parent.
      # Remember: descendant/ancestor
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
