#' Find the first instance of an element using a selector
#'
#' @param x The parent element.
#' @param type The type of the selector (e.g. "css selector", "xpath").
#' @param value The value of the selector as a string.
#' @param driver The [chromote::ChromoteSession] or [selenium::SeleniumSession].
#'
#' @returns
#' A backendNodeId (chromote) or a WebElement (selenium)
#'
#' @noRd
find_actual_element <- function(x, type, value, driver) {
  using <- switch(type,
    css = "css selector",
    class_name = "class name",
    type
  )

  if (is.null(x)) {
    x <- driver
  }

  if (inherits_any(x, c("webElement", "mock_element"))) {
    try_fetch(
      suppressMessages(x$findChildElement(using = using, value = value)),
      error = function(cnd) {
        if (grepl("NoSuchElement", cnd$message, fixed = TRUE)) {
          NULL
        } else {
          zap()
        }
      }
    )
  } else if (inherits_any(x, c("remoteDriver", "mock_client"))) {
    try_fetch(
      suppressMessages(x$findElement(using = using, value = value)),
      error = function(cnd) {
        if (grepl("NoSuchElement", cnd$message, fixed = TRUE)) {
          return(NULL)
        } else {
          return(zap())
        }
      }
    )
  } else if (inherits_any(x, c("SeleniumSession", "WebElement"))) {
    if (!using %in% c("css", "xpath", "tag name", "link text")) {
      value <- selector_to_css(type, value)
      using <- "css selector"
    }

    try_fetch(
      x$find_element(using = using, value = value),
      error = function(cnd) {
        if (grepl("No such element", cnd$message, fixed = TRUE)) {
          NULL
        } else {
          zap()
        }
      }
    )
  } else if (inherits(x, "ChromoteSession")) {
    tryCatch(
      wrap_error_chromote(find_element_chromote_session(x, type, value)),
      selenider_error_resolve_element = function(e) {
        NULL
      }
    )
  } else if (is.numeric(x)) {
    tryCatch(
      wrap_error_chromote(find_element_chromote_node(x, type, value, driver)),
      selenider_error_resolve_element = function(e) {
        NULL
      }
    )
  }
}

#' Find every occurance of an element using a selector
#'
#' Same as `find_element()`, but returns every element instead of just the
#' first one.
#'
#' @noRd
find_actual_elements <- function(x, type, value, driver) {
  using <- switch(type,
    css = "css selector",
    class_name = "class name",
    type
  )

  if (is.null(x)) {
    x <- driver
  }

  if (inherits_any(x, c("webElement", "mock_element"))) {
    x$findChildElements(using = using, value = value)
  } else if (inherits_any(x, c("remoteDriver", "mock_client"))) {
    x$findElements(using = using, value = value)
  } else if (inherits(x, "ChromoteSession")) {
    if (type == "xpath") {
      return(use_xpath_chromote(value, x, NULL, multiple = TRUE))
    }

    selector <- selector_to_css(type, value)
    document <- x$DOM$getDocument()
    node_ids <- x$DOM$querySelectorAll(document$root$nodeId, selector)$nodeIds
    lapply(node_ids, chromote_backend_id, driver = x)
  } else if (inherits_any(x, c("SeleniumSession", "WebElement"))) {
    if (!type %in% c("css selector", "xpath", "tag name", "link text")) {
      value <- selector_to_css(type, value)
      using <- "css selector"
    }

    x$find_elements(using = using, value = value)
  } else if (is.numeric(x)) {
    if (type == "xpath") {
      return(use_xpath_chromote(value, x, driver, multiple = TRUE))
    }

    selector <- selector_to_css(type, value)
    node_ids <- driver$DOM$querySelectorAll(
      chromote_node_id(backend_id = x, driver = driver),
      selector
    )$nodeIds
    lapply(node_ids, chromote_backend_id, driver = driver)
  }
}


find_element_chromote_session <- function(x, type, value) {
  if (type == "xpath") {
    return(use_xpath_chromote(value, x, NULL))
  }

  selector <- selector_to_css(type, value)
  document <- x$DOM$getDocument()

  node_id <- x$DOM$querySelector(document$root$nodeId, selector)$nodeId

  chromote_backend_id(node_id, driver = x)
}

find_element_chromote_node <- function(x, type, value, driver) {
  if (type == "xpath") {
    return(use_xpath_chromote(value, x, driver = driver))
  }

  selector <- selector_to_css(type, value)
  node_id <- driver$DOM$querySelector(
    chromote_node_id(backend_id = x, driver = driver),
    selector
  )$nodeId

  chromote_backend_id(node_id, driver = driver)
}

selector_to_css <- function(type, value) {
  switch(type,
    "css" = value,
    "id" = paste0("#", value),
    "class_name" = paste0(".", value),
    "name" = paste0("[name = '", value, "']"),
    cli::cli_abort(paste0("Unexpected `type` value: ", type), .internal = TRUE)
  )
}

#' Evaluate an XPath on a chromote node
#'
#' Uses JavaScript since chromote doesn't support xpaths directly.
#'
#' @noRd
use_xpath_chromote <- function(xpath, element, driver, multiple = FALSE) {
  xpath <- jsonlite::toJSON(xpath, auto_unbox = TRUE)
  if (multiple) {
    if (is.null(driver)) {
      driver <- element
      array_object_id <- driver$Runtime$evaluate(paste0("(() => {
        let xpath = document.evaluate(", xpath, ", document, null, 5, null);

        let nodes = [];
        for (let node = xpath.iterateNext(); node; node = xpath.iterateNext()) {
          nodes.push(node);
        }

        return nodes;
      })()"))$result$objectId
    } else {
      element_object_id <- chromote_object_id(
        backend_id = element,
        driver = driver
      )

      array_object_id <- driver$Runtime$callFunctionOn(paste0("function() {
        let xpath = document.evaluate(", xpath, ", this, null, 5, null);

        let nodes = [];
        for (let node = xpath.iterateNext(); node; node = xpath.iterateNext()) {
          nodes.push(node);
        }

        return nodes;
      }"), element_object_id)$result$objectId
    }

    length <- driver$Runtime$callFunctionOn(
      "function() { return this.length; }",
      array_object_id
    )$result$value
    nodes <- vector("list", length)
    for (i in seq_len(length)) {
      object_id <- driver$Runtime$callFunctionOn(
        paste0("function() { return this[", i - 1L, "]; }"),
        array_object_id
      )$result$objectId
      nodes[[i]] <- chromote_backend_id(object_id = object_id, driver = driver)
    }

    nodes
  } else {
    if (is.null(driver)) {
      driver <- element
      result <- driver$Runtime$evaluate(paste0("(() => {
        let xpath = document.evaluate(", xpath, ", document, null, 9, null);

        let node = xpath.singleNodeValue;

        return node;
      })()"))$result
    } else {
      element_object_id <- chromote_object_id(
        backend_id = element,
        driver = driver
      )

      result <- driver$Runtime$callFunctionOn(paste0("function() {
        let xpath = document.evaluate(", xpath, ", this, null, 9, null);

        let node = xpath.singleNodeValue;

        return node;
      }"), element_object_id)$result
    }

    if (identical(result$subtype, "null")) {
      NULL
    } else {
      chromote_backend_id(object_id = result$objectId, driver = driver)
    }
  }
}
