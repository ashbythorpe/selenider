
find_element <- function(x, using, value, driver) {
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
  } else if (inherits(x, "ChromoteSession")) {
    if (using == "xpath") {
      return(use_xpath_chromote(value, x, NULL))
    }
    
    selector <- selector_to_css(using, value)
    document <- x$DOM$getDocument()
    nodeId <- x$DOM$querySelector(document$root$nodeId, value)$nodeId
    if (nodeId == 0) {
      return(NULL)
    }

    chromote_backend_id(nodeId)
  } else if (is.numeric(x)) {
    if (using == "xpath") {
      return(use_xpath_chromote(value, x, driver))
    }

    selector <- selector_to_css(using, value)
    nodeId <- driver$DOM$querySelector(x, value)$nodeId
    if (nodeId == 0) {
      return(NULL)
    }

    chromote_backend_id(nodeId)
  }
}

find_elements <- function(x, using, value, driver) {
  if (inherits_any(x, c("webElement", "mock_element"))) {
    x$findChildElements(using = using, value = value)
  } else if(inherits_any(x, c("remoteDriver", "mock_client"))) {
    x$findElements(using = using, value = value)
  } else if (inherits(x, "ChromoteSession")) {
    if (using == "xpath") {
      return(use_xpath_chromote(value, x, NULL, multiple = TRUE))
    }
    
    selector <- selector_to_css(using, value)
    document <- x$DOM$getDocument()
    node_ids <- x$DOM$querySelectorAll(document$root$nodeId, value)$nodeIds
    lapply(node_ids, chromote_backend_id)
  } else if (is.numeric(x)) {
    if (using == "xpath") {
      return(use_xpath_chromote(value, x, driver, multiple = TRUE))
    }

    selector <- selector_to_css(using, value)
    node_ids <- driver$DOM$querySelectorAll(x, value)$nodeIds
    lapply(node_ids, chromote_backend_id)
  }
}

selector_to_css <- function(using, value) {
  switch(using,
    "css selector" = value,
    "id" = paste0("#", value),
    "class name" = paste0(".", value),
    "name" = paste0("[name = '", value, "']"),
    "link text" = paste0("a:contains(^", value, "$)"),
  )
}

use_xpath_chromote <- function(xpath, element, driver, multiple = FALSE) {
  if(multiple) {
    if (is.null(driver)) {
      driver <- element
      array_object_id <- driver$Runtime$evaluate(paste0("(() => {
        let xpath = document.evaluate('", xpath, "', document, null, 5, null);
        
        let nodes = [];
        for (let node = xpath.iterateNext(); node; node = xpath.iterateNext()) {
          nodes.push(node);
        }

        return nodes;
      })()"))$result$objectId
    } else {
      element_object_id <- driver$DOM$resolveNode(element)$object$objectId

      array_object_id <- driver$Runtime$callFunctionOn(paste0("function() {
        let xpath = document.evaluate('", xpath, "', this, null, 5, null);

        let nodes = [];
        for (let node = xpath.iterateNext(); node; node = xpath.iterateNext()) {
          nodes.push(node);
        }

        return nodes;
      }"), element_object_id)$result$objectId
    }

    length <- driver$Runtime$callFunctionOn("function() { return this.length; }", array_object_id)$result$value
    nodes <- vector("list", length)
    for (i in (seq_len(length) - 1L)) {
      object_id <- driver$Runtime$callFunctionOn(paste0("function() { return this[", i, "]; }"), array_object_id)$result$objectId
      nodes[[i + 1]] <- chromote_backend_id(object_id = object_id)
    }

    nodes
  } else {
    if (is.null(driver)) {
      driver <- element
      result <- driver$Runtime$evaluate(paste0("(() => {
        let xpath = document.evaluate('", xpath, "', document, null, 5, null);

        let node = xpath.iterateNext();

        return node;
      })()"))$result
    } else {
      element_object_id <- driver$DOM$resolveNode(element)$object$objectId

      result <- driver$Runtime$callFunctionOn(paste0("function() {
        let xpath = document.evaluate('", xpath, "', this, null, 5, null);

        let node = xpath.iterateNext();

        return node;
      }"), element_object_id)$result
    }

    if (identical(result$subclass, "null")) {
      NULL
    } else {
      chromote_backend_id(object_id = result$objectId)
    }
  }
}
