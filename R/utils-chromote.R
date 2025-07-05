chromote_object_id <- function(node_id = NULL, backend_id = NULL, driver) {
  if (!is.null(node_id)) {
    ignore_error_chromote(
      driver$DOM$resolveNode(node_id)$object$objectId,
      chromote_errors$RESOLVE_NODE
    )
  } else {
    ignore_error_chromote(
      driver$DOM$resolveNode(backendNodeId = backend_id)$object$objectId,
      chromote_errors$BACKEND_ID_NOT_FOUND
    )
  }
}

chromote_backend_id <- function(node_id = NULL, object_id = NULL, driver) {
  if (!is.null(node_id)) {
    ignore_error_chromote(
      driver$DOM$describeNode(node_id)$node$backendNodeId,
      chromote_errors$NODE_NOT_FOUND
    )
  } else {
    ignore_error_chromote(
      driver$DOM$describeNode(objectId = object_id)$node$backendNodeId,
      chromote_errors$OBJECT_ID_NOT_FOUND
    )
  }
}

chromote_root_id <- function(x) {
  document <- x$DOM$getDocument()
  document$root$nodeId
}

chromote_node_id <- function(object_id = NULL, backend_id = NULL, driver) {
  if (is.null(object_id)) {
    object_id <- chromote_object_id(backend_id = backend_id, driver = driver)

    if (is.null(object_id)) {
      return(NULL)
    }
  }

  ignore_error_chromote(
    driver$DOM$requestNode(object_id)$nodeId,
    chromote_errors$OBJECT_ID_NOT_FOUND
  )
}

chromote_errors <- list(
  RESOLVE_NODE = "No node with given id found",
  NODE_NOT_FOUND = "Could not find node with given id",
  BACKEND_ID_NOT_FOUND = "Node with given id does not belong to the document",
  OBJECT_ID_NOT_FOUND = "Cannot find context with specified id"
)

chromote_get_xy <- function(node_id = NULL, backend_id = NULL, driver) {
  if (is.null(node_id)) {
    node_id <- chromote_node_id(backend_id = backend_id, driver = driver)

    if (is.null(node_id)) {
      return(NULL)
    }
  }

  coords <- ignore_error_chromote(
    driver$DOM$getBoxModel(nodeId = node_id)$model$content,
    chromote_errors$NODE_NOT_FOUND
  )

  if (is.null(coords)) {
    return(NULL)
  }

  x <- mean(range(unlist(coords[seq(1, 7, 2)])))
  y <- mean(range(unlist(coords[seq(2, 8, 2)])))
  list(x = x, y = y)
}

chromote_is_in_view <- function(node_id = NULL, backend_id = NULL, driver) {
  layout <- driver$Page$getLayoutMetrics()$cssLayoutViewport
  width <- layout$clientWidth
  height <- layout$clientHeight
  coords <- chromote_get_xy(node_id, backend_id, driver = driver)

  if (is.null(coords)) {
    return(NULL)
  }

  x <- coords$x
  y <- coords$y

  output <- list()
  if (x < 0) {
    output$x <- x
  } else if (x > width) {
    output$x <- x - width
  }

  if (y < 0) {
    output$y <- y
  } else if (y > height) {
    output$y <- y - height
  }

  if (length(output) == 0) {
    TRUE
  } else {
    output
  }
}

chromote_scroll_into_view <- function(node_id = NULL,
                                      backend_id = NULL,
                                      driver) {
  driver$Runtime$callFunctionOn("function() {
    this.scrollIntoView({
      block: 'center',
      inline: 'center',
      behaviour: 'instant',
    })
  }", chromote_object_id(node_id, backend_id, driver = driver))
}

chromote_scroll_into_view_if_needed <- function(node_id = NULL, # nolint: object_length_linter
                                                backend_id = NULL,
                                                driver) {
  if (!is.null(driver$DOM$scrollIntoViewIfNeeded)) {
    if (!is.null(node_id)) {
      driver$DOM$scrollIntoViewIfNeeded(node_id)
    } else {
      driver$DOM$scrollIntoViewIfNeeded(backendNodeId = backend_id)
    }
  }
  if (!isTRUE(chromote_is_in_view(node_id, backend_id, driver = driver))) {
    chromote_scroll_into_view(node_id, backend_id, driver = driver)
  }
}

chromote_press <- function(driver, ...) {
  driver$Input$dispatchKeyEvent(type = "keyDown", ...)
  driver$Input$dispatchKeyEvent(type = "keyUp", ...)
}

chromote_clickable_point <- function(node_id = NULL,
                                     backend_id = NULL,
                                     driver) {
  chromote_scroll_into_view_if_needed(node_id, backend_id, driver = driver)

  boxes <- driver$Runtime$callFunctionOn(
    "function() {
      return [...this.getClientRects()].map(rect => {
        return {x: rect.x, y: rect.y, width: rect.width, height: rect.height};
      });
    }",
    chromote_object_id(
      node_id = node_id,
      backend_id = backend_id,
      driver = driver
    ),
    returnByValue = TRUE
  )$result$value

  if (length(boxes) == 0) {
    return(NULL)
  }

  document_rect <- driver$Runtime$evaluate("{
    documentWidth: document.documentElement.clientWidth,
    documentHeight: document.documentElement.clientHeight,
  }")$result$value
  width <- document_rect$documentWidth
  height <- document_rect$documentHeight

  boxes <- lapply(boxes, intersect_box, width, height)

  box <- find_using(boxes, function(box) {
    box$width >= 1 && box$height >= 1
  })

  if (is.null(box)) {
    return(NULL)
  }

  list(
    x = box$x + box$width / 2,
    y = box$y + box$height / 2
  )
}

intersect_box <- function(box, width, height) {
  width <- max(if (box$x >= 0) {
    min(width - box$x, box$width)
  } else {
    min(width, box$width - box$x)
  }, 0)

  height <- max(if (box$y >= 0) {
    min(height - box$y, box$height)
  } else {
    min(height, box$height - box$y)
  })

  list(
    x = box$x,
    y = box$y,
    width = width,
    height = height
  )
}

ignore_error_chromote <- function(expr, message) {
  catch_error_chromote(
    expr,
    error = function(error) {
      if (error$message == message) {
        NULL
      } else {
        rlang::zap()
      }
    }
  )
}

catch_error_chromote <- function(expr, error) {
  rlang::try_fetch(
    expr,
    error = function(e) {
      parsed_error <- parse_error_chromote(e)

      if (is.null(parsed_error) || is.null(parsed_error$code)) {
        rlang::zap()
      } else {
        error(parsed_error)
      }
    }
  )
}

parse_error_chromote <- function(error) {
  err_message <- error$message

  if (is.null(err_message) || is.na(err_message)) {
    return(NULL)
  }

  code <- regmatches(err_message, regexec("code: (-?\\d+)", err_message))[[1]][2]

  if (is.na(code)) {
    code <- NULL
  }

  message <- regmatches(err_message, regexec("message: (.*)", err_message))[[1]][2]

  if (is.na(message)) {
    message <- NULL
  }

  list(
    code = code,
    message = message
  )
}
