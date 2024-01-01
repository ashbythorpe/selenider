chromote_object_id <- function(node_id = NULL, backend_id = NULL, driver) {
  if (!is.null(node_id)) {
    driver$DOM$resolveNode(node_id)$object$objectId
  } else {
    driver$DOM$resolveNode(backendNodeId = backend_id)$object$objectId
  }
}

chromote_backend_id <- function(node_id = NULL, object_id = NULL, driver) {
  if (!is.null(node_id)) {
    driver$DOM$describeNode(node_id)$node$backendNodeId
  } else {
    driver$DOM$describeNode(objectId = object_id)$node$backendNodeId
  }
}

chromote_root_id <- function(x) {
  document <- x$DOM$getDocument()
  document$root$nodeId
}

chromote_node_id <- function(object_id = NULL, backend_id = NULL, driver) {
  if (!is.null(object_id)) {
    driver$DOM$requestNode(object_id)$nodeId
  } else {
    driver$DOM$requestNode(
      driver$DOM$resolveNode(backendNodeId = backend_id)$object$objectId
    )$nodeId
  }
}

chromote_get_xy <- function(node_id = NULL, backend_id = NULL, driver) {
  coords <- if (!is.null(node_id)) {
    driver$DOM$getBoxModel(node_id)$model$content
  } else {
    driver$DOM$getBoxModel(backendNodeId = backend_id)$model$content
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
