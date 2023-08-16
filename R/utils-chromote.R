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
    driver$DOM$requestNode(driver$DOM$resolveNode(backendNodeId = backend_id)$object$objectId)$nodeId
  }
}

chromote_get_xy <- function(node_id = NULL, backend_id = NULL, driver) {
  coords <- if (!is.null(node_id)) {
    driver$DOM$getBoxModel(node_id)$model$content
  } else {
    driver$DOM$getBoxModel(backendNodeId = backend_id)$model$content
  }

  x <- (coords[[1]] + coords[[3]]) / 2
  y <- (coords[[2]] + coords[[6]]) / 2
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

chromote_scroll_into_view <- function(node_id = NULL, backend_id = NULL, driver) {
  driver$Runtime$callFunctionOn("function() {
    this.scrollIntoView({
      block: 'center',
      inline: 'center',
      behaviour: 'instant',
    })
  }", chromote_object_id(node_id, backend_id, driver = driver))
}

chromote_scroll_into_view_if_needed <- function(node_id = NULL, backend_id = NULL, driver) {
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
