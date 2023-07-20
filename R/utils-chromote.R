chromote_object_id <- function(x, driver) {
  driver$DOM$resolveNode(x)$object$objectId
}

chromote_root_id <- function(x) {
  document <- x$DOM$getDocument()
  document$root$nodeId
}

chromote_node_id <- function(x, driver) {
  driver$DOM$requestNode(x)$nodeId
}

chromote_get_xy <- function(x, driver) {
  coords <- driver$DOM$getBoxModel(x)$model$content
  x <- (coords[[1]] + coords[[3]]) / 2
  y <- (coords[[2]] + coords[[6]]) / 2
  list(x = x, y = y)
}

chromote_is_in_view <- function(x, driver) {
  layout <- driver$Page$getLayoutMetrics()$cssLayoutViewport
  width <- layout$clientWidth
  height <- layout$clientHeight
  coords <- chromote_get_xy(x, driver)
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

chromote_scroll_into_view <- function(x, driver) {
  driver$Runtime$callFunctionOn("function() { 
    this.scrollIntoView({
      block: 'center',
      inline: 'center',
      behaviour: 'instant',
    }) 
  }", chromote_object_id(x))
}

chromote_scroll_into_view_if_needed <- function(x, driver) {
  if (!isTRUE(chromote_is_in_view(x, driver))) {
    chromote_scroll_into_view(x, driver)
  }
}

chromote_press <- function(driver, ...) {
  driver$Input$dispatchKeyEvent(type = "keyDown", ...)
  driver$Input$dispatchKeyEvent(type = "keyUp", ...)
}
