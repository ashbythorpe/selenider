#' @exportS3Method xml2::read_html selenider_session
read_html.selenider_session <- function(x, encoding = "", ..., options = c("RECOVER", "NOERROR", "NOBLANKS")) {
  driver <- get_driver(x)
  
  if (uses_selenium(driver)) {
    page_source <- driver$client$getPageSource()

    if (length(page_source) == 0) {
      return(NULL)
    } else {
      x <- page_source[[1]]
    }
  } else {
    document <- driver$DOM$getDocument()
    root <- document$root$nodeId
    x <- driver$DOM$getOuterHTML(root)$outerHTML
  }

  NextMethod()
}

#' @exportS3Method xml2::read_html selenider_element
read_html.selenider_element <- function(x, encoding = "", timeout = NULL, outer = TRUE, ..., options = c("RECOVER", "NOERROR", "NOBLANKS")) {
  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(outer)

  timout <- get_timeout(timeout, x$timeout)

  driver <- x$driver

  element <- get_element_for_property(
    x,
    action = paste0("Read the HTML of {.arg x}"),
    timeout = timeout
  )

  if (outer) {
    if (uses_selenium(driver)) {
      x <- driver$executeScript("
        let element = arguments[0];
        let html = element.outerHTML;
        return html;
      ", list(element))
    } else {
      x <- driver$DOM$getOuterHTML(backendNodeId = element)$outerHTML
    }
  } else {
    x <- execute_js_fn("x => x.innerHTML", element, driver)
  }

  NextMethod()
}
