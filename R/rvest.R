#' Read a live HTML document
#'
#' @description
#' [xml2::read_html()] can be used on a selenider session to read the HTML of
#' the entire page, or on a selenider element to get the HTML of that element.
#'
#' @param x A `selenider_session`/`selenider_element` object.
#' @param timeout How long to wait for `x` to exist in the DOM before throwing
#'   an error.
#' @param outer Whether to read the inner (all children of the current element) or
#'   outer (including the element itself) HTML of `x`.
#' @param encoding,...,options Passed into [xml2::read_html()].
#'
#' @returns
#' `read_html()` returns an XML document. Note that HTML will always be wrapped
#' in a `<html>` and `<body>` tag, if it isn't already.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' library(rvest)
#'
#' html <- "
#' <div>
#' <p>Example text</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' read_html(session)
#' read_html(s("div"))
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @exportS3Method xml2::read_html selenider_session
read_html.selenider_session <- function(x, encoding = "", ..., options = c("RECOVER", "NOERROR", "NOBLANKS")) {
  driver <- get_driver(x)

  if (uses_selenium(driver)) {
    x <- unpack_list(driver$getPageSource())
  } else {
    document <- driver$DOM$getDocument()
    root <- document$root$nodeId
    x <- driver$DOM$getOuterHTML(root)$outerHTML
  }

  NextMethod()
}

#' @rdname read_html.selenider_session
#'
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
      x <- unpack_list(driver$executeScript("
        let element = arguments[0];
        let html = element.outerHTML;
        return html;
      ", list(element)))
    } else {
      x <- driver$DOM$getOuterHTML(backendNodeId = element)$outerHTML
    }
  } else {
    x <- unpack_list(execute_js_fn_on("x => x.innerHTML", element, driver = driver))
  }

  NextMethod()
}
