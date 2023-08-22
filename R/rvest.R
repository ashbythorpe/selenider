#' Compatibility with rvest
#'
#' @description
#' These functions ensure compatibility with `rvest` and `xml2`.
#'
#' `selenider_prefer()` ensures that all functions common to `selenider` and
#' `rvest` (e.g. `html_attr()`) work on both selenider elements and rvest node
#' objects. `conflicted` is used to resolve function conflicts.
#'
#' selenider allows [xml2::read_html()] and [rvest::html_table()] to work
#' on `selenider_element` and `selenider_session` objects.
#'
#' @param quiet If `TRUE`, output from [conflicted::conflict_prefer()] will
#'   be suppressed.
#' @param x A `selenider_session`/`selenider_element` object.
#' @param timeout How long to wait for `x` to exist in the DOM before throwing
#'   an error.
#' @param outer Whether to read the inner (all children of the current element) or
#'   outer (including the element itself) HTML of `x`.
#' @param encoding,...,options Passed into [xml2::read_html()].
#' @param header,trim,fill,dec,na.strings,convert Passed into [rvest::html_table()].
#'
#' @returns
#' `read_html()` returns an XML document.
#' `html_table()` returns a tibble or list of tibbles (when applied to a page).
#'
#' @examples
#' selenider_prefer(quiet = FALSE)
#'
#' @export
selenider_prefer <- function(quiet = TRUE) {
  conflicted::conflict_prefer("html_attr", winner = "selenider", quiet = quiet)
  conflicted::conflict_prefer("html_attrs", winner = "selenider", quiet = quiet)
  conflicted::conflict_prefer("html_element", winner = "selenider", quiet = quiet)
  conflicted::conflict_prefer("html_elements", winner = "selenider", quiet = quiet)
  conflicted::conflict_prefer("html_name", winner = "selenider", quiet = quiet)
  conflicted::conflict_prefer("html_text", winner = "selenider", quiet = quiet)
  invisible(NULL)
}

#' @rdname selenider_prefer
#'
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

#' @rdname selenider_prefer
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
      x <- driver$executeScript("
        let element = arguments[0];
        let html = element.outerHTML;
        return html;
      ", list(element))
    } else {
      x <- driver$DOM$getOuterHTML(backendNodeId = element)$outerHTML
    }
  } else {
    x <- execute_js_fn("x => x.innerHTML", element, driver = driver)
  }

  NextMethod()
}

#' @rdname selenider_prefer
#'
#' @exportS3Method rvest::html_table selenider_session
html_table.selenider_session <- function(x,
                                         header = NA,
                                         trim = TRUE,
                                         fill = deprecated(),
                                         dec = ".",
                                         na.strings = "NA",
                                         convert = TRUE) {
  x <- read_html(x)
  NextMethod()
}

#' @rdname selenider_prefer
#'
#' @exportS3Method rvest::html_table selenider_element
html_table.selenider_element <- function(x,
                                         header = NA,
                                         trim = TRUE,
                                         fill = deprecated(),
                                         dec = ".",
                                         na.strings = "NA",
                                         convert = TRUE) {
  x <- read_html(x)
  NextMethod()
}
