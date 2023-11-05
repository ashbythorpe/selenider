#' @export
format.selenider_element <- function(x, width = getOption("width"), ..., timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  element <- get_element_for_property(
    x,
    action = paste0("Print {.arg x}"),
    timeout = timeout
  )

  html <- outer_html(element, x$session, x$driver)

  match <- regmatches(html, regexec("^(<.*?>)(.*)(</.*?>)$", html))[[1]]

  if (length(match) == 0 || identical(match[3], "")) {
    out <- encode_with_width(html, width)
  } else {
    out <- c(
      encode_with_width(match[2], width),
      paste0("  ", encode_with_width(match[3], width - 2)),
      match[4]
    )
  }

  out <- c(
    "{ selenider_element }",
    out
  )
}

#' @export
print.selenider_element <- function(x, width = getOption("width"), ..., timeout = NULL) {
  cat(format(x, width = width, ..., timeout = timeout), sep = "\n")
}

#' @export
format.selenider_elements <- function(x, width = getOption("width"), ..., n = 20, timeout = NULL) {
  timeout <- get_timeout(timeout, x$timeout)

  elements <- get_elements_for_property(
    x,
    action = paste0("Print {.arg x}"),
    timeout = timeout
  )

  length <- length(elements)

  extra <- FALSE
  if (length(elements) > n) {
    elements <- elements[seq_len(n)]
    extra <- TRUE
  }

  html <- vapply(
    elements,
    outer_html,
    character(1),
    session = x$session,
    driver = x$driver
  )

  html <- encode_with_width(html, width)

  html <- paste0(
    "[", seq_along(elements), "] ",
    html
  )

  c(
    paste0("{ selenider_elements (", length, ") }"),
    html,
    if (extra) "..."
  )
}

#' @export
print.selenider_elements <- function(x, width = getOption("width"), ..., n = 20, timeout = NULL) {
  cat(format(x, width = width, ..., n = n, timeout = timeout), sep = "\n")
}

outer_html <- function(x, session, driver) {
  html <- if (session == "chromote") {
    driver$DOM$getOuterHTML(backendNodeId = x)$outerHTML
  } else {
    execute_js_fn_on(
      "x => x.outerHTML",
      x,
      session = x$session,
      driver = driver
    )
  }
}

# Copied from xml2, see:
# https://github.com/r-lib/xml2/blob/main/R/xml_nodeset.R
encode_with_width <- function(x, width) {
  truncate_raw <- nchar(x) > width
  x[truncate_raw] <- substr(x[truncate_raw], 1L, width - 3L)
  x <- encodeString(x)
  truncate_encoded <- truncate_raw | nchar(x) > width
  x[truncate_encoded] <- paste(substr(x[truncate_encoded], 1L, width - 3L), "...")
  x
}
