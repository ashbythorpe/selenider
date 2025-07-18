#' Print a live HTML element
#'
#' @description
#' Display an element or collection of elements by fetching the elements and
#' displaying their HTML contents.
#'
#' @param x A `selenider_element` or `selenider_elements` object.
#' @param width The maximum width of the output.
#' @param ... Not used.
#' @param n The maximum number of elements to print.
#' @param timeout How long to wait for `x` to exist in order to print its HTML.
#'
#' @returns `x`, invisibly.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div>
#' <p>Text 1</p>
#' <p>Text 2</p>
#' <p>Text 3</p>
#' <p>Text 4</p>
#' </div>
#' "
#'
#' session <- minimal_selenider_session(html)
#'
#' print(s("div"))
#'
#' print(ss("p"))
#'
#' print(ss("p"), n = 3)
#'
#' @export
print.selenider_element <- function(
  x,
  width = getOption("width"),
  ...,
  timeout = NULL
) {
  cat(format(x, width = width, ..., timeout = timeout), sep = "\n")

  invisible(x)
}

#' @rdname print.selenider_element
#'
#' @export
print.selenider_elements <- function(
  x,
  width = getOption("width"),
  ...,
  n = 20,
  timeout = NULL
) {
  cat(format(x, width = width, ..., n = n, timeout = timeout), sep = "\n")

  invisible(x)
}

#' @export
format.selenider_element <- function(
  x,
  width = getOption("width"),
  ...,
  timeout = NULL
) {
  timeout <- get_timeout(timeout, x$timeout)

  session <- x$session
  driver <- x$driver

  html <- perform_action(
    x,
    action = function(x) element_outer_html(x, session, driver),
    action_name = "print {.arg x}",
    timeout = timeout
  )

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
format.selenider_elements <- function(
  x,
  width = getOption("width"),
  ...,
  n = 20,
  timeout = NULL
) {
  timeout <- get_timeout(timeout, x$timeout)

  session <- x$session
  driver <- x$driver

  perform_action(
    x,
    action = function(x) format_elements(as.list(x), width, n, session, driver),
    action_name = "print {.arg x}",
    timeout = timeout
  )
}

format_elements <- function(elements, width, n, session, driver) {
  length <- length(elements)

  extra <- FALSE
  if (length(elements) > n) {
    elements <- elements[seq_len(n)]
    extra <- TRUE
  }

  html <- vapply(
    elements,
    element_outer_html,
    character(1),
    session = session,
    driver = driver
  )

  html <- encode_with_width(html, width)

  html <- paste0(
    "[",
    seq_along(elements),
    "] ",
    html
  )

  c(
    paste0("{ selenider_elements (", length, ") }"),
    html,
    if (extra) "..."
  )
}

element_outer_html <- function(x, session, driver) {
  html <- if (session == "chromote") {
    wrap_error_chromote(driver$DOM$getOuterHTML(backendNodeId = x)$outerHTML)
  } else {
    execute_js_fn_on(
      "x => x.outerHTML",
      x,
      session = session,
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
  x[truncate_encoded] <-
    paste(substr(x[truncate_encoded], 1L, width - 3L), "...")
  x
}
