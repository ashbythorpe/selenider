#' Create a session with custom HTML
#'
#' Create a `selenider_session` using custom HTML/JavaScript. Similar to
#' [rvest::minimal_html()].
#'
#' @param html A string to use as HTML.
#' @param js A string (or `NULL`) to use as JavaScript.
#' @param ... Passed into [selenider_session()].
#' @param .env The environment in which the session will be used.
#'
#' @returns
#' A `selenider_session` object.
#'
#' @export
minimal_selenider_session <- function(html, js = NULL, ..., .env = rlang::caller_env()) {
  check_string(js, allow_null = TRUE)
  if (!is.character(html) || length(html) != 1) {
    if (inherits_any(html, c("xml_missing", "xml_node", "xml_nodeset"))) {
      html <- paste0(as.character(html), sep = "\n")
    }
  }
  if (!grepl("doctype", tolower(html), fixed = TRUE)) {
    html <- paste0(
      "<!DOCTYPE html>\n",
      html
    )
  }

  if (!is.null(js)) {
    html <- paste0(
      html,
      "<script>\n",
      js,
      "</script>"
    )
  }

  file <- withr::local_tempfile(fileext = ".html", .local_envir = .env)
  print(file)
  writeLines(html, file(file))
  session <- selenider_session(..., .env = .env)
  open_url(paste0("file://", file), session = session)
  session
}
