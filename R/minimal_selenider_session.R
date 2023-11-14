#' Create a session with custom HTML
#'
#' Create a `selenider_session` using custom HTML/JavaScript.
#'
#' @param html A string to use as HTML. Can also be an `xml2` object.
#' @param js A string (or `NULL`) to use as JavaScript.
#' @param ... Passed into [selenider_session()].
#' @param .env The environment in which the session will be used.
#'
#' @details
#' The function works by combining `html` and `js` into a single string, then
#' writing this to a temporary file (and opening it in the session's browser).
#'
#' @returns
#' A `selenider_session` object.
#'
#' @seealso [selenider_session()]
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' session <- minimal_selenider_session("<p>Example</p>")
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
minimal_selenider_session <- function(html,
                                      js = NULL,
                                      ...,
                                      .env = rlang::caller_env()) {
  # nocov start
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

  session <- selenider_session(..., .env = .env)
  open_url(paste0("data:text/html,", utils::URLencode(html)), session = session)
  session
} # nocov end
