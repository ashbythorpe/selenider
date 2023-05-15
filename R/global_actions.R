#' Actions on the current webpage
#'
#' @description
#' These functions perform certain actions that are specific to the current
#' session, rather than a specific element. Each can be used with or without
#' explicitly specifying the session object.
#'
#' `open_url()` navigates the webpage to a specified URL, waiting until the
#' page is considered open.
#'
#' `back()` navigates to the previously opened URL, or the previously opened
#' page in your browsing history.
#'
#' `forward()` reverses the action of `back()`, going to the next page in your
#' browsing history.
#'
#' `reload()`, or `refresh()`, reloads the current page.
#'
#' `take_screenshot()` takes a screenshot of the page currently being viewed,
#' saving it to a file.
#'
#' @param session A `selenider_session object. If not specified, the global 
#'   session object (the result of [get_session()]) is used.
#' @param url The URL to navigate to.
#' @param file The file path to save the screenshot to.
#'
#' @details
#' Arguments to all functions are processed in a way such that enable the
#' session argument to be omitted.
#' 
#' @returns
#' The session object, invisibly.
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url(session = session, "https://www.google.com")
#'
#' # Or:
#' open_url("https://www.google.com")
#'
#' @name global-actions
NULL

#' @rdname global-actions
#'
#' @export
open_url <- function(url, session = NULL) {
  if (is.null(session)) {
    session <- get_session()
  }
  
  session$driver$client$navigate(url)
  
  invisible(session)
}

#' @rdname global-actions
#'
#' @export
back <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session()
  }
  
  session$driver$client$goBack()
  
  invisible(session)
}

#' @rdname global-actions
#'
#' @export
forward <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session()
  }
  
  session$driver$client$goForward()
  
  invisible(session)
}

#' @rdname global-actions
#'
#' @export
reload <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session()
  }
  
  session$driver$client$refresh()
  
  invisible(session)
}

#' @rdname global-actions
#'
#' @export
refresh <- reload

#' @rdname global-actions
#'
#' @export
take_screenshot <- function(session, file = NULL) {
  if (is.null(session)) {
    session <- get_session()
  }
  
  session$driver$client$screenshot(file)
  
  invisible(session)
}
