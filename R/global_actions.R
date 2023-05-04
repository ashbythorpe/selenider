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
#' @param x A `selenider_session object. If not specified, the global session 
#'   object is used.
#' @param url The URL to navigate to.
#'
#' @details
#' Arguments to all functions are processed in a way such that enable the
#' session argument to be omitted.
#' 
#' @returns
#' The session object, invisibly
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url(session, "https://www.google.com")
#'
#' # Or:
#' open_url("https://www.google.com")
#'
open_url <- function(x, url = NULL) {
  if(inherits(x, "selenider_session")) {
    x$driver$client$navigate(url)
  } else {
    session <- get_session()
    session$driver$client$navigate(x)
    session$current_url <- x
  }
}

back <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_session()
  }
  
  x$driver$client$goBack()
}

forward <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_session()
  }
  
  x$driver$client$goForward()
}

reload <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_session()
  }
  
  x$driver$client$refresh()
}

refresh <- reload

take_screenshot <- function(x, file = NULL) {
  if(inherits(x, "selenider_session")) {
    x$driver$client$screenshot(file)
  } else {
    session <- get_session()
    session$driver$client$screenshot(x)
  }
}
