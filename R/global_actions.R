#' Open a URL
#' 
#' Navigate the browser to specified URL, waiting until the page is considered

#' open before finishing.
#' 
#' @param url The URL to navigate to; a string.
#' @inheritParams back
#'
#' @returns
#' The session object, invisibly.
#' 
#' @family global actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url("https://www.google.com")
#'
#' # Or:
#' open_url(session = session, "https://www.google.com")
#'
#' @export
open_url <- function(url, session = NULL) {
  if (is.null(session)) {
    session <- get_session(.env = rlang::caller_env())
  }
  
  session$driver$client$navigate(url)
  
  invisible(session)
}

#' Move back or forward in browsing history
#' 
#' @description
#' `back()` navigates to the previously opened URL, or the previously opened
#' page in your browsing history.
#'
#' `forward()` reverses the action of `back()`, going to the next page in your
#' browsing history.
#' 
#' @param session A `selenider_session object. If not specified, the global 
#'   session object (the result of [get_session()]) is used.
#'
#' @returns
#' The session object, invisibly.
#' 
#' @family global actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url("https://www.google.com")
#'
#' open_url("https://www.tidyverse.org/")
#' 
#' back()
#' 
#' forward()
#'
#' @export
back <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session(.env = rlang::caller_env())
  }
  
  session$driver$client$goBack()
  
  invisible(session)
}

#' @rdname back
#'
#' @export
forward <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session(.env = rlang::caller_env())
  }
  
  session$driver$client$goForward()
  
  invisible(session)
}

#' Reload the current page
#' 
#' `reload()` and `refresh()` both reload the current page.
#'
#' @inheritParams back
#'
#' @returns
#' The session object, invisibly.
#' 
#' @family global actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url("https://www.google.com")
#'
#' reload()
#'
#' @export
reload <- function(session = NULL) {
  if (is.null(session)) {
    session <- get_session(.env = rlang::caller_env())
  }
  
  session$driver$client$refresh()
  
  invisible(session)
}

#' @rdname reload
#'
#' @export
refresh <- reload

#' Take a screenshot of the current page
#' 
#' Take a screenshot of the current session state, saving this image to a file.
#' 
#' @param file The file path to save the screenshot to.
#' @inheritParams back
#'
#' @returns
#' The session object, invisibly.
#' 
#' @family global actions
#'
#' @examples
#' session <- mock_selenider_session()
#'
#' open_url("https://www.google.com")
#' 
#' file_path <- tempfile(fileext = "jpeg")
#'
#' take_screenshot(file_path)
#'
#' @export
take_screenshot <- function(file = NULL, session = NULL) {
  if (is.null(session)) {
    session <- get_session(.env = rlang::caller_env())
  }
  
  session$driver$client$screenshot(file)
  
  invisible(session)
}
