open_url <- function(x, url = NULL) {
  if(inherits(x, "selenider_session")) {
    x$driver$navigate(url)
  } else {
    session <- get_session()
    session$driver$client$navigate(x)
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
