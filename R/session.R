#' @export
selenider_session <- function(browser = c(
                                "chrome", "firefox", 
                                "phantomjs", "internet explorer"
                              ),
                              timeout = 4,
                              driver = NULL) {
  browser <- rlang::arg_match(browser)

  chromever <- if (browser == "chrome") "latest" else NULL
  geckover <- if (browser == "firefox") "latest" else NULL
  iedrver <- if (browser == "internet explorer") "latest" else NULL
  phantomver <- if (browser == "phantomjs") "latest" else NULL

  if (is.null(driver)) {
    driver <- RSelenium::rsDriver(
      browser = browser,
      chromever = chromever,
      geckover = geckover,
      iedrver = iedrver,
      phantomver = phantomver
    )
  }
  
  new_selenider_session(driver, timeout)
}

new_selenider_session <- function(driver, timeout) {
  res <- list(
    driver = driver,
    timeout = timeout
  )
  
  class(res) <- "selenider_session"
  
  res
}

close_session <- function(x) {
  x$client$close()
  x$server$stop()
}
