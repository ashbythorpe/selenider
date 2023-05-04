#' @export
mock_selenider_session <- function(browser = c(
                                     "chrome", "firefox", 
                                     "phantomjs", "internet explorer"
                                   ),
                                   timeout = 4,
                                   driver = NULL,
                                   global = TRUE) {
  browser <- rlang::arg_match(
    browser,
    c(browser, tools::toTitleCase(browser), "PhantomJS")
  )
  
  session <- new_selenider_session(driver = NULL, timeout)
  
  if (global) {
    set_session(session)
  }
  
  session
}
