#' @export
selenider_session <- function(browser = c(
                                "chrome", "firefox", 
                                "phantomjs", "internet explorer"
                              ),
                              timeout = 4,
                              driver = NULL,
                              global = TRUE) {
  # Allow e.g. 'Firefox'
  browser <- rlang::arg_match(
    browser,
    c(browser, tools::toTitleCase(browser), "PhantomJS")
  )

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
  
  session <- new_selenider_session(driver, timeout)
  
  if (global) {
    set_session(session)
  }
  
  session
}

new_selenider_session <- function(driver, timeout) {
  res <- list(
    driver = driver,
    timeout = timeout,
    start_time = Sys.time(),
    current_url = NULL,
    start_time = Sys.time()
  )
  
  class(res) <- "selenider_session"
  
  res
}

#' @export
close_session <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_session()
  }
  
  x$client$close()
  x$server$stop()
}

#' @export
print.selenider_session <- function(x, ...) {
  time <- as_pretty_dt(prettyunits::pretty_dt(Sys.time() - x$start_time))
  
  current_url <- if (is.null(x$current_url)) {
    "Nothing" 
  } else {
    paste0("{.url ", x$current_url, "}")
  }
  
  timeout <- as_pretty_dt(prettyunits::pretty_sec(x$timeout))
  
  cli::cli({
    cli::cli_text("A selenider session object")
    cli::cli_bullets(c(
      "*" = "Open for {.val {time}}",
      "*" = "Browser: {.val {x$driver$client$browserName}}",
      "*" = "Port: {.val {x$driver$client$port}}",
      "*" = paste0("Currently opened: ", current_url),
      "*" = "Timeout: {.val {timeout}}"
    ))
  })
}
