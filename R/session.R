#' Start a session
#'
#' Begin a session in selenider, setting the session globally unless otherwise
#' specified.
#'
#' @param browser The name of the browser to run the session in; one of
#'   "chrome", "firefox", "phantomjs" or "internet explorer" (only on
#'   Windows). IF `NULL`, the function will try to work out which browser
#'   you have installed.
#' @param timeout The default time to wait when collecting an element.
#' @param driver A driver object to use instead of creating one manually.
#' @param local Whether to set the session as the local session object, 
#'   using [local_session()].
#' @param quiet Whether to let [RSelenium::rsDriver()] display messages. By
#'   default, this output is suppressed, as it is not usually useful.
#' @param .env Passed into [local_session()] function, to define the 
#'   environment in which the session is used. Change this if you want to
#'   create the session inside a function and then use it outside the
#'   function.
#'
#' @details
#' `selenider_session()` uses [RSelenium::rsDriver()] to create a browser
#' session. It then uses withr to set the session locally (unless otherwise
#' specified). To reset the local session object, use [withr::deferred_run()].
#'
#' @seealso 
#' * [close_session()] to close the session. Note that this will not reset the 
#'   result of [get_session()], which is why [withr::deferred_run()] is preferred.
#' * [local_session()] and [with_session()] to manually set the local session
#'   object.
#'
#' @returns
#' A `selenider_session` object.
#'
#' @examples
#' \dontrun{
#' # Note that these examples will create real browser sessions, so should only be
#' # run interactively.
#'
#' session <- selenider_session(browser = "firefox")
#'
#' withr::deferred_run() # Reset the session
#'
#' # If we want to use the session manually:
#' selenider_session(local = FALSE)
#'
#' session_1 <- selenider_session(timeout = 10)
#' # session_1 is the local session here
#'
#' my_function <- function() {
#'   session <- selenider_session(browser = "internet explorer")
#'
#'   # In here, session_2 is the local session
#'   get_session()
#' } # When the function finishes executing, the session is closed
#'
#' my_function() # Returns `session_2`
#'
#' # But outside the function, session_1 is the global session again
#'
#' get_session() # Returns `session_1`
#'  
#' withr::deferred_run() # Close `session_1`
#'
#' # If we want to use a session outside the scope of a function,
#' # we need to use the `.env` argument.
#' create_session <- function(timeout = 10, .env = rlang::caller_env()) {
#'   # caller_env() is the environment where the function is called
#'   selenider_session(browser = "firefox", timeout = timeout, .env = .env)
#' }
#'
#' my_session <- create_session()
#'
#' # We can now use this session outside the `create_session()` function
#' get_session()
#'
#' # `my_session` will be closed automatically.
#' }
#'
#' @export
selenider_session <- function(browser = NULL,
                              timeout = 4,
                              driver = NULL,
                              local = TRUE,
                              quiet = TRUE,
                              .env = rlang::caller_env()) {
  if (is.null(browser)) {
    bv <- find_browser_and_version()

    if (is.null(bv)) {
      stop_default_browser()
    }

    browser <- bv$browser
    version <- bv$version
  } else {
    browser <- tolower(browser)

    browser_opts <- c(
      "chrome", "firefox", 
      "phantomjs", "internet explorer"
    )

    # Allow e.g. 'Firefox'
    browser <- rlang::arg_match0(browser, browser_opts)
    
    if (browser != "phantomjs") {
      version <- get_browser_version(browser)
    }
  }

  if (is.null(driver)) {
    driver <- list(
      server = create_server(browser, version, quiet),
      client = create_client(browser)
    )
  }
  
  session <- new_selenider_session(driver, timeout)
  
  if (local) {
    local_session(session, .local_envir = .env)
  }
  
  session
}

create_server <- function(browser, version, quiet) {
  chromever <- if (browser == "chrome") version else NULL
  geckover <- if (browser == "firefox") version else NULL
  iedrver <- if (browser == "internet explorer") version else NULL
  phantomver <- if (browser == "phantomjs") "latest" else NULL

  rlang::try_fetch(
    wdman::selenium(
      browser = browser,
      chromever = chromever,
      geckover = geckover,
      iedrver = iedrver,
      phantomver = phantomver,
      verbose = !quiet
    ),
    error = function(e) {
      if (grepl("Selenium server  couldn't be started", e$message)) {
        output <- wdman::selenium(
          browser = browser,
          chromever = chromever,
          geckover = geckover,
          iedrver = iedrver,
          phantomver = phantomver,
          verbose = FALSE,
          retcommand = TRUE
        )
        
        pattern <- "'[^']+LICENSE.chromedriver'"
        license <- regmatches(output, regexpr(pattern, output))[1]
        license <- gsub("'", "", license, fixed = TRUE)

        if (!is.null(license)) {
          cli::cli_abort(c(
            "The server of the session could not be started.",
            "i" = "Try deleting the following directory:",
            " " = "{.file {license}}"
          ), parent = e)
        }
      }

      cli::cli_abort(c(
        "The server of the session could not be started."
      ), parent = e)
    }
  )
}

create_client <- function(browser) {
  driver <- RSelenium::remoteDriver(browserName = browser, port = 4567L)
  
  count <- 1L
  res <- NULL
  repeat {
    res <- rlang::try_fetch(
      driver$getStatus(),
      error = function(e) {
        print(e)
        if (count >= 5) {
          cli::cli_abort(c(
            "We could not determine whether the server was successfully started after {count} attempts."
          ), parent = e)
        }
        NULL
      }
    )

    if (is.null(res)) {
      count <- count + 1L
      Sys.sleep(1)
    } else {
       break
    }
  }

  rlang::try_fetch(
    driver$open(silent = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "The client of the session ({tools::toTitleCase(browser)}) failed to start."
      ), parent = e)
    }
  )

  driver
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

#' Close a session object
#'
#' Shut down a session object, closing the browser and stopping the server.
#'
#' @param x A `selenider_session` object. If omitted, the global session object
#'   will be closed.
#' 
#' @returns 
#' Nothing.
#' 
#' @examples
#' session <- mock_selenider_session(local = FALSE)
#'
#' close_session(session)
#' 
#' # Reopen the session, this time letting it be set locally.
#' session <- mock_selenider_session()
#'
#' # We don't have to specify the session if it is set locally.
#' close_session()
#'
#' # Since we already closed `session`, we don't need the deferred events to
#' # run
#' withr::deferred_clear()
#'
#' @export
close_session <- function(x = NULL) {
  if (is.null(x)) {
    x <- get_session(set = FALSE)
  }
  
  rlang::try_fetch(
    x$driver$client$close(),
    error = function(e) {
      x$driver$server$stop()
      cli::cli_abort(
        "Could not close session", 
        class = "selenider_error_close_session", 
        parent = e
      )
    }
  )
  
  invisible(x$driver$server$stop())
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
