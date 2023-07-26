#' Start a session
#'
#' Begin a session in selenider, setting the session globally unless otherwise
#' specified.
#'
#' @param session The package to use as a backend.
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
#' @param extra_args A list of arguments to pass into [RSelenium::remoteDriver()]
#'   and [wdman::selenium()] if Selenium is used, or 
#'   [`chromote::ChromoteSession$new()`][chromote::ChromoteSession] if chromote is used.
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
selenider_session <- function(session = NULL,
                              browser = NULL,
                              timeout = 4,
                              driver = NULL,
                              local = TRUE,
                              quiet = TRUE,
                              .env = rlang::caller_env(),
                              extra_args = list()) {
  if (is.null(session)) {
    chromote_installed <- rlang::is_installed("chromote")
    if (!chromote_installed && !rlang::is_installed("selenium")) {
      cli::cli_abort(c(
        "One of {.pkg chromote} or {.pkg selenium} must be installed to use {.pkg selenider}."
      ))
    }

    session <- if (chromote_installed) "chromote" else "selenium"
  } else {
    session <- rlang::arg_match(session, values = c("chromote", "selenium"))

    if (session == "chromote") {
      rlang::check_installed("chromote")
    } else {
      rlang::check_installed("selenium")
    }
  }

  check_number_decimal(timeout, allow_null = TRUE)
  check_driver(driver, allow_null = TRUE)
  check_bool(local)
  check_bool(quiet)
  check_environment(.env)
  
  if (session == "chromote") {
    if (!is.null(browser) && browser != "chrome") {
      cli::cli_warn(c(
        "{.pkg chromote} only supports {.val chrome}"
      ))
    }

    browser <- "chrome"
  } else if (is.null(browser)) {
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
    browser <- arg_match0(browser, browser_opts)
    
    if (browser != "phantomjs") {
      version <- get_browser_version(browser)
    }
  }

  if (is.null(driver)) {
    if (session == "chromote") {
      driver <- create_chromote_driver(extra_args)
    } else {
      client_args <- c(
        "remoteServerAddr",
        "port",
        "browserName",
        "path",
        "version",
        "platform",
        "javascript",
        "nativeEvents",
        "extraCapabilities"
      )
      both_args <- c("port", "browserName", "path", "version", "platform", "javascript", "nativeEvents")
      only_client_args <- setdiff(client_args, both_args)
      server_args <- extra_args[setdiff(names(extra_args), only_client_args)]
      client_args <- extra_args[intersect(names(extra_args), client_args)]
      driver <- list(
        server = create_server(browser, version, quiet, server_args),
        client = create_client(browser, client_args)
      )
    }
  } else if (inherits(driver, "AppDriver")) {
    driver <- driver$get_chromote_session()
  }
  
  session <- new_selenider_session(session, driver, timeout)
  
  if (local) {
    local_session(session, .local_envir = .env)
  }
  
  session
}

create_server <- function(browser, version, quiet, extra_args) {
  chromever <- if (browser == "chrome") version else NULL
  geckover <- if (browser == "firefox") version else NULL
  iedrver <- if (browser == "internet explorer") version else NULL
  phantomver <- if (browser == "phantomjs") "latest" else NULL

  try_fetch({
    default_args <- list(
      browser = browser,
      chromever = chromever,
      geckover = geckover,
      iedrver = iedrver,
      phantomver = phantomver,
      verbose = !quiet
    )

    args_used <- default_args[setdiff(names(default_args), names(extra_args))]
    args <- c(args_used, extra_args)
    rlang::exec(wdman::selenium, !!!args)
  }, error = function(e) {
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
      } else if (browser == "chrome" && grepl("version requested doesnt match versions available", e$message)) {
        tryCatch({
          return(create_server(browser, "latest", quiet, extra_args))
        }, error = function(e) NULL)
      }

      cli::cli_abort(c(
        "The server of the session could not be started."
      ), parent = e)
    }
  )
}

create_chromote_driver <- function(extra_args) {
  rlang::check_installed("chromote")
  rlang::inject(chromote::default_chromote_object()$new_session(!!!extra_args))
}

create_client <- function(browser, extra_args) {
  default_args <- list(browserName = browser, port = 4567L)
  args_used <- default_args[setdiff(names(default_args), names(extra_args))]
  args <- c(args_used, extra_args)
  driver <- rlang::inject(RSelenium::remoteDriver(!!!args))
  
  count <- 1L
  res <- NULL
  repeat {
    res <- try_fetch(
      driver$getStatus(),
      error = function(e) {
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

  try_fetch(
    driver$open(silent = TRUE),
    error = function(e) {
      cli::cli_abort(c(
        "The client of the session ({tools::toTitleCase(browser)}) failed to start."
      ), parent = e)
    }
  )

  driver
}

new_selenider_session <- function(session, driver, timeout) {
  res <- list(
    id = round(stats::runif(1, min = 0, max = 1000000)),
    session = session,
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
  check_class(x, "selenider_session", allow_null = TRUE)

  if (is.null(x)) {
    x <- get_session(create = FALSE)
  }
  
  if (uses_selenium(x$driver)) {
    try_fetch(
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
  } else {
    invisible(x$driver$close())
  }
}

#' @export
print.selenider_session <- function(x, ...) {
  check_dots_empty()
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
