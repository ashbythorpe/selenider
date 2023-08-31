#' Start a session
#'
#' @description
#' Begin a session in selenider, setting the session globally unless otherwise
#' specified.
#'
#' `create_chromote_session()`, `create_selenium_client()` and `create_selenium_server()`
#' are low-level functions that allow more control over making a web driver, which
#' can then be passed into the `driver` argument to `selenider_session()`.
#'
#' @param session The package to use as a backend: either "chromote" or
#'   "selenium". By default, chromote is used, since this tends to be
#'   faster and more reliable.
#' @param browser The name of the browser to run the session in; one of
#'   "chrome", "firefox", "phantomjs" or "internet explorer" (only on
#'   Windows). IF `NULL`, the function will try to work out which browser
#'   you have installed. If we are using chromote, this option is ignored,
#'   since chromote only works on Chrome.
#' @param view Whether to open the browser and view it, for visual testing.
#'   This is ignored if session is "selenium", since selenium drivers cannot
#'   be headless.
#' @param timeout The default time to wait when collecting an element.
#' @param driver A driver object to use instead of creating one manually. This
#'   can be one of: 
#'   * A [chromote::ChromoteSession] object (the result of `create_chromote_session()`).
#'   * A [shinytest2::AppDriver] object.
#'   * An [RSelenium::remoteDriver()] object (the result of `create_selenium_client()`).
#'   * A Selenium server object (the result of [wdman::selenium()], or 
#'     `create_selenium_server()`). In this case, the client object will be created using
#'     the server object.
#'   * A list/environment containing the [RSelenium::remoteDriver()] object, the Selenium
#'     server object, or both.
#'   See Details for more information about providing a custom driver object.
#' @param local Whether to set the session as the local session object, 
#'   using [local_session()].
#' @param quiet Whether to let [RSelenium::rsDriver()] display messages. By
#'   default, this output is suppressed, as it is not usually useful. Chromote
#'   does not display any output when creating a session.
#' @param .env Passed into [local_session()] function, to define the 
#'   environment in which the session is used. Change this if you want to
#'   create the session inside a function and then use it outside the
#'   function.
#' @param ... Arguments to finetune the creation of the specific driver. 
#'   * For `create_chromote_session()`, these are passed into 
#'     [`chromote::ChromoteSession$new()`][chromote::ChromoteSession].
#'  * For `create_selenium_server()`, these are passed into [wdman::selenium()].
#'  * For `create_selenium_server()`, these are passed into [RSelenium::remoteDriver()].
#'
#' @details
#' # Structure of a selenider session
#' A `selenider_session` object is an S3 list, meaning its properties can be accessed using `$`.
#' Most notably, using `session$driver` allows access to the driver object which actually controls
#' the browser. If you are using Selenium, use `session$driver$client` to access the `remoteDriver`
#' object. These objects are useful if you want to do something with the driver that is not directly
#' supported by selenider. See [get_actual_element()] for some examples of this.
#'
#' # Custom drivers
#' Custom driver objects are good if you want more low-level control over the underlying
#' functions that create the webdrivers that actually control the browser. However, it is
#' recommended to use the selenider functions (e.g. `create_selenium_client()`) over 
#' `wdman::selenium()` for better error messages and more reliable behaviour.
#'
#' # TODO: Add vignette references when available
#'
#' ## Chromote
#' Supplying a custom [chromote::ChromoteSession] object can allow you to manage the
#' underlying [chromote::Chromote] process that is used to spawn sessions. For example:
#' ```
#' my_chromote_object <- chromote::Chromote$new()
#'
#' session <- selenider_session(
#'   driver = create_chromote_session(parent = my_chromote_object)
#' )
#' ```
#'
#' You can also supply a [shinytest2::AppDriver] object, allowing selenider and
#' shinytest2 to share a session:
#'
#' ```
#' shiny_app <- shiny::shinyApp(
#'   ui = shiny::fluidPage(
#'     # ... Your UI
#'   ),
#'   server = function(input, output) {
#'     # ... Your server
#'   }
#' )
#'
#' app <- shinytest2::AppDriver$new()
#'
#' session <- selenider_session(
#'   driver = app
#' )
#' ```
#'
#' ## RSelenium
#' If you want to manually create both the client and the server, you can
#' do the equivalent of the following:
#' ```
#' session <- selenider_session(
#'   driver = list(
#'     client = create_selenium_client("chrome"),
#'     server = create_selenium_server("chrome")
#'   )
#' )
#' ```
#'
#' However, it can sometimes be useful to omit the server, for example when you are running
#' the Selenium server using Docker. In this case, you need to make sure the ip and port are
#' matched correctly.
#'
#' ```
#' session <- selenider_session(
#'   driver = create_selenium_client(
#'     remoteServerAddr = "<IP ADDRESS>",
#'     port = 1234L
#'   )
#' )
#' ```
#'
#' @seealso 
#' * [close_session()] to close the session. Note that this will not reset the 
#'   result of [get_session()], which is why [withr::deferred_run()] is preferred.
#' * [local_session()] and [with_session()] to manually set the local session
#'   object.
#'
#' @returns
#' A `selenider_session` object. Use `session$driver` to retrieve the driver object that controls the
#' browser.
#'
#' @examplesIf selenider_available()
#'
#' session_1 <- selenider_session(timeout = 10)
#' # session_1 is the local session here
#'
#' get_session() # Returns session 1
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' my_function <- function() {
#'   session_2 <- selenider_session()
#'
#'   # In here, session_2 is the local session
#'   get_session()
#'   \dontshow{
#'   # Clean up all connections and invalidate default chromote object
#'   selenider_cleanup()
#'   }
#' } # When the function finishes executing, the session is closed
#'
#' my_function() # Returns `session_2`
#'
#' # If we want to use a session outside the scope of a function,
#' # we need to use the `.env` argument.
#' create_session <- function(timeout = 10, .env = rlang::caller_env()) {
#'   # caller_env() is the environment where the function is called
#'   selenider_session(timeout = timeout, .env = .env)
#' }
#'
#' my_session <- create_session()
#'
#' # We can now use this session outside the `create_session()` function
#' get_session()
#'
#' # `my_session` will be closed automatically.
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
selenider_session <- function(session = NULL,
                              browser = NULL,
                              view = FALSE,
                              timeout = 4,
                              driver = NULL,
                              local = TRUE,
                              quiet = TRUE,
                              .env = rlang::caller_env()) {
  if (is.null(session)) {
    chromote_installed <- rlang::is_installed("chromote")
    if (!chromote_installed && !rlang::is_installed("RSelenium")) {
      cli::cli_abort(c(
        "One of {.pkg chromote} or {.pkg RSelenium} must be installed to use {.pkg selenider}."
      ))
    }

    session <- if (chromote_installed) "chromote" else "selenium"
  } else {
    session <- rlang::arg_match(session, values = c("chromote", "selenium"))

    if (session == "chromote") {
      rlang::check_installed("chromote")
    } else {
      rlang::check_installed("RSelenium")
      rlang::check_installed("wdman")
    }
  }

  check_bool(view)
  if (!view && session == "selenium") {
    cli::cli_warn("{.arg session} is {.val selenium}, ignoring {.arg view}.")
  }
  check_number_decimal(timeout, allow_null = TRUE)
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
  } else if (is.null(browser) && (is.null(driver) || is_selenium_server(driver))) {
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
      driver <- create_chromote_session()
      if (view) {
        driver$view()
      }
    } else {
      driver <- list(
        server = create_selenium_server(browser, version = version, quiet = quiet),
        client = create_selenium_client(browser)
      )
    }
  } else {
    driver <- check_supplied_driver(driver, browser = browser)
  }
  
  session <- new_selenider_session(session, driver, timeout)
  
  if (local) {
    local_session(session, .local_envir = .env)
    if (inherits(session$driver, "ChromoteSession")) {
      withr::defer({
        # Delete the Crashpad folder if it exists
        unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
      }, envir = .env)
      timeout <- if (on_ci()) 60 * 5 else 60
      withr::local_options(list(chromote.timeout = timeout), .local_envir = .env)
    }
  }
  
  session
}

#' @rdname selenider_session
#'
#' @export
create_chromote_session <- function(...) {
  rlang::check_installed("chromote")
  timeout <- if (on_ci()) 60 * 5 else 60
  withr::with_options(list(chromote.timeout = timeout), {
    chromote::ChromoteSession$new(...)
  })
}

#' @rdname selenider_session
#'
#' @param version The version of the webdriver (chromedriver, geckodriver, etc.)
#'   to use. You should only need to change this if Chrome is being used, as the
#'   version of the driver depends on the version of Chrome.
#' @param port The port to run Selenium on.
#'
#' @export
create_selenium_server <- function(browser, version = "latest", port = 4567L, quiet = TRUE, ...) {
  chromever <- if (browser == "chrome") version else NULL
  geckover <- if (browser == "firefox") version else NULL
  iedrver <- if (browser == "internet explorer") version else NULL
  phantomver <- if (browser == "phantomjs") "latest" else NULL

  try_fetch({
    wdman::selenium(
      port = port,
      chromever = chromever,
      geckover = geckover,
      iedrver = iedrver,
      phantomver = phantomver,
      verbose = !quiet,
      ...
    )
  }, error = function(e) {
      if (grepl("couldn't be started", e$message)) {
        output <- wdman::selenium(
          port = port,
          chromever = chromever,
          geckover = geckover,
          iedrver = iedrver,
          phantomver = phantomver,
          verbose = FALSE,
          retcommand = TRUE,
          ...
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
        # If the version of chrome we found is not available as a webdriver, use the latest one instead.
        return(create_selenium_server(browser, port = port, version = "latest", quiet = quiet, ...))
      }

      cli::cli_abort(c(
        "The server of the session could not be started."
      ), parent = e)
    }
  )
}

#' @rdname selenider_session
#'
#' @export
create_selenium_client <- function(browser, port = 4567L, ...) {
  driver <- RSelenium::remoteDriver(browserName = browser, port = port, ...)
  
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

#' Check the driver argument to selenider_server()
#'
#' If the `driver` argument to `selenider_server()` is not `NULL`, check it and convert it
#' into a valid/consistent session object, throwing an error if it is invalid.
#'
#' @param x The supplied driver.
#' @param browser The supplied/calculated browser.
#' @param call The environment to throw errors in.
#'
#' @returns
#' Either a `chromote::ChromoteSession` object, or a named list with two items:
#' * `client` - A `remoteDriver` object.
#' * `server` - A Selenium server object (the result of [wdman::selenium()]). This
#'   is optional, and will not be included if the user only supplied a client.
#'
#' @noRd
check_supplied_driver <- function(x, browser = NULL, call = rlang::caller_env()) {
  if (inherits(x, "ChromoteSession")) {
    x
  } else if (inherits(x, "AppDriver")) {
    x$get_chromote_session()
  } else if (is_selenium_server(x)) {
    port <- find_port_from_server(x, call = call)
    client <- create_selenium_client(browser, port = port)

    list(client = client, server = server)
  } else if (is_selenium_client(x)) {
    list(client = x)
  } else if (is.list(x) || is.environment(x)) {
    nms <- names(x)

    client <- if ("client" %in% nms) check_selenium_client(x$client, call = call) else NULL
    server <- if ("server" %in% nms) check_selenium_server(x$server, call = call) else NULL

    if (is.null(client)) {
      client <- find_using(x, is_selenium_client)
    }

    if (is.null(server)) {
      server <- find_using(x, is_selenium_server)
    }

    if (is.null(client) && is.null(server)) {
      cli::cli_abort(c(
        "{.arg driver} is a list, but seems to contain neither a Selenium client, nor a Selenium server."
      ))
    } else if (is.null(client)) {
      bv <- find_browser_and_version()

      if (is.null(bv)) {
        stop_default_browser(call = call)
      }

      browser <- bv$browser
      port <- find_port_from_server(x, call = call)
      client <- create_selenium_client(browser, port = port)
      result <- list(
        client = client,
        server = server
      )
    } else if (is.null(server)) {
      result <- list(client = client)
    } else {
      result <- list(client = client, server = server)
    }

    if (is.environment(x)) {
      # In case `x` is the result of `RSelenium::rsDriver`, make sure the `reg.finalizer` method
      # does not close the session when the environment is garbage collected.
      x$client <- list(close = function() {})
      x$server <- list(stop = function() {})
    }

    result
  } else {
    cls <- c("ChromoteSession", "AppDriver", "remoteDriver")
    what <- cli::format_inline("A {.cls {cls}} object, a list or an environment")
    stop_input_type(x, what, call = call)
  }
}

is_selenium_server <- function(x) {
  is.list(x) && all(c("process", "log", "stop") %in% names(x))
}

check_selenium_server <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_server(x)) {
    cli::cli_abort(c(
      "{.code driver$server} must be a valid Selenium server object",
      "i" = "This can be the result of {.fun selenider::create_selenium_server} or {.fun wdman::selenium}."
    ), call = call)
  }
  x
}

is_selenium_client <- function(x) {
  inherits(x, "remoteDriver")
}

check_selenium_client <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_client(x)) {
    cli::cli_abort(c(
      "{.code driver$client} must be a {.cls remoteDriver} object",
      "i" = "This can be the result of {.fun selenider::create_selenium_client} or {.fun RSelenium::rsDriver}."
    ), call = call)
  }
  x
}

find_port_from_server <- function(x, call = rlang::caller_env()) {
  log <- x$log()

  port <- find_port_from_logs(log$stderr)

  if (is.na(port)) {
    port <- find_port_from_logs(log$stdout)
  }

  if (is.na(port)) {
    cli::cli_warn(c(
      "Could not find port number from server object.",
      "i" = "Using default port {.val {4567L}}"
    ), call = call)
    port <- 4567L
  }

  port
}

find_port_from_logs <- function(x) {
  matches <- regmatches(x, regexec("port ([0-9]+)", x))
  port_matches <- stats::na.omit(unlist(lapply(matches, function(x) x[2])))
  numeric_ports <- suppressWarnings(as.integer(port_matches))
  stats::na.omit(numeric_ports)[1]
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
#' @examplesIf selenider_available()
#' session <- selenider_session(local = FALSE)
#'
#' close_session(session)
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup(FALSE)
#' }
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
        if ("server" %in% names(x$driver)) {
          x$driver$server$stop()
        }
        cli::cli_abort(
          "Could not close session", 
          class = "selenider_error_close_session", 
          parent = e
        )
      }
    )

    if ("server" %in% names(x$driver)) {
      invisible(x$driver$server$stop())
    }
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
