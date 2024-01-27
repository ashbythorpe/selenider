#' Start a session
#'
#' @description
#' Create a session in selenider, setting it as the local session unless
#' otherwise specified, allowing the session to be accessed globally in the
#' environment where it was defined.
#'
#' @param session The package to use as a backend: either "chromote",
#'   "selenium" or "rselenium". By default, chromote is used, since this tends
#'   to be faster and more reliable. Change the default value using the
#'   `selenider.session` option.
#' @param browser The name of the browser to run the session in; one of
#'   "chrome", "firefox", "edge", "safari", or another valid browser name.
#'   If `NULL`, the function will try to work out which browser you have
#'   installed. If we are using chromote, this option is ignored, since
#'   chromote only works on Chrome. Change the default value of this parameter
#'   using the `selenider.browser` option.
#' @param timeout The default time to wait when collecting an element.
#' @param options A [chromote_options()] or [selenium_options()] object,
#'   used to specify options that are specific to chromote or selenium. See
#'   Details for some useful examples of this.
#' @param driver A driver object to use instead of creating one manually. This
#'   can be one of:
#'   * A [chromote::ChromoteSession] object.
#'   * A [shinytest2::AppDriver] object.
#'   * An [selenium::SeleniumSession] object.
#'   * A Selenium server object, created by [selenium::selenium_server()] or
#'     [wdman::selenium()]. In this case, a client will be created using the
#'     server object.
#'   * A list/environment containing the [selenium::SeleniumSession] object,
#'     the Selenium server object, or both.
#'   * An [RSelenium::remoteDriver()] object can be used instead of a
#'     [selenium::SeleniumSession] object.
#' @param local Whether to set the session as the local session object,
#'   using [local_session()].
#' @param .env Passed into [local_session()], to define the
#'   environment in which the session is used. Change this if you want to
#'   create the session inside a function and then use it outside the
#'   function.
#' @param view,selenium_manager,quiet `r lifecycle::badge("deprecated")`
#'   Use the `options` argument instead.
#'
#' @details
#' # Useful session-specific options
#' See [chromote_options()] and [selenium_options()] for the full range.
#'
#' ## Making a chromote session non-headless
#' By default, chromote will run in headless mode, meaning that you won't
#' actually be able to see the browser as you control it. Use the `view`
#' argument to [chromote_options()] to change this:
#'
#' ``` r
#' session <- selenider_session(
#'   options = chromote_options(view = TRUE)
#' )
#' ```
#'
#' ## Prevent creation of a selenium server
#' Sometimes, you want to manage the Selenium server separately, and only let
#' selenider create client objects to attach to the server. You can do this by
#' passing `NULL` into the `server_options` argument to [selenium_options()]:
#'
#' ``` r
#' session <- selenider_session(
#'   "selenium",
#'   options = selenium_options(server_options = NULL)
#' )
#' ```
#'
#' If the port you are using is not 4444, you will need to pass in the `port`
#' argument to [selenium_client_options()] as well:
#'
#' ``` r
#' session <- selenider_session(
#'   "selenium",
#'   options = selenium_options(
#'     client_options = selenium_client_options(port = YOUR_PORT),
#'     server_options = NULL
#'   )
#' )
#' ```
#'
#' One example of when this may be useful is when you are managing the Selenium
#' server using Docker.
#'
#' ## Store the Selenium server persistently
#' By default, selenium will download and store the Selenium server JAR file
#' in a temporary directory, which will be deleted when the R session finishes.
#' This means that every time you start a new R session, this file will be
#' re-downloaded. You can store the JAR file permanently using the `temp`
#' argument to [selenium_server_options()]:
#'
#' ``` r
#' session <- selenider_session(
#'   "selenium",
#'   options = selenium_options(
#'     server_options = selenium_server_options(temp = TRUE)
#'   )
#' )
#' ```
#'
#' The downside of this is you may end up using a lot of storage, especially
#' if a new version of Selenium is released and the old server file is left
#' on the filesystem.
#'
#' You can also use the `path` argument to [selenium_server_options()] to
#' specify the directory where the JAR file should be stored.
#'
#' # Structure of a selenider session
#' A `selenider_session` object has several components that can be useful to
#' access:
#'
#' * `session` - The type of session, either `"chromote"` or `"selenium"`.
#' * `driver` - The driver object used to control the browser. This is either a
#'   [chromote::ChromoteSession] or [selenium::SeleniumSession] object. This is
#'   useful if you want to do something with the driver that is not directly
#'   supported by selenider. See [get_actual_element()] for some examples of
#'   this.
#' * `server` - The Selenium server object, if one was created or passed in.
#' * `id` - A unique ID that can be used to identify the session.
#'
#' Access these components using `$` (e.g. `session$driver`).
#'
#' # Custom drivers
#' If you want complete manual control over creating the underlying driver,
#' you can pass your own `driver` argument to stop selenider from creating
#' the driver for you.
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
#' @seealso
#' * [close_session()] to close the session. Note that this will not reset the
#'   result of [get_session()], which is why [withr::deferred_run()] is
#'   preferred.
#' * [local_session()] and [with_session()] to manually set the local session
#'   object (and [get_session()] to get it).
#' * [open_url()], [s()] and [find_elements()] to get started once you have
#'   created a session.
#'
#' @returns
#' A `selenider_session` object. Use `session$driver` to retrieve the driver
#' object that controls the browser.
#'
#' @examplesIf selenider::selenider_available()
#'
#' session_1 <- selenider_session(timeout = 10)
#' # session_1 is the local session here
#'
#' get_session() # Returns session 1
#'
#' my_function <- function() {
#'   session_2 <- selenider_session()
#'
#'   # In here, session_2 is the local session
#'   get_session()
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
#' @export
selenider_session <- function(session = getOption("selenider.session"),
                              browser = getOption("selenider.browser"),
                              timeout = 4,
                              options = NULL,
                              driver = NULL,
                              local = TRUE,
                              .env = rlang::caller_env(),
                              view = FALSE,
                              selenium_manager = TRUE,
                              quiet = TRUE) {
  if (isTRUE(view)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "selenider_session(view)",
      I("`options = chromote_options(headless = FALSE)`")
    )

    if (inherits(options, "chromote_options")) {
      options$headless <- FALSE
    }
  }

  if (!isTRUE(selenium_manager)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "selenider_session(selenium_manager)",
      I("`options = selenium_options(server_options = wdman_server_options())`")
    )

    if (inherits(options, "selenium_options") && !inherits(options$server_options, "wdman_server_options")) {
      options$server_options <- wdman_server_options()
    }
  }

  if (!isTRUE(quiet)) {
    if (inherits(options$server_options, "wdman_server_options")) {
      lifecycle::deprecate_warn(
        "0.3.0",
        "selenider_session(quiet)",
        I("`options = selenium_options(server_options = wdman_server_options(verbose = TRUE))`")
      )

      options$server_options$verbose <- TRUE
    } else {
      lifecycle::deprecate_warn(
        "0.3.0",
        "selenider_session(quiet)",
        I("`options = selenium_options(server_options = selenium_server_options(verbose = TRUE))`")
      )

      if (inherits(options$server_options, "selenium_server_options")) {
        options$server_options$verbose <- TRUE
      }
    }
  }



  check_string(session, allow_null = TRUE)
  check_string(browser, allow_null = TRUE)
  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(local)
  check_environment(.env)

  session <- check_session_dependencies(session, options)

  options <- check_options(session, options)

  browser_version <- browser_and_version(
    session,
    browser = browser,
    driver = driver
  )

  browser <- browser_version$browser

  if (inherits(options, "selenium_options") &&
    inherits(options$server_options, "wdman_server_options") && # nolint: indentation_linter
    is.null(options$server_options$version)) {
    options$server_options$version <- browser_version$version
  }

  driver <- get_driver(
    browser = browser,
    options = options,
    driver = driver
  )

  session <- if (uses_selenium(driver)) {
    "selenium"
  } else if (uses_rselenium(driver)) {
    "rselenium"
  } else {
    "chromote"
  }

  if (session == "chromote") {
    server <- NULL
    cache_server <- NULL
  } else {
    server <- driver$server
    cache_server <- driver$cache_server
    driver <- driver$client
  }

  session <- new_selenider_session(session, driver, server, cache_server, timeout)

  if (local) {
    local_session(session, .local_envir = .env)

    if (inherits(session$driver, "ChromoteSession")) {
      timeout <- if (on_ci()) 60 * 5 else 60

      withr::with_options(list(withr.hook_source = TRUE), {
        withr::defer(
          {
            # Delete the Crashpad folder if it exists
            unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
          },
          envir = .env
        )

        withr::local_options(
          list(chromote.timeout = timeout),
          .local_envir = .env
        )
      })
    }
  }

  session
}

check_options <- function(session, options, call = rlang::caller_env()) {
  check_class(
    options,
    c("chromote_options", "selenium_options"),
    allow_null = TRUE
  )

  if (session == "chromote") {
    if (is.null(options)) {
      options <- chromote_options()
    } else if (!inherits(options, "chromote_options")) {
      cli::cli_abort(c(
        "Since {.arg session} is {.val chromote}, {.arg options} must be a {.cls chromote_options} object.",
        "x" = "A {.cls {class(options)}} object was provided instead."
      ), call = call)
    }
  } else if (session == "selenium") {
    if (is.null(options)) {
      options <- selenium_options()
    } else if (!inherits(options, "selenium_options")) {
      cli::cli_abort(c(
        "Since {.arg session} is {.val selenium}, {.arg options} must be a {.cls selenium_options} object.",
        "x" = "A {.cls {class(options)}} object was provided instead."
      ))
    }
  } else if (session == "rselenium") {
    if (is.null(options)) {
      options <- selenium_options(client_options = rselenium_client_options())
    } else if (!inherits(options, "selenium_options")) {
      cli::cli_abort(c(
        "Since {.arg session} is {.val rselenium}, {.arg options} must be a {.cls selenium_options} object.",
        "x" = "A {.cls {class(options)}} object was provided instead."
      ))
    } else if (!inherits(options$client_options, "rselenium_client_options")) {
      cli::cli_abort(c(
        "Since {.arg session} is {.val rselenium}, {.arg options$client_options} must be a {.cls rselenium_client_options} object.",
        "x" = "A {.cls {class(options$client_options)}} object was provided instead."
      ))
    }
  }

  options
}

get_driver <- function(browser, options, driver) {
  if (is.null(driver)) {
    if (inherits(options, "chromote_options")) {
      driver <- skip_error_if_testing(
        create_chromote_session_internal(options),
        message = "Chromote session failed to start."
      )

      if (!options$headless) {
        driver$view()
      }
    } else {
      reuse_server <- inherits(options$server_options, "selenium_server_options") &&
        has_default_selenium_object() &&
        identical(options$server_options, default_selenium_options())

      server <- if (reuse_server) {
        default_selenium_object()
      } else if (!is.null(options$server_options)) {
        skip_error_if_testing(create_selenium_server_internal(
          browser,
          options$server_options
        ), message = "Selenium server failed to start.")
      } else {
        NULL
      }

      if (inherits(server, "SeleniumServer")) {
        # For some reason, without this, the server would stop working
        # after a while
        server$read_output()
        server$read_error()
      }

      if (inherits(options$client_options, "selenium_client_options")) {
        options$capabilities <- if (is.null(options$capabilities) && browser == "chrome") {
          list(`goog:chromeOptions` = list(
            args = list(
              "remote-debugging-port=9222"
            )
          ))
        } else {
          options$capabilities
        }

        client <- skip_error_if_testing(
          create_selenium_client_internal(browser, options$client_options),
          message = "Selenium client failed to start."
        )
      } else {
        client <- skip_error_if_testing(
          create_rselenium_client_internal(browser, options$client_options),
          message = "RSelenium client failed to start."
        )
      }

      cache <- inherits(options$server_options, "selenium_server_options") &&
        !has_default_selenium_object()

      if (cache) {
        set_default_selenium_object(server)
        set_default_selenium_options(options$server_options)
      }

      driver <- list(
        server = server,
        client = client,
        cache_server = cache || reuse_server
      )
    }
  } else {
    driver <- check_supplied_driver(driver, browser = browser)
  }

  driver
}

check_session_dependencies <- function(session,
                                       options,
                                       call = rlang::caller_env()) {
  if (is.null(session)) {
    chromote_installed <- rlang::is_installed("chromote")
    if (!chromote_installed && !rlang::is_installed("selenium")) {
      stop_no_dependencies(call = call)
    }

    session <- if (chromote_installed) "chromote" else "selenium"
  } else {
    session <- rlang::arg_match(
      session,
      values = c("chromote", "selenium", "rselenium"),
      call = call
    )

    if (session == "chromote") {
      rlang::check_installed("chromote", call = call)
    } else {
      selenium_checked <- FALSE
      if (session == "rselenium" || inherits(options$client_options, "rselenium_client_options")) {
        rlang::check_installed("RSelenium", call = call)
      } else {
        rlang::check_installed("selenium", call = call)
        selenium_checked <- TRUE
      }

      if (!inherits(options$server_options, "wdman_server_options")) {
        if (!selenium_checked) {
          rlang::check_installed("selenium", call = call)
        }
      } else {
        rlang::check_installed("wdman", call = call)
      }
    }
  }

  session
}

browser_and_version <- function(session,
                                browser,
                                driver,
                                call = rlang::caller_env()) {
  version <- NULL
  if (session == "chromote" && is.null(driver)) {
    if (!is.null(browser) && browser != "chrome") {
      warn_browser_chromote(call = call)
    }

    browser <- "chrome"
  } else if (is.null(browser)) {
    if (is.null(driver) || is_selenium_server(driver)) {
      bv <- find_browser_and_version()

      if (is.null(bv)) {
        stop_default_browser(call = call)
      }

      browser <- bv$browser
      version <- bv$version
    }
  } else {
    if (browser != "phantomjs") {
      version <- get_browser_version(browser)
    }
  }

  list(
    browser = browser,
    version = version
  )
}

#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated and will be removed in a future release.
#' Use the `options` argument to [selenider_session()] instead. If you want
#' to manually create a chromote or selenium session, use
#' [chromote::ChromoteSession], [selenium::SeleniumSession] and
#' [selenium::selenium_server()] manually, since these functions
#' are only a thin wrapper around them.
#'
#' @param parent,...,version,driver_version,port,quiet,host See the
#'   documentation for [chromote_options()], [selenium_options()],
#'   [selenium_client_options()], [wdman_server_options()],
#'   [selenium_client_options()] and [rselenium_client_options()] for details
#'   about what these arguments mean.
#'
#' @returns
#' `create_chromote_session()` returns a [chromote::ChromoteSession] object.
#'
#' `create_selenium_server()` returns a [processx::process] or wdman
#' equivalent.
#'
#' `create_selenium_client()` returns a [selenium::SeleniumSession] object.
#'
#' `create_rselenium_client()` returns an [RSelenium::remoteDriver] object.
#'
#' @export
create_chromote_session <- function(parent = NULL, ...) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "create_chromote_session()",
    details = "Use the `options` argument to selenider_session() instead."
  )

  options <- chromote_options(parent = parent, ...)

  create_chromote_session_internal(options)
}

create_chromote_session_internal <- function(options = chromote_options()) {
  rlang::check_installed("chromote")

  parent <- options$parent
  options <- options[!names(options) %in% c("parent", "headless")]

  timeout <- if (on_ci()) 60 * 5 else 60

  withr::local_options(list(chromote.timeout = timeout))

  if (is.null(parent) && default_chromote_object_alive()) {
    reset_default_chromote_object()
  }

  if (is.null(parent)) {
    parent <- chromote::default_chromote_object()
  }

  rlang::inject(chromote::ChromoteSession$new(parent = parent, !!!options))
}

default_chromote_object_alive <- function() {
  chromote::has_default_chromote_object() &&
    !chromote::default_chromote_object()$get_browser()$get_process()$is_alive()
}

reset_default_chromote_object <- function() {
  chromote::set_default_chromote_object(chromote::Chromote$new())
}

#' @rdname create_chromote_session
#'
#' @param browser The browser to use.
#' @param selenium_manager If this is `FALSE`, [wdman::selenium()] will be used
#'   instead of [selenium::selenium_server()]. The equivalent of using
#'   [wdman_server_options()] over [selenium_server_options()] in
#'   [selenium_options()].
#'
#' @export
create_selenium_server <- function(browser,
                                   version = "latest",
                                   driver_version = "latest",
                                   port = 4444L,
                                   quiet = TRUE,
                                   selenium_manager = TRUE,
                                   ...) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "create_selenium_server()",
    details = "Use the `options` argument to selenider_session() instead."
  )

  if (selenium_manager) {
    options <- selenium_server_options(
      version = version,
      port = port,
      verbose = !quiet,
      ...
    )
  } else {
    options <- wdman_server_options(
      version = version,
      driver_version = driver_version,
      port = port,
      verbose = !quiet,
      ...
    )
  }

  create_selenium_server_internal(browser, options)
}

create_selenium_server_internal <- function(browser, options) {
  if (inherits(options, "selenium_server_options")) {
    selenium_manager <- if (is.null(options$selenium_manager)) {
      options$version == "latest" || numeric_version(options$version) >= "4.9.0"
    } else {
      options$selenium_manager
    }

    selenium::selenium_server(
      version = options$version,
      selenium_manager = selenium_manager,
      verbose = options$verbose,
      temp = options$temp,
      path = options$path,
      interactive = options$interactive,
      echo_cmd = options$echo_cmd,
      extra_args = c("-p", as.character(options$port), options$extra_args)
    )
  } else {
    if (is.null(options$driver_version)) {
      options$driver_version <- "latest"
    }

    chromever <- if (browser == "chrome") options$driver_version else NULL
    geckover <- if (browser == "firefox") options$driver_version else NULL
    iedrver <- if (browser == "internet explorer") options$driver_version else NULL
    phantomver <- if (browser == "phantomjs") "latest" else NULL

    try_fetch(
      {
        rlang::inject(wdman::selenium(
          port = options$port,
          version = options$version,
          chromever = chromever,
          geckover = geckover,
          iedrver = iedrver,
          phantomver = phantomver,
          verbose = options$verbose,
          check = options$check,
          retcommand = options$retcommand,
          !!!options$extra_args
        ))
      },
      error = function(e) {
        to_match <- "version requested doesnt match versions available"
        if (browser == "chrome" && grepl(to_match, e$message)) {
          # If the version of chrome we found is not available as a webdriver,
          # use the latest one instead.
          options$driver_version <- "latest"
          return(create_selenium_server_internal(browser, options))
        }

        stop_selenium_server(e)
      }
    )
  }
}

#' @rdname create_chromote_session
#'
#' @export
create_selenium_client <- function(browser,
                                   port = 4444L,
                                   host = "localhost",
                                   ...) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "create_selenium_client()",
    details = "Use the `options` argument to selenider_session() instead."
  )

  options <- selenium_client_options(
    port = port,
    host = host,
    ...
  )

  create_selenium_client_internal(browser, options)
}

create_selenium_client_internal <- function(browser, options = selenium_client_options()) {
  res <- rlang::try_fetch(
    selenium::wait_for_selenium_available(
      timeout = 20,
      port = options$port,
      host = options$host,
      error = TRUE
    ),
    error = function(e) {
      stop_connect_selenium_server(timeout = 20, error = e)
    }
  )

  if (!res) {
    stop_connect_selenium_server(timeout = 20)
  }

  rlang::try_fetch(
    selenium::SeleniumSession$new(
      browser = browser,
      port = options$port,
      host = options$host,
      verbose = options$verbose,
      capabilities = options$capabilities,
      request_body = options$request_body,
      timeout = options$timeout
    ),
    error = function(e) {
      stop_selenium_session(e)
    }
  )
}

#' @rdname create_chromote_session
#'
#' @export
create_rselenium_client <- function(browser, port = 4444L, ...) {
  lifecycle::deprecate_warn(
    "0.3.0",
    "create_rselenium_client()",
    details = "Use the `options` argument to selenider_session() instead."
  )

  options <- rselenium_client_options(
    port = port,
    ...
  )

  create_rselenium_client_internal(browser, options)
}

create_rselenium_client_internal <- function(browser, options = rselenium_client_options()) {
  driver <- RSelenium::remoteDriver(
    remoteServerAddr = options$host,
    port = options$port,
    browserName = browser,
    path = options$path,
    version = options$version,
    platform = options$platform,
    javascript = options$javascript,
    nativeEvents = options$native_events,
    extraCapabilities = options$extra_capabilities
  )

  count <- 1L
  res <- NULL
  repeat {
    res <- try_fetch(
      driver$getStatus(),
      error = function(e) {
        if (count >= 5) {
          stop_connect_rselenium_server(count, error = e, driver = driver)
        }
        NULL
      }
    )

    if (is.null(res) || !isTRUE(res$ready)) {
      if (count >= 5) {
        stop_connect_rselenium_server(count, res = res, driver = driver)
      }

      count <- count + 1L
      Sys.sleep(1)
    } else {
      break
    }
  }

  try_fetch(
    driver$open(silent = TRUE),
    error = function(e) {
      stop_selenium_client(e, driver = driver)
    }
  )

  driver
}

new_selenider_session <- function(session, driver, server, cache_server, timeout) {
  res <- list(
    id = round(stats::runif(1, min = 0, max = 1000000)),
    session = session,
    driver = driver,
    server = server,
    cache_server = cache_server,
    timeout = timeout,
    start_time = Sys.time()
  )

  class(res) <- "selenider_session"

  res
}

#' Check the driver argument to selenider_server()
#'
#' If the `driver` argument to `selenider_server()` is not `NULL`, check it
#' and convert it into a valid/consistent session object, throwing an error if
#' it is invalid.
#'
#' @param x The supplied driver.
#' @param browser The supplied/calculated browser.
#' @param call The environment to throw errors in.
#'
#' @returns
#' Either a `chromote::ChromoteSession` object, or a named list with two items:
#' * `client` - A [selenium::SeleniumSession] object.
#' * `server` - A Selenium server object (the result of [wdman::selenium()]).
#'   This is optional, and will not be included if the user only supplied a
#'   client.
#'
#' @noRd
check_supplied_driver <- function(x,
                                  browser = NULL,
                                  options = chromote_options(),
                                  call = rlang::caller_env()) {
  if (inherits(x, "ChromoteSession")) {
    x
  } else if (inherits(x, "AppDriver")) {
    x$get_chromote_session()
  } else if (is_selenium_server(x)) {
    client_options <- get_client_options(options, x, call = call)

    client <- skip_error_if_testing(
      create_selenium_client_internal(browser, options = client_options),
      message = "Selenium client failed to start."
    )

    list(client = client, server = x, cache_server = FALSE)
  } else if (is_selenium_client(x)) {
    list(client = x)
  } else if (is.list(x) || is.environment(x)) {
    check_supplied_driver_list(x, browser = browser, options = options, call = call)
  } else {
    stop_invalid_driver(x, call = call)
  }
}

get_client_options <- function(options, server, call = rlang::caller_env()) {
  if (is.null(options$client_options$port)) {
    port <- find_port_from_server(server, call = call)

    if (!is.null(options$client_options)) {
      client_options <- options$client_options
      client_options$port <- port
      client_options
    } else {
      selenium_client_options(port = port)
    }
  } else {
    options$client_options
  }
}

check_supplied_driver_list <- function(x, browser, options, call = rlang::caller_env()) {
  nms <- names(x)

  client <- if ("client" %in% nms) {
    check_selenium_client(x$client, call = call)
  } else {
    NULL
  }

  server <- if ("server" %in% nms) {
    check_selenium_server(x$server, call = call)
  } else {
    NULL
  }

  if (is.null(client)) {
    client <- find_using(x, is_selenium_client)
  }

  if (is.null(server)) {
    server <- find_using(x, is_selenium_server)
  }

  if (is.null(client) && is.null(server)) {
    stop_invalid_driver(x, is_list = TRUE, call = call)
  } else if (is.null(client)) {
    if (is.null(browser)) {
      bv <- find_browser_and_version()

      if (is.null(bv)) {
        stop_default_browser(call = call)
      }

      browser <- bv$browser
    }

    client_options <- get_client_options(options, server, call = call)

    client <- skip_error_if_testing(
      create_selenium_client_internal(browser, options = client_options),
      message = "Selenium client failed to start."
    )
    result <- list(
      client = client,
      server = server,
      cache_server = FALSE
    )
  } else if (is.null(server)) {
    result <- list(client = client)
  } else {
    result <- list(client = client, server = server, cache_server = FALSE)
  }

  if (is.environment(x)) {
    # In case `x` is the result of `RSelenium::rsDriver`, make sure the
    # `reg.finalizer` method does not close the session when the environment is
    # garbage collected.
    x$client <- list(close = function() {})
    x$server <- list(stop = function() {})
  }

  result
}

find_port_from_server <- function(x, call = rlang::caller_env()) {
  if (inherits(x, "process")) {
    port <- get_with_timeout(10, find_port_selenium, x)
  } else {
    log <- x$log()

    port <- find_port_from_logs(log$stderr, pattern = "port ([0-9]+)")

    if (is.na(port)) {
      port <- find_port_from_logs(log$stdout, pattern = "port ([0-9]+)")
    }
  }

  if (is.null(port) || is.na(port)) {
    warn_default_port(call = call)
    port <- 4444L
  }

  port
}

find_port_selenium <- function(x) {
  result <- find_port_from_logs(x$read_output())
  if (is.na(result)) NULL else result
}

find_port_from_logs <- function(
    x,
    pattern = "http://(:?[0-9]+\\.)*[0-9]+:([0-9]+)") {
  matches <- regmatches(x, regexec(pattern, x))
  port_matches <- stats::na.omit(unlist(lapply(
    matches,
    function(x) x[length(x)]
  )))
  numeric_ports <- suppressWarnings(as.integer(port_matches))
  stats::na.omit(numeric_ports)[1]
}

skip_error_if_testing <- function(expr, message) {
  if (is_installed("testthat") && testthat::is_testing()) {
    res <- try(expr)
    if (inherits(res, "try-error")) {
      testthat::skip(message)
    }

    res
  } else {
    expr
  }
}

#' Close a session object
#'
#' Shut down a session object, closing the browser and stopping the server.
#' This will be done automatically if the session is set as the local session
#' (which happens by default).
#'
#' @param x A `selenider_session` object. If omitted, the local session object
#'   will be closed.
#'
#' @returns
#' Nothing.
#'
#' @seealso [selenider_session()]
#'
#' @examplesIf selenider::selenider_available()
#' session <- selenider_session(local = FALSE)
#'
#' close_session(session)
#'
#' @export
close_session <- function(x = NULL) {
  check_class(x, "selenider_session", allow_null = TRUE)

  if (is.null(x)) {
    x <- get_session(create = FALSE)
  }

  if (x$session == "chromote") {
    invisible(x$driver$close())
  } else if (x$session == "selenium") {
    try_fetch(
      x$driver$close(),
      error = function(e) {
        if (!is.null(x$server) && !x$cache_server) {
          # Don't close a selenium_server() object if it is being shared.
          # The process is supervised, so will be closed when the R process
          # finishes.
          x$server$kill()
        }
        stop_close_session(e)
      }
    )

    if (!is.null(x$server) && !x$cache_server) {
      invisible(x$server$kill())
    }
  } else {
    try_fetch(
      x$driver$close(),
      error = function(e) {
        if (!is.null(x$server)) {
          x$server$stop()
        }
        stop_close_session(e)
      }
    )

    if (!is.null(x$server)) {
      invisible(x$server$stop())
    }
  }
}

#' @export
print.selenider_session <- function(x, ..., .time = NULL) {
  check_dots_empty()
  if (is.null(.time)) {
    time <- Sys.time() - x$start_time
  } else {
    time <- .time
  }

  time <- as_pretty_dt(prettyunits::pretty_dt(time))

  timeout <- as_pretty_dt(prettyunits::pretty_sec(x$timeout))

  browser_name <- if (x$session == "chromote") {
    "Chrome"
  } else if (x$session == "selenium") {
    x$driver$browser
  } else {
    x$driver$browserName
  }

  port <- if (x$session != "chromote") x$driver$port else NA

  cli::cli({
    cli::cli_text("A selenider session object")
    cli::cli_bullets(c(
      "*" = "Open for {.val {time}}",
      "*" = "Session: {.val {x$session}}",
      "*" = "Browser: {.val {browser_name}}",
      "*" = "Port: {.val {port}}",
      "*" = "Timeout: {.val {timeout}}"
    ))
  })
}
