#' Start a session
#'
#' @description
#' Begin a session in selenider, setting it as the local session unless
#' otherwise specified, allowing the session to be accessed globally in the
#' environment where it was defined.
#'
#' `create_chromote_session()`, `create_selenium_client()` and
#' `create_selenium_server()` are low-level functions that allow more control
#' over making a web driver, which can then be passed into the `driver`
#' argument to `selenider_session()`.
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
#' @param view Whether to open the browser and view it, for visual testing.
#'   This is ignored if session is "selenium", since selenium drivers cannot
#'   be headless.
#' @param timeout The default time to wait when collecting an element.
#' @param selenium_manager If `TRUE`, we use [selenium::selenium_server()] to
#'   start the server, relying on Selenium Manager to download the necessary
#'   browser drivers. If `FALSE`, [wdman::selenium()] is used to start the
#'   server, which will try to download the drivers automatically. This is not
#'   used by default because it is less reliable (especially on Chrome), and
#'   does not support Microsoft Edge.
#' @param driver A driver object to use instead of creating one manually. This
#'   can be one of:
#'   * A [chromote::ChromoteSession] object (the result of
#'     `create_chromote_session()`).
#'   * A [shinytest2::AppDriver] object.
#'   * An [selenium::SeleniumSession] object (the result of
#'     `create_selenium_client()`).
#'   * A Selenium server object (the result of [selenium::selenium_server()],
#'     [wdman::selenium()] or `create_selenium_server()`). In this case, the
#'     client object will be created using the server object.
#'   * A list/environment containing the [selenium::SeleniumSession] object,
#'     the Selenium server object, or both.
#'   * An [RSelenium::remoteDriver()] object (see [create_rselenium_client()])
#'     can be used instead of a [selenium::SeleniumSession] object.
#'   See Details for more information about providing a custom driver object.
#' @param local Whether to set the session as the local session object,
#'   using [local_session()].
#' @param quiet Whether to let [selenium::selenium_server()] or
#'   [wdman::selenium()] display messages. By default, this output is
#'   suppressed, as it is not usually useful. Chromote does not display any
#'   output when creating a session.
#' @param .env Passed into [local_session()], to define the
#'   environment in which the session is used. Change this if you want to
#'   create the session inside a function and then use it outside the
#'   function.
#' @param ... Arguments to finetune the creation of the specific driver.
#'   * For `create_chromote_session()`, these are passed into
#'     [`chromote::ChromoteSession$new()`][chromote::ChromoteSession].
#'  * For `create_selenium_server()`, these are passed into
#'    [`selenium::selenium_server()`], or [wdman::selenium()] if
#'    `selenium_manager` is `FALSE`.
#'  * For `create_selenium_client()`, these are passed into
#'    [`selenium::SeleniumSession$new()`][selenium::SeleniumSession].
#'
#' @details
#' # Structure of a selenider session
#' A `selenider_session` object is an S3 list, meaning its properties can be
#' accessed using `$`. Most notably, using `session$driver` allows access to
#' the driver object which actually controls the browser. If you are using
#' Selenium, use `session$driver` to access the [selenium::SeleniumSession]
#' object. These objects are useful if you want to do something with the
#' driver that is not directly supported by selenider. See
#' [get_actual_element()] for some examples of this.
#'
#' # Custom drivers
#' Custom driver objects are good if you want more low-level control over the
#' underlying functions that create the webdrivers that actually control the
#' browser. However, it is recommended to use the selenider functions (e.g.
#' `create_selenium_client()`) over [selenium::SeleniumSession] for better
#' error messages and more reliable behaviour. See
#' `vignette("unit-testing", package = "selenider")` for more information on
#' using selenider with docker/Github Actions.
#'
#' ## Chromote
#' Supplying a custom [chromote::ChromoteSession] object can allow you to
#' manage the underlying [chromote::Chromote] process that is used to spawn
#' sessions. For example:
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
#' ## selenium
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
#' However, it can sometimes be useful to omit the server, for example when you
#' are running the Selenium server using Docker. In this case, you need to make
#' sure the host and port are matched correctly.
#'
#' ```
#' session <- selenider_session(
#'   driver = create_selenium_client(
#'     host = "<IP ADDRESS>",
#'     port = 1234L
#'   )
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
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
selenider_session <- function(session = getOption("selenider.session"),
                              browser = getOption("selenider.browser"),
                              timeout = 4,
                              options = NULL,
                              driver = NULL,
                              local = TRUE,
                              .env = rlang::caller_env()) {
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
  } else {
    server <- driver$server
    driver <- driver$client
  }

  session <- new_selenider_session(session, driver, server, timeout)

  if (local) {
    local_session(session, .local_envir = .env)

    if (inherits(session$driver, "ChromoteSession")) {
      withr::defer(
        {
          # Delete the Crashpad folder if it exists
          unlink(file.path(tempdir(), "Crashpad"), recursive = TRUE)
        },
        envir = .env
      )
      timeout <- if (on_ci()) 60 * 5 else 60
      withr::local_options(
        list(chromote.timeout = timeout),
        .local_envir = .env
      )
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
        create_chromote_session(options),
        message = "Chromote session failed to start."
      )

      if (options$view) {
        driver$view()
      }
    } else {
      server <- if (inherits(options$server_options, "selenium_server_options") && has_default_selenium_object()) {
        new_server <- FALSE
        default_selenium_object()
      } else if (!is.null(options$server_options)) {
        new_server <- TRUE
        skip_error_if_testing(create_selenium_server(
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
          create_selenium_client(browser, options$client_options),
          message = "Selenium client failed to start."
        )
      } else {
        client <- skip_error_if_testing(
          create_rselenium_client(browser, options$client_options),
          message = "RSelenium client failed to start."
        )
      }

      driver <- list(
        server = server,
        client = client
      )

      if (inherits(options$server_options, "selenium_server_options") && new_server) {
        set_default_selenium_object(server)
      }
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

#' @rdname selenider_session
#'
#' @param parent The [chromote::Chromote] object to create the session from.
#'   Passed into [chromote::ChromoteSession$new()][chromote::ChromoteSession].
#'
#' @export
create_chromote_session <- function(options = chromote_options()) {
  rlang::check_installed("chromote")

  parent <- options$parent
  options <- options[!names(options) %in% c("parent", "view")]

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

#' @rdname selenider_session
#'
#' @param version The version of Selenium Server to run.
#' @param driver_version The version of the webdriver (chromedriver,
#'   geckodriver, etc.) to use. You only need to change this if
#'   `selenium_manager` is `FALSE` and Chrome is being used, as the
#'   version of the driver depends on the version of the Chrome browser.
#' @param port The port to run Selenium on.
#'
#' @export
create_selenium_server <- function(browser, options) {
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
          return(create_selenium_server(browser, options))
        }

        stop_selenium_server(e)
      }
    )
  }
}

#' @rdname selenider_session
#'
#' @param host The host on which the Selenium server is running. This is
#'   usually your local machine (`"localhost"`), but can be an IP address.
#'
#' @export
create_selenium_client <- function(browser, options = selenium_client_options()) {
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

  capabilities <- if (browser == "chrome" && !is.null(options$capabilities)) {
    list(`goog:chromeOptions` = list(
      args = list(
        "remote-debugging-port=9222"
      )
    ))
  } else {
    options$capabilities
  }

  rlang::try_fetch(
    selenium::SeleniumSession$new(
      browser = browser,
      port = options$port,
      host = options$host,
      verbose = options$verbose,
      capabilities = capabilities,
      request_body = options$request_body,
      timeout = options$timeout
    ),
    error = function(e) {
      stop_selenium_session(e)
    }
  )
}

#' Start a Selenium session using RSelenium
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' We recommend using [create_selenium_client()] instead of this function,
#' as RSelenium is not compatible with newer versions of Selenium.
#'
#' @param browser The browser to use.
#' @param port The port to run RSelenium on.
#' @param ... Other arguments to pass to RSelenium.
#'
#' @returns An [RSelenium::remoteDriver] object. This can be passed into
#'   [selenider_session()] in place of a [selenium::SeleniumSession] object.
#'
#' @export
create_rselenium_client <- function(browser, options = rselenium_client_options()) {
  lifecycle::signal_stage("superseded", "create_rselenium_client()")

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

new_selenider_session <- function(session, driver, server, timeout) {
  res <- list(
    id = round(stats::runif(1, min = 0, max = 1000000)),
    session = session,
    driver = driver,
    server = server,
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
                                  call = rlang::caller_env()) {
  if (inherits(x, "ChromoteSession")) {
    x
  } else if (inherits(x, "AppDriver")) {
    x$get_chromote_session()
  } else if (is_selenium_server(x)) {
    port <- find_port_from_server(x, call = call)
    client <- skip_error_if_testing(
      create_selenium_client(browser, port = port),
      message = "Selenium client failed to start."
    )

    list(client = client, server = x)
  } else if (is_selenium_client(x)) {
    list(client = x)
  } else if (is.list(x) || is.environment(x)) {
    check_supplied_driver_list(x, browser, call = call)
  } else {
    stop_invalid_driver(x, call = call)
  }
}

check_supplied_driver_list <- function(x, browser, call = rlang::caller_env()) {
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

    port <- find_port_from_server(server, call = call)
    client <- skip_error_if_testing(
      create_selenium_client(browser, port = port),
      message = "Selenium client failed to start."
    )
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

  if (x$session == "chromote") {
    invisible(x$driver$close())
  } else if (x$session == "selenium") {
    try_fetch(
      x$driver$close(),
      error = function(e) {
        stop_close_session(e)
      }
    )
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
