#' Driver options
#'
#' @description
#' `chromote_options()` and `selenium_options()` return a list of options that
#' can be passed to the `options` argument of `selenider_session()`.
#'
#' `chromote_options()` allows you to control the creation of a chromote driver
#' created using [chromote::ChromoteSession$new()][chromote::ChromoteSession].
#'
#' `selenium_options()` allows you to control the creation of a selenium driver.
#'
#' `selenium_server_options()` should be passed to the `server_options`
#' argument of `selenium_options()`, allowing you to control the creation of
#' the server using [selenium::selenium_server()].
#'
#' `selenium_client_options()` should be passed to the `client_options` argument
#' of `selenium_options()`, allowing you to control the creation of a Selenium
#' client created using
#' [selenium::SeleniumSession$new()][selenium::SeleniumSession].
#'
#' @param headless Whether to run the browser in headless mode, meaning
#'   that you won't actually be able to see the browser as you control it.
#'   For debugging purposes and interactive use, it is often useful to set
#'   this to `FALSE`.
#' @param user_agent A string containing a custom user agent to use for all
#'   requests.
#' @param proxy_server A proxy server to use for all requests. Either a string
#'   of the form `"HOST:PORT"`, or a list containing the `host` and `port`.
#'   Optionally, `username` and `password` can be provided if the proxy server
#'   requires authentication.
#' @param extra_args A character vector of arguments. In `chromote_options()`,
#'   these are passed to the Chrome process. In `selenium_client_options()`,
#'   these are passed to the Selenium server process.
#' @param parent The parent chromote session.
#' @param width,height,targetId,wait_,auto_events,mobile Passed into
#'   [chromote::ChromoteSession$new()][chromote::ChromoteSession].
#'
#' @export
chromote_options <- function(headless = TRUE,
                             user_agent = NULL,
                             proxy_server = NULL,
                             extra_args = NULL,
                             parent = NULL,
                             width = 992,
                             height = 1323,
                             targetId = NULL, # nolint: object_name_linter
                             wait_ = TRUE,
                             auto_events = NULL,
                             mobile = FALSE) {
  check_bool(headless)
  check_string(user_agent, allow_null = TRUE)
  proxy_server <- check_proxy_server(proxy_server)
  check_string(extra_args, allow_null = TRUE)
  check_class(parent, "Chromote", allow_null = TRUE)
  check_number_whole(width)
  check_number_whole(height)
  check_string(targetId, allow_null = TRUE)
  check_bool(wait_)
  check_bool(auto_events, allow_null = TRUE)
  check_bool(mobile)

  if (!is.null(parent)) {
    if (!is.null(proxy_server)) {
      rlang::warn(c(
        "`proxy_server` requires passing in arguments to the Chrome process",
        "x" = "Since `parent` was provided, `proxy_server` will be ignored."
      ))
    }
    if (!is.null(extra_args)) {
      rlang::warn("Since `parent` was provided, `extra_args` will be ignored.")
    }
  }

  result <- list(
    headless = headless,
    user_agent = user_agent,
    proxy_server = proxy_server,
    extra_args = extra_args,
    parent = parent,
    width = width,
    height = height,
    targetId = targetId,
    wait = wait_,
    auto_events = auto_events,
    mobile = mobile
  )

  class(result) <- c("chromote_options")

  result
}

#' @rdname chromote_options
#'
#' @param client_options A [selenium_client_options()] object.
#' @param server_options A [selenium_server_options()] object, or `NULL` if you
#'   don't want one to be created.
#'
#' @export
selenium_options <- function(client_options = selenium_client_options(),
                             server_options = selenium_server_options()) {
  check_class(client_options, c("selenium_client_options", "rselenium_client_options"))
  check_class(server_options, c("selenium_server_options", "wdman_server_options"), allow_null = TRUE)

  result <- list(
    client_options = client_options,
    server_options = server_options
  )

  class(result) <- c("selenium_options", "list")

  result
}

#' @rdname chromote_options
#'
#' @param version The version of Selenium server to use.
#' @param host The host to use.
#' @param port The port number to use.
#' @param selenium_manager,interactive,stdout,stderr,verbose,temp,path,...
#'   Passed into [selenium::selenium_server()].
#'
#' @export
selenium_server_options <- function(version = "latest",
                                    host = "localhost",
                                    port = 4444L,
                                    selenium_manager = TRUE,
                                    interactive = FALSE,
                                    stdout = NULL,
                                    stderr = NULL,
                                    verbose = FALSE,
                                    temp = TRUE,
                                    path = NULL,
                                    extra_args = c(),
                                    ...) {
  check_string(version)
  check_string(host)
  check_number_whole(port)
  check_bool(selenium_manager, allow_null = TRUE)
  check_bool(verbose)
  check_bool(temp)
  check_string(path, allow_null = TRUE)
  check_bool(interactive)
  check_character(extra_args, allow_null = TRUE)

  result <- list(
    version = version,
    host = host,
    port = port,
    selenium_manager = selenium_manager,
    interactive = interactive,
    stdout = stdout,
    stderr = stderr,
    verbose = verbose,
    temp = temp,
    path = path,
    extra_args = extra_args,
    process_args = rlang::list2(...)
  )

  class(result) <- "selenium_server_options"

  result
}


#' @rdname chromote_options
#'
#' @param capabilities,request_body,timeout
#'   Passed into [selenium::SeleniumSession$new()][selenium::SeleniumSession].
#'
#' @export
selenium_client_options <- function(port = 4444L,
                                    host = "localhost",
                                    verbose = FALSE,
                                    capabilities = NULL,
                                    request_body = NULL,
                                    timeout = 60) {
  check_number_whole(port)
  check_string(host)
  check_bool(verbose)
  check_list(capabilities, allow_null = TRUE)
  check_list(request_body, allow_null = TRUE)
  check_number_decimal(timeout)

  result <- list(
    port = port,
    host = host,
    verbose = verbose,
    capabilities = capabilities,
    request_body = request_body,
    timeout = timeout
  )

  class(result) <- "selenium_client_options"

  result
}

#' RSelenium options
#'
#' `r lifecycle::badge("superseded")`
#' Instruct selenider to use RSelenium instead of selenium. Passed into
#' [selenium_options()]. This is not recommended, since RSelenium does not
#' support the latest version of Selenium, and wdman (the server manager that
#' RSelenium) uses, is not compatible with the latest version of Chrome.
#'
#' @param version The version of Selenium server to use.
#' @param driver_version The version of the browser-specific driver to use.
#' @param port The port to run selenium client/server on.
#' @param check,verbose,retcommand,...
#'   Passed into [wdman::selenium()].
#'
#' @details
#' In [selenium_options()], you can supply options to configure a server and
#' client run by RSelenium instead of selenium.
#' Instead of `selenium_server_options()`, you can use `wdman_server_options()`
#' to allow `wdman` to run the Selenium server using [wdman::selenium()].
#'
#' Instead of using `selenium_client_options()`, you can use
#' `rselenium_client_options()` to control the creation of an
#' [RSelenium::remoteDriver()] object instead.
#'
#' Note that the `driver` option of `selenider_session()` also accepts these
#' objects in place of their selenium equivalents.
#'
#' @returns
#' An options object that can be passed into [selenium_options()].
#'
#' @export
wdman_server_options <- function(version = "latest",
                                 driver_version = "latest",
                                 port = 4444L,
                                 check = TRUE,
                                 verbose = FALSE,
                                 retcommand = FALSE,
                                 ...) {
  check_string(version)
  check_string(driver_version)
  check_number_whole(port)
  check_bool(check)
  check_bool(verbose)
  check_bool(retcommand)

  result <- list(
    version = version,
    driver_version = driver_version,
    port = port,
    check = check,
    verbose = verbose,
    retcommand = retcommand,
    extra_args = rlang::list2(...)
  )

  class(result) <- "wdman_server_options"

  result
}

#' @rdname wdman_server_options
#'
#' @param host The host to connect to the selenium server over.
#' @param path,platform,javascript,native_events,extra_capabilities
#'   Passed into [RSelenium::remoteDriver()].
#'
#' @export
rselenium_client_options <- function(port = 4444L,
                                     host = "localhost",
                                     path = "/wd/hub",
                                     version = "",
                                     platform = "ANY",
                                     javascript = TRUE,
                                     native_events = TRUE,
                                     extra_capabilities = list()) {
  lifecycle::signal_stage("superseded", "rselenium_client_options()")

  check_number_whole(port)
  check_string(host)
  check_string(path)
  check_string(version)
  check_string(platform)
  check_bool(javascript)
  check_bool(native_events)
  check_list(extra_capabilities)

  result <- list(
    port = port,
    host = host,
    path = path,
    version = version,
    platform = platform,
    javascript = javascript,
    native_events = native_events,
    extra_capabilities = extra_capabilities
  )

  class(result) <- "rselenium_client_options"

  result
}
