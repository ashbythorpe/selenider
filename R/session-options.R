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
#' `selenium_server_options()` and `wdman_server_options()` should be passed to
#' the `server_options` argument of `selenium_options()`. By default, the former
#' is used, meaning that the server is created using
#' [selenium::selenium_server()]. If `wdman_server_options()` is used instead,
#' the server will be created using [wdman::selenium()].
#'
#' `selenium_client_options()` should be passed to the `client_options` argument
#' of `selenium_options()`, allowing you to control the creation of a Selenium
#' client created using
#' [selenium::SeleniumSession$new()][selenium::SeleniumSession].
#'
#' `r lifecycle::badge("superseded")`
#'
#' Instead of using `selenium_client_options()`, you can use
#' `rselenium_client_options()` to control the creation of an
#' [RSelenium::remoteDriver()] object instead. This is not recommended, since
#' RSelenium is incompatible with newer versions of Selenium.
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
#' @param width,height,targetId,wait_,auto_events Passed into
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
                             auto_events = NULL) {
  check_bool(headless)
  check_string(user_agent, allow_null = TRUE)

  if (!is.null(proxy_server)) {
    if (is_string(proxy_server)) {
      proxy_server <- list(server = url)
    } else if (is.list(proxy_server)) {
      check_string(proxy_server$host)
      check_number_whole(proxy_server$port)

      # TODO: Require chromote version that allows .enable() methods
      check_string(proxy_server$username, allow_null = TRUE)
      check_string(proxy_server$password, allow_null = TRUE)

      if (!is.null(proxy_server$username) && is.null(proxy_server$password)) {
        rlang::abort("`proxy_server$username` was provided but `proxy_server$password` was not.")
      } else if (is.null(proxy_server$username) && !is.null(proxy_server$password)) {
        rlang::abort("`proxy_server$password` was provided but `proxy_server$username` was not.")
      }

      proxy_server <- list(
        server = paste0(proxy_server$host, ":", proxy_server$port),
        username = proxy_server$username,
        password = proxy_server$password
      )
    } else {
      stop_input_type(proxy_server, c("a string", "a list"), allow_null = TRUE)
    }
  }

  check_string(extra_args, allow_null = TRUE)
  check_class(parent, "Chromote", allow_null = TRUE)
  check_number_whole(width)
  check_number_whole(height)
  check_string(targetId, allow_null = TRUE)
  check_bool(wait_)
  check_bool(auto_events, allow_null = TRUE)

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
    auto_events = auto_events
  )

  class(result) <- c("chromote_options")

  result
}

#' @rdname chromote_options
#'
#' @param client_options A [selenium_client_options()] object.
#' @param server_options A [selenium_server_options()] or
#'   [wdman_server_options()] object.
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
#' @param port The port number to use.
#' @param selenium_manager,verbose,temp,path,interactive,echo_cmd
#'   Passed into [selenium::selenium_server()].
#'
#' @export
selenium_server_options <- function(version = "latest",
                                    port = 4444L,
                                    selenium_manager = NULL,
                                    verbose = FALSE,
                                    temp = TRUE,
                                    path = NULL,
                                    interactive = FALSE,
                                    echo_cmd = FALSE,
                                    extra_args = c()) {
  check_string(version)
  check_number_whole(port)
  check_bool(selenium_manager, allow_null = TRUE)
  check_bool(verbose)
  check_bool(temp)
  check_string(path, allow_null = TRUE)
  check_bool(interactive)
  check_bool(echo_cmd)
  check_character(extra_args, allow_null = TRUE)

  result <- list(
    version = version,
    port = port,
    selenium_manager = selenium_manager,
    verbose = verbose,
    temp = temp,
    path = path,
    interactive = interactive,
    echo_cmd = echo_cmd,
    extra_args = extra_args
  )

  class(result) <- "selenium_server_options"

  result
}

#' @rdname chromote_options
#'
#' @param driver_version The version of the browser-specific driver to use.
#' @param check,retcommand,...
#'   Passed into [wdman::selenium()].
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

#' @rdname chromote_options
#'
#' @param host,capabilities,request_body,timeout
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

#' @rdname chromote_options
#'
#' @param platform,javascript,native_events,extra_capabilities
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
