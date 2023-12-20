chromote_options <- function(view = FALSE,
                             parent = NULL,
                             width = 992,
                             height = 1323,
                             targetId = NULL, # nolint: object_name_linter
                             wait_ = TRUE,
                             auto_events = NULL) {
  check_class(parent, "Chromote", allow_null = TRUE)
  check_number_whole(width)
  check_number_whole(height)
  check_string(targetId, allow_null = TRUE)
  check_bool(wait_)
  check_bool(auto_events, allow_null = TRUE)

  result <- list(
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

selenium_options <- function(client_options = selenium_client_options(),
                             server_options = selenium_server_options()) {
  check_class(client_options, c("selenider_client_options"))
  check_class(server_options, c("selenider_server_options"))

  result <- list(
    client = client_options,
    server = server_options
  )

  class(result) <- c("selenider_options", "list")
}

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
  check_bool(selenium_manager)
  check_bool(verbose)
  check_bool(temp)
  check_string(path, allow_null = TRUE)
  check_bool(interactive)
  check_bool(echo_cmd)
  check_character(extra_args)

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

wdman_server_options <- function(version = "latest",
                                 driver_version = "latest",
                                 port = 4444L,
                                 check = TRUE,
                                 verbose = TRUE,
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

selenium_client_options <- function(port = 4444L,
                                    host = "localhost",
                                    verbose = FALSE,
                                    capabilities = NULL,
                                    request_body = NULL,
                                    timeout = 20) {
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

rselenium_client_options <- function(port = 4444L,
                                     host = "localhost",
                                     path = "/wd/hub",
                                     version = "",
                                     platform = "ANY",
                                     javascript = TRUE,
                                     native_events = TRUE,
                                     extra_capabilities = list()) {
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
