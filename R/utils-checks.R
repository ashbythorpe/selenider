check_class <- function(x,
                        cls,
                        ...,
                        allow_null = FALSE,
                        arg = rlang::caller_arg(x),
                        call = rlang::caller_env()) {
  if (allow_null) {
    if (!is.null(x) && !inherits_any(x, cls)) {
      what <- cli::format_inline("a {.cls {cls}} object or `NULL`")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  } else {
    if (!inherits_any(x, cls)) {
      what <- cli::format_inline("a {.cls {cls}} object")
      stop_input_type(x, what, ..., arg = arg, call = call)
    }
  }
}

is_selenium_server <- function(x) {
  inherits(x, "SeleniumServer") ||
    (is.list(x) && all(c("process", "log", "stop") %in% names(x)))
}

check_selenium_server <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_server(x)) {
    cli::cli_abort(c(
      "{.code driver$server} must be a valid Selenium server object",
      "i" = paste0(
        "This can be the result of {.fun selenium::selenium_server} ",
        "or {.fun wdman::selenium}."
      )
    ), class = "selenider_error_invalid_server", call = call)
  }
  x
}

is_selenium_client <- function(x) {
  inherits_any(x, c("SeleniumSession", "remoteDriver"))
}

check_selenium_client <- function(x, call = rlang::caller_env()) {
  if (!is_selenium_client(x)) {
    cli::cli_abort(c(
      "{.code driver$client} must be a {.cls SeleniumSession} object",
      "i" = paste0(
        "This can be the result of {.fun selenium::SeleniumSession$new}."
      )
    ), class = "selenider_error_invalid_client", call = call)
  }
  x
}

check_vector <- function(x,
                         check_fun,
                         ...,
                         allow_null = FALSE,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (is.vector(x)) {
    good <- TRUE
    for (a in x) {
      good <- tryCatch(
        {
          check_fun(
            a,
            allow_null = allow_null,
            ...
          )
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )
    }

    if (good) {
      return(invisible(NULL))
    }
  } else if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a whole number larger than or equal to 1",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_list <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (is.list(x)) {
    return(invisible(NULL))
  } else if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a list",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_proxy_server <- function(proxy_server,
                               ...,
                               allow_username_password = FALSE,
                               arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  if (is.null(proxy_server)) {
    return(NULL)
  }

  if (is_string(proxy_server)) {
    list(server = url)
  } else if (proxy_server) {
    check_string(proxy_server$host, call = call)
    check_number_whole(proxy_server$port, call = call)

    if (allow_username_password) {
      check_string(proxy_server$username, allow_null = TRUE, call = call)
      check_string(proxy_server$password, allow_null = TRUE, call = call)

      if (!is.null(proxy_server$username) && is.null(proxy_server$password)) {
        rlang::abort("`proxy_server$username` was provided but `proxy_server$password` was not.", call = call)
      } else if (is.null(proxy_server$username) && !is.null(proxy_server$password)) {
        rlang::abort("`proxy_server$password` was provided but `proxy_server$username` was not.", call = call)
      }

      list(
        server = paste0(proxy_server$host, ":", proxy_server$port),
        username = proxy_server$username,
        password = proxy_server$password
      )
    }
  } else {
    stop_input_type(proxy_server, c("a string", "a list"), ..., allow_null = TRUE, call = call)
  }
}

check_selector_args <- function(css, xpath, id, class_name, name, call = rlang::caller_env()) {
  check_string(css, allow_null = TRUE)
  check_string(xpath, allow_null = TRUE)
  check_string(id, allow_null = TRUE)
  check_string(class_name, allow_null = TRUE)
  check_string(name, allow_null = TRUE)

  if (is.null(css) && is.null(xpath) && is.null(id) && is.null(class_name) && is.null(name)) {
    stop_bad_selector(call = call)
  }
}
