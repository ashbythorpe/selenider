get_with_timeout <- function(timeout, .f, ...) {
  if (timeout == 0) {
    .f(...)
  } else {
    end <- Sys.time() + timeout

    while (Sys.time() <= end) {
      res <- .f(...)

      if (!is.null(res)) {
        return(res)
      }
    }

    NULL
  }
}

retry_until_true <- function(timeout, .f, ...) {
  if (timeout == 0) {
    .f(...)
  } else {
    end <- Sys.time() + timeout

    while (Sys.time() <= end) {
      result <- .f(...)

      if (isTRUE(result)) {
        return(TRUE)
      }
    }

    result
  }
}

#' Check if selenider can be used
#'
#' @description
#' Checks if selenider's dependencies are available, and that we are in an
#' environment where it makes sense to open a selenider session.
#'
#' `skip_if_selenider_unavailable()` skips a testthat test if
#' `selenider_available()` returns `FALSE`.
#'
#' @param session Which session we should check. `"chromote"` is used by
#'   default.
#' @param online Whether we need to check for an internet connection.
#'
#' @details
#' Specifically, the following is checked:
#'
#' * The `SELENIDER_AVAILABLE` environment variable. Set this to `"TRUE" `or
#'   `"FALSE"` to override this function.
#' * Whether we are on CRAN (using the `NOT_CRAN` environment variable). If we
#'   are, the function returns `FALSE`.
#' * Whether an internet connection is available (using [curl::nslookup()]).
#'
#' If `session` is `"chromote"`, we also check:
#'
#' * Whether `chromote` is installed.
#' * Whether [chromote::find_chrome()] does not error.
#'
#' If `session` is `"selenium"`, we check:
#'
#' * Whether `selenium` is installed.
#' * Whether we can find a valid browser that is supported by `RSelenium`.
#'
#' @returns
#' A boolean flag: `TRUE` or `FALSE`.
#'
#' @examples
#' selenider_available()
#'
#' @export
selenider_available <- function(
    session = c("chromote", "selenium", "rselenium"),
    online = TRUE) {
  check_bool(online)
  session <- arg_match(session)

  env_variable <- as.logical(Sys.getenv("SELENIDER_AVAILABLE"))
  if (isTRUE(env_variable)) {
    return(TRUE)
  } else if (identical(env_variable, FALSE)) {
    return(FALSE)
  }

  if (online) {
    internet_available <- is_installed("curl") &&
      !is_cran_check() &&
      !is.null(curl::nslookup("r-project.org", error = FALSE))

    if (!internet_available) {
      return(FALSE)
    }
  } else {
    if (is_cran_check()) {
      return(FALSE)
    }
  }

  if (session == "chromote") {
    selenider_available_chromote()
  } else if (session == "selenium") {
    selenider_available_selenium()
  } else {
    selenider_available_rselenium()
  }
}

selenider_available_chromote <- function() {
  Sys.getenv("SELENIDER_SESSION") %in% c("", "chromote") &&
    is_installed("chromote") &&
    tryCatch(
      {
        result <- !is.null(suppressMessages(chromote::find_chrome()))
        if (result && is_check()) {
          Sys.setenv("_R_CHECK_CONNECTIONS_LEFT_OPEN_" = "FALSE")
        }
        result
      },
      error = function(e) FALSE
    )
}

selenider_available_selenium <- function() {
  rlang::is_installed("selenium") &&
    !is.null(find_browser_and_version()$browser)
}

selenider_available_rselenium <- function() {
  rlang::is_installed("RSelenium") &&
    !is.null(find_browser_and_version()$browser)
}

#' @rdname selenider_available
#'
#' @export
skip_if_selenider_unavailable <- function(session = c("chromote", "selenium")) {
  testthat::skip_if_not(
    selenider_available(session),
    "Selenider dependencies unavailable"
  )
}

check_active <- function(x) {
  check_driver_active(x$session, x$driver)
}

check_session_active <- function(session) {
  check_driver_active(session$session, session$driver)
}

check_driver_active <- function(session, driver) {
  if (session == "chromote") {
    chromote_version <- get_from_env("CHROMOTE_VERSION")
    if (is.null(chromote_version)) {
      chromote_version <- utils::packageVersion("chromote")
      set_in_env(CHROMOTE_VERSION = chromote_version)
    }

    if (chromote_version >= "0.1.2.9000") {
      driver$check_active()
    }
  }
}

on_cran <- function() {
  !is_interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

is_cran_check <- function() {
  if (isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    FALSE
  } else {
    is_check()
  }
}

is_check <- function() Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""

on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}

is_testing <- function() {
  rlang::is_installed("testthat") && testthat::is_testing()
}

is_r6 <- function(x) inherits(x, "R6")

elem_common <- function(x, driver) {
  comparison_function <- get_comparison_function(driver)

  lazy_intersect_by(x, comparison_function)
}

elem_unique <- function(x, driver) {
  comparison_function <- get_comparison_function(driver)

  lazy_unique(x, comparison_function)
}

get_comparison_function <- function(driver) {
  compare_selenium <- function(x, y) selenium_equal(x, y, driver)
  compare_rselenium <- function(x, y) rselenium_equal(x, y, driver)

  if (uses_chromote(driver)) {
    `==`
  } else if (uses_selenium(driver)) {
    compare_selenium
  } else {
    compare_rselenium
  }
}

every <- function(x, .f) {
  all(vapply(x, .f, FUN.VALUE = logical(1)))
}

# Adapted from scales::ordinal()
ordinal <- function(x) {
  res <- character(length(x))

  res[x == 1] <- "first"
  res[x == 2] <- "second"
  res[x == 3] <- "third"
  res[res == ""] <- ordinal_numbers(x[res == ""])

  res
}

ordinal_numbers <- function(x) {
  rules <- list(
    st = "(?<!1)1$",
    nd = "(?<!1)2$",
    rd = "(?<!1)3$",
    th = "(?<=1)[123]$",
    th = "[0456789]$",
    th = "."
  )

  out <- utils::stack(lapply(rules, grep, x = x, perl = TRUE))
  out <- out[!duplicated(out$values), ] # only first result should be considered
  res <- paste0(
    x,
    out$ind[order(out$values)]
  )

  res
}

call_insert <- function(call, elem_name, quo = TRUE) {
  if (quo) {
    new_call <- call2(
      as.list(quo_get_expr(call))[[1]],
      parse_expr(elem_name),
      !!!call_args(call)
    )

    new_quosure(new_call, quo_get_env(call))
  } else {
    new_call <- call2(
      as.list(call)[[1]],
      parse_expr(elem_name),
      !!!call_args(call)
    )

    new_call
  }
}

find_using <- function(x, .f, .default = NULL) {
  for (a in x) {
    if (.f(a)) {
      return(a)
    }
  }
  .default
}

escape_single_quotes <- function(x) {
  gsub("'", "\\'", x, fixed = TRUE)
}

format_value <- function(x) {
  if (is.null(x)) {
    "{.code NULL}"
  } else if (is.na(x)) {
    paste0("{.code ", x, "}")
  } else {
    paste0("{.val ", x, "}")
  }
}

is_multiple_elements <- function(x) {
  !(inherits_any(
    x,
    c(
      "webElement",
      "remoteDriver",
      "mock_element",
      "mock_client",
      "ChromoteSession",
      "WebElement",
      "SeleniumSession"
    )
  ) || (is.numeric(x) && length(x) == 1))
}

uses_selenium <- function(x) {
  inherits(x, "SeleniumSession") ||
    ("client" %in% names(x) && inherits(x$client, "SeleniumSession"))
}

uses_rselenium <- function(x) {
  inherits(x, "remoteDriver") ||
    ("client" %in% names(x) && inherits(x$client, "remoteDriver"))
}

uses_chromote <- function(x) {
  inherits(x, "ChromoteSession")
}

execute_js_fn_on <- function(fn, x, session, driver) {
  if (session == "chromote") {
    script <- paste0("function() { return (", fn, ")(this) }")
    result <- driver$Runtime$callFunctionOn(
      script,
      chromote_object_id(backend_id = x, driver = driver),
      returnByValue = TRUE
    )

    if (!is.null(result$exceptionDetails)) {
      print(result$exceptionDetails)
    }

    result$result$value
  } else {
    script <- paste0("let fn = ", fn, ";", "return fn(arguments[0]);")
    if (session == "selenium") {
      driver$execute_script(script, x)
    } else {
      unpack_list(driver$executeScript(script, list(x)))
    }
  }
}

execute_js_fn_on_multiple <- function(fn, x, session, driver) {
  if (length(x) == 1) {
    return(execute_js_fn_on(fn, x[[1]], session, driver))
  }

  if (session == "chromote") {
    arg_names <- get_argument_names(seq_along(x) - 1)
    args <- paste0(arg_names[-1], collapse = ", ")
    inner_args <- paste0(arg_names, collapse = ", ")
    script <- paste0(
      "function(", args, ") { return (", fn, ")([", inner_args, "]) }"
    )

    first_element <- chromote_object_id(backend_id = x[[1]], driver = driver)
    other_elements <- rest <- lapply(x[-1], function(y) {
      list(objectId = chromote_object_id(backend_id = y, driver = driver))
    })

    result <- driver$Runtime$callFunctionOn(
      script,
      chromote_object_id(backend_id = x[[1]], driver = driver),
      arguments = other_elements,
      returnByValue = TRUE
    )

    if (!is.null(result$exceptionDetails)) {
      print(result$exceptionDetails)
    }

    result$result$value
  } else {
    script <- paste0("let fn = ", fn, ";", "return fn(Array.from(arguments));")
    if (session == "selenium") {
      driver$execute_script(script, !!!x)
    } else {
      unpack_list(driver$executeScript(script, x))
    }
  }
}

unpack_list <- function(x) {
  if (length(x) == 0) {
    NULL
  } else if (length(x) == 1) {
    x[[1]]
  } else {
    x
  }
}

is_windows <- function() .Platform$OS.type == "windows"

is_mac <- function() Sys.info()[["sysname"]] == "Darwin"

is_linux <- function() Sys.info()[["sysname"]] == "Linux"
