#' Execute a JavaScript function
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Execute a JavaScript function on zero or more arguments.
#'
#' `execute_js_expr()` is a simpler version of `execute_js_fn()` that can evaluate
#' simple expressions (e.g. "alert()"). To return a value, you must do so explicitly
#' using "return".
#'
#' These functions are experimental because their names and parameters are liable to
#' change. Additionally, their behaviour can be inconsistent between different session types
#' (chromote and selenium) and different browsers.
#'
#' @param fn A string defining the function.
#' @param ... Arguments to the function/expression. These must be unnamed, since
#'   JavaScript does not support named arguments.
#' @param .timeout How long to wait for any elements to exist in the DOM.
#' @param .session The session to use, if `...` does not contain any
#'   selenider elements.
#' @param .debug Whether to print the final expression that is executed. Mostly
#'   used for debugging the functions themselves, but can also be used to
#'   identify problems in your own JavaScript code.
#'
#' @details
#' `...` can contain `selenider_element`/`selenider_elements` objects,
#' which will be collected and then passed into the function. However,
#' more complex objects (e.g. lists of selenider elements) will not be
#' moved into the JavaScript world correctly.
#'
#' Similarly, nodes and lists of nodes returned from a JavaScript function will
#' be converted into their corresponding `selenider_element`/`selenider_elements`
#' objects, while more complex objects will not. These elements are not lazy (see
#' [elem_cache()]), so make sure you only use them while you are sure they are
#' still on the page.
#'
#' @returns
#' The return value of the JavaScript function, turned back into an R object.
#'
#' @family global actions
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <button class='mybutton'>Click me</button>
#' "
#' session <- minimal_selenider_session(html)
#'
#' execute_js_fn("(x, y) => x + y", 1, 1)
#'
#' execute_js_expr("arguments[0] + arguments[1]", 1, 1)
#'
#' execute_js_fn("x => x.click()", s(".mybutton"))
#'
#' execute_js_expr("arguments[0].click()", s(".mybutton"))
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
execute_js_fn <- function(fn, ..., .timeout = NULL, .session = NULL, .debug = FALSE) {
  lifecycle::signal_stage("experimental", "execute_js_fn()")
  check_dots_unnamed()
  args <- rlang::list2(...)

  info <- get_info_from_args(args, .timeout, .session)
  driver <- info$driver
  driver_id <- info$driver_id
  timeout <- info$timeout

  if (uses_selenium(driver)) {
    execute_selenium_expr(fn, args, fn = TRUE, driver = driver, timeout = timeout, driver_id = driver_id, .debug = .debug)
  } else {
    chromote_execute_js_fn(fn, args, .driver = driver, .driver_id = driver_id, .timeout = timeout, .debug = .debug)
  }
}

#' @rdname execute_js_fn
#'
#' @param expr An expression to execute.
#'
#' @export
execute_js_expr <- function(expr, ..., .timeout = NULL, .session = NULL, .debug = FALSE) {
  lifecycle::signal_stage("experimental", "execute_js_expr()")
  check_dots_unnamed()
  args <- rlang::list2(...)

  info <- get_info_from_args(args, .timeout, .session)
  driver <- info$driver
  driver_id <- info$driver_id
  timeout <- info$timeout

  if (uses_selenium(driver)) {
    execute_selenium_expr(expr, args, fn = FALSE, driver = driver, timeout = timeout, driver_id = driver_id, .debug = .debug)
  } else {
    chromote_execute_js_fn(expr, args, .driver = driver, .driver_id = driver_id, .timeout = timeout, fn = FALSE, .debug = .debug)
  }
}

get_info_from_args <- function(args, .session, .timeout) {
  driver <- NULL
  for (arg in args) {
    if (inherits_any(arg, c("selenider_element", "selenider_elements"))) {
      driver <- arg$driver
      driver_id <- arg$driver_id
      arg_timeout <- arg$timeout
      break
    }
  }

  if (is.null(driver)) {
    if (is.null(.session)) {
      .session <- get_session()
    }
    driver <- .session$driver
    if (uses_selenium(driver)) {
      driver <- driver$client
    }

    driver_id <- .session$driver_id
    arg_timeout <- .session$timeout
  }

  timeout <- get_timeout(.timeout, arg_timeout)

  list(
    driver = driver,
    driver_id = driver_id,
    timeout = timeout
  )
}

chromote_execute_js_fn <- function(expr, args, .driver = NULL, .driver_id = NULL, .timeout = NULL, fn = TRUE, .debug = FALSE) {
  rlang::check_installed("jsonlite")

  arg_n <- 0
  expr_body <- ""
  element_args <- list()
  for (i in seq_along(args)) {
    arg <- args[[i]]
    if (is_selenider_element(arg)) {
      name <- get_argument_name(arg_n)
      expr_body <- paste0(expr_body, "let inner_arg_", i, " = ", name, ";")
      element_args <- append(element_args, list(chromote_object_id(backend_id = get_element(arg), driver = arg$driver)))
      arg_n <- arg_n + 1
    } else if (is_selenider_elements(arg)) {
      elements <- get_elements(arg)

      if (length(elements) == 0) {
        expr_body <- paste0(expr_body, "let inner_arg_", i, " = [];")
        next
      }

      names <- get_argument_names(arg_n + seq_along(elements) - 1)
      arg_n <- arg_n + length(elements)

      expr_body <- paste0(expr_body, "let inner_arg_", i, " = [", paste(names, collapse = ","), "];")
      element_args <- c(element_args, lapply(elements, function(x) chromote_object_id(backend_id = x, driver = arg$driver)))
    } else {
      expr_body <- paste0(expr_body, "let inner_arg_", i, " = ", jsonlite::toJSON(arg, auto_unbox = TRUE), ";")
    }
  }

  outer_fn_expr <- if(arg_n <= 1) {
    "function() {"
  } else {
    paste0("function(", paste0("arg_", seq_len(arg_n - 1), collapse = ","), ") {")
  }

  if (fn) {
    arg_names <- names(args)
    inner_args <- if (is.null(arg_names)) {
      if (length(args) == 0) "" else paste0("inner_arg_", seq_along(args), collapse = ",")
    } else {
      prefixes <- ifelse(arg_names == "", "", paste0(arg_names, " = "))
      paste0(prefixes, paste0("inner_arg_", seq_along(args)), collapse = ",")
    }

    inner_fn_expr <- paste0("return (", expr, ")(", inner_args, ");")
  } else {
    arguments_definition <- if (length(args) == 0) {
      "const arguments = [];"
    } else {
      paste0("const arguments = [", paste0("inner_arg_", seq_along(args), collapse = ","), "];")
    }

    inner_fn_expr <- paste0(
      arguments_definition,
      expr
    )
  }

  final_expr <- paste0(
    outer_fn_expr, expr_body, inner_fn_expr, "}"
  )

  first_element <- if (length(element_args) == 0) {
    # Create a mock object that is not actually used.
    chromote_object_id(chromote_root_id(.driver), driver = .driver)
  } else {
    element_args[[1]]
  }

  rest <- lapply(element_args[-1], function(x) list(objectId = x))

  if (.debug) {
    print(final_expr)
  }

  result <- if (length(rest) == 0) {
    .driver$Runtime$callFunctionOn(final_expr, objectId = first_element, returnByValue = FALSE)
  } else {
    .driver$Runtime$callFunctionOn(final_expr, objectId = first_element, arguments = rest, returnByValue = FALSE)
  }

  if (!is.null(result$exceptionDetails)) {
    details <- if (is.null(result$exceptionDetails$exception$description)) {
      result$exceptionDetails$text
    } else {
      result$exceptionDetails$exception$description
    }
    stop_js_error(details)
  } else {
    if (identical(result$result$subtype, "node")) {
      id <- chromote_backend_id(object_id = result$result$objectId, driver = .driver)
      new_js_node(id, .driver, .driver_id, .timeout)
    } else if (identical(result$result$subtype, "array")) {
      object_id <- result$result$objectId
      l <- .driver$Runtime$callFunctionOn("function() { return this.length; }", objectId = object_id)$result$value
      if (l == 0) {
        return(list())
      }

      ids <- vector("list", length = l)
      for (i in seq_len(l)) {
        res_i <- .driver$Runtime$callFunctionOn(paste0("function() { return this[", i - 1, "]; }"), objectId = object_id, returnByValue = FALSE)
        if (!identical(res_i$result$subtype, "node")) {
          return(get_objectid_value(object_id, driver = .driver))
        }
        ids[[i]] <- chromote_backend_id(object_id = res_i$result$objectId, driver = .driver)
      }

      new_js_nodes(ids, .driver, .driver_id, .timeout)
    } else {
      if (is.null(result$objectId)) {
        result$result$value
      } else {
        get_objectid_value(result$result$objectId, driver = .driver)
      }
    }
  }
}

get_objectid_value <- function(x, driver) {
  driver$Runtime$callFunctionOn("function() { return this; }", objectId = x, returnByValue = TRUE)$result$value
}

get_argument_name <- function(n) {
  if (n == 0) "this" else paste0("arg_", n)
}

get_argument_names <- function(n) {
  ifelse(n == 0, "this", paste0("arg_", n))
}

is_selenider_element <- function(x) inherits(x, "selenider_element")

is_selenider_elements <- function(x) inherits(x, "selenider_elements")

new_js_node <- function(x, driver, driver_id, timeout) {
  res <- list(
    driver = driver,
    driver_id = driver_id,
    element = x,
    timeout = timeout,
    selectors = list(new_js_selector()),
    to_be_found = 0
  )

  class(res) <- "selenider_element"

  res
}

new_js_nodes <- function(x, driver, driver_id, timeout) {
  res <- list(
    driver = driver,
    driver_id = driver_id,
    element = x,
    timeout = timeout,
    selectors = list(new_js_selector()),
    to_be_found = 0
  )

  class(res) <- c("selenider_elements", "list")

  res
}

new_js_selector <- function() {
  res <- list(filters = list(), to_be_filtered = 0)

  class(res) <- "selenider_js_selector"

  res
}

parse_selenium_result <- function(x, driver, driver_id, timeout) {
  if (is_selenium_element(x)) {
    new_js_node(as_webelement(x, driver), driver, driver_id, timeout)
  } else if (inherits(x, "webElement")) {
    new_js_node(x, driver, driver_id, timeout)
  } else if (is.list(x)) {
    if (length(x) == 0) {
      return(NULL)
    } else if (length(x) == 1 && inherits(x[[1]], "webElement")) {
      return(new_js_node(x[[1]], driver, driver_id, timeout))
    }

    ret <- TRUE
    res <- vector("list", length = length(x))

    for (i in seq_along(x)) {
      a <- x[[i]]
      if (is_selenium_element(a)) {
        res[[i]] <- as_webelement(a, driver)
      } else if (inherits(a, "webElement")) {
        res[[i]] <- a
      } else {
        ret <- FALSE
        break
      }
    }

    if (ret) {
      new_js_nodes(res, driver, driver_id, timeout)
    } else {
      unpack_list(x)
    }
  } else {
    unpack_list(x)
  }
}

is_selenium_element <- function(x) {
  is.list(x) && (length(names(x)) == 1) && (nchar(names(x)) == 35L) && grepl("^element", names(x))
}

as_webelement <- function(x, driver) {
  rlang::check_installed("RSelenium")
  RSelenium::webElement$new(as.character(x))$import(driver$export("remoteDriver"))
}

execute_selenium_expr <- function(expr, args, fn = FALSE, driver, timeout, driver_id, .debug = FALSE) {
  n <- 0
  expr_body <- ""
  final_args <- list()
  for (i in seq_along(args)) {
    arg <- args[[i]]
    if (is_selenider_element(arg)) {
      name <- paste0("inner_arg_", i)
      expr_body <- paste0(expr_body, name, " = arguments[", n, "];")
      final_args <- append(final_args, list(get_actual_element(arg, timeout = timeout)))
      n <- n + 1
    } else if (is_selenider_elements(arg)) {
      elements <- get_actual_elements(arg, timeout = timeout)

      if (length(elements) == 0) {
        expr_body <- paste0(expr_body, "let inner_arg_", i, " = [];")
        next
      }

      names <- paste0("arguments[", n + seq_along(elements) - 1, "]")
      n <- n + length(elements)

      expr_body <- paste0(expr_body, "let inner_arg_", i, " = [", paste(names, collapse = ","), "];")
      final_args <- append(final_args, elements)
    } else {
      expr_body <- paste0(expr_body, "let inner_arg_", i, " = arguments[", n, "];")
      final_args <- append(final_args, list(arg))
      n <- n + 1
    }
  }

  inner_args <- if (is.null(i)) "" else paste0("inner_arg_", seq_len(i), collapse = ", ")
  if (fn) {
    return_expr <- paste0("return (", expr, ")(", inner_args, ");")
  } else {
    return_expr <- paste0(
      "return (() => {",
      "const arguments = [", inner_args, "];",
      expr,
      "})();"
    )
  }

  final_expr <- paste0(
    expr_body,
    return_expr
  )

  if (.debug) {
    print(final_expr)
  }

  final_args <- if (length(final_args) == 0) list("") else final_args

  result <- driver$executeScript(final_expr, args = final_args)

  parse_selenium_result(result, driver, driver_id, timeout)
}
