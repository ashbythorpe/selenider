#' Execute a JavaScript function
#'
#' Execute a JavaScript function on zero or more arguments.
#'
#' @param fn A string defining the function.
#' @param ... Arguments to the function.
#' @param timeout How long to wait for any elements to exist in the DOM.
#' @param session The session to use, if `...` does not contain any
#'   selenider elements.
#'
#' @details
#' `...` can contain `selenider_element`/`selenider_elements` objects,
#' which will be collected and then passed into the function. However,
#' more complex objects (e.g. lists of selenider elements) will not be
#' moved into the JavaScript world correctly.
#'
#' Additionally, if the JavaScript function returns a node or array of
#' nodes, they will be turned back into `selenider_element`/`selenider_elements`
#' objects.
#'
#' @returns
#' The return value of the JavaScript function, turned back into an R object.
#'
#' @export
execute_js_fn <- function(fn, ..., timeout = NULL, session = NULL) {
  args <- rlang::list2(...)
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
    if (is.null(session)) {
      session <- get_session()
    }
    driver <- session$driver
    driver_id <- session$driver_id
    arg_timeout <- session$timeout
  }

  timeout <- get_timeout(timeout, arg_timeout)

  if (uses_selenium(driver)) {
    script <- paste0("let fn = ", fn, ";", "return fn(...arguments);")
    final_args <- vector("list", length = length(args))
    for (i in seq_along(args)) {
      arg <- args[[i]]
      if (inherits(arg, "selenider_element")) {
        final_args[[i]] <- get_actual_element(arg, timeout = timeout)
      } else if (inherits(arg, "selenider_elements")) {
        final_args[[i]] <- get_actual_elements(arg, timeout = timeout)
      } else {
        final_args[[i]] <- arg
      }
    }
    unpack_list(driver$executeScript(script, args))
  } else {
    chromote_execute_js_fn(fn, args, .driver = driver, .driver_id = driver_id, .timeout = timeout)
  }
}

chromote_execute_js_fn <- function(fn, args, .driver = NULL, .driver_id = NULL, .timeout = NULL) {
  rlang::check_installed("jsonlite")

  arg_n <- 0
  expr <- ""
  element_args <- list()
  for (i in seq_along(args)) {
    arg <- args[[i]]
    if (is_selenider_element(arg)) {
      name <- get_argument_name(arg_n)
      expr <- paste0(expr, "let inner_arg_", i, " = ", name, ";")
      element_args <- append(element_args, list(chromote_object_id(backend_id = get_element(arg), driver = arg$driver)))
      arg_n <- arg_n + 1
    } else if (is_selenider_elements(arg)) {
      elements <- get_elements(arg)

      if (length(elements) == 0) {
        next
      }

      names <- character(length(elements))
      names <- get_argument_names(arg_n + seq_along(elements) - 1)
      arg_n <- arg_n + length(elements)

      expr <- paste0(expr, "let inner_arg_", i, " = [", paste(names, collapse = ","), "];")
      element_args <- c(element_args, lapply(elements, function(x) chromote_object_id(backend_id = x, driver = arg$driver)))
    } else {
      expr <- paste0(expr, "let inner_arg_", i, " = ", jsonlite::toJSON(arg, auto_unbox = TRUE), ";")
    }
  }
  print(expr)

  outer_fn_expr <- if(arg_n <= 1) {
    "function() {"
  } else {
    paste0("function(", paste0("arg_", seq_len(arg_n - 1), collapse = ","), ") {")
  }

  arg_names <- names(args)
  inner_args <- if (is.null(arg_names)) {
    if (length(args) == 0) "" else paste0("inner_arg_", seq_along(args), collapse = ",")
  } else {
    prefixes <- ifelse(arg_names == "", "", paste0(arg_names, " = "))
    paste0(prefixes, paste0("inner_arg_", seq_along(args)), collapse = ",")
  }
  inner_fn_expr <- paste0("return (", fn, ")(", inner_args, ");")

  final_expr <- paste0(
    outer_fn_expr, expr, inner_fn_expr, "}"
  )

  first_element <- if (length(element_args) == 0) {
    # Create a mock object that is not actually used.
    chromote_object_id(chromote_root_id(.driver), driver = .driver)
  } else {
    element_args[[1]]
  }

  rest <- lapply(element_args[-1], function(x) list(objectId = x))

  print(final_expr)
  result <- if (length(rest) == 0) {
    .driver$Runtime$callFunctionOn(final_expr, objectId = first_element, returnByValue = FALSE)
  } else {
    .driver$Runtime$callFunctionOn(final_expr, objectId = first_element, arguments = rest, returnByValue = FALSE)
  }
  print(result)

  if (!is.null(result$exceptionDetails)) {
    details <- if (is.null(result$exceptionDetails$exception$description)) {
      result$exceptionDetails$text
    } else {
      result$exceptionDetails$exception$description
    }
    cli::cli_abort(c(
      "JavaScript function returned the following error:",
      "i" = "{.val {details}}"
    ))
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
  res <- list(filters = list())

  class(res) <- "selenider_js_selector"

  res
}
