#' Test one or more conditions on HTML elements
#'
#' @description
#' `elem_expect()` waits for a set of conditions to return TRUE. If, after a
#' certain period of time (by default 4 seconds), this does not happen, an
#' informative error is thrown. Otherwise, the original element is returned.
#'
#' `elem_wait_until()` does the same, but returns a logical value (whether or
#' not the test passed), allowing you to handle the failure case explicitly.
#'
#' @param x A `selenider_element`/`selenider_elements` object, or a condition.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Function calls or functions
#'   that must return a logical value. If multiple conditions are given, they
#'   must all be `TRUE` for the test to pass.
#' @param testthat Whether to treat the expectation as a `testthat` test. You
#'   *do not* need to explicitly provide this most of the time, since by
#'   default, we can use [testthat::is_testing()] to figure out whether
#'   `elem_expect()` is being called from within a `testthat` test.
#' @param timeout The number of seconds to wait for a condition to pass. If not
#'   specified, the timeout used for `x` will be used, or the timeout of the
#'   local session if an element is not given.
#'
#' @details
#' # Conditions
#' Conditions can be supplied as functions or calls.
#'
#' Functions allow you to use unary conditions without formatting them as a
#' call (e.g. `is_present` rather than `is_present()`). It also allows you to
#' make use of R's [anonymous function syntax][base::function] to quickly
#' create custom conditions. `x` will be supplied as the first argument of this
#' function.
#'
#' Function calls allow you to use conditions that take multiple arguments
#' (e.g. `has_text()`) without the use of an intermediate function. The call
#' will be modified so that `x` is the first argument to the function call. For
#' example, `has_text("a")` will be modified to become: `has_text(x, "a")`.
#'
#' The and (`&&`), or (`||`) and not (`!`) functions can be used on both types
#' of conditions. If more than one condition are given in `...`, they are
#' combined using `&&`.
#'
#' # Custom conditions
#' Any function which takes a selenider element or element collection as its
#' first argument, and returns a logical value, can be used as a condition.
#'
#' Additionally, these functions provide a few features that make creating
#' custom conditions easy:
#'
#' * Errors with class `expect_error_continue` are handled, and
#'   the function is prevented from terminating early. This means that if
#'   an element is not found, the function will retry instead of immediately
#'   throwing an error.
#' * `selenider` functions used inside conditions have their timeout, by
#'   default, set to 0, ignoring the local timeout. This is important, since
#'   `elem_expect()` and `elem_wait_until()` implement a retry mechanic
#'   manually. To override this default, manually specify a timeout.
#'
#' These two features allow you to use functions like [elem_text()] to access
#' properties of an element, without needing to worry about the errors that
#' they throw or the timeouts that they use. See Examples for a few examples of
#' a custom condition.
#'
#' These custom conditions can also be used with [elem_filter()] and
#' [elem_find()].
#'
#' @returns
#' `elem_expect()` invisibly returns the element(s) `x`, or `NULL` if an
#' element or collection of elements was not given in `x`.
#'
#' `elem_wait_for()` returns a boolean flag: TRUE if the test passes, FALSE
#' otherwise.
#'
#' @seealso
#' * [is_present()] and other conditions for predicates for HTML elements.
#'   (If you scroll down to the *See also* section, you will find the rest).
#' * [elem_expect_all()] and [elem_wait_until_all()] for an easy way to test a
#'   single condition on multiple elements.
#' * [elem_filter()] and [elem_find()] to use conditions to filter elements.
#'
#' @examplesIf selenider::selenider_available(online = FALSE)
#' html <- "
#' <div class='class1'>
#' <button id='disabled-button' disabled>Disabled</button>
#' <p>Example text</p>
#' <button id='enabled-button'>Enabled</button>
#' </div>
#'
#' <div class='class3'>
#' </div>
#' "
#' session <- minimal_selenider_session(html)
#'
#' s(".class1") |>
#'   elem_expect(is_present)
#'
#' s("#enabled-button") |>
#'   elem_expect(is_visible, is_enabled)
#'
#' s("#disabled-button") |>
#'   elem_expect(is_disabled)
#'
#' # Error: element is visible but not enabled
#' s("#disabled-button") |>
#'   elem_expect(is_visible, is_enabled, timeout = 0.5) |>
#'   try() # Since this condition will fail
#'
#' s(".class2") |>
#'   elem_expect(!is_present, !is_in_dom, is_absent) # All 3 are equivalent
#'
#' # All other conditions will error if the element does not exist
#' s(".class2") |>
#'   elem_expect(is_invisible, timeout = 0.1) |>
#'   try()
#'
#' # elem_expect() returns the element, so can be used in chains
#' s("#enabled-button") |>
#'   elem_expect(is_visible && is_enabled) |>
#'   elem_click()
#' # Note that elem_click() will do this automatically
#'
#' s("p") |>
#'   elem_expect(is_visible, has_exact_text("Example text"))
#'
#' # Or use an anonymous function
#' s("p") |>
#'   elem_expect(\(elem) identical(elem_text(elem), "Example text"))
#'
#' # If your conditions are not specific to an element, you can omit the `x`
#' # argument
#' elem_1 <- s(".class1")
#' elem_2 <- s(".class2")
#'
#' elem_expect(is_present(elem_1) || is_present(elem_2))
#'
#' # We can now use the conditions on their own to figure out which element
#' # exists
#' if (is_present(elem_1)) {
#'   print("Element 1 is visible")
#' } else {
#'   print("Element 2 is visible")
#' }
#'
#' # Use elem_wait_until() to handle failures manually
#' elem <- s(".class2")
#' if (elem_wait_until(elem, is_present)) {
#'   elem_click(elem)
#' } else {
#'   reload()
#' }
#'
#' # Creating a custom condition is easiest with an anonymous function
#' s("p") |>
#'   elem_expect(
#'     \(elem) elem |>
#'       elem_text() |>
#'       grepl(pattern = "Example .*")
#'   )
#'
#' # Or create a function, to reuse the condition multiple times
#' text_contains <- function(x, pattern) {
#'   text <- elem_text(x)
#'
#'   grepl(pattern, text)
#' }
#'
#' s("p") |>
#'   elem_expect(text_contains("Example *"))
#'
#' # If we want to continue on error, we need to use the
#' # "expect_error_continue" class.
#' # This involves making a custom error object.
#' error_condition <- function() {
#'   my_condition <- list(message = "Custom error!")
#'   class(my_condition) <- c("expect_error_continue", "error", "condition")
#'   stop(my_condition)
#' }
#'
#' # This is much easier with rlang::abort() / cli::cli_abort():
#' error_condition_2 <- function() {
#'   rlang::abort("Custom error!", class = "expect_error_continue")
#' }
#'
#' # This error will not be caught
#' try(elem_expect(stop("Uncaught error!")))
#'
#' # These will eventually throw an error, but will wait 0.5 seconds to do so.
#' try(elem_expect(error_condition(), timeout = 0.5))
#' try(elem_expect(error_condition_2(), timeout = 0.5))
#'
#' \dontshow{
#' # Clean up all connections and invalidate default chromote object
#' selenider_cleanup()
#' }
#'
#' @export
elem_expect <- function(x, ..., testthat = NULL, timeout = NULL) {
  x <- enquo(x)
  dots <- enquos(...)

  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(testthat, allow_null = TRUE)
  # `testthat` can only be TRUE if it is installed.
  if (is.null(testthat)) {
    testthat <- is_installed("testthat") && testthat::is_testing()
  } else if (testthat) {
    check_installed("testthat", reason = "for `elem_expect(testthat = TRUE)`.")
  }

  result <- eval_conditions(x, dots, timeout)
  timeout <- result$timeout
  calls <- result$calls
  exprs <- result$exprs
  res <- result$res
  x_res <- result$x_res

  if (!isTRUE(res)) {
    n <- res$n
    val <- res$val
    classes <- c("selenider_element", "selenider_elements")
    x_res <- if (inherits_any(x_res, classes)) {
      x_res
    } else {
      NULL
    }

    call <- calls[[n]]
    expr <- exprs[[n]]

    diagnose_condition(
      x_res,
      n = n,
      call = call,
      original_expr = expr,
      result = val,
      timeout = timeout,
      testthat = testthat,
      original_env = quo_get_env(x),
    )
  }

  if (testthat) {
    # We have already checked that testthat is installed if `testthat` is
    # `TRUE`.
    testthat::succeed()
  }

  if (inherits(x_res, c("selenider_element", "selenider_elements"))) {
    invisible(x_res)
  } else {
    invisible(NULL)
  }
}

#' @export
#'
#' @rdname elem_expect
elem_wait_until <- function(x, ..., timeout = NULL) {
  x <- enquo(x)
  dots <- enquos(...)

  check_number_decimal(timeout, allow_null = TRUE)
  result <- eval_conditions(x, dots, timeout)
  res <- result$res

  return(isTRUE(res))
}

#' If a condition fails, throw an informative error
#'
#' Throw an informative error (or test failure) for a [elem_expect()]
#' failure.
#'
#' @param x The element that conditions are evaluated on.
#' @param n The condition number that failed.
#' @param call The formatted call that failed when executed.
#' @param original_expr The non-formatted original expression of the condition
#'   that failed.
#' @param result The result returned by the condition (since the condition fails
#'   if this is not `TRUE`).
#' @param timeout The number of seconds of timeout that was used.
#' @param original_env The environment in which the conditions wer evaluated.
#' @param testthat Whether to create a testthat test failure instead of an
#'   error.
#' @param call_name The name of the condition.
#' @param negated_call_name The negated name of the condition.
#' @param call_env The environment of the [elem_expect()] call, to throw the
#'   error in.
#' @param x_name Used by [elem_expect_all()] to specify that a specific
#'   argument failed (e.g. "x[[1]]").
#'
#' @noRd
diagnose_condition <- function(x,
                               n,
                               call,
                               original_expr,
                               result,
                               timeout,
                               original_env,
                               testthat,
                               call_env = caller_env(),
                               x_name = "x") {
  call_name <- if (is_call_simple(call)) call_name(call) else ""

  expr_print <- paste0(
    expr_deparse(quo_squash(original_expr)), collapse = "\n"
  )

  if (timeout == 0) {
    condition <- c(
      "Condition failed:",
      "{.code {expr_print}}"
    )
  } else {
    condition <- c(
      "Condition failed after waiting for {timeout} seconds:",
      "{.code {expr_print}}"
    )
  }

  parent <- NULL
  if (!identical(result, FALSE)) {
    if (inherits(result, "error")) {
      parent <- result
    } else {
      condition <- c(
        condition,
        "i" = paste0(
          "The condition returned {.obj_type_friendly {result}} instead of ",
          "{.val {TRUE}}."
        )
      )

      if (is_function(result) && n == 1) {
        condition <- c(
          condition,
          "i" = "Did you forget to supply {.arg x}?",
          "x" = "Instead of {.code elem_expect({expr_print})}",
          "v" = "Use: {.code elem_expect(element, {expr_print})}"
        )

        stop_expect_error(condition, parent = parent, call = call_env)
      }
    }
  }

  condition_result <- diagnose_condition_call(
    condition,
    x,
    call = call,
    call_name = call_name,
    original_env = original_env
  )
  condition <- condition_result$condition
  call_name <- condition_result$call_name
  negated_call_name <- condition_result$negated_call_name

  if (testthat) {
    # We have already checked that testthat is installed if `testthat` is
    # `TRUE`.
    elem_expect_fail(
      condition,
      parent = parent,
      call = call_env,
      x = x,
      x_name = x_name
    )
  } else {
    stop_expect_error(condition, parent = parent, call = call_env)
  }
}

get_call_arg <- function(call, name) {
  fn <- eval_tidy(parse_expr(call_name(call)), env = ns_env("selenider"))
  args <- call_args(call_match(quo_get_expr(call), fn))
  args[[name]]
}

diagnose_condition_call <- function(condition,
                                    x,
                                    call,
                                    call_name,
                                    original_env,
                                    negated_call_name = NULL) {
  call_name <- switch(call_name,
    is_in_dom = "is in the DOM",
    has_name = "has tag name",
    gsub("_", " ", call_name, fixed = TRUE)
  )

  condition <- if (call_name %in% c("(", "!")) {
    return(diagnose_condition_inverse(
      condition,
      x = x,
      call_name = call_name,
      call = call,
      original_env = original_env
    ))
  } else if (call_name %in% condition_dependencies$none) {
    diagnose_condition_none(condition, call_name, negated_call_name)
  } else if (call_name %in% condition_dependencies$name) {
    diagnose_condition_name(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else if (call_name %in% condition_dependencies$text) {
    diagnose_condition_text(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else if (call_name %in% condition_dependencies$attribute) {
    diagnose_condition_attribute(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else if (call_name %in% condition_dependencies$value) {
    diagnose_condition_value(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else if (call_name %in% condition_dependencies$css) {
    diagnose_condition_css(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else if (call_name %in% condition_dependencies$length) {
    diagnose_condition_length(
      condition,
      x = x,
      call_name = call_name,
      negated_call_name = negated_call_name,
      call = call,
      original_env = original_env
    )
  } else {
    diagnose_condition_general(condition, x)
  }

  list(
    condition = condition,
    call_name = call_name,
    negated_call_name = negated_call_name
  )
}

diagnose_condition_inverse <- function(condition,
                                       x,
                                       call_name,
                                       call,
                                       original_env) {
  invert <- FALSE
  inner_call_name <- call_name
  new_call <- call
  while (inner_call_name %in% c("(", "!")) {
    if (inner_call_name == "!") {
      invert <- !invert
    }

    new_call <- call_args(new_call)[[1]]

    inner_call_name <- if (is_call_simple(new_call)) {
      call_name(new_call)
    } else {
      ""
    }
  }

  inner_call_name <- switch(inner_call_name,
    is_in_dom = "is in the DOM",
    has_name = "has tag name",
    gsub("_", " ", inner_call_name, fixed = TRUE)
  )

  if (!invert) {
    return(diagnose_condition_call(
      condition,
      x,
      call = new_call,
      call_name = inner_call_name,
      original_env = original_env
    ))
  }

  new_call_name <- negate_call_name(inner_call_name)

  diagnose_condition_call(
    condition,
    x,
    call = new_call,
    call_name = new_call_name,
    negated_call_name = inner_call_name,
    original_env = original_env
  )
}

diagnose_condition_none <- function(condition, call_name, negated_call_name) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  c(
    condition,
    "i" = paste0("{.arg {x_name}} ", negated_call_name, ".")
  )
}

diagnose_condition_name <- function(condition,
                                    x,
                                    call_name,
                                    call,
                                    negated_call_name,
                                    original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  expected_name <- eval_tidy(get_call_arg(call, "name"), env = original_env)
  actual_name <- get_or_null(x, elem_name, timeout = 0)

  if (is.null(actual_name) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(expected_name),
        "."
      )
    )
  } else {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(expected_name),
        "."
      ),
      "i" = paste0("Actual tag name: ", format_value(actual_name), ".")
    )
  }
}

diagnose_condition_text <- function(condition,
                                    x,
                                    call_name,
                                    negated_call_name,
                                    call,
                                    original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  target_text <- eval_tidy(get_call_arg(call, "text"), env = original_env)
  actual_text <- get_or_null(x, elem_text, timeout = 0)

  if (is.null(actual_text) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(target_text),
        "."
      )
    )
  } else {
    actual_text <- tryCatch(elem_text(x, timeout = 0), error = function(e) NULL)

    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(target_text),
        "."
      ),
      "i" = paste0("Actual text: ", format_value(actual_text), ".")
    )
  }
}

diagnose_condition_attribute <- function(condition,
                                         x,
                                         call_name,
                                         negated_call_name,
                                         call,
                                         original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  negated_call_name <- switch(
    negated_call_name,
    "has attr" = "is",
    "attr contains" = "contains",
    "does not have attr" = "is not",
    "attr does not contain" = "does not contain"
  )

  name <- eval_tidy(get_call_arg(call, "name"), env = original_env)
  expected_value <- eval_tidy(get_call_arg(call, "value"), env = original_env)
  actual_value <- get_or_null(x, elem_attr, name, timeout = 0)

  if (is.null(actual_value) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}}'s ",
        format_value(name),
        " attribute ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      )
    )
  } else {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}}'s ",
        format_value(name),
        " attribute ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      ),
      "i" = paste0("Actual value: ", format_value(actual_value), ".")
    )
  }
}

diagnose_condition_value <- function(condition,
                                     x,
                                     call_name,
                                     negated_call_name,
                                     call,
                                     original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  value <- eval_tidy(get_call_arg(call, "value"), env = original_env)
  expected_value <- eval_tidy(get_call_arg(call, "value"), env = original_env)
  actual_value <- get_or_null(x, elem_value, timeout = 0)

  if (is.null(actual_value) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      )
    )
  } else {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}} ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      ),
      "i" = paste0("Actual value: ", format_value(actual_value), ".")
    )
  }
}

diagnose_condition_css <- function(condition,
                                   x,
                                   call_name,
                                   negated_call_name,
                                   call,
                                   original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  negated_call_name <- switch(
    negated_call_name,
    "has css property" = "is",
    "does not have css property" = "is not"
  )

  name <- eval_tidy(get_call_arg(call, "property"), env = original_env)
  expected_value <- eval_tidy(get_call_arg(call, "value"), env = original_env)
  actual_value <- get_or_null(x, elem_css_property, name, timeout = 0)

  if (is.null(actual_value) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}}'s ",
        format_value(name),
        " CSS property ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      )
    )
  } else {
    c(
      condition,
      "i" = paste0(
        "{.arg {x_name}}'s ",
        format_value(name),
        " CSS property ",
        negated_call_name,
        " ",
        format_value(expected_value),
        "."
      ),
      "i" = paste0("Actual value: ", format_value(actual_value), ".")
    )
  }
}

diagnose_condition_length <- function(condition,
                                      x,
                                      call_name,
                                      negated_call_name,
                                      call,
                                      original_env) {
  if (is.null(negated_call_name)) {
    negated_call_name <- negate_call_name(call_name)
  }

  value <- eval_tidy(get_call_arg(call, "n"), env = original_env)
  elements <- if (value == 1) "element" else "elements"
  actual_length <- get_or_null(x, elem_size, timeout = 0)

  cond <- switch(
    negated_call_name,
    "has length" = paste("contains", value, elements),
    "does not have length" = paste("does not contain", value, elements),
    "has at least" = paste("contains at least", value, elements),
    "does not have at least" = paste("contains less than", value, elements),
  )

  if (is.null(actual_length) && !is_in_dom(x)) {
    c(
      condition,
      "i" = paste0("{.arg {x_name}} ", cond, ".")
    )
  } else {
    c(
      condition,
      "i" = paste0("{.arg {x_name}} ", cond, "."),
      "i" = paste0("Actual number of elements: {.val {", actual_length, "}}.")
    )
  }
}

diagnose_condition_general <- function(condition,
                                       x) {
  if (inherits(x, "selenider_element")) {
    if (is_present(x)) {
      c(
        condition,
        "i" = "{.arg {x_name}} exists, but the condition still failed."
      )
    } else {
      c(
        condition,
        "i" = paste0(
          "{.arg {x_name}} does not exist, which may have caused the ",
          "condition to fail."
        )
      )
    }
  } else if (inherits(x, "selenider_elements")) {
    l <- tryCatch(
      elem_size(x, timeout = 0),
      selenider_error_absent_parent = function(e) NULL
    )

    if (is.null(l)) {
      c(
        condition,
        "i" = paste0(
          "{.arg {x_name}}'s parent element does not exist, which may have ",
          "caused the condition to fail."
        )
      )
    } else if (l == 0L) {
      c(
        condition,
        "i" = paste0(
          "{.arg {x_name}} contains no elements, which may have caused the ",
          "condition to fail."
        )
      )
    } else {
      c(
        condition,
        "i" = paste0(
          "{.arg {x_name}} contains {.val {",
          l,
          "}} element{?s}, but the condition still failed."
        )
      )
    }
  }
}

negate_call_name <- function(x) {
  # Replace e.g. !is present with is not present, but cancel out double
  # negatives
  switch(x,
    "is present" = "is not present",
    "is in the DOM" = "is not in the DOM",
    "is missing" = "is present",
    "is absent" = "is in the DOM",
    "is visible" = "is not visible",
    "is displayed" = "is not displayed",
    "is hidden" = "is displayed",
    "is invisible" = "is visible",
    "is enabled" = "is not enabled",
    "is disabled" = "is enabled",
    "has tag name" = "does not have tag name",
    "has text" = "does not have text",
    "has exact text" = "does not have exact text",
    "has attr" = "does not have attr",
    "attr contains" = "attr does not contain",
    "has value" = "does not have value",
    "has css property" = "does not have css property",
    "has length" = "does not have length",
    "has at least" = "does not have at least",
    ""
  )
}

get_or_null <- function(x, .f, ...) {
  if (is.null(x)) {
    NULL
  } else {
    tryCatch(.f(x, ...), error = function(e) NULL)
  }
}
