#' Test one or more conditions on HTML elements
#' 
#' @description 
#' `html_expect()` waits for a set of conditions to return TRUE. If, after a
#' certain period of time (by default 4 seconds), this does not happen, an
#' informative error is thrown. Otherwise, the original element is returned.
#' 
#' `html_wait_until()` does the same, but returns a logical value (whether or
#' not the test passed), allowing you to handle the failure case explicitly.
#' 
#' @param x A `selenider_element`/`selenider_elements` object, or a condition.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Function calls or functions
#'   that must return a logical value. If multiple conditions are given, they
#'   must all be `TRUE` for the test to pass.
#' @param testthat Whether to treat the expectation as a `testthat` test. You 
#'   *do not* need to explicitly provide this most of the time, since by default,
#'   we can use [testthat::is_testing()] to figure out whether `html_expect()` is
#'   being called from within a `testthat` test.
#' @param timeout The number of seconds to wait for a condition to pass. If not
#'   specified, the timeout used for `x` will be used, or the timeout of the
#'   local session if an element is not given.
#' 
#' @details
#' # Conditions
#' Conditions can be supplied as functions or calls.
#' 
#' Functions allow you to use unary conditions without formatting them as a
#' call (e.g. `is_present` rather than `is_present()`). It also allows you to make
#' use of R's [anonymous function syntax][base::function] to quickly create
#' custom conditions. `x` will be used as the first argument of this function.
#' 
#' Function calls allow you to use conditions that take multiple arguments (e.g.
#' `has_text()`) without the use of an intermediate function. The call will
#' be modified so that `x` is the first argument to the function call. For 
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
#' * Errors with class `selenider_element_absent_element` are handled, and
#'   the function is prevented from terminating early. This means that if
#'   an element is not found, the function will retry instead of immediately
#'   throwing an error.
#' * `selenider` functions used inside conditions have their timeout, by
#'   default, set to 0, ignoring the local timeout. This is important, since
#'   `html_expect()` and `html_wait_until()` implement a retry mechanic
#'   manually. To override this default, manually specify a timeout.
#'
#' These two features allow you to use functions like [html_text()] to access
#' properties of an element, without needing to worry about the errors that
#' they throw or the timeouts that they use. See Examples for an example of a
#' custom condition.
#'
#' These custom conditions can also be used with [html_filter()] and
#' [html_find()].
#'
#' @returns 
#' `html_expect()` returns the element(s) `x`, or `NULL` if an element or
#' collection of elements was not given in `x`.
#' 
#' `html_wait_for()` returns a boolean flag: TRUE if the test passes, FALSE
#' otherwise.
#' 
#' @seealso 
#' * [is_present()] and other conditions for predicates for a single HTML element.
#' * [has_length()] and other conditions for predicates for multiple HTML elements.
#' * [html_expect_all()] and [html_wait_until_all()] for an easy way to test a single
#'   condition on multiple elements.
#' * [html_filter()] and [html_find()] to use conditions to filter elements.
#' 
#' @examples
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_expect(is_present)
#'
#' s(".class1") |>
#'   html_expect(is_visible, is_enabled)
#' 
#' s(".class1") |>
#'   html_expect(is_visible || is_enabled)
#' 
#' s(".class2") |>
#'   html_expect(!is_present, timeout = 0.5) |>
#'   try() # Since this condition will fail
#' # Or is_absent, etc.
#'
#' # html_expect() returns the element, so can be used in chains
#' s(".button1") |>
#'   html_expect(is_visible) |>
#'   click()
#' # Note that click() will do this automatically
#'
#' s(".text1") |>
#'   html_expect(has_exact_text("Example text"))
#'
#' # Or use an anonymous function
#' s(".text1") |>
#'   html_expect(\(elem) identical(html_text(elem), "Example text"))
#'
#' # If your conditions are not specific to an element, you can omit the `x` argument
#' elem_1 <- s(".class1")
#' elem_2 <- s(".class2")
#'
#' html_expect(is_present(elem_1) || is_present(elem_2))
#' 
#' # We can now use the conditions on their own to figure out which element exists
#' if (is_present(elem_1)) {
#'   click(elem_1)
#' } else {
#'   click(elem_2)
#' }
#'
#' # Use html_wait_for() to handle failures manually
#' elem <- s(".class2")
#' if (html_wait_until(elem, is_present)) {
#'   click(elem)
#' } else {
#'   reload()
#' }
#'
#' # Creating a custom condition is easiest with an anonymous function
#' s(".text1") |>
#'   html_expect(
#'     \(elem) elem |>
#'       html_text() |>
#'       grepl(pattern = "Example .*")
#'   )
#' 
#' # Or create a function, to reuse the condition multiple times
#' text_contains <- function(x, pattern) {
#'   text <- html_text(x)
#'   
#'   grepl(pattern, text)
#' }
#'
#' s(".text1") |>
#'   html_expect(text_contains("Example *"))
#' 
#' @export
html_expect <- function(x, ..., testthat = NULL, timeout = NULL) {
  x <- enquo(x)
  dots <- enquos(...)

  check_number_decimal(timeout, allow_null = TRUE)
  check_bool(testthat, allow_null = TRUE)
  # `testthat` can only be TRUE if it is installed.
  if (is.null(testthat)) {
    testthat <- is_installed("testthat") && testthat::is_testing()
  } else {
    if (testthat) {
      check_installed("testthat", reason = "for `html_expect(testthat = TRUE)`.")
    }
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
    x_res <- if (inherits(x_res, c("selenider_element", "selenider_elements"))) x_res else NULL
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
    # We have already checked that testthat is installed if `testthat` is `TRUE`.
    testthat::succeed()
  }

  if (inherits(x_res, c("selenider_element", "selenider_elements"))) {
    x_res
  } else {
    NULL
  }
}

diagnose_condition <- function(x,
                               n,
                               call,
                               original_expr,
                               result,
                               timeout,
                               original_env,
                               testthat,
                               call_name = NULL,
                               negated_call_name = NULL,
                               call_env = caller_env(),
                               x_name = "x") {
  if (is.null(call_name)) {
    call_name <- if (is_call_simple(call)) call_name(call) else ""
  }
  
  expr_print <- paste0(
    expr_deparse(quo_squash(original_expr)), collapse = "\n"
  )

  condition <- c(
    "Condition failed after waiting for {timeout} seconds:",
    "{.code {expr_print}}"
  )
  
  call_name <- switch(call_name,
      is_in_dom = "is in the DOM",
      has_name = "has tag name",
      gsub("_", " ", call_name, fixed = TRUE)
  )

  parent <- NULL
  if (!identical(result, FALSE)) {
    if (inherits(result, "error")) {
      parent <- result
    } else {
      condition <- c(
        condition,
        "i" = "The condition returned {.obj_type_friendly {result}} instead of {.val {TRUE}}."
      )

      if (is_function(result) && n == 1) {
        condition <- c(
          condition,
          "i" = "Did you forget to supply {.arg x}?",
          "x" = "Instead of {.code html_expect({expr_print})}",
          "v" = "Use: {.code html_expect(element, {expr_print})}"
        )
      }
    }
  }

  if (call_name == "!") {
    invert <- TRUE
    inner_call_name <- call_name
    while (inner_call_name == "!") {
      new_call <- call_args(call)[[1]]
      inner_call_name <- if (is_call_simple(new_call)) call_name(new_call) else ""
      invert <- !invert
    }
    
    if (!invert) {
      return(diagnose_condition(
        x, n, new_call, original_expr, result, timeout, inner_call_name,
        call_env = call_env
      ))
    }

    new_call_name <- negate_call_name(inner_call_name)

    if (!is.null(x)) {
      return(diagnose_condition(
        x,
        n = n,
        call = new_call,
        original_expr = original_expr,
        result = result,
        timeout = timeout,
        call_name = new_call_name,
        negated_call_name = inner_call_name,
        call_env = call_env
      ))
    }
  } else if (call_name %in% condition_dependencies$none) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    condition <- c(
      condition,
      "i" = "{.arg {x_name}} {negated_call_name}."
    )
  } else if (call_name %in% condition_dependencies$name) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    expected_name <- eval_tidy(call_args(call)[[2]], env = original_env)

    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {expected_name}}."
      )
    } else {
      actual_text <- tryCatch(html_name(x, timeout = 0), error = function(e) NULL)

      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {expected_name}}.",
        "i" = "Actual text: {.val {expected_name}}."
      )
    }
  } else if (call_name %in% condition_dependencies$text) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    target_text <- eval_tidy(call_args(call)[[2]], env = original_env)

    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {target_text}}."
      )
    } else {
      actual_text <- tryCatch(html_text(x, timeout = 0), error = function(e) NULL)

      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {target_text}}.",
        "i" = "Actual text: {.val {actual_text}}."
      )
    }
  } else if (call_name %in% condition_dependencies$attribute) {
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

    name <- eval_tidy(call_args(call)[[2]], env = original_env)
    expected_value <- eval_tidy(call_args(call)[[3]], env = original_env)
    
    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}}'s {.val {name}} attribute {negated_call_name} {.val {expected_value}}."
      )
    } else {
      actual_value <- tryCatch(html_attr(x, name, timeout = 0), error = function(e) NULL)

      condition <- c(
        condition,
        "i" = "{.arg {x_name}}'s {.val {name}} attribute {negated_call_name} {.val {expected_value}}.",
        "i" = "Actual value: {.val {actual_value}}."
      )
    }
  } else if (call_name %in% condition_dependencies$attribute) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    value <- eval_tidy(call_args(call)[[2]], env = original_env)
    
    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {expected_value}}."
      )
    } else {
      actual_value <- tryCatch(html_value(x, timeout = 0), error = function(e) NULL)

      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {negated_call_name} {.val {expected_value}}.",
        "i" = "Actual value: {.val {actual_value}}."
      )
    }
  } else if (call_name %in% condition_dependencies$css) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    negated_call_name <- switch(
      negated_call_name,
      "has css property" = "is",
      "does not have css property" = "is not"
    )

    name <- eval_tidy(call_args(call)[[2]], env = original_env)
    expected_value <- eval_tidy(call_args(call)[[3]], env = original_env)
    
    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}}'s {.val {name}} CSS property {negated_call_name} {.val {expected_value}}."
      )
    } else {
      actual_value <- tryCatch(html_css_property(x, name, timeout = 0), error = function(e) NULL)

      condition <- c(
        condition,
        "i" = "{.arg {x_name}}'s {.val {name}} CSS property {negated_call_name} {.val {expected_value}}.",
        "i" = "Actual value: {.val {actual_value}}."
      )
    }
  } else if (call_name %in% condition_dependencies$length) {
    if (is.null(negated_call_name)) {
      negated_call_name <- negate_call_name(call_name)
    }

    value <- eval_tidy(call_args(call)[[2]], env = original_env)
    elements <- if(value == 1) "element" else "elements"

    cond <- switch(
      negated_call_name,
      "has length" = paste("contains", value, elements),
      "does not have length" = paste("does not contain", value, elements),
      "has at least" = paste("contains at least", value, elements),
      "does not have at least" = paste("contains less than", value, elements),
    )

    if (is.null(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {cond}."
      )
    } else {
      actual_length <- length(x)
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} {cond}.",
        "i" = "Actual number of elements: {.val {actual_length}}."
      )
    }
  } else if (inherits(x, "selenider_element")) {
    if (is_present(x)) {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} exists, but the condition still failed."
      )
    } else {
      condition <- c(
        condition,
        "i" = "{.arg {x_name}} does not exist, which may have caused the condition to fail."
      )
    }
  } else {
    l <- length(x)

    if (l == 0L) {
      condition <- c(
        condition,
        "i" = "{.arg x} contains no elements, which may have caused the condition to fail."
      )
    } else {
      condition <- c(
        condition,
        "i" = "{.arg x} contains {.val {l}} elements, but the condition still failed."
      )
    }
  }
  
  if (testthat) {
    # We have already checked that testthat is installed if `testthat` is `TRUE`.
    html_expect_fail(condition, parent = parent, call = call_env, x = x, x_name = x_name)
  } else {
    stop_expect_error(condition, parent = parent, call = call_env)
  }
}

negate_call_name <- function(x) {
  # Replace e.g. !is present with is not present, but cancel out double negatives
  switch(x,
    "is present" = "is not present",
    "is in dom" = "is not in the DOM",
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
    NULL
  )
}
