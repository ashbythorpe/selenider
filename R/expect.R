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
#' @param timeout The number of seconds to wait for a condition to pass. If not
#'   specified, the timeout used for `x` will be used, or the timeout of the
#'   local session if an element is not given.
#' 
#' @details 
#'
#' # Custom conditions
#' Any function which takes a selenider element or element collection as its
#' first argument, and returns a logical value, can be used as a condition.
#'
#' # Conditions
#' Conditions can be supplied as functions or calls.
#' 
#' Functions allow you to use unary conditions without formatting them as a
#' call (e.g. `exists` rather than `exists()`). It also allows you to make
#' use of R's [anonymous function syntax][base::function] to quickly create
#' custom conditions. `x` will be used as the first argument of this function.
#' 
#' Function calls allow you to use conditions that take multiple arguments (e.g.
#' `has_text()`) without the use of an intermediate function. The call will
#' be modified so that `x` is the first argument to the function call. For 
#' example, `has_text("a")` will be modified to become: `has_text(x, "a")`.
#' 
#' @returns 
#' `html_expect()` returns the element(s) `x` when the test passes, or `NULL` if
#' an element or collection of elements was not given in `x`.
#' 
#' `html_wait_for()` returns a boolean flag: TRUE if the test passes, FALSE
#' otherwise.
#' 
#' @seealso 
#' * [`html-conditions`] for predicates for a single HTML element.
#' * [`html-conditions-multiple`] for predicates for multiple HTML elements.
#' 
#' @examples
#' \dontshow{
#' # This allows `local_session()` to work when being sourced.
#' prev_options <- options(withr.hook_source = TRUE)
#' }
#' session <- mock_selenider_session()
#'
#' s(".class1") |>
#'   html_expect(exists)
#'
#' s(".class1") |>
#'   html_expect(is_visible, is_enabled)
#' 
#' s(".class1") |>
#'   html_expect(is_visible || is_enabled)
#' 
#' s(".class2") |>
#'   html_expect(!exists) |>
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
#' # This is useful for complex conditions:
#' s(".text1") |>
#'   html_expect(
#'     \(elem) elem |>
#'       html_text() |>
#'       grepl("Example .*", x = _)
#'   )
#' 
#' # If your conditions are not specific to an element, you can omit the `x` argument
#' elem_1 <- s(".class1")
#' elem_2 <- s(".class2")
#'
#' html_expect(exists(elem_1) || exists(elem_2))
#' 
#' # We can now use the conditions on their own to figure out which element exists
#' if (exists(elem_1)) {
#'   click(elem_1)
#' } else {
#'   click(elem_2)
#' }
#'
#' # Use html_wait_for() to handle failures manually
#' elem <- s(".class2")
#' if (html_wait_until(elem, exists)) {
#'   click(elem)
#' } else {
#'   reload()
#' }
#' \dontshow{
#' options(prev_options)
#' }
#' 
#' @export
html_expect <- function(x, ..., timeout = NULL) {
  x <- rlang::enquo(x)
  dots <- rlang::enquos(...)
  
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

    diagnose_condition(x_res, n, call, expr, val, timeout)
  }

  if (inherits(x_res, c("selenider_element", "selenider_elements"))) {
    x_res
  } else {
    NULL
  }
}

diagnose_condition <- function(x, n, call, original_expr, result, timeout, call_name = NULL, negated_call_name = NULL, call_env = rlang::caller_env()) {
  if (is.null(call_name)) {
    call_name <- if (rlang::is_call_simple(call)) rlang::call_name(call) else ""
  }
  
  expr_print <- paste0(
    rlang::expr_deparse(rlang::quo_squash(original_expr)), collapse = "\n"
  )

  condition <- c(
    "Condition failed after waiting for {timeout} seconds:",
    "{.code {expr_print}}"
  )
  
  call_name <- switch(call_name,
      is_in_dom = "is in the DOM",
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

      if (is.function(result) && n == 1) {
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
      new_call <- rlang::call_args(call)[[1]]
      inner_call_name <- if (rlang::is_call_simple(new_call)) rlang::call_name(new_call) else ""
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
      "i" = "{.arg x} {negated_call_name}."
    )
  } else if (!is.null(x)) {
    if (exists(x)) {
      condition <- c(
        condition,
        "i" = "{.arg x} exists, but the condition still failed."
      )
    } else {
      condition <- c(
        condition,
        "i" = "{.arg x} does not exist, which may have caused the condition to fail."
      )
    }
  }

  stop_expect_error(condition, parent = parent, call = call_env)
}

negate_call_name <- function(x) {
  # Replace e.g. !exists with doesn't exist, but cancel out double negatives
  switch(x,
    "exists" = "doesn't exist",
    "is present" = "is not present",
    "is in_dom" = "is not in the DOM",
    "is missing" = "is present", 
    "is absent" = "is in the DOM",
    "is visible" = "is not visible",
    "is displayed" = "is not displayed",
    "is hidden" = "is displayed", 
    "is invisible" = "is visible",
    "is enabled" = "is not enabled",
    "is disabled" = "is enabled",
    NULL
  )
}
